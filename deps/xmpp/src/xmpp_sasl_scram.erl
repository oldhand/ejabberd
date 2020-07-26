%%%-------------------------------------------------------------------
%%% @author Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%% Copyright (C) 2002-2018 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(xmpp_sasl_scram).
-behaviour(xmpp_sasl).
-author('stephen.roettger@googlemail.com').
-protocol({rfc, 5802}).

-export([mech_new/4, mech_step/2, format_error/1]).

-include("scram.hrl").

-type password() :: binary() | #scram{}.
-type get_password_fun() :: fun((binary()) -> {false | password(), module()}).

-record(state,
	{step = 2              :: 2 | 4,
         stored_key = <<"">>   :: binary(),
         server_key = <<"">>   :: binary(),
         username = <<"">>     :: binary(),
	 auth_module           :: module(),
         get_password          :: get_password_fun(),
         auth_message = <<"">> :: binary(),
         client_nonce = <<"">> :: binary(),
	 server_nonce = <<"">> :: binary()}).

-define(SALT_LENGTH, 16).
-define(NONCE_LENGTH, 16).

-type error_reason() :: unsupported_extension | bad_username |
			not_authorized | saslprep_failed |
			parser_failed | bad_attribute |
			nonce_mismatch | bad_channel_binding.

-export_type([error_reason/0]).

-spec format_error(error_reason()) -> {atom(), binary()}.
format_error(unsupported_extension) ->
    {'bad-protocol', <<"Unsupported extension">>};
format_error(bad_username) ->
    {'invalid-authzid', <<"Malformed username">>};
format_error(not_authorized) ->
    {'not-authorized', <<"Invalid username or password">>};
format_error(saslprep_failed) ->
    {'not-authorized', <<"SASLprep failed">>};
format_error(parser_failed) ->
    {'bad-protocol', <<"Response decoding failed">>};
format_error(bad_attribute) ->
    {'bad-protocol', <<"Malformed or unexpected attribute">>};
format_error(nonce_mismatch) ->
    {'bad-protocol', <<"Nonce mismatch">>};
format_error(bad_channel_binding) ->
    {'bad-protocol', <<"Invalid channel binding">>}.

mech_new(_Host, GetPassword, _CheckPassword, _CheckPasswordDigest) ->
    #state{step = 2, get_password = GetPassword}.

mech_step(#state{step = 2} = State, ClientIn) ->
    case re:split(ClientIn, <<",">>, [{return, binary}]) of
      [_CBind, _AuthorizationIdentity, _UserNameAttribute, _ClientNonceAttribute, ExtensionAttribute | _]
	  when ExtensionAttribute /= <<"">> ->
	  {error, unsupported_extension};
      [CBind, _AuthorizationIdentity, UserNameAttribute, ClientNonceAttribute | _]
	  when (CBind == <<"y">>) or (CBind == <<"n">>) ->
	  case parse_attribute(UserNameAttribute) of
	    {error, Reason} -> {error, Reason};
	    {_, EscapedUserName} ->
		case unescape_username(EscapedUserName) of
		  error -> {error, bad_username};
		  UserName ->
		      case parse_attribute(ClientNonceAttribute) of
			{$r, ClientNonce} ->
			    {Pass, AuthModule} = (State#state.get_password)(UserName),
			    LPass = if is_binary(Pass) -> jid:resourceprep(Pass);
				       true -> Pass
				    end,
			    if Pass == false ->
				  {error, not_authorized, UserName};
			       LPass == error ->
				  {error, saslprep_failed, UserName};
			       true ->
				  {StoredKey, ServerKey, Salt, IterationCount} =
				      if is_record(Pass, scram) ->
					      {base64:decode(Pass#scram.storedkey),
					       base64:decode(Pass#scram.serverkey),
					       base64:decode(Pass#scram.salt),
					       Pass#scram.iterationcount};
					 true ->
					     TempSalt =
						 p1_rand:bytes(?SALT_LENGTH),
					     SaltedPassword =
						 scram:salted_password(Pass,
								       TempSalt,
								       ?SCRAM_DEFAULT_ITERATION_COUNT),
					     {scram:stored_key(scram:client_key(SaltedPassword)),
					      scram:server_key(SaltedPassword),
					      TempSalt,
					      ?SCRAM_DEFAULT_ITERATION_COUNT}
				      end,
				  ClientFirstMessageBare =
				      substr(ClientIn,
                                                 str(ClientIn, <<"n=">>)),
				  ServerNonce =
				      base64:encode(p1_rand:bytes(?NONCE_LENGTH)),
				  ServerFirstMessage =
                                        iolist_to_binary(
                                          ["r=",
                                           ClientNonce,
                                           ServerNonce,
                                           ",", "s=",
                                           base64:encode(Salt),
                                           ",", "i=",
                                           integer_to_list(IterationCount)]),
				  {continue, ServerFirstMessage,
				   State#state{step = 4, stored_key = StoredKey,
					       server_key = ServerKey,
					       auth_module = AuthModule,
					       auth_message =
						   <<ClientFirstMessageBare/binary,
						     ",", ServerFirstMessage/binary>>,
					       client_nonce = ClientNonce,
					       server_nonce = ServerNonce,
					       username = UserName}}
			    end;
			  _ -> {error, bad_attribute}
		      end
		end
	  end;
      _Else -> {error, parser_failed}
    end;
mech_step(#state{step = 4} = State, ClientIn) ->
    case tokens(ClientIn, <<",">>) of
      [GS2ChannelBindingAttribute, NonceAttribute,
       ClientProofAttribute] ->
	  case parse_attribute(GS2ChannelBindingAttribute) of
	    {$c, CVal} ->
		ChannelBindingSupport = try binary:first(base64:decode(CVal))
					catch _:badarg -> 0
					end,
		if (ChannelBindingSupport == $n)
		  or (ChannelBindingSupport == $y) ->
		    Nonce = <<(State#state.client_nonce)/binary,
				(State#state.server_nonce)/binary>>,
		    case parse_attribute(NonceAttribute) of
			{$r, CompareNonce} when CompareNonce == Nonce ->
			    case parse_attribute(ClientProofAttribute) of
			    {$p, ClientProofB64} ->
				  ClientProof = try base64:decode(ClientProofB64)
						catch _:badarg -> <<>>
						end,
				  AuthMessage = iolist_to_binary(
						    [State#state.auth_message,
						     ",",
						     substr(ClientIn, 1,
								    str(ClientIn, <<",p=">>)
								    - 1)]),
				  ClientSignature =
				    scram:client_signature(State#state.stored_key,
							     AuthMessage),
				  ClientKey = scram:client_key(ClientProof,
							     ClientSignature),
				  CompareStoredKey = scram:stored_key(ClientKey),
				  if CompareStoredKey == State#state.stored_key ->
					 ServerSignature =
					     scram:server_signature(State#state.server_key,
								    AuthMessage),
					 {ok, [{username, State#state.username},
					       {auth_module, State#state.auth_module},
					       {authzid, State#state.username}],
					  <<"v=",
					    (base64:encode(ServerSignature))/binary>>};
				     true -> {error, not_authorized, State#state.username}
				  end;
			    _ -> {error, bad_attribute}
			    end;
			{$r, _} -> {error, nonce_mismatch};
			_ -> {error, bad_attribute}
		    end;
		  true -> {error, bad_channel_binding}
		end;
	    _ -> {error, bad_attribute}
	  end;
      _ -> {error, parser_failed}
    end.

parse_attribute(<<Name, $=, Val/binary>>) when Val /= <<>> ->
    case is_alpha(Name) of
	true -> {Name, Val};
	false -> {error, bad_attribute}
    end;
parse_attribute(_) ->
    {error, bad_attribute}.

unescape_username(<<"">>) -> <<"">>;
unescape_username(EscapedUsername) ->
    Pos = str(EscapedUsername, <<"=">>),
    if Pos == 0 -> EscapedUsername;
       true ->
	   Start = substr(EscapedUsername, 1, Pos - 1),
	   End = substr(EscapedUsername, Pos),
	   EndLen = byte_size(End),
	   if EndLen < 3 -> error;
	      true ->
		  case substr(End, 1, 3) of
		    <<"=2C">> ->
			<<Start/binary, ",",
			  (unescape_username(substr(End, 4)))/binary>>;
		    <<"=3D">> ->
			<<Start/binary, "=",
			  (unescape_username(substr(End, 4)))/binary>>;
		    _Else -> error
		  end
	   end
    end.

is_alpha(Char) when Char >= $a, Char =< $z -> true;
is_alpha(Char) when Char >= $A, Char =< $Z -> true;
is_alpha(_) -> false.

-spec str(binary(), binary()) -> non_neg_integer().
str(B1, B2) ->
    case binary:match(B1, B2) of
        {R, _Len} -> R+1;
        _ -> 0
    end.

-spec substr(binary(), pos_integer()) -> binary().
substr(B, N) ->
    binary_part(B, N-1, byte_size(B)-N+1).

-spec substr(binary(), pos_integer(), non_neg_integer()) -> binary().
substr(B, S, E) ->
    binary_part(B, S-1, E).

-spec tokens(binary(), binary()) -> [binary()].
tokens(B1, B2) ->
    [iolist_to_binary(T) ||
        T <- string:tokens(binary_to_list(B1), binary_to_list(B2))].
