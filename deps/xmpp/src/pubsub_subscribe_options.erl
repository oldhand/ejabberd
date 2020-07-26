%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_options
%% Document: XEP-0060

-module(pubsub_subscribe_options).

-export([encode/1, encode/2]).

-export([decode/1, decode/2, format_error/1,
	 io_format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_subscribe_options.hrl").

-export_type([property/0, result/0, form/0]).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

dec_bool(<<"1">>) -> true;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"false">>) -> false.

enc_bool(true) -> <<"1">>;
enc_bool(false) -> <<"0">>.

format_error({form_type_mismatch, Type}) ->
    <<"FORM_TYPE doesn't match '", Type/binary, "'">>;
format_error({bad_var_value, Var, Type}) ->
    <<"Bad value of field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({missing_value, Var, Type}) ->
    <<"Missing value of field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({too_many_values, Var, Type}) ->
    <<"Too many values for field '", Var/binary,
      "' of type '", Type/binary, "'">>;
format_error({unknown_var, Var, Type}) ->
    <<"Unknown field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({missing_required_var, Var, Type}) ->
    <<"Missing required field '", Var/binary, "' of type '",
      Type/binary, "'">>.

io_format_error({form_type_mismatch, Type}) ->
    {<<"FORM_TYPE doesn't match '~s'">>, [Type]};
io_format_error({bad_var_value, Var, Type}) ->
    {<<"Bad value of field '~s' of type '~s'">>,
     [Var, Type]};
io_format_error({missing_value, Var, Type}) ->
    {<<"Missing value of field '~s' of type "
       "'~s'">>,
     [Var, Type]};
io_format_error({too_many_values, Var, Type}) ->
    {<<"Too many values for field '~s' of type "
       "'~s'">>,
     [Var, Type]};
io_format_error({unknown_var, Var, Type}) ->
    {<<"Unknown field '~s' of type '~s'">>, [Var, Type]};
io_format_error({missing_required_var, Var, Type}) ->
    {<<"Missing required field '~s' of type "
       "'~s'">>,
     [Var, Type]}.

decode(Fs) -> decode(Fs, []).

decode(Fs, Acc) ->
    case lists:keyfind(<<"FORM_TYPE">>, #xdata_field.var,
		       Fs)
	of
      false ->
	  decode(Fs, Acc,
		 <<"http://jabber.org/protocol/pubsub#subscribe_o"
		   "ptions">>,
		 []);
      #xdata_field{values = [XMLNS]}
	  when XMLNS ==
		 <<"http://jabber.org/protocol/pubsub#subscribe_o"
		   "ptions">> ->
	  decode(Fs, Acc, XMLNS, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end.

encode(Cfg) -> encode(Cfg, <<"en">>).

encode(List, Lang) when is_list(List) ->
    Fs = [case Opt of
	    {deliver, Val} -> [encode_deliver(Val, Lang)];
	    {deliver, _, _} -> erlang:error({badarg, Opt});
	    {digest, Val} -> [encode_digest(Val, Lang)];
	    {digest, _, _} -> erlang:error({badarg, Opt});
	    {digest_frequency, Val} ->
		[encode_digest_frequency(Val, Lang)];
	    {digest_frequency, _, _} -> erlang:error({badarg, Opt});
	    {expire, Val} -> [encode_expire(Val, Lang)];
	    {expire, _, _} -> erlang:error({badarg, Opt});
	    {include_body, Val} -> [encode_include_body(Val, Lang)];
	    {include_body, _, _} -> erlang:error({badarg, Opt});
	    {'show-values', Val} ->
		['encode_show-values'(Val, default, Lang)];
	    {'show-values', Val, Opts} ->
		['encode_show-values'(Val, Opts, Lang)];
	    {subscription_type, Val} ->
		[encode_subscription_type(Val, default, Lang)];
	    {subscription_type, Val, Opts} ->
		[encode_subscription_type(Val, Opts, Lang)];
	    {subscription_depth, Val} ->
		[encode_subscription_depth(Val, default, Lang)];
	    {subscription_depth, Val, Opts} ->
		[encode_subscription_depth(Val, Opts, Lang)];
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#subscribe_o"
				   "ptions">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#deliver">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{deliver, Result} | Acc], XMLNS, Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#deliver">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#deliver">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#deliver">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#deliver">>} | _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#deliver">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#digest">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{digest, Result} | Acc], XMLNS, Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#digest">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#digest">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#digest">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#digest">>} | _], _,
       XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#digest">>, XMLNS}});
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{digest_frequency, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#digest_frequency">>, XMLNS}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#digest_frequency">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#digest_frequency">>,
		   XMLNS}});
decode([#xdata_field{var = <<"pubsub#expire">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{expire, Result} | Acc], XMLNS, Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#expire">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#expire">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#expire">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#expire">>} | _], _,
       XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#expire">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#include_body">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{include_body, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#include_body">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#include_body">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#include_body">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#include_body">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#include_body">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#show-values">>,
		     values = Values}
	| Fs],
       Acc, XMLNS, Required) ->
    try [dec_enum(Value, [away, chat, dnd, online, xa])
	 || Value <- Values]
    of
      Result ->
	  decode(Fs, [{'show-values', Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#show-values">>, XMLNS}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_enum(Value, [items, nodes]) of
      Result ->
	  decode(Fs, [{subscription_type, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscription_type">>, XMLNS}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#subscription_type">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscription_type">>,
		   XMLNS}});
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_enum(Value, ['1', all]) of
      Result ->
	  decode(Fs, [{subscription_depth, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscription_depth">>,
			 XMLNS}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#subscription_depth">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscription_depth">>,
		   XMLNS}});
decode([#xdata_field{var = Var} | Fs], Acc, XMLNS,
       Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE, {unknown_var, Var, XMLNS}});
       true -> decode(Fs, Acc, XMLNS, Required)
    end;
decode([], Acc, _, []) -> Acc.

encode_deliver(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#deliver">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"Whether an entity wants to receive or "
				  "disable notifications">>)}.

encode_digest(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#digest">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"Whether an entity wants to receive digests "
				  "(aggregations) of notifications or all "
				  "notifications individually">>)}.

encode_digest_frequency(Value, Lang) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#digest_frequency">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"The minimum number of milliseconds between "
				  "sending any two notification digests">>)}.

encode_expire(Value, Lang) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#expire">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"The DateTime at which a leased subscription "
				  "will end or has ended">>)}.

encode_include_body(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#include_body">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"Whether an entity wants to receive an "
				  "XMPP message body in addition to the "
				  "payload format">>)}.

'encode_show-values'(Value, Options, Lang) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_enum(V) || V <- Value]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"XMPP Show Value of Away">>),
				 value = <<"away">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"XMPP Show Value of Chat">>),
				 value = <<"chat">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"XMPP Show Value of DND (Do Not Disturb)">>),
				 value = <<"dnd">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Mere Availability in XMPP (No Show Value)">>),
				 value = <<"online">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"XMPP Show Value of XA (Extended Away)">>),
				 value = <<"xa">>}];
	      true ->
		  [#xdata_option{label = xmpp_tr:tr(Lang, L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#show-values">>,
		 values = Values, required = false, type = 'list-multi',
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"The presence states for which an entity "
				  "wants to receive notifications">>)}.

encode_subscription_type(Value, Options, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Receive notification of new items only">>),
				 value = <<"items">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Receive notification of new nodes only">>),
				 value = <<"nodes">>}];
	      true ->
		  [#xdata_option{label = xmpp_tr:tr(Lang, L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#subscription_type">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>, label = <<>>}.

encode_subscription_depth(Value, Options, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Receive notification from direct child "
						  "nodes only">>),
				 value = <<"1">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Receive notification from all descendent "
						  "nodes">>),
				 value = <<"all">>}];
	      true ->
		  [#xdata_option{label = xmpp_tr:tr(Lang, L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#subscription_depth">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>, label = <<>>}.
