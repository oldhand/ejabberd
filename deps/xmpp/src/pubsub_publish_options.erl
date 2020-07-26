%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_publish_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#publish-options
%% Document: XEP-0060

-module(pubsub_publish_options).

-export([encode/1, encode/2]).

-export([decode/1, decode/2, format_error/1,
	 io_format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_publish_options.hrl").

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
		 <<"http://jabber.org/protocol/pubsub#publish-opt"
		   "ions">>,
		 []);
      #xdata_field{values = [XMLNS]}
	  when XMLNS ==
		 <<"http://jabber.org/protocol/pubsub#publish-opt"
		   "ions">> ->
	  decode(Fs, Acc, XMLNS, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#publish-opt"
			   "ions">>}})
    end.

encode(Cfg) -> encode(Cfg, <<"en">>).

encode(List, Lang) when is_list(List) ->
    Fs = [case Opt of
	    {persist_items, Val} ->
		[encode_persist_items(Val, Lang)];
	    {persist_items, _, _} -> erlang:error({badarg, Opt});
	    {access_model, Val} ->
		[encode_access_model(Val, default, Lang)];
	    {access_model, Val, Opts} ->
		[encode_access_model(Val, Opts, Lang)];
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#publish-opt"
				   "ions">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#persist_items">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{persist_items, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#persist_items">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#persist_items">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#persist_items">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#persist_items">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#persist_items">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#access_model">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_enum(Value,
		 [authorize, open, presence, roster, whitelist])
    of
      Result ->
	  decode(Fs, [{access_model, Result} | Acc], XMLNS,
		 Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#access_model">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#access_model">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#access_model">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#access_model">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#access_model">>, XMLNS}});
decode([#xdata_field{var = Var} | Fs], Acc, XMLNS,
       Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE, {unknown_var, Var, XMLNS}});
       true -> decode(Fs, Acc, XMLNS, Required)
    end;
decode([], Acc, _, []) -> Acc.

encode_persist_items(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#persist_items">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang, <<"Persist items to storage">>)}.

encode_access_model(Value, Options, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Access model of authorize">>),
				 value = <<"authorize">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Access model of open">>),
				 value = <<"open">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Access model of presence">>),
				 value = <<"presence">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Access model of roster">>),
				 value = <<"roster">>},
		   #xdata_option{label =
				     xmpp_tr:tr(Lang,
						<<"Access model of whitelist">>),
				 value = <<"whitelist">>}];
	      true ->
		  [#xdata_option{label = xmpp_tr:tr(Lang, L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#access_model">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang, <<"Specify the access model">>)}.
