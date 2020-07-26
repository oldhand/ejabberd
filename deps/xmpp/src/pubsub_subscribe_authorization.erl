%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_authorization.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_authorization
%% Document: XEP-0060

-module(pubsub_subscribe_authorization).

-export([encode/1, encode/2]).

-export([decode/1, decode/2, format_error/1,
	 io_format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_subscribe_authorization.hrl").

-export_type([property/0, result/0, form/0]).

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
		 <<"http://jabber.org/protocol/pubsub#subscribe_a"
		   "uthorization">>,
		 [<<"pubsub#allow">>, <<"pubsub#node">>,
		  <<"pubsub#subscriber_jid">>]);
      #xdata_field{values = [XMLNS]}
	  when XMLNS ==
		 <<"http://jabber.org/protocol/pubsub#subscribe_a"
		   "uthorization">> ->
	  decode(Fs, Acc, XMLNS,
		 [<<"pubsub#allow">>, <<"pubsub#node">>,
		  <<"pubsub#subscriber_jid">>]);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end.

encode(Cfg) -> encode(Cfg, <<"en">>).

encode(List, Lang) when is_list(List) ->
    Fs = [case Opt of
	    {allow, Val} -> [encode_allow(Val, Lang)];
	    {allow, _, _} -> erlang:error({badarg, Opt});
	    {node, Val} -> [encode_node(Val, Lang)];
	    {node, _, _} -> erlang:error({badarg, Opt});
	    {subscriber_jid, Val} ->
		[encode_subscriber_jid(Val, Lang)];
	    {subscriber_jid, _, _} -> erlang:error({badarg, Opt});
	    {subid, Val} -> [encode_subid(Val, Lang)];
	    {subid, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#subscribe_a"
				   "uthorization">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#allow">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow, Result} | Acc], XMLNS,
		 lists:delete(<<"pubsub#allow">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#allow">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#allow">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#allow">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#allow">>} | _], _,
       XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#allow">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#node">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{node, Result} | Acc], XMLNS,
		 lists:delete(<<"pubsub#node">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#node">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#node">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#node">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#node">>} | _], _,
       XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#node">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try jid:decode(Value) of
      Result ->
	  decode(Fs, [{subscriber_jid, Result} | Acc], XMLNS,
		 lists:delete(<<"pubsub#subscriber_jid">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscriber_jid">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#subscriber_jid">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>}
	| _],
       _, XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscriber_jid">>, XMLNS}});
decode([#xdata_field{var = <<"pubsub#subid">>,
		     values = [Value]}
	| Fs],
       Acc, XMLNS, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{subid, Result} | Acc], XMLNS, Required)
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subid">>, XMLNS}})
    end;
decode([#xdata_field{var = <<"pubsub#subid">>,
		     values = []} =
	    F
	| Fs],
       Acc, XMLNS, Required) ->
    decode([F#xdata_field{var = <<"pubsub#subid">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, XMLNS, Required);
decode([#xdata_field{var = <<"pubsub#subid">>} | _], _,
       XMLNS, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subid">>, XMLNS}});
decode([#xdata_field{var = Var} | Fs], Acc, XMLNS,
       Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE, {unknown_var, Var, XMLNS}});
       true -> decode(Fs, Acc, XMLNS, Required)
    end;
decode([], _, XMLNS, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var, XMLNS}});
decode([], Acc, _, []) -> Acc.

encode_allow(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#allow">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"Allow this Jabber ID to subscribe to "
				  "this pubsub node?">>)}.

encode_node(Value, Lang) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#node">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>, label = xmpp_tr:tr(Lang, <<"Node ID">>)}.

encode_subscriber_jid(Value, Lang) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [jid:encode(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#subscriber_jid">>,
		 values = Values, required = false, type = 'jid-single',
		 options = Opts, desc = <<>>,
		 label = xmpp_tr:tr(Lang, <<"Subscriber Address">>)}.

encode_subid(Value, Lang) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#subid">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label =
		     xmpp_tr:tr(Lang,
				<<"The subscription identifier associated "
				  "with the subscription request">>)}.
