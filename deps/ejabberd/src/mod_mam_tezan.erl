%%%-------------------------------------------------------------------
%%% File    : mod_mam_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 15 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_mam_tezan).

-behaviour(mod_mam).

%% API
-export([init/2, remove_user/2, remove_room/3, delete_old_messages/3,
	 extended_fields/0, store/8, store/9, write_prefs/4, get_prefs/2, select/6, remove_from_archive/3]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_mam.hrl").

-define(BIN_GREATER_THAN(A, B),
	((A > B andalso byte_size(A) == byte_size(B))
	 orelse byte_size(A) > byte_size(B))).
-define(BIN_LESS_THAN(A, B),
	((A < B andalso byte_size(A) == byte_size(B))
	 orelse byte_size(A) < byte_size(B))).

-define(TABLE_SIZE_LIMIT, 2000000000). % A bit less than 2 GiB.

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, archive_msg,
			[{disc_only_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, archive_msg)}]),
    ejabberd_mnesia:create(?MODULE, archive_prefs,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, archive_prefs)}]).

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({archive_msg, US}),
		mnesia:delete({archive_prefs, US})
	end,
    mnesia:transaction(F).

remove_room(_LServer, LName, LHost) ->
    remove_user(LName, LHost).

remove_from_archive(LUser, LServer, none) ->
    US = {LUser, LServer},
    case mnesia:transaction(fun () -> mnesia:delete({archive_msg, US}) end) of
	{atomic, _} -> ok;
	{aborted, Reason} -> {error, Reason}
    end;
remove_from_archive(LUser, LServer, WithJid) ->
    US = {LUser, LServer},
    Peer = jid:remove_resource(jid:split(WithJid)),
    F = fun () ->
	    Msgs = mnesia:match_object(#archive_msg{us = US, bare_peer = Peer, _ = '_'}),
	    lists:foreach(fun mnesia:delete_object/1, Msgs)
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} -> {error, Reason}
    end.

delete_old_messages(global, TimeStamp, Type) ->
    mnesia:change_table_copy_type(archive_msg, node(), disc_copies),
    Result = delete_old_user_messages(mnesia:dirty_first(archive_msg), TimeStamp, Type),
    mnesia:change_table_copy_type(archive_msg, node(), disc_only_copies),
    Result.

delete_old_user_messages('$end_of_table', _TimeStamp, _Type) ->
    ok;
delete_old_user_messages(User, TimeStamp, Type) ->
    F = fun() ->
		Msgs = mnesia:read(archive_msg, User),
		Keep = lists:filter(
			 fun(#archive_msg{timestamp = MsgTS,
					  type = MsgType}) ->
				 MsgTS >= TimeStamp orelse (Type /= all andalso
							    Type /= MsgType)
			 end, Msgs),
		if length(Keep) < length(Msgs) ->
			mnesia:delete({archive_msg, User}),
			lists:foreach(fun(Msg) -> mnesia:write(Msg) end, Keep);
		   true ->
			ok
		end
	end,
    NextRecord = mnesia:dirty_next(archive_msg, User),
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    delete_old_user_messages(NextRecord, TimeStamp, Type);
	{aborted, Err} ->
	    ?ERROR_MSG("Cannot delete old MAM messages: ~s", [Err]),
	    Err
    end.

extended_fields() ->
    [].
	
ignoreMsgtype(<<"1">>) -> false;
ignoreMsgtype(<<"2">>) -> false;
ignoreMsgtype(<<"3">>) -> false;
ignoreMsgtype(<<"4">>) -> false;
ignoreMsgtype(<<"5">>) -> false;
ignoreMsgtype(<<"7">>) -> false;
ignoreMsgtype(<<"8">>) -> false;
ignoreMsgtype(<<"10">>) -> false;
ignoreMsgtype(<<"11">>) -> false;
ignoreMsgtype(<<"12">>) -> false;
ignoreMsgtype(<<"13">>) -> false;
ignoreMsgtype(<<"14">>) -> false;
ignoreMsgtype(<<"15">>) -> false;
ignoreMsgtype(<<"16">>) -> false;
ignoreMsgtype(<<"17">>) -> false;
ignoreMsgtype(<<"18">>) -> false;
ignoreMsgtype(_) -> true.




 
 
decrypt_public(Ciphertext,Key) when is_binary(Ciphertext)-> decrypt_public(binary_to_list(Ciphertext),Key);
decrypt_public(Ciphertext,Key) ->
    ChunkList = [ lists:sublist(Ciphertext, X, 128) || X <- lists:seq(1,length(Ciphertext),128) ],  
	Fun = fun(X) ->  binary_to_list(public_key:decrypt_public(list_to_binary(X), Key,[{rsa_padding, rsa_pkcs1_padding}])) end, 
	lists:flatten(lists:map(Fun,ChunkList)).	
 
 
 
 
store(Pkt, _, {LUser, LServer}, Type, Peer, _Nick, _Dir, _TS) when Type =:= chat ->   
	LPeer = Peer#jid.user,   
    Body = fxml:get_subtag_cdata(Pkt, <<"body">>),
    % SType = misc:atom_to_binary(Type),
	Msgid = fxml:get_tag_attr_s(<<"id">>, Pkt),
	Sub_els = fxml:get_subtag(Pkt, <<"chat">>),
	Msgbar = fxml:get_subtag_cdata(Sub_els, <<"msgbar">>),
	Msgtype = fxml:get_subtag_cdata(Sub_els, <<"msgtype">>),
	Sendtime = fxml:get_subtag_cdata(Sub_els, <<"sendtime">>), 
	xn:application("admin"),  
    Tablename = "im_archivechats",
    case Msgtype =:= <<"6">> of
         true ->
                Public_key = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDI6dGvkKSHB6Q3TE6TKGFR4Nyt\n6XH3gc7/LAzvW0aDNGZjkoA7jrMTBd/T0N/R4miBK7XNMI+4Z/gd8OgS0wShPwyq\nFwv8Q54goPr6dAXAQifzwK+eOs+Avu9rrVfT31i8wJeIzpk1aySoYB40ozasTdXg\nQ2AHZH0AqU/Rne5GuQIDAQAB\n-----END PUBLIC KEY-----">>,
                [Entry] = public_key:pem_decode(Public_key),
				Key = public_key:pem_entry_decode(Entry),
                PlainText = decrypt_public(base64:decode(Body),Key),
		        {ok,Content} = xn_query:create("YearMonthContent"), 
			    xn_query:filter(Content,"type","eic",Tablename),  
				xn_query:filter(Content,"my.deleted","=","0"),    
				xn_query:filter(Content,"my.msgid","=",PlainText),      
			    xn_query:'end'(Content,1), 
				case xn_query:execute(Content) of
					{ok,Archivechats} when Archivechats =/= [] ->    
						 {ok,Archivechat_info} = xn_content:new(hd(Archivechats)), 
						 xn_content:set(Archivechat_info,"deleted",<<"1">>),
						 xn_content:save(Archivechat_info,lists:flatten([Tablename,",",Tablename,"_",binary_to_list(LUser),",",Tablename,"_",binary_to_list(LPeer)])),
						 xn_query:close(Archivechat_info),
						 xn_query:close(Content);
						
					_ -> 
						 xn_query:close(Content)
				end;
         _ ->
            case ignoreMsgtype(Msgtype) of
		          false ->
		           		{ok,Fsm} = xn_content:create(Tablename,"",false,9), 	
						xn_content:set(Fsm,"deleted",<<"0">>),
						xn_content:set(Fsm,"msgfrom",LUser),
						xn_content:set(Fsm,"msgto",LPeer),
						xn_content:set(Fsm,"msgid",Msgid),
						xn_content:set(Fsm,"msgbar",Msgbar),
						xn_content:set(Fsm,"msgtype",Msgtype),
						xn_content:set(Fsm,"sendtime",Sendtime),
						xn_content:set(Fsm,"body",Body),
					 	xn_content:save(Fsm,lists:flatten([Tablename,",",Tablename,"_",binary_to_list(LUser),",",Tablename,"_",binary_to_list(LPeer)])),	 
					 	xn_content:close(Fsm);
				   _ -> ok	 	
		    end
    end.
   
	
	
store(Pkt, _, {LUser, LServer}, Type, Peer, _Nick, _Dir, _TS, Affiliations) ->  
    Groupchatid =  LUser, 
	LPeer = Peer#jid.user,   
    Body = fxml:get_subtag_cdata(Pkt, <<"body">>),
    % SType = misc:atom_to_binary(Type),
	Msgid = fxml:get_tag_attr_s(<<"id">>, Pkt),
	Sub_els = fxml:get_subtag(Pkt, <<"chat">>),
	Msgbar = fxml:get_subtag_cdata(Sub_els, <<"msgbar">>),
	Msgtype = fxml:get_subtag_cdata(Sub_els, <<"msgtype">>),
	Sendtime = fxml:get_subtag_cdata(Sub_els, <<"sendtime">>), 
	%%io:format("____Affiliations_____~p____~n", [_Affiliations]),
	%%io:format("___mod_mam:store_____~p____~n", [{LPeer,Msgid,Body,Msgbar,Msgtype,Sendtime}]),
	xn:application("admin"),  
    Tablename = "im_archivegroupchats",
    case Msgtype =:= <<"6">> of
         true ->
         		Public_key = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDI6dGvkKSHB6Q3TE6TKGFR4Nyt\n6XH3gc7/LAzvW0aDNGZjkoA7jrMTBd/T0N/R4miBK7XNMI+4Z/gd8OgS0wShPwyq\nFwv8Q54goPr6dAXAQifzwK+eOs+Avu9rrVfT31i8wJeIzpk1aySoYB40ozasTdXg\nQ2AHZH0AqU/Rne5GuQIDAQAB\n-----END PUBLIC KEY-----">>,
                [Entry] = public_key:pem_decode(Public_key),
				Key = public_key:pem_entry_decode(Entry),
                PlainText = decrypt_public(base64:decode(Body),Key),
		        {ok,Content} = xn_query:create("YearMonthContent"), 
			    xn_query:filter(Content,"type","eic",Tablename),  
				xn_query:filter(Content,"my.deleted","=","0"),    
				xn_query:filter(Content,"my.msgid","=",PlainText),      
			    xn_query:'end'(Content,1), 
				case xn_query:execute(Content) of
					{ok,Archivechats} when Archivechats =/= [] ->    
						 {ok,Archivechat_info} = xn_content:new(hd(Archivechats)), 
						 xn_content:set(Archivechat_info,"deleted",<<"1">>),
						 xn_content:save(Archivechat_info,lists:flatten([Tablename,",",Tablename,"_",binary_to_list(LUser),",",Tablename,"_",binary_to_list(LPeer)])),
						 xn_query:close(Archivechat_info),
						 xn_query:close(Content);
						
					_ -> 
						 xn_query:close(Content)
				end;
         _ ->
         	case ignoreMsgtype(Msgtype) of
		          false ->
		               {ok,Fsm} = xn_content:create(Tablename,"",false,9), 	
						xn_content:set(Fsm,"deleted",<<"0">>),
					 	xn_content:set(Fsm,"groupchatid",Groupchatid),
						xn_content:set(Fsm,"msgfrom",LPeer), 
						xn_content:set(Fsm,"msgid",Msgid),
						xn_content:set(Fsm,"msgbar",Msgbar),
						xn_content:set(Fsm,"msgtype",Msgtype),
						xn_content:set(Fsm,"sendtime",Sendtime),
						xn_content:set(Fsm,"affiliations",Affiliations),
						xn_content:set(Fsm,"body",Body),
					 	xn_content:save(Fsm,lists:flatten([Tablename,",",Tablename,"_",binary_to_list(Groupchatid),",",Tablename,"_",binary_to_list(LPeer)])),	 
					 	xn_content:close(Fsm);
		          _ -> ok
		    end
    end.
    
	
write_prefs(_LUser, _LServer, Prefs, _ServerHost) ->
    mnesia:dirty_write(Prefs).

get_prefs(LUser, LServer) ->
    case mnesia:dirty_read(archive_prefs, {LUser, LServer}) of
	[Prefs] ->
	    {ok, Prefs};
	_ ->
	    error
    end.

select(_LServer, JidRequestor,
       #jid{luser = LUser, lserver = LServer} = JidArchive,
       Query, RSM, MsgType) ->
    Start = proplists:get_value(start, Query),
    End = proplists:get_value('end', Query),
    With = proplists:get_value(with, Query),
    LWith = if With /= undefined -> jid:tolower(With);
	       true -> undefined
	    end,
    MS = make_matchspec(LUser, LServer, Start, End, LWith),
    Msgs = mnesia:dirty_select(archive_msg, MS),
    SortedMsgs = lists:keysort(#archive_msg.timestamp, Msgs),
    {FilteredMsgs, IsComplete} = filter_by_rsm(SortedMsgs, RSM),
    Count = length(Msgs),
    Result = {lists:flatmap(
		fun(Msg) ->
			case mod_mam:msg_to_el(
			       Msg, MsgType, JidRequestor, JidArchive) of
			    {ok, El} ->
				[{Msg#archive_msg.id,
				  binary_to_integer(Msg#archive_msg.id),
				  El}];
			    {error, _} ->
				[]
			end
		end, FilteredMsgs), IsComplete, Count},
    erlang:garbage_collect(),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_matchspec(LUser, LServer, Start, undefined, With) ->
    %% List is always greater than a tuple
    make_matchspec(LUser, LServer, Start, [], With);
make_matchspec(LUser, LServer, Start, End, {_, _, <<>>} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       bare_peer = BPeer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 BPeer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, {_, _, _} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 Peer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, undefined) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer} ->
	      Msg
      end).

filter_by_rsm(Msgs, undefined) ->
    {Msgs, true};
filter_by_rsm(_Msgs, #rsm_set{max = Max}) when Max < 0 ->
    {[], true};
filter_by_rsm(Msgs, #rsm_set{max = Max, before = Before, 'after' = After}) ->
    NewMsgs = if is_binary(After), After /= <<"">> ->
		      lists:filter(
			fun(#archive_msg{id = I}) ->
				?BIN_GREATER_THAN(I, After)
			end, Msgs);
		 is_binary(Before), Before /= <<"">> ->
		      lists:foldl(
			fun(#archive_msg{id = I} = Msg, Acc)
				when ?BIN_LESS_THAN(I, Before) ->
				[Msg|Acc];
			   (_, Acc) ->
				Acc
			end, [], Msgs);
		 is_binary(Before), Before == <<"">> ->
		      lists:reverse(Msgs);
		 true ->
		      Msgs
	      end,
    filter_by_max(NewMsgs, Max).

filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.
