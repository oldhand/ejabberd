%%%-------------------------------------------------------------------
%%% File    : mod_muc_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_muc_mnesia).

-behaviour(mod_muc).
-behaviour(mod_muc_room).

%% API
-export([init/2, import/3, store_room/5, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4]).
-export([register_online_room/4, unregister_online_room/4, find_online_room/3,
	 get_online_rooms/3, count_online_rooms/2, rsm_supported/0,
	 register_online_user/4, unregister_online_user/4,
	 count_online_rooms_by_user/3, get_online_rooms_by_user/3,
	 get_subscribed_rooms/3]).
-export([set_affiliation/6, set_affiliations/4, get_affiliation/5,
	 get_affiliations/3, search_affiliation/4]).
%% gen_server callbacks
-export([start_link/2, init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3]).
-export([need_transform/1, transform/1]).

-include("mod_muc.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).

-record(content, {application,id, xn_type, title, published, updated, author,datatype, my}). 

%%%===================================================================
%%% API
%%%===================================================================
init(Host, Opts) ->
	io:format("___________muc________mnesia__________~n"),
    Spec = {?MODULE, {?MODULE, start_link, [Host, Opts]},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

start_link(Host, Opts) ->
    Name = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Name}, ?MODULE, [Host, Opts], []).

store_room(_LServer, Host, Name, Opts, _) ->
	io:format("__________store_room__________~p________~n",[{Host, Name,Opts}]),
    F = fun () ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(_LServer, Host, Name) ->
	io:format("__________restore_room__________~p________~n",[{Host, Name}]),
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
	[#muc_room{opts = Opts}] -> Opts;
	_ -> error
    end.

forget_room(_LServer, Host, Name) ->
	io:format("__________forget_room__________~p________~n",[{Host, Name}]),
    F = fun () -> mnesia:delete({muc_room, {Name, Host}})
	end,
    mnesia:transaction(F).

can_use_nick(_LServer, Host, JID, Nick) ->
    {LUser, LServer, _} = jid:tolower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_select(muc_registered,
				   [{#muc_registered{us_host = '$1',
						     nick = Nick, _ = '_'},
				     [{'==', {element, 2, '$1'}, Host}],
				     ['$_']}])
	of
      {'EXIT', _Reason} -> true;
      [] -> true;
      [#muc_registered{us_host = {U, _Host}}] -> U == LUS
    end.

get_room_opts(Affiliations) ->
       [{title,<<>>},
        {description,<<>>},
        {allow_change_subj,true},
        {allow_query_users,true},
        {allow_private_messages,true},
        {allow_private_messages_from_visitors,anyone},
        {allow_visitor_status,true},
        {allow_visitor_nickchange,true},
        {public,false},
        {public_list,true},
        {persistent,true},
        {moderated,true},
        {members_by_default,true},
        {members_only,true},
        {allow_user_invites,true},
        {password_protected,false},
        {captcha_protected,false},
        {password,<<>>},
        {anonymous,true},
        {logging,false},
        {max_users,500},
        {allow_voice_requests,true},
        {allow_subscription,true},
        {mam,true},
        {presence_broadcast,[moderator,participant,visitor]},
        {voice_request_min_interval,1800},
        {vcard,<<>>},
        {vcard_xupdate,undefined},
        {pubsub,<<>>},
        {lang,<<>>},
        {captcha_whitelist,[]},
        {affiliations,Affiliations},
        {subject,[]},
        {subject_author,<<>>},
        {subscribers,[]}].

content_get_my([{Fieldname,_,Value}|_],Fieldname) -> {ok,Value}; 
content_get_my([_|R],Fieldname) -> content_get_my(R,Fieldname);
content_get_my([],_) -> {error,"not found"}.

im_get_groupchat_info(Info) ->  
    Groupchatid = Info#content.id,
	My = Info#content.my,
	Author = Info#content.author, 
	case content_get_my(My,<<"profileid">>) of
		{ok,Owner} -> {Groupchatid,Owner};
		_ -> 
		    {Groupchatid,Author}
	end.
	
im_get_groupchatprofile_info(Info) ->   
	My = Info#content.my, 
	case content_get_my(My,<<"profileid">>) of
		{ok,Member} -> {member,Member};
		_ -> {member,<<>>} 
	end.	
	  
	
im_get_groupchats(Page,Acc) ->  
	{ok,Content} = xn_query:create("Content"), 
    xn_query:filter(Content,"type","eic","im_groupchats"),  
	xn_query:filter(Content,"my.deleted","=","0"),    
	xn_query:tag(Content,"im_groupchats"), 
	xn_query:'begin'(Content,Page*100),
    xn_query:'end'(Content,(Page+1)*100), 
	case xn_query:execute(Content) of
		{ok,Groupchats} when Groupchats =/= [] ->  
		     Im_Groupchats = [ im_get_groupchat_info(Info) || Info <- Groupchats], 
			 xn_query:close(Content),
			 io:format("________Im_Groupchats_______~p________~n",[Im_Groupchats]),
			 im_get_groupchats(Page+1,Acc++Im_Groupchats);  
		_ -> 
			 xn_query:close(Content),
			 Acc
	end.	
	
im_get_groupchat_profiles(Groupchatid) ->  
	{ok,Content} = xn_query:create("Content"), 
    xn_query:filter(Content,"type","eic","im_groupchatprofiles"),  
	xn_query:filter(Content,"my.deleted","=","0"),  
	xn_query:filter(Content,"my.groupchatid","=",Groupchatid),   
	xn_query:tag(Content,"im_groupchatprofiles"), 
	xn_query:'begin'(Content,0),
    xn_query:'end'(Content,-1), 
	case xn_query:execute(Content) of
		{ok,Groupchatprofiles} when Groupchatprofiles =/= [] ->  
		     Im_Groupchatprofiles = [ im_get_groupchatprofile_info(Info) || Info <- Groupchatprofiles],
		     %%io:format("________Im_Groupchatprofiles_________~p________~n",[Im_Groupchatprofiles]), 
			 xn_query:close(Content),
			 Im_Groupchatprofiles; 
		_ -> 
			 xn_query:close(Content),
			 []
	end.	
		
	 
				  
build_affiliations(Host,Owner,[{_,Member}|R],Acc) when Owner =:= Member ->
	build_affiliations(Host,Owner,R,[{{Member,Host,<<>>},{owner,<<>>}}|Acc]);
build_affiliations(Host,Owner,[{Type,Member}|R],Acc) ->
	build_affiliations(Host,Owner,R,[{{Member,Host,<<>>},{Type,<<>>}}|Acc]);	    
build_affiliations(Host,Owner,[_|R],Acc) ->
	build_affiliations(Host,Owner,R,Acc); 
build_affiliations(_,_,[],Acc) -> Acc. 

re_store_room(_LServer, Host, Name, Opts) ->
	
    F = fun () ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
	end,
    mnesia:transaction(F).		
		
im_rebuild_groupchats(LServer, Host,[{Name,Owner}|R]) ->
	  case restore_room(LServer, Host, Name)  of
		  error -> 
		      Members = im_get_groupchat_profiles(Name), 
			  Affiliations = build_affiliations(<<"tezan.cn">>,Owner,Members,[]), 
			  Opts = get_room_opts(Affiliations),  
			  re_store_room(LServer, Host, Name, Opts),
			  io:format("__________re_store_room__________~p________~n",[{Name,Owner,Members}]),
			  im_rebuild_groupchats(LServer, Host,R);
		  _ ->
			  im_rebuild_groupchats(LServer, Host,R)
	  end;
	  
im_rebuild_groupchats(LServer, Host,[_|R]) -> im_rebuild_groupchats(LServer, Host,R);
im_rebuild_groupchats(_,_,[]) -> ok.
		
get_rooms(_LServer, Host) ->
	xn:application("admin"),
	io:format("__________get_rooms__________~p________~n",[{Host}]),
	Groupchats = im_get_groupchats(0,[]),
	io:format("__________get_rooms__________~p________~n",[Groupchats]),
	im_rebuild_groupchats(_LServer, Host,Groupchats),
    Rooms = mnesia:dirty_select(muc_room,
			[{#muc_room{name_host = {'_', Host},
				    _ = '_'},
			  [], ['$_']}]),
    %%io:format("__________get_rooms__________~p________~n",[Rooms]),		  
    Rooms.
get_nick(_LServer, Host, From) ->
	io:format("__________get_nick__________~p________~n",[{Host,From}]),
    {LUser, LServer, _} = jid:tolower(From),
    LUS = {LUser, LServer},
    case mnesia:dirty_read(muc_registered, {LUS, Host}) of
	[] -> error;
	[#muc_registered{nick = Nick}] -> Nick
    end.

set_nick(_LServer, Host, From, Nick) ->
    {LUser, LServer, _} = jid:tolower(From),
    LUS = {LUser, LServer},
    F = fun () ->
		case Nick of
		    <<"">> ->
			mnesia:delete({muc_registered, {LUS, Host}}),
			ok;
		    _ ->
			Allow = case mnesia:select(
				       muc_registered,
				       [{#muc_registered{us_host =
							     '$1',
							 nick = Nick,
							 _ = '_'},
					 [{'==', {element, 2, '$1'},
					   Host}],
					 ['$_']}]) of
				    [] -> true;
				    [#muc_registered{us_host = {U, _Host}}] ->
					U == LUS
				end,
			if Allow ->
				mnesia:write(#muc_registered{
						us_host = {LUS, Host},
						nick = Nick}),
				ok;
			   true ->
				false
			end
		end
	end,
    mnesia:transaction(F).

set_affiliation(_ServerHost, _Room, _Host, _JID, _Affiliation, _Reason) ->
    {error, not_implemented}.

set_affiliations(_ServerHost, _Room, _Host, _Affiliations) ->
    {error, not_implemented}.

get_affiliation(_ServerHost, _Room, _Host, _LUser, _LServer) ->
    {error, not_implemented}.

get_affiliations(_ServerHost, _Room, _Host) ->
    {error, not_implemented}.

search_affiliation(_ServerHost, _Room, _Host, _Affiliation) ->
    {error, not_implemented}.

register_online_room(_ServerHost, Room, Host, Pid) ->
    F = fun() ->
		mnesia:write(
		  #muc_online_room{name_host = {Room, Host}, pid = Pid})
	end,
    mnesia:transaction(F).

unregister_online_room(_ServerHost, Room, Host, Pid) ->
    F = fun () ->
		mnesia:delete_object(
		  #muc_online_room{name_host = {Room, Host}, pid = Pid})
	end,
    mnesia:transaction(F).

find_online_room(_ServerHost, Room, Host) ->
    find_online_room(Room, Host).

find_online_room(Room, Host) ->
    case mnesia:dirty_read(muc_online_room, {Room, Host}) of
	[] -> error;
	[#muc_online_room{pid = Pid}] -> {ok, Pid}
    end.

count_online_rooms(_ServerHost, Host) ->
    ets:select_count(
      muc_online_room,
      ets:fun2ms(
	fun(#muc_online_room{name_host = {_, H}}) ->
		H == Host
	end)).

get_online_rooms(_ServerHost, Host,
		 #rsm_set{max = Max, 'after' = After, before = undefined})
  when is_binary(After), After /= <<"">> ->
    lists:reverse(get_online_rooms(next, {After, Host}, Host, 0, Max, []));
get_online_rooms(_ServerHost, Host,
		 #rsm_set{max = Max, 'after' = undefined, before = Before})
  when is_binary(Before), Before /= <<"">> ->
    get_online_rooms(prev, {Before, Host}, Host, 0, Max, []);
get_online_rooms(_ServerHost, Host,
		 #rsm_set{max = Max, 'after' = undefined, before = <<"">>}) ->
    get_online_rooms(last, {<<"">>, Host}, Host, 0, Max, []);
get_online_rooms(_ServerHost, Host, #rsm_set{max = Max}) ->
    lists:reverse(get_online_rooms(first, {<<"">>, Host}, Host, 0, Max, []));
get_online_rooms(_ServerHost, Host, undefined) ->
    mnesia:dirty_select(
      muc_online_room,
      ets:fun2ms(
	fun(#muc_online_room{name_host = {Name, H}, pid = Pid})
	      when H == Host -> {Name, Host, Pid}
	end)).

-spec get_online_rooms(prev | next | last | first,
		       {binary(), binary()}, binary(),
		       non_neg_integer(), non_neg_integer() | undefined,
		       [{binary(), binary(), pid()}]) ->
			      [{binary(), binary(), pid()}].
get_online_rooms(_Action, _Key, _Host, Count, Max, Items) when Count >= Max ->
    Items;
get_online_rooms(Action, Key, Host, Count, Max, Items) ->
    Call = fun() ->
		   case Action of
		       prev -> mnesia:dirty_prev(muc_online_room, Key);
		       next -> mnesia:dirty_next(muc_online_room, Key);
		       last -> mnesia:dirty_last(muc_online_room);
		       first -> mnesia:dirty_first(muc_online_room)
		   end
	   end,
    NewAction = case Action of
		    last -> prev;
		    first -> next;
		    _ -> Action
		end,
    try Call() of
	'$end_of_table' ->
	    Items;
	{Room, Host} = NewKey ->
	    case find_online_room(Room, Host) of
		{ok, Pid} ->
		    get_online_rooms(NewAction, NewKey, Host,
				     Count + 1, Max, [{Room, Host, Pid}|Items]);
		error ->
		    get_online_rooms(NewAction, NewKey, Host,
				     Count, Max, Items)
	    end;
	NewKey ->
	    get_online_rooms(NewAction, NewKey, Host, Count, Max, Items)
    catch _:{aborted, {badarg, _}} ->
	    Items
    end.

rsm_supported() ->
    true.

register_online_user(_ServerHost, {U, S, R}, Room, Host) ->
    ets:insert(muc_online_users,
	       #muc_online_users{us = {U, S}, resource = R,
				 room = Room, host = Host}).

unregister_online_user(_ServerHost, {U, S, R}, Room, Host) ->
    ets:delete_object(muc_online_users,
		      #muc_online_users{us = {U, S}, resource = R,
					room = Room, host = Host}).

count_online_rooms_by_user(ServerHost, U, S) ->
    MucHost = gen_mod:get_module_opt_host(ServerHost, mod_muc, <<"conference.@HOST@">>),
    ets:select_count(
      muc_online_users,
      ets:fun2ms(
	fun(#muc_online_users{us = {U1, S1}, host = Host}) ->
		U == U1 andalso S == S1 andalso MucHost == Host
	end)).

get_online_rooms_by_user(ServerHost, U, S) ->
    MucHost = gen_mod:get_module_opt_host(ServerHost, mod_muc, <<"conference.@HOST@">>),
    ets:select(
      muc_online_users,
      ets:fun2ms(
	fun(#muc_online_users{us = {U1, S1}, room = Room, host = Host})
	      when U == U1 andalso S == S1 andalso MucHost == Host -> {Room, Host}
	end)).

import(_LServer, <<"muc_room">>,
       [Name, RoomHost, SOpts, _TimeStamp]) ->
    Opts = mod_muc:opts_to_binary(ejabberd_sql:decode_term(SOpts)),
    mnesia:dirty_write(
      #muc_room{name_host = {Name, RoomHost},
                opts = Opts});
import(_LServer, <<"muc_registered">>,
       [J, RoomHost, Nick, _TimeStamp]) ->
    #jid{user = U, server = S} = jid:decode(J),
    mnesia:dirty_write(
      #muc_registered{us_host = {{U, S}, RoomHost},
                      nick = Nick}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, Opts]) ->
    MyHosts = proplists:get_value(hosts, Opts),
    case gen_mod:db_mod(Host, Opts, mod_muc) of
	?MODULE ->
	    ejabberd_mnesia:create(?MODULE, muc_room,
				   [{disc_copies, [node()]},
				    {attributes,
				     record_info(fields, muc_room)}]),
	    ejabberd_mnesia:create(?MODULE, muc_registered,
				   [{disc_copies, [node()]},
				    {attributes,
				     record_info(fields, muc_registered)},
				    {index, [nick]}]);
	_ ->
	    ok
    end,
    case gen_mod:ram_db_mod(Host, Opts, mod_muc) of
	?MODULE ->
	    ejabberd_mnesia:create(?MODULE, muc_online_room,
				   [{ram_copies, [node()]},
				    {type, ordered_set},
				    {attributes, record_info(fields, muc_online_room)}]),
	    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
	    lists:foreach(
	      fun(MyHost) ->
		      clean_table_from_bad_node(node(), MyHost)
	      end, MyHosts),
	    mnesia:subscribe(system);
	_ ->
	    ok
    end,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_up, _Node}}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       muc_online_room,
		       [{#muc_online_room{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:async_dirty(F).

clean_table_from_bad_node(Node, Host) ->
    F = fun() ->
		Es = mnesia:select(
		       muc_online_room,
		       [{#muc_online_room{pid = '$1',
					  name_host = {'_', Host},
					  _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:async_dirty(F).

need_transform(#muc_room{name_host = {N, H}})
  when is_list(N) orelse is_list(H) ->
    ?INFO_MSG("Mnesia table 'muc_room' will be converted to binary", []),
    true;
need_transform(#muc_registered{us_host = {{U, S}, H}, nick = Nick})
  when is_list(U) orelse is_list(S) orelse is_list(H) orelse is_list(Nick) ->
    ?INFO_MSG("Mnesia table 'muc_registered' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#muc_room{name_host = {N, H}, opts = Opts} = R) ->
    R#muc_room{name_host = {iolist_to_binary(N), iolist_to_binary(H)},
	       opts = mod_muc:opts_to_binary(Opts)};
transform(#muc_registered{us_host = {{U, S}, H}, nick = Nick} = R) ->
    R#muc_registered{us_host = {{iolist_to_binary(U), iolist_to_binary(S)},
				iolist_to_binary(H)},
		     nick = iolist_to_binary(Nick)}.

get_subscribed_rooms(_, _, _) ->
    not_implemented.
