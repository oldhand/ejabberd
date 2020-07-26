-module (mod_offline_push_session).
-compile(export_all).

-behaviour(gen_server).

-export([start/0]).  
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  
     terminate/2, code_change/3]).    
  
-record(content, {application, id, xn_type, title, published, updated, author,datatype, my}).   
-record(push_sessions, {sessions,givennames,groups}).     
-record(session, {profileid,token,platform,language,messagepush}). 

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).  

stop() -> gen_server:call(?MODULE, stop).   

get(Profileid) -> gen_server:call(?MODULE, {get,Profileid},infinity).    
get_givenname(Profileid) -> gen_server:call(?MODULE, {get_givenname,Profileid},infinity).   
get_mucname(Mucid) -> gen_server:call(?MODULE, {get_mucname,Mucid},infinity).   
check_groups(Profileid,Mucid) -> gen_server:call(?MODULE, {check_groups,Profileid,Mucid},infinity).  

init([]) ->        
	 io:format("push_session starting...~n"), 
	 erlang:send_after(1000, self(), {trigger, 0}),   
	 {ok, #push_sessions{sessions=[],givennames=[],groups=[]}}; 
init(_) -> {error,"push_session is error."}.	 
    
handle_info({trigger, N}, State = #push_sessions{sessions=Sessions,givennames=Givennames,groups=Groups}) ->   
      io:format("___push_session_tick_____~p____~n", [N]), 
	  {NewSessions,NewGivennames,NewGroups} = update_push_sessions(Sessions,Givennames,Groups),
      erlang:send_after(1000*60*1, self(), {trigger, N+1}),  
      {noreply,State#push_sessions{sessions=NewSessions,givennames=NewGivennames,groups=NewGroups}}; 
	 
handle_info(Info, State) ->
     io:format("~p catchall: ~p, ~p~n", [?MODULE, Info, State]),
     {noreply,State}.	
	 
handle_call({get,Profileid}, _From, State = #push_sessions{sessions=Sessions}) ->
	case get(Sessions,Profileid) of
		{ok,Session} ->
			{reply, {ok,Session}, State};
		_ -> 
			case get_profile_info(Profileid) of
				{ok,Session} ->  
					NewSessions = [Session|Sessions], 
					{reply, {ok,Session}, State#push_sessions{sessions=NewSessions}};
				_ ->
					{reply, {error,"not found"}, State}
			end 
	end; 
	
handle_call({get_givenname,Profileid}, _From, State = #push_sessions{givennames=Givennames}) ->
	case get_givenname(Givennames,Profileid) of
		{ok,Givenname} ->
			{reply, Givenname, State};
		_ -> 
			case get_givenname_info(Profileid) of
				{ok,Givenname} ->  
					NewGivennames = [{Profileid,Givenname}|Givennames], 
					{reply, Givenname, State#push_sessions{givennames=NewGivennames}};
				_ ->
					{reply, <<>>, State}
			end 
	end; 	 

handle_call({get_mucname,Mucid}, _From, State = #push_sessions{groups=Groups}) ->
	case get_mucname(Groups,Mucid) of
		{ok,Mucname} ->
			{reply, Mucname, State};
		_ -> 
			case get_mucname_info(Mucid) of
				{ok,Mucname,Members} ->   
					NewGroups = [{Mucid,Mucname,Members}|Groups], 
					{reply, Mucname, State#push_sessions{groups=NewGroups}};
				_ ->
					{reply, <<>>, State}
			end 
	end;
	
handle_call({check_groups,Profileid,Mucid}, _From, State = #push_sessions{groups=Groups}) ->
	case get_muc_members(Groups,Mucid) of
		{ok,Members} -> 
			{reply, lists:member(Profileid,Members), State};
		_ -> 
			case get_mucname_info(Mucid) of
				{ok,Mucname,Members} ->   
					NewGroups = [{Mucid,Mucname,Members}|Groups], 
					{reply, Mucname, State#push_sessions{groups=NewGroups}};
				_ ->
					{reply, <<>>, State}
			end 
	end; 	
		
handle_call(stop, _From, Tab) ->  
    {stop, normal, stopped, Tab}.  

  
handle_cast(_Msg, State) -> {noreply, State}.   
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.


get([#session{profileid=Profileid}=L|_],Profileid) -> {ok,L};
get([_|R],Profileid) -> get(R,Profileid);
get([],_) -> <<>>.

get_profile_info(Profileid) when is_binary(Profileid) -> get_profile_info(binary_to_list(Profileid));	
get_profile_info(Profileid) -> 
	xn:application("admin"), 
	{ok,Content} = xn_query:create("Content"), 
    xn_query:filter(Content,"type","eic","pushmsgbinds"),  
	xn_query:filter(Content,"my.deleted","=","0"),   
	xn_query:filter(Content,"my.profileid","=",Profileid),     
	xn_query:tag(Content,"pushmsgbinds"),   
    xn_query:'end'(Content,1), 
	case xn_query:execute(Content) of
		{ok,Pushmsgbinds} when Pushmsgbinds =/= [] ->    
			 Pushmsgbind_info = hd(Pushmsgbinds),  
			 Platform = field(Pushmsgbind_info#content.my,<<"platform">>),
			 Token = field(Pushmsgbind_info#content.my,<<"token">>),
			 {Language,Messagepush} = get_profilesetting_info(Profileid),
			 Session = #session{profileid = list_to_binary(Profileid),token = Token,platform = Platform,language = Language,messagepush = Messagepush}, 
			 xn_query:close(Content),
			 {ok,Session};
		_ -> 
			 xn_query:close(Content),
			 {error,"no bind"}
	end.
	
	
	
get_profilesetting_info(Profileid) when is_binary(Profileid) -> get_profilesetting_info(binary_to_list(Profileid));	
get_profilesetting_info(Profileid) ->  
		xn:application("admin"), 
		{ok,Content} = xn_query:create("Content"), 
	    xn_query:filter(Content,"type","eic","im_profilesettings"),  
		xn_query:filter(Content,"my.deleted","=","0"),    
		xn_query:filter(Content,"my.profileid","=",Profileid),    
		xn_query:tag(Content,"im_profilesettings"),  
	    xn_query:'end'(Content,1), 
		case xn_query:execute(Content) of
			{ok,Profilesettings} when Profilesettings =/= [] ->   
				 Profilesetting_info = hd(Profilesettings), 
				 Language = field(Profilesetting_info#content.my,<<"language">>), 
				 Messagepush = field(Profilesetting_info#content.my,<<"messagepush">>),
				 xn_query:close(Content), 
				 {Language,Messagepush};
			_ -> 
				 xn_query:close(Content), 
				 {<<"zh_cn">>,<<"1">>}
		end.
			

get_givenname([{Profileid,Givenname}|_],Profileid) -> {ok,Givenname};
get_givenname([_|R],Profileid) -> get_givenname(R,Profileid);
get_givenname([],_) -> <<>>.	
	
	
get_givenname_info(Profileid) when is_binary(Profileid) -> get_givenname_info(binary_to_list(Profileid));		
get_givenname_info(Profileid) ->	
	xn:application("admin"),
	case xn_profile:load(Profileid,"profile") of
		{ok, Profile} -> 
			 Givenname = xn_profile:givenname(Profile),  
			 xn_profile:close(Profile), 
			 {ok,Givenname};
		_ ->
			{error,"not found"}
	end.


get_mucname([{Mucid,Mucname,_}|_],Mucid) -> {ok,Mucname};
get_mucname([_|R],Mucid) -> get_mucname(R,Mucid);
get_mucname([],_) -> <<>>.

get_muc_members([{Mucid,_,Members}|_],Mucid) -> {ok,Members};
get_muc_members([_|R],Mucid) -> get_muc_members(R,Mucid);
get_muc_members([],_) -> <<>>.


get_mucname_info(Mucid) when is_binary(Mucid) -> get_mucname_info(binary_to_list(Mucid));	     
get_mucname_info(Mucid) ->  
    xn:application("admin"), 
	case xn_content:load(Mucid,"im_groupchats") of
		{ok, Groupchat_info} ->
			{ok,{_,_,Mucname}} = xn_content:get(Groupchat_info,"name"),
			case xn_content:get(Groupchat_info,"contactids") of
				{ok,{_,_,Contactids}} -> 
					Members = [list_to_binary(T) || T <- string:tokens(binary_to_list(Contactids), ",")], 
					xn_content:close(Groupchat_info), 
					{ok,get_mucnames(Mucname,Members),Members};
				_ ->
					{error,"no muc Contactids"}	
			end;
		_ -> {error,"no muc"}	
	end.
	
get_mucnames(<<>>,Members) when length(Members) > 5 -> 
	case xn_profile:loadMany(lists:sublist(Members,5),"profile") of
		{ok,{_,Profiles}} ->
			Names = [binary_to_list(field(T#content.my,<<"givenname">>)) || T <- Profiles], 
			list_to_binary(xn_common:list_to_string(Names,","));
		_ ->  <<>>
	end; 	
get_mucnames(<<>>,Members) -> 
	case xn_profile:loadMany(Members,"profile") of
		{ok,{_,Profiles}} ->
			Names = [binary_to_list(field(T#content.my,<<"givenname">>)) || T <- Profiles], 
			list_to_binary(xn_common:list_to_string(Names,","));
		_ ->  <<>>
	end; 
get_mucnames(Name,_) -> Name.
     
	
	
update_push_sessions(Sessions,Givennames,Groups) -> 
   xn:application("admin"), 
   Update_sessions = update_sessions(Sessions), 
   % io:format("___Update_sessions____~p____~n", [Update_sessions]),
   Update_givennames = update_givennames(Givennames), 
   % io:format("___Update_givennames____~p____~n", [Update_givennames]),
   Update_groups = update_mucs(Groups), 
   io:format("___Update_Groups____~p____~n", [Groups]),
   {Update_sessions,Update_givennames,Update_groups}.
   
   
update_sessions(Sessions) ->     
   Update_sessions = update_pushmsgbinds(Sessions),
   update_profilesettings(Update_sessions).
   
update_mucs(Groups) ->     
 	{ok,Content} = xn_query:create("Content"),
    xn_query:filter(Content,"type","eic","im_groupchats"), 
	xn_query:filter(Content,"my.deleted","=","0"),
	xn_query:filter(Content,"updated",">",get_difftime()),
 	xn_query:tag(Content,"im_groupchats"),
    xn_query:'end'(Content,-1),
 	case xn_query:execute(Content) of
 		{ok,Groupchats} when Groupchats =/= [] ->  
			  NewGroups = update_mucs(Groupchats,Groups),
 			  xn_query:close(Content),
 			  NewGroups;
 		_ ->
 			 xn_query:close(Content),
 			 Groups
 	end. 
update_mucs([Groupchat_info|R],Groups) ->
	 Name = field(Groupchat_info#content.my,<<"name">>), 
	 Contactids = field(Groupchat_info#content.my,<<"contactids">>),  
	 Members = [list_to_binary(T) || T <- string:tokens(binary_to_list(Contactids), ",")],
	 Groupid = Groupchat_info#content.id,   
	 NewGroups = update_mucs(Groups,Groupid,get_mucnames(Name,Members),Members,[]),
	 update_mucs(R,NewGroups);
update_mucs([],Groups) -> Groups. 
	
update_mucs([{Groupid,_,_}|R],Groupid,Name,Members,Acc) ->
	update_mucs(R,Groupid,Name,Members,[{Groupid,Name,Members}|Acc]);
update_mucs([L|R],Groupid,Name,Members,Acc) ->	
	update_mucs(R,Groupid,Name,Members,[L|Acc]);
update_mucs([],_,_,_,Acc) -> Acc.		
	
	
	   
update_givennames(Givennames) ->     
 	{ok,Content} = xn_query:create("Profile"),
    xn_query:filter(Content,"type","=","im"), 
	xn_query:filter(Content,"updated",">",get_difftime()),
 	xn_query:tag(Content,"profile"),
    xn_query:'end'(Content,-1),
 	case xn_query:execute(Content) of
 		{ok,Profiles} when Profiles =/= [] ->  
			  NewGivennames = update_givennames(Profiles,Givennames),
 			  xn_query:close(Content),
 			  NewGivennames;
 		_ ->
 			 xn_query:close(Content),
 			 Givennames
 	end. 
update_givennames([Profile_info|R],Givennames) ->
	 Givenname = field(Profile_info#content.my,<<"givenname">>),  
	 Profileid = Profile_info#content.id,   
	 NewGivennames = update_givennames(Givennames,Profileid,Givenname,[]),
	 update_givennames(R,NewGivennames);
update_givennames([],Givennames) ->	Givennames. 
	
update_givennames([{Profileid,_}|R],Profileid,Givenname,Acc) ->
	update_givennames(R,Profileid,Givenname,[{Profileid,Givenname}|Acc]);
update_givennames([L|R],Profileid,Givenname,Acc) ->	
	update_givennames(R,Profileid,Givenname,[L|Acc]);
update_givennames([],_,_,Acc) -> Acc.		
	
	
   
update_pushmsgbinds(Sessions) ->     
 	{ok,Content} = xn_query:create("Content"),
    xn_query:filter(Content,"type","eic","pushmsgbinds"),
	xn_query:filter(Content,"my.deleted","=","0"),
	xn_query:filter(Content,"updated",">",get_difftime()),
 	xn_query:tag(Content,"pushmsgbinds"),
    xn_query:'end'(Content,-1),
 	case xn_query:execute(Content) of
 		{ok,Pushmsgbinds} when Pushmsgbinds =/= [] -> 
			  NewSessions = update_pushmsgbinds(Pushmsgbinds,Sessions),
 			 xn_query:close(Content),
 			 NewSessions;
 		_ ->
 			 xn_query:close(Content),
 			 Sessions
 	end.
	
update_pushmsgbinds([Profilesetting_info|R],Sessions) ->
	 Profileid = field(Profilesetting_info#content.my,<<"profileid">>), 
	 Token = field(Profilesetting_info#content.my,<<"token">>), 
	 Platform = field(Profilesetting_info#content.my,<<"platform">>),
	 NewSessions = update_pushmsgbinds(Sessions,Profileid,Token,Platform,[]),
	 update_pushmsgbinds(R,NewSessions);
update_pushmsgbinds([],Sessions) ->	Sessions. 
	
update_pushmsgbinds([#session{profileid=Profileid,language=Language,messagepush=Messagepush}|R],Profileid,Token,Platform,Acc) ->
	update_pushmsgbinds(R,Profileid,Token,Platform,[#session{profileid = Profileid,token = Token,platform = Platform,language=Language,messagepush=Messagepush}|Acc]);
update_pushmsgbinds([L|R],Profileid,Token,Platform,Acc) ->	
	update_pushmsgbinds(R,Profileid,Token,Platform,[L|Acc]);
update_pushmsgbinds([],_,_,_,Acc) -> Acc.		
	
update_profilesettings(Sessions) ->     
 	{ok,Content} = xn_query:create("Content"),
    xn_query:filter(Content,"type","eic","im_profilesettings"),
	xn_query:filter(Content,"my.deleted","=","0"),
	xn_query:filter(Content,"updated",">",get_difftime()),
 	xn_query:tag(Content,"im_profilesettings"),
    xn_query:'end'(Content,-1),
 	case xn_query:execute(Content) of
 		{ok,Profilesettings} when Profilesettings =/= [] -> 
			 NewSessions = update_profilesettings(Profilesettings,Sessions),
 			 xn_query:close(Content),
 			 NewSessions;
 		_ ->
 			 xn_query:close(Content),
 			 Sessions
 	end.	
	
update_profilesettings([Profilesetting_info|R],Sessions) ->
	 Profileid = field(Profilesetting_info#content.my,<<"profileid">>), 
	 Language = field(Profilesetting_info#content.my,<<"language">>), 
	 Messagepush = field(Profilesetting_info#content.my,<<"messagepush">>),
	 NewSessions = update_profilesettings(Sessions,Profileid,Language,Messagepush,[]),
	 update_profilesettings(R,NewSessions);
update_profilesettings([],Sessions) ->	Sessions. 
	
update_profilesettings([#session{profileid = Profileid,token = Token,platform = Platform}|R],Profileid,Language,Messagepush,Acc) ->
	update_profilesettings(R,Profileid,Language,Messagepush,[#session{profileid = Profileid,token = Token,platform = Platform,language=Language,messagepush=Messagepush}|Acc]);
update_profilesettings([L|R],Profileid,Language,Messagepush,Acc) ->	
	update_profilesettings(R,Profileid,Language,Messagepush,[L|Acc]);
update_profilesettings([],_,_,_,Acc) -> Acc.		
	
	
get_difftime() ->	
 	Sec = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 60 * 60 * 2,
 	{{Year, Month, Day}, {_Hour, _Minute, _Second}} = calendar:gregorian_seconds_to_datetime(Sec),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, 0, 0, 0])).
	
	

field([{Key,_,Value}|_],Key) -> Value;
field([_|R],Key) -> field(R,Key);
field([],_) -> <<>>.






 