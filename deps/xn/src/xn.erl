
-module(xn).

-compile(export_all).

-behaviour(gen_server). 
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/0,stop/0]).
 
    

-record(xn, {server,port,viewer,application}).

  
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).  

stop()  -> gen_server:call(?MODULE, stop).  
  



init([]) ->  
  Path = code:lib_dir('xn'),  
  %%crypto:start(), 
  inets:start(),
  case file:consult(Path ++ "/xn.conf") of
		{ok,Data} -> 
		      Server = proplists_get_value(server, Data,"localhost"),
		      Port = proplists_get_value(port, Data,8000), 
			  io:format("XN client starting...~n"),  
		      ets:new(xn_store,[set,named_table,public]),  
			  ets:insert(xn_store, {authtoken,[]}),
		      {ok, #xn{server=Server,port=Port,viewer = <<>>,application = <<"localhost">>}}; 
		ErrorResult ->
   		     {error,lists:flatten(["load ",Path , "/xn.conf" , " failure!" , io_lib:format("~p",[ErrorResult])])}
    end.  
  
 
getconfig()      -> gen_server:call(?MODULE,{config},infinity).  

viewer()      -> gen_server:call(?MODULE,{getviewer},infinity). 
application()      -> gen_server:call(?MODULE,{getapplication},infinity).

viewer(Viewer) when is_list(Viewer)  -> gen_server:call(?MODULE,{setviewer,list_to_binary(Viewer)},infinity);
viewer(Viewer) -> gen_server:call(?MODULE,{setviewer,Viewer},infinity). 
application(Appliction) when is_list(Appliction)  -> gen_server:call(?MODULE,{setapplication,list_to_binary(Appliction)},infinity);
application(Appliction) -> gen_server:call(?MODULE,{setapplication,Appliction},infinity).
    
 

handle_call({config}, _From, Tab) -> 
    {reply,{ok,Tab#xn.server,Tab#xn.port,Tab#xn.viewer,Tab#xn.application},Tab}; 

handle_call({getviewer}, _From, Tab) -> 
    {reply,Tab#xn.viewer,Tab}; 

handle_call({getapplication}, _From, Tab) -> 
    {reply,Tab#xn.application,Tab}; 			
 
handle_call({setviewer,Viewer}, _From, Tab) ->     
    {reply, ok, Tab#xn{viewer=Viewer}};
	
handle_call({setapplication,Application}, _From, Tab) ->     
    {reply, ok, Tab#xn{application=Application}};     

handle_call(stop, _From, Tab) ->  {stop, normal, stopped, Tab}. 
  
handle_cast(_Msg, State) -> {noreply, State}.  
handle_info(_Info, State) -> {noreply, State}.  
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.


proplists_get_value(Key, Data,Default) ->
     case proplists:get_value(Key, Data)  of
	 undefined -> Default;
	 Value -> Value
     end.
	 
	 
	 
t()-> 
    xn:application("localhost"),
 	xn:viewer("8btgoi9eke3"),
 	{ok,Fsm} = xn_query:create("Content"),
	xn_query:tag(Fsm,"tabs"),
	xn_query:'begin'(Fsm,0),
	xn_query:'end'(Fsm,10),
	xn_query:filter(Fsm,"type","eic","tabs"),
	xn_query:filter(Fsm,"my.tabid",">",100),
	xn_query:order(Fsm,"my.tabid","ASC"),
	XFilter = xn_filter:any(xn_filter:filter("my.tabid",">",200),xn_filter:filter("my.tabid",">",300)),
	xn_query:filter(Fsm,XFilter),
	io:format("xn_content:execute => ~p~n",[xn_query:execute(Fsm)]),
	io:format("xn_content:count => ~p~n",[xn_query:count(Fsm)]),
	
	
	{ok,Profile} = xn_profile:load("my90e8b5rei","profile") ,
 	io:format("published => ~p~n",[xn_profile:published(Profile)]),  
 	io:format("updated => ~p~n",[xn_profile:updated(Profile)]), 
	io:format("profileid => ~p~n",[xn_profile:profileid(Profile)]),
	io:format("screenname => ~p~n",[xn_profile:screenname(Profile)]),
	io:format("invitationcode => ~p~n",[xn_profile:invitationcode(Profile)]),
	io:format("invitationcode => ~p~n",[xn_profile:invitationcode(Profile,"123123123")]),
	io:format("invitationcode => ~p~n",[xn_profile:invitationcode(Profile)]),
	io:format("mobile => ~p~n",[xn_profile:mobile(Profile)]),
	io:format("email => ~p~n",[xn_profile:email(Profile)]),
	io:format("email => ~p~n",[xn_profile:email(Profile,"68594864@qq.com")]),
	io:format("email => ~p~n",[xn_profile:email(Profile)]),	
	io:format("xn_profile:save => ~p~n",[xn_profile:save(Profile,"profile")]),	
	io:format("invitationcode => ~p~n",[xn_profile:invitationcode(Profile)]),
	io:format("email => ~p~n",[xn_profile:email(Profile)]),
	
	
	
	{ok,Application} = xn_application:load("localhost"), 
 	io:format("published => ~p~n",[xn_application:published(Application)]),  
 	io:format("updated => ~p~n",[xn_application:updated(Application)]),
	io:format("domain => ~p~n",[xn_application:domain(Application)]),
	io:format("description => ~p~n",[xn_application:description(Application)]),
	io:format("trialtime => ~p~n",[xn_application:trialtime(Application)]),
	io:format("author => ~p~n",[xn_application:author(Application)]),
	io:format("province => ~p~n",[xn_application:province(Application)]),
	io:format("city => ~p~n",[xn_application:city(Application)]), 
	ok.	
		
test()-> 
    xn:application("localhost"),
 	xn:viewer("8btgoi9eke3"),
 	{ok,Fsm} = xn_content:create("products","test"),
 	io:format("application => ~p~n",[xn_content:application(Fsm)]), 
 	io:format("author => ~p~n",[xn_content:author(Fsm)]),
 	io:format("title => ~p~n",[xn_content:title(Fsm)]), 
 	io:format("type => ~p~n",[xn_content:type(Fsm)]), 
 	io:format("set => ~p~n",[xn_content:set(Fsm,"key","123141234234")]), 
 	io:format("get => ~p~n",[xn_content:get(Fsm,"key")]),
 	xn_content:close(Fsm),

 	{ok,Loadcontent} = xn_content:load(1209919,"tabs"), 
 	io:format("~n~napplication => ~p~n",[xn_content:application(Loadcontent)]), 
 	io:format("author => ~p~n",[xn_content:author(Loadcontent)]),
 	io:format("title => ~p~n",[xn_content:title(Loadcontent)]), 
 	io:format("type => ~p~n",[xn_content:type(Loadcontent)]),  
 	io:format("published => ~p~n",[xn_content:published(Loadcontent)]),  
 	io:format("updated => ~p~n",[xn_content:updated(Loadcontent)]),  
 	io:format("get tablabel => ~p~n",[xn_content:get(Loadcontent,"tablabel")]), 
 	io:format("get tabid => ~p~n",[xn_content:get(Loadcontent,"tabid")]), 
 	xn_content:close(Loadcontent),


 	io:format("xn_content:loadMany => ~p~n",[xn_content:loadMany([1213801,1209919,1213546],"tabs")]), 


 	{ok,Fsm1} = xn_content:create("test","test"),
 	io:format("application => ~p~n",[xn_content:application(Fsm1)]), 
 	io:format("author => ~p~n",[xn_content:author(Fsm1)]),
 	io:format("title => ~p~n",[xn_content:title(Fsm1)]), 
 	io:format("type => ~p~n",[xn_content:type(Fsm1)]), 
 	io:format("set => ~p~n",[xn_content:set(Fsm1,"key","123141234234")]), 
 	io:format("set => ~p~n",[xn_content:set(Fsm1,"name","asadsdasd")]),
 	io:format("get => ~p~n",[xn_content:get(Fsm1,"key")]), 
 	io:format("xn_content:save => ~p~n",[xn_content:save(Fsm1,"test")]),	
 	io:format("id => ~p~n",[xn_content:id(Fsm1)]), 
 	io:format("published => ~p~n",[xn_content:published(Fsm1)]),  
 	io:format("updated => ~p~n",[xn_content:updated(Fsm1)]), 

 	io:format("set => ~p~n",[xn_content:set(Fsm1,"aaaa","aaaaa")]), 

 	io:format("xn_content:save => ~p~n",[xn_content:save(Fsm1,"test")]),
 	io:format("get => ~p~n",[xn_content:get(Fsm1,"aaaa")]), 
 	io:format("id => ~p~n",[xn_content:id(Fsm1)]), 
 	io:format("published => ~p~n",[xn_content:published(Fsm1)]),  
 	io:format("updated => ~p~n",[xn_content:updated(Fsm1)]),

 	io:format("updated => ~p~n",[xn_content:delete(Fsm1)]),

 	xn_content:close(Fsm1), 
 	ok.
