 
-module(xn_application).

-behaviour(gen_fsm).
-vsn('v1.1').
 	 
-export([handle_event/3, handle_sync_event/4, init/1,
        code_change/4, handle_info/3,
     terminate/3]).	 
-compile(export_all).  

-record(content, {application,id, xn_type, title, published, updated, author,datatype, my}). 
 

load(Domain) -> load(Domain,""). 
load(Domain,Tag) ->   
      Url =  lists:flatten(["/application(domain='",Domain,"')"]),
	  Rsp = case Tag of
		  "" -> xn_rest:get(Url);
		  _ -> xn_rest:get(Url,[{"tag",Tag}])
	  end,  
	  case Rsp of
		  {ok,{Count,ContentList}} when Count =:= 1 ->
			  Content = hd(ContentList),
			  new(Content);
		  {ok,{0,_}} -> {error,"no record"};
		  ErrorResult -> {error,ErrorResult}
	  end. 
 
create(Domain, Description) -> 
	gen_fsm:start(?MODULE,{Domain, Description},[]). 

new(Content) -> gen_fsm:start(?MODULE,Content,[]).

close(Fsm) -> gen_fsm:send_all_state_event(Fsm, close).
  
domain(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"domain"}).
description(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"description"}).
author(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"author"}).
published(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"published"}).
updated(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"updated"}).
remainnumberofsmss(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"remainnumberofsmss"}).
numberofsmss(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"numberofsmss"}).
numberofusers(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"numberofusers"}).
remainstoragespace(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"remainstoragespace"}).
storagespace(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"storagespace"}).
city(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"city"}).
province(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"province"}).
trialtime(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"trialtime"}).
point(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"point"}).
link(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"link"}).
active(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"active"}).

 
description(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"description",Value}).
author(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"author",Value}).
published(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"published",Value}).
updated(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"updated",Value}).
remainnumberofsmss(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"remainnumberofsmss",Value}).
numberofsmss(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"numberofsmss",Value}).
numberofusers(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"numberofusers",Value}).
remainstoragespace(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"remainstoragespace",Value}).
storagespace(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"storagespace",Value}).
city(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"city",Value}).
province(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"province",Value}).
trialtime(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"trialtime",Value}).
point(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"point",Value}).
link(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"link",Value}).
active(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"active",Value}).
 

 

 
save(Fsm) ->gen_fsm:sync_send_event(Fsm, {save,""},infinity).
save(Fsm,Tag) ->gen_fsm:sync_send_event(Fsm, {save,Tag},infinity).

init(Content) when is_record(Content,content)  ->
    %%io:format("xn_application:init(~p)~n",[Content]), 
    {ok, loop,Content}; 	
init({Domain, Description}) ->
     %%io:format("xn_application:create(~p)~n",[{Domain,Description}]),  
     {ok, loop,#content{application = <<>>,
					   xn_type = <<"application">>,
					   title = list_to_binary(Description),
					   id = list_to_binary(Domain),
					   author = xn:viewer(),
					   published = <<>>, 
					   updated = <<>>,
					   datatype=0,
					   my=[]}};
init(_) -> {error,"not a content record."}.

loop({get,KeyName}, _Pid, Info)-> {reply, application_get(Info,KeyName), loop, Info};
loop({set,KeyName,Value}, _Pid, Info)->  
       Newinfo = application_set(Info,KeyName,Value),
	   {reply, ok, loop, Newinfo}; 


   
loop({save,Tag}, _Pid, Info)->  
   Url =  "/application",
   NewInfo = term_to_binary([Info]), 
   Rsp = case Tag of
		 "" ->  
		     case Info#content.id of
				 <<>> -> xn_rest:post(Url,NewInfo);
				 _ ->  xn_rest:put(Url,NewInfo)
			 end;   
		  _ ->  
		     case Info#content.id of
				 <<>> -> xn_rest:post(Url,NewInfo,[{"tag",Tag}]);
				 _ ->  xn_rest:put(Url,NewInfo,[{"tag",Tag}])
			 end
	    end, 
	case Rsp of
		{ok,{1,SaveContent}} -> {reply,ok, loop, hd(SaveContent)};
		ErrorMsg -> {reply,ErrorMsg, loop, Info} 
	end;   
   	
loop(Filter, _Pid, Info) ->  
	    io:format("loop: ~p~n", [Filter]),  
	   {reply,Filter,loop, Info}.
		
handle_sync_event(rset, _Pid, _State, Info) -> 
       {reply, {ok, ""}, loop, Info}.
    
		
handle_event(close, _State, Info) -> 
    {stop, normal, Info}.

terminate(normal, _StateName, _StateData)->
    normal;
terminate(Reason,_StateName,_StateData) ->
    {terminated, Reason}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.	


application_get(Info,KeyName) when is_list(KeyName)	-> application_get(Info,list_to_binary(KeyName));
application_get(Info,<<"domain">>) -> Info#content.id;
application_get(Info,<<"description">>) -> Info#content.title; 
application_get(Info,<<"author">>) -> Info#content.author;
application_get(Info,<<"published">>) -> Info#content.published;
application_get(Info,<<"updated">>) -> Info#content.updated;
application_get(Info,KeyName) -> content_get(Info#content.my,KeyName). 
 
content_get([{Name,_,Value}|_],Name) -> Value;
content_get([_|R],Name) -> content_get(R,Name);
content_get([],_)	-> {error,"no key"}.

application_set(Info,KeyName,Value) when is_list(KeyName) -> application_set(Info,list_to_binary(KeyName),Value);
application_set(Info,KeyName,Value) when is_list(Value) -> application_set(Info,KeyName,list_to_binary(Value)); 
application_set(Info,<<"description">>,Value)  -> Info#content{title=Value}; 
application_set(Info,<<"author">>,Value)  -> Info#content{author=Value};  
application_set(Info,KeyName,Value)  -> 
	Newmy = content_set(Info#content.my,KeyName,Value,0,[]),
    Info#content{my=Newmy}. 

	
content_set([{Name,_,_}|R],Name,Value,_,Acc)	-> content_set(R,Name,Value,1,[{Name,<<"string">>,Value}|Acc]);
content_set([L|R],Name,Value,Have,Acc)	-> content_set(R,Name,Value,Have,[L|Acc]);	
content_set([],_,_,1,Acc)	-> lists:reverse(Acc);
content_set([],Name,Value,_,Acc) -> lists:reverse([{Name,<<"string">>,Value}|Acc]).