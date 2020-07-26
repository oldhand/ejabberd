
-module(xn_content). 

-behaviour(gen_fsm).
-vsn('v1.1').
 	 
-export([handle_event/3, handle_sync_event/4, init/1,
        code_change/4, handle_info/3,
     terminate/3]).	 
-compile(export_all).  

-record(content, {application,id, xn_type, title, published, updated, author,datatype, my}). 

  

	
load(Id) when is_integer(Id) -> load(integer_to_list(Id),"",0);	
load(Id) -> load(Id,"",0). 

load(Id,Tag) when is_integer(Id) -> load(integer_to_list(Id),Tag,0);
load(Id,Tag) -> load(Id,Tag,0).		  
load(Id,Tag,Datatype) -> 
      Url = case Datatype of
		  0 -> lists:flatten(["/content(id='",Id,"')"]);
		  1 -> lists:flatten(["/bigcontent(id='",Id,"')"]);
		  2 -> lists:flatten(["/mq(id='",Id,"')"]);
		  3 -> lists:flatten(["/simplecontent(id='",Id,"')"]);
		  4 -> lists:flatten(["/maincontent(id='",Id,"')"]);
		  5 -> lists:flatten(["/schedule(id='",Id,"')"]);
		  6 -> lists:flatten(["/message(id='",Id,"')"]); 
		  7 -> lists:flatten(["/yearcontent(id='",Id,"')"]); 
		  8 -> lists:flatten(["/mainyearcontent(id='",Id,"')"]); 
		  9 -> lists:flatten(["/yearmonthcontent(id='",Id,"')"]); 
		  10 -> lists:flatten(["/mainyearmonthcontent(id='",Id,"')"]);  
		  _ -> lists:flatten(["/maincontent(id='",Id,"')"])
	  end,   
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
	  
loadMany(Ids) -> loadMany(Ids,[],0). 
loadMany(Ids,Tag) -> loadMany(Ids,Tag,0). 
loadMany(Ids,Tag,Datatype) -> 
      Url = case Datatype of
		  0 -> lists:flatten(["/content(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
		  1 -> lists:flatten(["/bigcontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
		  2 -> lists:flatten(["/mq(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
		  3 -> lists:flatten(["/simplecontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
		  _ -> lists:flatten(["/maincontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"])
	  end,   
	  case Tag of
		  "" -> xn_rest:get(Url);
		  _ -> xn_rest:get(Url,[{"tag",Tag}])
	  end.
delete(Fsm) when is_pid(Fsm) -> gen_fsm:sync_send_event(Fsm, {delete,""}). 	  
delete(Fsm,Tag) when is_pid(Fsm) -> gen_fsm:sync_send_event(Fsm, {delete,Tag}); 
delete(Id,Tag) when is_integer(Id) -> delete(integer_to_list(Id),Tag,0);
delete(Id,Tag) -> delete(Id,Tag,0).

delete(Id,Tag,Datatype) when not is_list(hd(Id)) ->  delete([Id],Tag,Datatype);
delete(Ids,Tag,Datatype) ->
      Url = case Datatype of
  		  0 -> lists:flatten(["/content(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
  		  1 -> lists:flatten(["/bigcontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
  		  2 -> lists:flatten(["/mq(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
  		  3 -> lists:flatten(["/simplecontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"]);
  		  _ -> lists:flatten(["/maincontent(id in [",xn_common:list_to_string(Ids,",",[]),"])"])
  	  end, 
	  case Tag of
		  "" -> xn_rest:delete(Url);
		  _ -> xn_rest:delete(Url,[{"tag",Tag}])
	  end. 

create(TypeOrNode,Title) -> create(TypeOrNode,Title,false,0).
	 
create(TypeOrNode,Title,Anonymous) -> create(TypeOrNode,Title,Anonymous,0). 

create(TypeOrNode,Title,Anonymous,Datatype) ->
	gen_fsm:start(?MODULE,{TypeOrNode,Title,Anonymous,Datatype},[]). 

new(Content) -> gen_fsm:start(?MODULE,Content,[]).

close(Fsm) -> gen_fsm:send_all_state_event(Fsm, close).

application(Fsm) -> gen_fsm:sync_send_event(Fsm, application). 
author(Fsm) -> gen_fsm:sync_send_event(Fsm, author). 
type(Fsm) -> gen_fsm:sync_send_event(Fsm, type).
title(Fsm) -> gen_fsm:sync_send_event(Fsm, title).
id(Fsm) -> gen_fsm:sync_send_event(Fsm, id).
published(Fsm) -> gen_fsm:sync_send_event(Fsm, published).
updated(Fsm) -> gen_fsm:sync_send_event(Fsm, updated).

set(Fsm,Name,Value) ->	gen_fsm:sync_send_event(Fsm, {set,Name,<<"string">>,Value}). 
set(Fsm,Name,Type,Value) ->	gen_fsm:sync_send_event(Fsm, {set,Name,Type,Value}). 
get(Fsm,Name) -> gen_fsm:sync_send_event(Fsm, {get,Name}). 

save(Fsm) ->gen_fsm:sync_send_event(Fsm, {save,""},infinity).
save(Fsm,Tag) ->gen_fsm:sync_send_event(Fsm, {save,Tag},infinity).

dump(Fsm) -> gen_fsm:sync_send_event(Fsm, dump,infinity).

batchsave([],_,_) -> {error,"empty content list"};
batchsave(Contents,Tag,Datatype) when is_list(Contents) -> 
   Url = case Datatype of
	  0 -> "/content";
	  1 -> "/bigcontent";
	  2 -> "/mq";
	  3 -> "/simplecontent";
	  4 -> "/maincontent";
	  5 -> "/schedule";
	  6 -> "/message";
	  7 -> "/yearcontent";
	  8 -> "/mainyearcontent";
	  9 -> "/yearmonthcontent";
	  10 -> "/mainyearmonthcontent";
	  _ -> "/content"
   end, 
   case checkcontent(Contents,[]) of
	   {ok,"POST"} -> 
	       NewInfo = term_to_binary(Contents),  
		   case Tag of
		   		 "" ->   
		   		     xn_rest:post(Url,NewInfo); 
		   		  _ ->  
		   		      xn_rest:post(Url,NewInfo,[{"tag",Tag}])
		   	    end;
       {ok,"PUT"} ->
		   NewInfo = term_to_binary(Contents),  
		   case Tag of
		   		 "" ->  
		   		     xn_rest:put(Url,NewInfo); 
		   		  _ ->  
		   		      xn_rest:put(Url,NewInfo,[{"tag",Tag}])
		   	    end;
       ErrorMsg -> ErrorMsg 		
   end; 
	
batchsave(_,_,_) -> {error,"no content list"}.


checkcontent([L|R],"POST") when is_record(L,content) ->
    case L#content.id of
		 <<>> -> checkcontent(R,"POST");
		 _ ->  {error,"no same method"}
	 end;
checkcontent([L|R],"PUT") when is_record(L,content) ->
    case L#content.id of
		 <<>> -> {error,"no same method"};
		 _ ->  checkcontent(R,"PUT")
	 end;	 
checkcontent([L|R],_) when is_record(L,content) ->
    case L#content.id of
		 <<>> -> checkcontent(R,"POST");
		 _ ->  checkcontent(R,"PUT")
	 end; 
checkcontent([_|_],_) -> {error,"no content record"}; 
checkcontent([],Method) -> {ok,Method}.
	
init(Content) when is_record(Content,content)  ->
    %%io:format("xn_content:init(~p)~n",[Content]), 
    {ok, loop,Content}; 	
init({TypeOrNode,Title,Anonymous,Datatype}) when is_list(TypeOrNode) -> init({list_to_binary(TypeOrNode),Title,Anonymous,Datatype});	
init({TypeOrNode,Title,Anonymous,Datatype}) when is_list(Title) -> init({TypeOrNode,list_to_binary(Title),Anonymous,Datatype});
init({TypeOrNode,Title,Anonymous,Datatype}) ->
     %%io:format("xn_content:create(~p)~n",[{TypeOrNode,Title,Anonymous,Datatype}]), 
	 Application = xn:application(),
	 Author =  case Anonymous of
				 true -> <<"anonymous">>;
				 false -> xn:viewer();
				 _ -> <<"anonymous">>
			    end, 
    {ok, loop,#content{application=Application,
					   xn_type=TypeOrNode,
					   title=Title,
					   id = <<>>,
					   author=Author,
					   published = <<>>, 
					   updated = <<>>,
					   datatype=Datatype,
					   my=[]}};
init(_) -> {error,"not a content record."}.

loop(application, _Pid, Info)-> {reply, Info#content.application, loop, Info};
loop(id, _Pid, Info)-> {reply, Info#content.id, loop, Info}; 
loop(author, _Pid, Info)-> {reply, Info#content.author, loop, Info}; 
loop(type, _Pid, Info)-> {reply, Info#content.xn_type, loop, Info};
loop(title, _Pid, Info)-> {reply, Info#content.title, loop, Info};
loop(published, _Pid, Info)-> {reply, Info#content.published, loop, Info};
loop(updated, _Pid, Info)-> {reply, Info#content.updated, loop, Info}; 
loop(dump, _Pid, Info)-> {reply, Info, loop, Info}; 
loop({set,Name,Type,Value}, _Pid, Info)-> 
   My = Info#content.my,
   NewMy = content_set(My,Name,Type,Value), 
   {reply,ok,loop, Info#content{my=NewMy}};
loop({get,Name}, _Pid, Info)-> 
   My = Info#content.my, 
   {reply, content_get(My,Name), loop, Info}; 
   
loop({save,Tag}, _Pid, Info)-> 
   Datatype = Info#content.datatype, 
   Url = case Datatype of
	  0 -> "/content";
	  1 -> "/bigcontent";
	  2 -> "/mq";
	  3 -> "/simplecontent";
	  4 -> "/maincontent";
	  5 -> "/schedule";
	  6 -> "/message";
	  7 -> "/yearcontent";
	  8 -> "/mainyearcontent";
	  9 -> "/yearmonthcontent";
	  10 -> "/mainyearmonthcontent";
	  _ -> "/maincontent" 
   end, 
   NewInfo = term_to_binary([Info]), 
   %%io:format("xn_content:save(~p)~n",[{Url,Tag,Info}]), 
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
	
loop({delete,Tag}, _Pid, Info)-> 
   Id = Info#content.id,
   Datatype = Info#content.datatype,
   Result = delete([binary_to_list(Id)],Tag,Datatype),
   {reply,Result,loop, Info};	
   	
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


content_get(My,Name) when is_list(Name)	-> content_get(My,list_to_binary(Name));
content_get([{Name,Type,Value}|_],Name) -> {ok,{Name,Type,Value}};
content_get([_|R],Name) -> content_get(R,Name);
content_get([],_)	-> {error,"no key"}.

content_set(My,Name,Type,Value) when is_float(Value) -> content_set(My,Name,Type,list_to_binary(xn_common:float_to_str(Value)));
content_set(My,Name,Type,Value) when is_integer(Value) -> content_set(My,Name,Type,list_to_binary(integer_to_list(Value)));
content_set(My,Name,Type,Value) when is_list(Name) -> content_set(My,list_to_binary(Name),Type,Value);
content_set(My,Name,Type,Value) when is_list(Type) -> content_set(My,Name,list_to_binary(Type),Value);
content_set(My,Name,Type,Value) when is_binary(hd(Value)) -> content_set(My,Name,Type,Value,0,[]);
content_set(My,Name,Type,Value) when is_list(hd(Value)) -> 
    Fun = fun(X) when is_list(X) -> list_to_binary(X);
		     (X) -> X
		  end,
    content_set(My,Name,Type,lists:map(Fun,Value),0,[]); 
content_set(My,Name,Type,Value) when is_list(Value) -> content_set(My,Name,Type,list_to_binary(Value),0,[]);
content_set(My,Name,Type,Value)  -> content_set(My,Name,Type,Value,0,[]).

	
content_set([{Name,_,_}|R],Name,Type,Value,_,Acc)	-> content_set(R,Name,Type,Value,1,[{Name,Type,Value}|Acc]);
content_set([L|R],Name,Type,Value,Have,Acc)	-> content_set(R,Name,Type,Value,Have,[L|Acc]);	
content_set([],_,_,_,1,Acc)	-> lists:reverse(Acc);
content_set([],Name,Type,Value,_,Acc)	-> lists:reverse([{Name,Type,Value}|Acc]).