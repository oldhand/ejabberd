
-module(xn_query).
  
-behaviour(gen_fsm).
-vsn('v1.1').
 	 
-export([handle_event/3, handle_sync_event/4, init/1,
        code_change/4, handle_info/3,
     terminate/3]).	 
-compile(export_all).  

% -record(content, {application,id, xn_type, title, published, updated, author,datatype, my}).
-record(xn_query,{subject,tag,count,filters,rollups,rollup,groups,'begin','end',sortorder,orderby}). 
 
-define(OPERATORS, ["=","<>","eic","neic","<","<=",">",">=","like","likeic","in","!in"]).
-define(SUBJECTS,["Content",
            "BigContent",
            "MainContent",
            "YearContent",
            "YearMonthContent",
            "Mq",
            "Profile",
			"Application",
            "Content_Count",
            "BigContent_Count",
            "Profile_Count",
            "MainContent_Count",
            "YearContent_Count",
            "YearMonthContent_Count" ]).
			
-define(SUBJECTS_COUNTS,{"Content_Count","BigContent_Count","Profile_Count","MainContent_Count","YearContent_Count","YearMonthContent_Count"}).

 
create(Subject) -> create(Subject,lists:member(Subject, ?SUBJECTS)).
	
create(Subject,true) -> gen_fsm:start(?MODULE,Subject,[]);
create(_,_) -> {error,"no subject name"}.  

close(Fsm) -> gen_fsm:send_all_state_event(Fsm, close).

tag(Fsm,Tag) -> gen_fsm:sync_send_event(Fsm, {tag,Tag}). 
'begin'(Fsm,Begin) -> gen_fsm:sync_send_event(Fsm, {'begin',Begin}). 
'end'(Fsm,End) -> gen_fsm:sync_send_event(Fsm, {'end',End}).
filter(Fsm,Prop,Operator,Value) -> gen_fsm:sync_send_event(Fsm, {filter,Prop,Operator,Value}).
filter(Fsm,Filter) -> gen_fsm:sync_send_event(Fsm, {filter,Filter}).
rollup(Fsm) -> gen_fsm:sync_send_event(Fsm, {rollup}).
rollup(Fsm,Rollup) -> gen_fsm:sync_send_event(Fsm, {rollup,Rollup}).
group(Fsm,Group) -> gen_fsm:sync_send_event(Fsm, {group,Group}).
order(Fsm,Prop,Direction) -> gen_fsm:sync_send_event(Fsm, {order,Prop,Direction}).
count(Fsm) -> gen_fsm:sync_send_event(Fsm, count). 
execute(Fsm) -> gen_fsm:sync_send_event(Fsm, execute,infinity). 

init(NSubject) ->
     %%io:format("xn_query:create(~p)~n",[NSubject]),  
     {ok, loop,#xn_query{subject=NSubject,
	 					 tag="",
						 count=0,
						 filters=[],
						 rollups=[],
						 rollup=[], 
						 groups=[],
						 'begin'=0,
						 'end'=100,
						 sortorder="",
						 orderby=""}}. 

loop({tag,Tag}, _Pid, Info)-> {reply, ok, loop, Info#xn_query{tag=Tag}};
loop({'begin',Begin}, _Pid, Info)-> {reply, ok, loop, Info#xn_query{'begin'=Begin}};
loop({'end',End}, _Pid, Info)-> {reply, ok, loop, Info#xn_query{'end'=End}};


loop({rollup}, _Pid, Info)->  {reply, ok, loop, Info#xn_query{rollup=true}};
loop({rollup,Rollup}, _Pid, Info)-> 
    OldRollup = Info#xn_query.rollups,
	NewRollup = [Rollup|OldRollup],
    {reply, ok, loop, Info#xn_query{rollups=NewRollup}};
loop({group,Group}, _Pid, Info)-> 
    OldGroup = Info#xn_query.groups,
	NewGroup = [Group|OldGroup],
    {reply, ok, loop, Info#xn_query{groups=NewGroup}};

loop({order,Prop,Direction}, _Pid, Info)-> {reply, ok, loop, Info#xn_query{sortorder=Prop,orderby=Direction}};
loop({filter,Filter}, _Pid, Info)-> 
 	Filters = Info#xn_query.filters,
    {reply, ok, loop, Info#xn_query{filters=[Filter|Filters]}}; 
loop({filter,Prop,Operator,Value}, _Pid, Info) when is_integer(Value) -> 
    Filters = Info#xn_query.filters, 
	case lists:member(Operator, ?OPERATORS) of
		true ->
			 Filter = lists:flatten([Prop," ",Operator," ",integer_to_list(Value),""]),
			 {reply, ok, loop, Info#xn_query{filters=[Filter|Filters]}};
		_ ->
			{reply, {error,"error operator"}, loop, Info}
	end; 
loop({filter,Prop,"in",Value}, _Pid, Info)-> 
    Filters = Info#xn_query.filters,  
	Filter = lists:flatten([Prop," in [",xn_filter:list_to_string(Value),"]"]),
	{reply, ok, loop, Info#xn_query{filters=[Filter|Filters]}};	
loop({filter,Prop,"!in",Value}, _Pid, Info)-> 
    Filters = Info#xn_query.filters,  
	Filter = lists:flatten([Prop," !in [",xn_filter:list_to_string(Value),"]"]),
	{reply, ok, loop, Info#xn_query{filters=[Filter|Filters]}};	
loop({filter,Prop,Operator,Value}, _Pid, Info)-> 
    Filters = Info#xn_query.filters, 
	case lists:member(Operator, ?OPERATORS) of
		true ->
			Filter = lists:flatten([Prop," ",Operator," '",Value,"'"]),
			 {reply, ok, loop, Info#xn_query{filters=[Filter|Filters]}};
		_ ->
			{reply, {error,"error operator"}, loop, Info}
	end; 		

		 
loop(count, _Pid, Info)->  
    {reply, Info#xn_query.count, loop, Info}; 
loop(execute, _Pid, Info)-> 
   Filters = lists:reverse(Info#xn_query.filters), 
   Subject = Info#xn_query.subject, 
   
   Rollup = case Info#xn_query.rollup of
	            true -> "/rollup()";
				_ -> 
				    case Info#xn_query.rollups of
						[] -> "";
						Rollups -> 
							 RollupFields = [ ["field = '",Fieldname,"'"] || Fieldname  <- Rollups],
							 ["/rollup(",xn_common:list_to_string(RollupFields,"&"),")"] 
					end
  	 		end,
   Group =  case Info#xn_query.groups of
				[] -> "";
				Groups -> 
					 GroupFields = [ ["field = '",Fieldname,"'"] || Fieldname  <- Groups],
					 ["/group(",xn_common:list_to_string(GroupFields,"&"),")"] 
			 end,
			 
			 
   Prepareurl = case Subject of
				   "Content" -> lists:flatten(["/content(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "MainContent" -> lists:flatten(["/maincontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "Mq" -> lists:flatten(["/mq(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
		           "YearContent" -> lists:flatten(["/yearcontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
		           "YearMonthContent" -> lists:flatten(["/yearmonthcontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "BigContent" -> lists:flatten(["/bigcontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "Content_Count" -> lists:flatten(["/content(",xn_common:list_to_string(Filters,"&",[]),")",Rollup,Group]);
				   "Profile" -> lists:flatten(["/profile(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "Application" -> lists:flatten(["/application(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "Profile_Count" -> lists:flatten(["/profile(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   "YearContent_Count" -> lists:flatten(["/yearcontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
		           "YearMonthContent_Count" -> lists:flatten(["/yearmonthcontent(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"]);
				   _ -> lists:flatten(["/content(",xn_common:list_to_string(Filters,"&",[]),Rollup,Group,")"])
			    end,
     
   Begin = Info#xn_query.'begin', 
   End = Info#xn_query.'end', 

  	 		 
   Rangeurl = lists:flatten(["?from=" , integer_to_list(Begin) , "&to=" , integer_to_list(End) ]),
 
   Orderurl = case Info#xn_query.orderby of
	                 "DESC" -> ["&order=",Info#xn_query.sortorder,"@D"];
					 "ASC" -> ["&order=",Info#xn_query.sortorder,"@A"];
					 "DESC_NUMBER" -> ["&order=",Info#xn_query.sortorder,"@D_N"];
					 "ASC_NUMBER" -> ["&order=",Info#xn_query.sortorder,"@A_N"];
					 _ -> ""
   			    end,
				
   FinishUrl = lists:flatten([Prepareurl,Rangeurl,Orderurl ]), 
   case Info#xn_query.tag of
	 "" ->  
	      case xn_rest:get(FinishUrl) of
			   {ok,{Count,ContentLists}} ->
				   {reply, {ok,ContentLists}, loop, Info#xn_query{count=Count}};
			   ErrorMsg ->
				    {reply, ErrorMsg, loop, Info#xn_query{count=0}}
		  end;
	 Tag ->  
	 
	     case xn_rest:get(FinishUrl,[{"tag",Tag}]) of
		   {ok,{Count,ContentLists}} ->   
			   {reply, {ok,ContentLists}, loop, Info#xn_query{count=Count}};
		   ErrorMsg ->
			    {reply, ErrorMsg, loop, Info#xn_query{count=0}}
	     end 
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


