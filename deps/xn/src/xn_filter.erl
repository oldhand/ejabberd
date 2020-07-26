-module(xn_filter).

-compile(export_all).

-define(OPERATORS, ["=","<>","eic","neic","<","<=",">",">=","like","likeic","in","!in"]).

filter(Prop,Operator,Value) when is_integer(Value) -> 
   case lists:member(Operator, ?OPERATORS) of
		true ->
			 lists:flatten([Prop," ",Operator," ",integer_to_list(Value),""]);
		_ -> [] 
	end; 
	
filter(Prop,"in",Value)   -> 
    lists:flatten([Prop," in [",list_to_string(Value),"]"]);

filter(Prop,"!in",Value)   -> 
    lists:flatten([Prop," !in [",list_to_string(Value),"]"]);	
	 	
filter(Prop,Operator,Value) -> 
   case lists:member(Operator, ?OPERATORS) of
		true ->
			 lists:flatten([Prop," ",Operator," '",Value,"'"]);
		_ -> [] 
	end. 	
	
	
	
	
any(Filter1,Filter2) -> "("++filter([Filter1,Filter2],"|")++")".	
any(Filter1,Filter2,Filter3) -> "("++filter([Filter1,Filter2,Filter3],"|")++")".	
any(Filter1,Filter2,Filter3,Filter4) -> "("++filter([Filter1,Filter2,Filter3,Filter4],"|")++")".	
any(Filter1,Filter2,Filter3,Filter4,Filter5) -> "("++filter([Filter1,Filter2,Filter3,Filter4,Filter5],"|")++")".	
any(Filter1,Filter2,Filter3,Filter4,Filter5,Filter6) -> "("++filter([Filter1,Filter2,Filter3,Filter4,Filter5,Filter6],"|")++")".	

	
all(Filter1,Filter2) -> "("++filter([Filter1,Filter2],"&")++")".	
all(Filter1,Filter2,Filter3) -> "("++filter([Filter1,Filter2,Filter3],"&")++")".	
all(Filter1,Filter2,Filter3,Filter4) -> "("++filter([Filter1,Filter2,Filter3,Filter4],"&")++")".	
all(Filter1,Filter2,Filter3,Filter4,Filter5) -> "("++filter([Filter1,Filter2,Filter3,Filter4,Filter5],"&")++")".	
all(Filter1,Filter2,Filter3,Filter4,Filter5,Filter6) -> "("++filter([Filter1,Filter2,Filter3,Filter4,Filter5,Filter6],"&")++")".




filter_string([[]], Space,Acc) -> filter_string([], Space,Acc); 
filter_string([H], Space,Acc) -> filter_string([], Space,[H|Acc]); 
filter_string([[]|R], Space,Acc) ->  filter_string(R, Space,Acc); 
filter_string([H|R], Space,Acc)  ->  filter_string(R, Space,[Space,H|Acc]); 
filter_string([], _,Acc) -> lists:reverse(Acc).
filter(List,Space) -> lists:flatten(filter_string(List,Space,[])). 
	
 
 
list_to_string([<<>>],Acc) -> list_to_string([], Acc);
list_to_string([<<>>|R], Acc) ->  list_to_string(R, Acc);
list_to_string([[]], Acc) -> list_to_string([], Acc);
list_to_string([[]|R], Acc) ->  list_to_string(R, Acc);
list_to_string([H], Acc) when is_list(H)  -> list_to_string([], [["'",H,"'"]|Acc]);
list_to_string([H], Acc) when is_integer(H)  -> list_to_string([],[["'",integer_to_list(H),"'"]|Acc]);
list_to_string([H|R], Acc) when is_list(H) ->  list_to_string(R, [["'",H,"',"]|Acc]);
list_to_string([H|R], Acc) when is_integer(H) ->  list_to_string(R, [["'",integer_to_list(H),"',"]|Acc]);
list_to_string([H], Acc) -> list_to_string([], [["'",H,"'"]|Acc]);
list_to_string([H|R], Acc) ->  list_to_string(R, [["'",H,"',"]|Acc]);
list_to_string([],Acc) -> lists:reverse(Acc).
list_to_string(List) ->
		lists:flatten(list_to_string(List,[])).