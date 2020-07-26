-module(xn_common).
-compile(export_all).

list_to_string([<<>>], Space,Acc) -> list_to_string([], Space,Acc);
list_to_string([<<>>|R], Space,Acc) ->  list_to_string(R, Space,Acc);
list_to_string([[]], Space,Acc) -> list_to_string([], Space,Acc);
list_to_string([[]|R], Space,Acc) ->  list_to_string(R, Space,Acc);
list_to_string([H], Space,Acc) when is_list(H)  -> list_to_string([], Space,[H|Acc]);
list_to_string([H], Space,Acc) when is_integer(H)  -> list_to_string([], Space,[integer_to_list(H)|Acc]);
list_to_string([H|R], Space,Acc) when is_list(H) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([H|R], Space,Acc) when is_integer(H) ->  list_to_string(R, Space,[Space,integer_to_list(H)|Acc]);
list_to_string([H], Space,Acc) -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([], _,Acc) -> lists:reverse(Acc).
list_to_string(List,Space) ->
  lists:flatten(list_to_string(List,Space,[])). 
  
  
  
  
replace(List, Find, Replace)->	replace(List, Find, Replace, []).

replace_contact(Acc, [H|R])-> replace_contact([H|Acc],R);
replace_contact(Acc, [])-> Acc.

replace_compare([C|L], [C|R], Acc)-> replace_compare(L, R, [C|Acc]);
replace_compare(L, [], _)-> {true, L, []};
replace_compare(L, _, Acc)-> {false, L, lists:reverse(Acc)}.

replace([C|L], [C|R] = Find, Replace, Acc)->
case replace_compare(L, R, [C]) of
	{true, Rest1, _} ->
		replace(Rest1, Find, Replace, replace_contact(Acc, Replace));
	{_, Rest2, Result2}->
		replace(Rest2,  Find, Replace,replace_contact(Acc, Result2))
end;
replace([C|L], Find, Replace, Acc)-> replace(L, Find, Replace, [C|Acc]);
replace([], _, _, Acc)-> lists:reverse(Acc).

 
 

to_number(Bin) when is_binary(Bin) -> to_number(binary_to_list(Bin));
to_number(String) -> 
      NewString = number(String),
	  case string:to_float(NewString) of
		  {FloatValue,_} when is_float(FloatValue) -> FloatValue;
		  _ ->
	  	  case string:to_integer(NewString) of
	  		  {IntegerValue,_} when is_integer(IntegerValue) -> IntegerValue;
	  		  _ -> 0 
	  	  end
	  end. 

 

number(List) -> 
    Filters = "0123456789.",
    IsMatch = fun(Byte) ->  lists:any(fun(X) -> X == Byte end, Filters) end, 
    lists:filter(IsMatch, List).

to_float(Bin) when is_binary(Bin) -> to_float(binary_to_list(Bin));
to_float(String) -> string:to_float(number(String)).

to_integer(Bin) when is_binary(Bin) -> to_integer(binary_to_list(Bin));
to_integer(String) -> string:to_integer(number(String)).
	
money_to_str(Money) ->	
   string:strip(lists:flatten(io_lib:format("~40.2f",[Money])), both, $ ).	

float_to_str(0) ->	"0";
float_to_str(Float) when is_integer(Float) -> integer_to_list(Float);
float_to_str(Float) ->	
   Str = lists:flatten(io_lib:format("~40.10f",[Float])),
   NewStr = string:strip(Str,right,$0),
   string:strip(NewStr, left, $ ).	
   
float_to_money(0) ->	"0.00";
float_to_money(Float) when is_integer(Float) -> integer_to_list(Float)++".00";
float_to_money(Float) -> lists:flatten(io_lib:format("~.2f",[Float])).
	
chunk(List, N) ->
    partition(List, length(List)/N+1).


partition(List, N) ->
    partition(List, 1, N, []).

partition([], _C, _N, Acc) ->
    lists:reverse(Acc) ;

partition([H|T], 1, N, Acc) ->
    partition(T, 2, N, [[H]|Acc]) ;

partition([H|T], C, N, [HAcc|TAcc]) when C < N ->
    partition(T, C+1, N, [[H|HAcc]|TAcc]) ;

partition([H|T], C, N, [HAcc|TAcc]) when C == N ->
    partition(T, 1, N, [lists:reverse([H|HAcc])|TAcc]) ;

partition(L, C, N, Acc) when C > N ->
    partition(L, 1, N, Acc).
	
merge(List) ->  merge(List,[]).
merge([L|R],Acc) when is_list(L) ->   merge(R,L++Acc);
merge([L|R],Acc) ->  merge(R,[L|Acc]);
merge([],Acc) ->  lists:reverse(Acc).	
		