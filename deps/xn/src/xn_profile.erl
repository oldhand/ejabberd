
-module(xn_profile).

-behaviour(gen_fsm).
-vsn('v1.1').
 	 
-export([handle_event/3, handle_sync_event/4, init/1,
        code_change/4, handle_info/3,
     terminate/3]).	 
-compile(export_all).  

-record(content, {application,id, xn_type, title, published, updated, author,datatype, my}). 
 

load(Id) -> load(Id,"id",""). 
load(Id,Tag) -> load(Id,"id",Tag). 
load(Id,Keyname,Tag) -> 
      Url =  lists:flatten(["/profile(",Keyname,"='",Id,"')"]),
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
	  
loadMany(Ids) -> loadMany(Ids,[]). 
loadMany(Ids,Tag) -> 
      Url = lists:flatten(["/profile(id in [",xn_common:list_to_string(Ids,",",[]),"])"]),
	  case Tag of
		  "" -> xn_rest:get(Url);
		  _ -> xn_rest:get(Url,[{"tag",Tag}])
	  end.
 
create(Mail,PassWord) -> 
	gen_fsm:start(?MODULE,{Mail,PassWord},[]). 

new(Content) -> gen_fsm:start(?MODULE,Content,[]).

close(Fsm) -> gen_fsm:send_all_state_event(Fsm, close).
  
profileid(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"profileid"}).
screenname(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"screenname"}).
username(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"username"}).
published(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"published"}).
updated(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"updated"}).
activationdate(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"activationdate"}).
invitationcode(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"invitationcode"}).
accumulatedmoney(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"accumulatedmoney"}).
money(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"money"}).
rank(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"rank"}).
sharefund(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"sharefund"}).
unionid(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"unionid"}).
identifier(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"identifier"}).
wxopenid(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"wxopenid"}).
reg_ip(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"reg_ip"}).
bankaccount(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"bankaccount"}).
bankname(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"bankname"}).
bank(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"bank"}).
companyname(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"companyname"}).
isaudit(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"isaudit"}).
certificatelink(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"certificatelink"}).
businesscardlink(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"businesscardlink"}).
identitycardlink(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"identitycardlink"}).
identitycard(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"identitycard"}).
realname(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"realname"}).
cityarea(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"cityarea"}).
city(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"city"}).
province(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"province"}).
address(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"address"}).
mobileverified(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"mobileverified"}).
status(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"status"}).
qq(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"qq"}).
msn(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"msn"}).
mobile(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"mobile"}).
emailverified(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"emailverified"}).
birthdate(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"birthdate"}).
country(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"country"}).
zipcode(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"zipcode"}).
location(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"location"}).
gender(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"gender"}).
link(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"link"}).
email(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"email"}).
sn(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"sn"}).
givenname(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"givenname"}).
application(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"application"}).
type(Fsm) -> gen_fsm:sync_send_event(Fsm, {get,"type"}).

 
username(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"username",Value}). 
activationdate(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"activationdate",Value}).
invitationcode(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"invitationcode",Value}).
accumulatedmoney(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"accumulatedmoney",Value}).
money(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"money",Value}).
rank(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"rank",Value}).
sharefund(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"sharefund",Value}).
unionid(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"unionid",Value}).
identifier(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"identifier",Value}).
wxopenid(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"wxopenid",Value}).
reg_ip(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"reg_ip",Value}).
bankaccount(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"bankaccount",Value}).
bankname(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"bankname",Value}).
bank(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"bank",Value}).
companyname(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"companyname",Value}).
isaudit(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"isaudit",Value}).
certificatelink(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"certificatelink",Value}).
businesscardlink(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"businesscardlink",Value}).
identitycardlink(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"identitycardlink",Value}).
identitycard(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"identitycard",Value}).
realname(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"realname",Value}).
cityarea(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"cityarea",Value}).
city(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"city",Value}).
province(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"province",Value}).
address(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"address",Value}).
mobileverified(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"mobileverified",Value}).
status(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"status",Value}).
qq(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"qq",Value}).
msn(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"msn",Value}).
mobile(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"mobile",Value}).
emailverified(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"emailverified",Value}).
birthdate(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"birthdate",Value}).
country(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"country",Value}).
zipcode(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"zipcode",Value}).
location(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"location",Value}).
gender(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"gender",Value}).
link(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"link",Value}).
email(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"email",Value}).
sn(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"sn",Value}).
givenname(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"givenname",Value}).
application(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"application",Value}).
type(Fsm,Value) -> gen_fsm:sync_send_event(Fsm, {set,"type",Value}).

 
save(Fsm) ->gen_fsm:sync_send_event(Fsm, {save,""},infinity).
save(Fsm,Tag) ->gen_fsm:sync_send_event(Fsm, {save,Tag},infinity).

init(Content) when is_record(Content,content)  ->
    %%io:format("xn_profile:init(~p)~n",[Content]), 
    {ok, loop,Content}; 	
init({Mail,PassWord}) ->
     io:format("xn_profile:create(~p)~n",[{Mail,PassWord}]),  
     {ok, loop,#content{application = <<>>,
					   xn_type = <<"profile">>,
					   title = <<>>,
					   id = <<>>,
					   author = <<>>,
					   published = <<>>, 
					   updated = <<>>,
					   datatype=0,
					   my=[{<<"email">>,<<"string">>,list_to_binary(Mail)},
					       {<<"password">>,<<"string">>,list_to_binary(PassWord)}]}};
init(_) -> {error,"not a content record."}.

loop({get,KeyName}, _Pid, Info)-> {reply, profile_get(Info,KeyName), loop, Info};
loop({set,KeyName,Value}, _Pid, Info)->  
       Newinfo = profile_set(Info,KeyName,Value),
	   {reply, ok, loop, Newinfo}; 


   
loop({save,Tag}, _Pid, Info)->  
   Url =  "/profile",
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


profile_get(Info,KeyName) when is_list(KeyName)	-> profile_get(Info,list_to_binary(KeyName));
profile_get(Info,<<"profileid">>) -> Info#content.id;
profile_get(Info,<<"screenname">>) -> Info#content.id;
profile_get(Info,<<"username">>) -> Info#content.title;
profile_get(Info,<<"published">>) -> Info#content.published;
profile_get(Info,<<"updated">>) -> Info#content.updated;
profile_get(Info,KeyName) -> content_get(Info#content.my,KeyName). 
 
content_get([{Name,_,Value}|_],Name) -> Value;
content_get([_|R],Name) -> content_get(R,Name);
content_get([],_)	-> {error,"no key"}.


 


profile_set(Info,KeyName,Value) when is_float(Value) -> profile_set(Info,KeyName,list_to_binary(xn_common:float_to_str(Value))); 
profile_set(Info,KeyName,Value) when is_integer(Value) -> profile_set(Info,KeyName,list_to_binary(integer_to_list(Value))); 
profile_set(Info,KeyName,Value) when is_list(KeyName) -> profile_set(Info,list_to_binary(KeyName),Value);
profile_set(Info,KeyName,Value) when is_list(Value) -> profile_set(Info,KeyName,list_to_binary(Value)); 
profile_set(Info,<<"username">>,Value)  -> Info#content{title=Value};  
profile_set(Info,KeyName,Value)  -> 
	Newmy = content_set(Info#content.my,KeyName,Value,0,[]),
    Info#content{my=Newmy}. 

	
content_set([{Name,_,_}|R],Name,Value,_,Acc)	-> content_set(R,Name,Value,1,[{Name,<<"string">>,Value}|Acc]);
content_set([L|R],Name,Value,Have,Acc)	-> content_set(R,Name,Value,Have,[L|Acc]);	
content_set([],_,_,1,Acc)	-> lists:reverse(Acc);
content_set([],Name,Value,_,Acc) -> lists:reverse([{Name,<<"string">>,Value}|Acc]).