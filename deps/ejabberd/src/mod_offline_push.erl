-module(mod_offline_push).
% -compile(export_all).
-export([push/1]).  
-export([push/3]).  
-export([mucpush/4]).  

-include("xmpp.hrl").
-include("mod_offline.hrl").
-include("logger.hrl"). 

 
-record(session, {profileid,token,platform,language,messagepush}).  


authorization(<<"android">>) ->
    Appid = "14a1057f592d9",
    Secretkey = "245a0881ca8d205122e14c22c98838e6",
    lists:flatten([Appid,":",Secretkey]);
authorization(_) -> 
  Appid = "6e8ac64b2a34e",
  Secretkey = "1d4acae30fc18dd7d442cfe815b33807",
  lists:flatten([Appid,":",Secretkey]).
  
  

mucpush(Mucid, OnlineUers,Affiliations,Msg) when Msg#message.from =:= undefined ->  ok;
mucpush(Mucid, OnlineUers,Affiliations,Msg) ->  	 
	 mod_offline_push_session:start(),
	 io:format("________mucpush________~p_____________~n~n~n",[{Mucid, OnlineUers,Affiliations}]),
	 %%io:format("________push_________~p________________________~n~n~n",[{Mucid, OnlineUers,Affiliations,Msg}]),
   	 %MessageType = Msg#message.type,
	 From = Msg#message.from,
	 From_Profileid = From#jid.user, 
   	 Sub_els = Msg#message.sub_els,
   	 MessageBody = hd(Msg#message.body), 
   	 MessageText = MessageBody#text.data, 
	 case {Msg#message.id,sub_els(Sub_els,<<"msgtype">>)} of
		  {Msgid,Msgtype} when Msgid =/= <<>> andalso Msgtype =/= <<>> ->  
		     case  ignoreMsgtype(Msgtype) of
				 true ->
					 Mucname = mod_offline_push_session:get_mucname(Mucid), 
					 io:format("___muc___Msgid:~p______Msgtype:~p___________~n",[Msgid,Msgtype]), 
				     lists:foreach(
				        fun(To) ->
				  	        case lists:member(To, OnlineUers) of
								false -> 
									 case mod_offline_push_session:check_groups(To,Mucid) of 
										 true  ->
											 Base64Ciphertext = base64:decode(MessageText),
								   			 PlainText = mod_offline_crypto:decrypt_public(Base64Ciphertext, ""), 
											 case mod_offline_push_session:get(To) of
												  {ok,#session{token=Token,platform=Platform,language=Language,messagepush=Messagepush}} when Messagepush =:= <<"1">> ->
										 			  Base64Ciphertext = base64:decode(MessageText),
										 			  PlainText = mod_offline_crypto:decrypt_public(Base64Ciphertext, ""), 
													  io:format("~n~n____muc__Token:~p____Platform:~p____:Language~p_____Messagepush:~p____~n~n",[Token,Platform,Language,Messagepush]),
													  xg_mucpush(Mucid,Mucname,Token,From_Profileid,replace(PlainText,"\n",""),Msgtype,Msgid,Language,Platform);
												  {ok,Push} ->
											 		 io:format("_____muc___User[~p]_____~p_______________~n",[To,Push]);
								 				  _ ->
								 				     io:format("_____muc___User[~p] do not push binding information._______________~n",[To])
								 			 end;
										 _ ->
											 io:format("_____check_groups_false__User[~p]_____~p______~n",[To,Mucid]) 
									 end; 
								_ -> ok
							end
				        end, 
					 Affiliations);
				 _ -> ok
			 end;
		  _ -> ok 
	 end. 
	 
xg_mucpush(Mucid,Mucname,Token,From,MessageText,Msgtype,Msgid,Language,Platform) -> 
 	  Pushbody = xg_mucpushbody(Mucid,Mucname,Token,From,MessageText,Msgtype,Msgid,Language,Platform), 
  	  Url = lists:flatten(["https://openapi.xg.qq.com/v3/push/app"]), 
	  Authorization = list_to_binary(lists:flatten(["Basic ",binary_to_list(base64:encode(authorization(Platform)))])),
      case http_header_post(Url,Pushbody,[{"Authorization",Authorization}]) of
  		 {ok,ResponseBody} ->
  			    io:format("ResponseBody : ~p",[ResponseBody]);
  		 ErrorMsg -> 
  		     io:format("error : ~p",[ErrorMsg]),
  			 {error,ErrorMsg}
  	end.
	
xg_mucpushbody(Mucid,Mucname,Token,From,MessageText,Msgtype,Msgid,Language,<<"android">>) -> 
		Givenname = get_givenname(From,Language),
		Content = [binary_to_list(Givenname),": ",makepushmessage(Msgtype,MessageText,Language)],
	    lists:flatten(["{",
		    "\"platform\": \"android\",",
		    "\"audience_type\": \"token\",",
		    "\"token_list\": [\"",binary_to_list(Token),"\"],", 
		    "\"message_type\": \"notify\",",
		    "\"message\": {",
		        "\"title\": \"",binary_to_list(Mucname),"\",",
		        "\"content\": \"",lists:flatten(Content),"\",",
				"\"android\": {",
		        		"\"custom_content\": { \"from\": \"",binary_to_list(From),"\",\"groupchatid\": \"",binary_to_list(Mucid),"\",\"msgid\": \"",binary_to_list(Msgid),"\"}",
				 "}",
		    "}",
		  "}"]);	 
	 
xg_mucpushbody(Mucid,Mucname,Token,From,MessageText,Msgtype,Msgid,Language,_) -> 
		Givenname = get_givenname(From,Language),
		Content = [binary_to_list(Givenname),": ",makepushmessage(Msgtype,MessageText,Language)], 
 	    lists:flatten(["{",
 		    "\"platform\": \"ios\",",
 			% "\"environment\":\"dev\",",
 		    "\"audience_type\": \"token\",",
 		    "\"token_list\": [\"",binary_to_list(Token),"\"],", 
 		    "\"message_type\": \"notify\",",
 		    "\"message\": {",
 		        "\"title\": \"",binary_to_list(Mucname),"\",",
 		        "\"content\": \"",lists:flatten(Content),"\",",
 				"\"ios\":{",
 				        "\"aps\": {",  
 				            "\"sound\":\"Tassel.wav\"", 
 				        "},",
						"\"from\": \"",binary_to_list(From),"\",",
						"\"groupchatid\": \"",binary_to_list(Mucid),"\",",
						"\"msgid\": \"",binary_to_list(Msgid),"\",",
 				        "\"xg\": \"oops\"",
 				    "}",
 		    "}",
 		  "}"]). 
	 
ignoreMsgtype(<<"106">>) -> false;
ignoreMsgtype(<<"102">>) -> false;
ignoreMsgtype(<<"6">>) -> false;
ignoreMsgtype(_) -> true.
  
push(Msg) ->   
	 From = Msg#offline_msg.from,
	 To = Msg#offline_msg.to, 
	 Pkt = Msg#offline_msg.packet,   
	 push(To,From,Pkt).
	 
push(To,From,Pkt) when Pkt#message.body =:= [] ->  ok; 
	 
push(To,From,Pkt) ->  
	 % io:format("________push_________~p________~n~n~n",[{To,From,Pkt}]),
	 mod_offline_push_session:start(),
 	 To_Profileid = To#jid.user,
	 From_Profileid = From#jid.user,
	 Sub_els = Pkt#message.sub_els,
	 MessageBody = hd(Pkt#message.body), 
	 MessageText = MessageBody#text.data, 
	 case {Pkt#message.id,sub_els(Sub_els,<<"msgtype">>)} of
		  {Msgid,Msgtype} when Msgid =/= <<>> andalso Msgtype =/= <<>> -> 
		     case ignoreMsgtype(Msgtype) of
				   true ->
		  			 case mod_offline_push_session:get(To_Profileid) of
		  				  {ok,#session{token=Token,platform=Platform,language=Language,messagepush=Messagepush}} when Messagepush =:= <<"1">> ->
		  		 			  Base64Ciphertext = base64:decode(MessageText),
		  		 			  PlainText = mod_offline_crypto:decrypt_public(Base64Ciphertext, ""), 
		  		 			  io:format("~n______Msgid:~p______Msgtype:~p___________~n",[Msgid,Msgtype]),
		  					  io:format("______Token:~p____Platform:~p____:Language~p_____Messagepush:~p____~n",[Token,Platform,Language,Messagepush]),
		  					  xg_push(Token,From_Profileid,replace(PlainText,"\n",""),Msgtype,Msgid,Language,Platform);
		  				  {ok,Push} ->
		  			 		 io:format("________User[~p]_____~p_______________~n",[To_Profileid,Push]);
		   				  _ ->
		   				     io:format("________User[~p] do not push binding information._______________~n",[To_Profileid])
		   			 end;
				   _ -> ok
			 end;
		  _ -> ok 
	 end. 
	  
xg_push(Token,From,MessageText,Msgtype,Msgid,Language,Platform) -> 
 	  Pushbody = xg_pushbody(Token,From,MessageText,Msgtype,Msgid,Language,Platform), 
  	  Url = lists:flatten(["https://openapi.xg.qq.com/v3/push/app"]), 
	  Authorization = list_to_binary(lists:flatten(["Basic ",binary_to_list(base64:encode(authorization(Platform)))])),
      case http_header_post(Url,Pushbody,[{"Authorization",Authorization}]) of
  		 {ok,ResponseBody} ->
  			    io:format("~n~nResponseBody : ~p~n~n",[ResponseBody]);
  		 ErrorMsg -> 
  		     io:format("error : ~p",[ErrorMsg]),
  			 {error,ErrorMsg}
  	end.
	
xg_pushbody(Token,From,MessageText,Msgtype,Msgid,Language,<<"android">>) -> 
		Givenname = get_givenname(From,Language),
		Content = makepushmessage(Msgtype,MessageText,Language), 
	    lists:flatten(["{",
		    "\"platform\": \"android\",",
		    "\"audience_type\": \"token\",",
		    "\"token_list\": [\"",binary_to_list(Token),"\"],", 
		    "\"message_type\": \"notify\",",
		    "\"message\": {",
		        "\"title\": \"",binary_to_list(Givenname),"\",",
		        "\"content\": \"",Content,"\",",
				"\"android\": {",
		        	"\"custom_content\": { \"from\": \"",binary_to_list(From),"\",\"msgid\": \"",binary_to_list(Msgid),"\"}",
				"}",
		    "}",
		  "}"]);
xg_pushbody(Token,From,MessageText,Msgtype,Msgid,Language,_) -> 
		Givenname = get_givenname(From,Language),
		Content = makepushmessage(Msgtype,MessageText,Language), 
	    lists:flatten(["{",
		    "\"platform\": \"ios\",",
			% "\"environment\":\"dev\",",
		    "\"audience_type\": \"token\",",
		    "\"token_list\": [\"",binary_to_list(Token),"\"],", 
		    "\"message_type\": \"notify\",",
		    "\"message\": {",
		        "\"title\": \"",binary_to_list(Givenname),"\",",
		        "\"content\": \"",lists:flatten(Content),"\",",
				"\"ios\":{",
				        "\"aps\": {",  
				            "\"sound\":\"Tassel.wav\"", 
				        "},",
						"\"from\": \"",binary_to_list(From),"\",",
					    "\"msgid\": \"",binary_to_list(Msgid),"\",",
				        "\"xg\": \"oops\"",
				    "}",
		    "}",
		  "}"]).			 
	 
	
sub_els([{xmlel,<<"chat">>,_,Info}|_],Key) -> sub_els(Info,Key); 
sub_els([{xmlel,Key,[],[{_,Value}]}|_],Key) -> Value;
sub_els([_|R],Key) -> sub_els(R,Key);
sub_els([],_) -> <<>>.


get_givenname(<<"system">>,Language) -> list_to_binary(language(Language,"system"));
get_givenname(From,_) -> mod_offline_push_session:get_givenname(From).

msgtype(<<"1">>) -> "text"; 
msgtype(<<"2">>) -> "image"; 
msgtype(<<"3">>) -> "voice"; 
msgtype(<<"4">>) -> "video"; 
msgtype(<<"5">>) -> "link"; 
msgtype(<<"6">>) -> "recalled"; 
msgtype(<<"7">>) -> "chat history"; 
msgtype(<<"8">>) -> "quote and reply"; 
msgtype(<<"9">>) -> "contact card"; 
msgtype(<<"100">>) -> "validate apply"; 
msgtype(<<"101">>) -> "validate passed"; 
msgtype(<<"102">>) -> "read receipt"; 
msgtype(<<"103">>) -> "notify message"; 
msgtype(<<"104">>) -> "group notify message"; 
msgtype(<<"105">>) -> "burn after reading receipt"; 
msgtype(<<"106">>) -> "Pulled block"; 
msgtype(<<"107">>) -> "Relieve certification"; 
msgtype(<<"108">>) -> "moments notify message"; 
msgtype(<<"110">>) -> "validate refused"; 
msgtype(<<"201">>) -> "group notify message"; 
msgtype(_) -> "text".
  

makepushmessage(Msgtype,MessageText,Language) when is_binary(MessageText) -> makepushmessage(Msgtype,binary_to_list(MessageText),Language);			
makepushmessage(<<"1">>,MessageText,_) -> MessageText; 
makepushmessage(Msgtype,_,<<"en_us">>) -> lists:flatten([" send a ",msgtype(Msgtype)," message"]);
makepushmessage(Msgtype,_,Language) -> lists:flatten([language(Language," send a "),language(Language,msgtype(Msgtype)),language(Language,"message"),"."]). 
	  
 
	  
http_header_post(Url,Body,Headers) when is_binary(Url) ->  http_header_post(binary_to_list(Url),Body,Headers); 	 
http_header_post(Url,Body,Headers) ->   
	   NewHeaders = [{K,binary_to_list(V)} || {K,V} <- Headers],
	  try httpc:request(post,{Url,NewHeaders,"application/json",Body},[{timeout, timer:seconds(60)}],[{body_format, binary}]) of
	   	{ok, {{_,200,_},_,ResponseBody}} -> {ok,ResponseBody};  
	   	{error, ErrorResult} -> {error,ErrorResult};
	   	ErrorResult -> {error,ErrorResult}
	  catch
	         error:Error ->
		 	      ErrorInfo = io_lib:format("~p",[Error]),
		 	      ErrorMsg  = lists:flatten(["http_post catch Error : ",ErrorInfo]),
	              io:format(ErrorMsg),
	 	          {error,ErrorMsg}
	  end.	  
 
 

language(<<>>,Key) -> language(<<"zh_CN">>,Key);
language(<<"en_US">>,"system") -> utf8("Eyou Assistant");
language(<<"zh_CN">>," send a ") -> utf8("发送了一条");
language(<<"zh_CN">>,"message") -> utf8("消息");

 
language(<<"zh_CN">>,"text") -> utf8("文本"); 
language(<<"zh_CN">>,"image") -> utf8("图片"); 
language(<<"zh_CN">>,"voice") -> utf8("语音");
language(<<"zh_CN">>,"video") -> utf8("视频");
language(<<"zh_CN">>,"link") -> utf8("链接");
language(<<"zh_CN">>,"recalled") -> utf8("撤回"); 
language(<<"zh_CN">>,"chat history") -> utf8("聊天记录");
language(<<"zh_CN">>,"quote and reply") -> utf8("引用与回复");
language(<<"zh_CN">>,"contact card") -> utf8("名片");
language(<<"zh_CN">>,"validate apply") -> utf8("好友申请");
language(<<"zh_CN">>,"validate passed") -> utf8("好友申请通过");
language(<<"zh_CN">>,"read receipt") -> utf8("已读");
language(<<"zh_CN">>,"notify message") -> utf8("通知");
language(<<"zh_CN">>,"moments notify message") -> utf8("朋友圈通知");
language(<<"zh_CN">>,"group notify message") -> utf8("群通知");
language(<<"zh_CN">>,"burn after reading receipt") -> utf8("阅后即焚回执"); 
language(<<"zh_CN">>,"validate refused") -> utf8("申请拒绝");
language(<<"zh_CN">>,"group notify message") -> utf8("群通知"); 
language(<<"zh_CN">>,"system") -> utf8("医友助手");



language(<<"zh_HK">>," send a ") -> utf8("發送了壹條");
language(<<"zh_HK">>,"message") -> utf8("消息");

language(<<"zh_HK">>,"text") -> utf8("文本"); 
language(<<"zh_HK">>,"image") -> utf8("圖片"); 
language(<<"zh_HK">>,"voice") -> utf8("語音");
language(<<"zh_HK">>,"video") -> utf8("視頻");
language(<<"zh_HK">>,"link") -> utf8("鏈接");
language(<<"zh_HK">>,"recalled") -> utf8("撤回"); 
language(<<"zh_HK">>,"chat history") -> utf8("聊天記錄");
language(<<"zh_HK">>,"quote and reply") -> utf8("引用與回復");
language(<<"zh_HK">>,"contact card") -> utf8("名片");
language(<<"zh_HK">>,"validate apply") -> utf8("好友申請");
language(<<"zh_HK">>,"validate passed") -> utf8("好友申請通過");
language(<<"zh_HK">>,"read receipt") -> utf8("已讀");
language(<<"zh_HK">>,"notify message") -> utf8("通知");
language(<<"zh_HK">>,"moments notify message") -> utf8("朋友圈通知");
language(<<"zh_HK">>,"group notify message") -> utf8("群通知");
language(<<"zh_HK">>,"burn after reading receipt") -> utf8("閱後即焚回執"); 
language(<<"zh_HK">>,"validate refused") -> utf8("申請拒絕");
language(<<"zh_HK">>,"group notify message") -> utf8("群通知");
language(<<"zh_HK">>,"system") -> utf8("醫友助手");
language(_,Key) -> Key.
 
utf8(Str) ->
    binary_to_list(unicode:characters_to_binary(Str, utf8, utf8)).	  
	
	
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
	  
	  
	  