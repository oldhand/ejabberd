
-module(xn_rest).

-compile(export_all).


get(Url) -> get(Url, []).
	
get(Url, Headers) -> 
      try doRequest(get,Url,"",Headers) of 
    	  Result -> Result
	  catch
	         error:Error ->
		 	      ErrorInfo = io_lib:format("~p",[Error]),
		 	      ErrorMsg  = lists:flatten(["doRequest catch Error : ",ErrorInfo]),
	              io:format(ErrorMsg),
	 	          {ok,ErrorMsg}
	  end.


post(Url,Body) -> post(Url,Body, []).
	
post(Url,Body,Headers) -> doRequest(post,Url,Body,Headers).

put(Url,Body) -> put(Url,Body, []).
	
put(Url,Body,Headers) -> doRequest(put,Url,Body,Headers).


delete(Url) -> delete(Url, []).
	
delete(Url, Headers) -> doRequest(delete,Url,"",Headers).
	 
prepareUrl(Url) ->
	case xn:getconfig() of
		{ok,Server,Port,_Viewer,_Application} ->
			case string:rstr(Url, "?") of
				0 ->  {ok,lists:flatten([ "http://",Server,":",integer_to_list(Port),"/xn/rest/1.0",http_uri:encode(Url),"?xn_out=binary"])};
				Pos ->   
					Path = string:substr(Url,1,Pos-1),
					Raw_path = string:substr(Url,Pos+1,length(Url)-1), 
					{ok,lists:flatten([ "http://",Server,":",integer_to_list(Port),"/xn/rest/1.0",http_uri:encode(Path),"?",Raw_path,"&xn_out=binary"])}	 
			end;
		_ ->  {error,"Error XN config."}
	end. 
	 
doRequest(Method,Url,Body, Headers) ->     
   io:format("doRequest url:~p~n", [{Method,Url}]),
   case Method of
	  get -> 
		  case prepareUrl(Url) of
			  {ok,PrepareUrl} ->
				  Domain = xn:application(),
				  NewHeaders = [{"domain",binary_to_list(Domain)}|Headers], 
			      try httpc:request(get,{PrepareUrl,NewHeaders},[{timeout, timer:seconds(60)}],[{body_format, binary}]) of
				   	{ok, {{_,200,"OK"},_ResponseHeaders,ResponseBody}} -> binary_to_term(ResponseBody); 
				   	{error, ErrorResult} -> {error,ErrorResult};
				   	ErrorResult -> {error,ErrorResult}
	  			  catch
	  			         error:Error ->
	   	  			 	      ErrorInfo = io_lib:format("~p",[Error]),
	   	  			 	      ErrorMsg  = lists:flatten(["httpc:request catch Error : ",ErrorInfo]),
	  			              io:format(ErrorMsg),
	  			 	          {error,ErrorMsg}
	  			  end;
			  ErrorResult -> ErrorResult
		  end;
  	  post -> 
  		  case prepareUrl(Url) of
  			  {ok,PrepareUrl} ->
  				  Domain = xn:application(),
  				  NewHeaders = [{"domain",binary_to_list(Domain)}|Headers],
  				  %%io:format("doRequest : ~p~n",[{PrepareUrl,NewHeaders}]), 
				  try httpc:request(post,{PrepareUrl,NewHeaders,"application/x-www-form-urlencoded",Body},[{timeout, timer:seconds(60)}],[{body_format, binary}]) of
  				   	{ok, {{_,200,"OK"},_ResponseHeaders,ResponseBody}} -> binary_to_term(ResponseBody); 
  				   	{error, ErrorResult} -> {error,ErrorResult};
  				   	ErrorResult -> {error,ErrorResult}
	  			  catch
	  			         error:Error ->
	   	  			 	      ErrorInfo = io_lib:format("~p",[Error]),
	   	  			 	      ErrorMsg  = lists:flatten(["httpc:request catch Error : ",ErrorInfo]),
	  			              io:format(ErrorMsg),
	  			 	          {error,ErrorMsg}
	  			  end;
  			  ErrorResult -> ErrorResult
  		  end;  
  	  put -> 
  		  case prepareUrl(Url) of
  			  {ok,PrepareUrl} ->
  				  Domain = xn:application(),
  				  NewHeaders = [{"domain",binary_to_list(Domain)}|Headers],
  				  %%io:format("doRequest : ~p~n",[{PrepareUrl,NewHeaders}]), 
  			      try httpc:request(put,{PrepareUrl,NewHeaders,"application/x-www-form-urlencoded",Body},[{timeout, timer:seconds(60)}],[{body_format, binary}]) of
  				   	{ok, {{_,200,"OK"},_ResponseHeaders,ResponseBody}} -> binary_to_term(ResponseBody); 
  				   	{error, ErrorResult} -> {error,ErrorResult};
  				   	ErrorResult -> {error,ErrorResult}
     			  catch
     			         error:Error ->
	   	  			 	      ErrorInfo = io_lib:format("~p",[Error]),
	   	  			 	      ErrorMsg  = lists:flatten(["httpc:request catch Error : ",ErrorInfo]),
     			              io:format(ErrorMsg),
     			 	          {error,ErrorMsg}
     			  end;
  			  ErrorResult -> ErrorResult
  		  end;
      delete -> 
	  case prepareUrl(Url) of
		  {ok,PrepareUrl} ->
			  Domain = xn:application(),
			  NewHeaders = [{"domain",binary_to_list(Domain)}|Headers],
			  %%io:format("doRequest : ~p~n",[{PrepareUrl,NewHeaders}]), 
		      try httpc:request(delete,{PrepareUrl,NewHeaders},[{timeout, timer:seconds(60)}],[{body_format, binary}]) of
				   	{ok, {{_,200,"OK"},_ResponseHeaders,ResponseBody}} -> binary_to_term(ResponseBody); 
				   	{error, ErrorResult} -> {error,ErrorResult};
				   	ErrorResult -> {error,ErrorResult}
  			  catch
  			         error:Error ->
  	  			 	      ErrorInfo = io_lib:format("~p",[Error]),
  	  			 	      ErrorMsg  = lists:flatten(["httpc:request catch Error : ",ErrorInfo]),
  			              io:format(ErrorMsg),
  			 	          {error,ErrorMsg}
  			  end; 
		  ErrorResult -> ErrorResult
	  end;
	  _ ->  {error,"error Method."}
   end. 
 