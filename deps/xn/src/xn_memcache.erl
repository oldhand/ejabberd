
-module(xn_memcache). 

 
-compile(export_all).    
 
get(Key) ->  
      Url =  lists:flatten(["/memcache(id='",Key,"')"]),
	  xn_rest:get(Url).
  
put(Value,Key) ->  put(Value,Key,0).
put(Value,Key,Expire) -> 
    Url = "/memcache",
    xn_rest:put(Url,term_to_binary({Key,"string",Value,Expire})).