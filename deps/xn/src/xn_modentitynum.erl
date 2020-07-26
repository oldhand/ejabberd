
-module(xn_modentitynum). 

 
-compile(export_all).    
 
get(Module) ->  
      Url =  lists:flatten(["/modentitynum(module='",Module,"')"]), 
	  case  xn_rest:get(Url) of
		  {ok,Modentitynum} -> {ok,Modentitynum}; 
		  ErrorResult -> {error,ErrorResult}
	  end.
  