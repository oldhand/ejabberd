-module(mod_offline_crypto).
-compile(export_all).
  
timestamp() ->  
	   {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
	   Universal_time = calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
	   Datime = calendar:datetime_to_gregorian_seconds(Universal_time) - 62167248000,
	   lists:flatten([io_lib:format("~10..0w", [Datime])]).	  
	      
md5(Str) when is_binary(Str) -> md5(binary_to_list(Str));
md5(Str) -> lists:flatten([io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(erlang:md5(Str))]).
 

string_to_byte(Data) when is_binary(Data) ->  string_to_byte(binary_to_list(Data));
string_to_byte(Data) when length(Data) rem 2 =:= 0 -> string_to_byte(Data,0,[],[]);
string_to_byte(_) -> "". 
string_to_byte([L|R],0,_,Bytes) -> string_to_byte(R,1,L,Bytes); 
string_to_byte([L|R],1,Acc,Bytes) -> 
     Byte = list_to_integer([Acc]++[L],16),
     string_to_byte(R,0,[],[Byte|Bytes]); 	
string_to_byte([],_,_,Bytes) ->  lists:reverse(Bytes).
 
decrypt(Src,Key) when is_binary(Key) ->  decrypt(Src,binary_to_list(Key)); 
decrypt(Src,Key) when is_binary(Src) ->  decrypt(binary_to_list(Src),Key); 
decrypt(Src,Key) when length(Src) rem 16  =/= 0 -> 
   Iver = "0000000000000000",
   Length = length(Src) rem 16,
   NewSrc = Src ++ lists:sublist(Iver, 1, 16 - Length),
   decrypt(NewSrc,Key);

decrypt(Src,Keys) when length(Keys) =:= 16 ->  
		  Key1 = lists:sublist(Keys, 1, 8),
		  Key2 = lists:sublist(Keys, 9, 8),  
		  SrcBytes = string_to_byte(Src),  	 
		  FirstCipher = crypto:block_decrypt(des_ecb,Key1,SrcBytes), %%crypto:des_ecb_decrypt(Key1, SrcBytes),
		  SecondCipher = crypto:block_encrypt(des_ecb,Key2,FirstCipher), %%crypto:des_ecb_encrypt(Key2, FirstCipher),
		  ThreeCipher = crypto:block_decrypt(des_ecb,Key1,SecondCipher), %%crypto:des_ecb_decrypt(Key1, SecondCipher),
		  lists:flatten([io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(ThreeCipher)]);	 
decrypt(Src,Key) when length(Key) =:= 32 -> 
		  Keys = string_to_byte(Key), 
		  Key1 = lists:sublist(Keys, 1, 8),
		  Key2 = lists:sublist(Keys, 9, 8),  
		  SrcBytes = string_to_byte(Src),  	 
		  FirstCipher = crypto:block_decrypt(des_ecb,Key1,SrcBytes), %%crypto:des_ecb_decrypt(Key1, SrcBytes),
		  SecondCipher = crypto:block_encrypt(des_ecb,Key2,FirstCipher), %%crypto:des_ecb_encrypt(Key2, FirstCipher),
		  ThreeCipher = crypto:block_decrypt(des_ecb,Key1,SecondCipher), %%crypto:des_ecb_decrypt(Key1, SecondCipher),
		  lists:flatten([io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(ThreeCipher)]);		  
decrypt(_,_)  -> "".  

encrypt(Src,Key) when is_binary(Key) ->  encrypt(Src,binary_to_list(Key));
encrypt(Src,Key) when is_binary(Src) ->  encrypt(binary_to_list(Src),Key);
encrypt(Src,Key) when length(Src) rem 8  =/= 0 -> 
   Iver = [0,0,0,0,0,0,0,0],
   Length = length(Src) rem 8,
   NewSrc = Src ++ lists:sublist(Iver, 1, 8 - Length),
   encrypt(NewSrc,Key);
encrypt(Src,Keys) when length(Keys) =:= 16 ->  
		  Key1 = lists:sublist(Keys, 1, 8),
		  Key2 = lists:sublist(Keys, 9, 8),   
		  FirstCipher = crypto:block_encrypt(des_ecb,Key1,Src), %%crypto:des_ecb_encrypt(Key1, SrcBytes),
		  SecondCipher = crypto:block_decrypt(des_ecb,Key2,FirstCipher), %%crypto:des_ecb_decrypt(Key2, FirstCipher),
		  ThreeCipher = crypto:block_encrypt(des_ecb,Key1,SecondCipher), %%crypto:des_ecb_encrypt(Key1, SecondCipher),
		  lists:flatten([io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(ThreeCipher)]);		      
encrypt(Src,Key) when length(Key) =:= 32 -> 
		  Keys = string_to_byte(Key), 
		  Key1 = lists:sublist(Keys, 1, 8),
		  Key2 = lists:sublist(Keys, 9, 8),   
		  FirstCipher = crypto:block_encrypt(des_ecb,Key1,Src), %%crypto:des_ecb_encrypt(Key1, SrcBytes),
		  SecondCipher = crypto:block_decrypt(des_ecb,Key2,FirstCipher), %%crypto:des_ecb_decrypt(Key2, FirstCipher),
		  ThreeCipher = crypto:block_encrypt(des_ecb,Key1,SecondCipher), %%crypto:des_ecb_encrypt(Key1, SecondCipher),
		  lists:flatten([io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(ThreeCipher)]);		  
encrypt(_,_)  -> "".



encrypt_private(PlainText,"") -> encrypt_private(PlainText,load_private_key());
encrypt_private(PlainText,Key) when is_binary(PlainText)-> encrypt_private(binary_to_list(PlainText),Key);
encrypt_private(PlainText,Key) ->
    ChunkList = [ lists:sublist(PlainText, X, 117) || X <- lists:seq(1,length(PlainText),117) ],  
	Fun = fun(X) ->  binary_to_list(public_key:encrypt_private(list_to_binary(X), Key,[{rsa_padding, rsa_pkcs1_padding}])) end, 
	lists:flatten(lists:map(Fun,ChunkList)).

encrypt_public(PlainText,"") -> encrypt_public(PlainText,load_public_key());	
encrypt_public(PlainText,Key) when is_binary(PlainText)-> encrypt_public(binary_to_list(PlainText),Key);
encrypt_public(PlainText,Key) ->
    ChunkList = [ lists:sublist(PlainText, X, 117) || X <- lists:seq(1,length(PlainText),117) ],  
	Fun = fun(X) ->  binary_to_list(public_key:encrypt_public(list_to_binary(X), Key,[{rsa_padding, rsa_pkcs1_padding}])) end, 
	lists:flatten(lists:map(Fun,ChunkList)).	

decrypt_private(Ciphertext,"") -> decrypt_private(Ciphertext,load_private_key());
decrypt_private(Ciphertext,Key) when is_binary(Ciphertext)-> decrypt_private(binary_to_list(Ciphertext),Key);
decrypt_private(Ciphertext,Key) ->
    ChunkList = [ lists:sublist(Ciphertext, X, 128) || X <- lists:seq(1,length(Ciphertext),128) ],  
	Fun = fun(X) ->  binary_to_list(public_key:decrypt_private(list_to_binary(X), Key,[{rsa_padding, rsa_pkcs1_padding}])) end, 
	lists:flatten(lists:map(Fun,ChunkList)).  
	
decrypt_public(Ciphertext,"") -> decrypt_public(Ciphertext,load_public_key());
decrypt_public(Ciphertext,Key) when is_binary(Ciphertext)-> decrypt_public(binary_to_list(Ciphertext),Key);
decrypt_public(Ciphertext,Key) ->
    ChunkList = [ lists:sublist(Ciphertext, X, 128) || X <- lists:seq(1,length(Ciphertext),128) ],  
	Fun = fun(X) ->  binary_to_list(public_key:decrypt_public(list_to_binary(X), Key,[{rsa_padding, rsa_pkcs1_padding}])) end, 
	lists:flatten(lists:map(Fun,ChunkList)).	

 

private_key() ->  <<"-----BEGIN RSA PRIVATE KEY-----
MIICXQIBAAKBgQDI6dGvkKSHB6Q3TE6TKGFR4Nyt6XH3gc7/LAzvW0aDNGZjkoA7
jrMTBd/T0N/R4miBK7XNMI+4Z/gd8OgS0wShPwyqFwv8Q54goPr6dAXAQifzwK+e
Os+Avu9rrVfT31i8wJeIzpk1aySoYB40ozasTdXgQ2AHZH0AqU/Rne5GuQIDAQAB
AoGATkWphzhWoLR9aX207ufAYmG0F5zm5YIQ3qCOuYFZyyvS59/fTVSetz6GoMKz
L9WA2THqNfJBS5pVs3RzNUI4fthhl06Q4Nmm+HGGQ7hvj05+v49E9deqtHMaTMcG
j99dMt2D18k/Gw8va5/Czoj6+yGQIaNUTSmOefGeZZhhWzECQQDuKpwoDhOunDut
H7jx6frdm4wAR2ykWFs5XBWtyWp1dVfc/uCO5wQgpUpxmUbiQNJISyfKMvscr4ow
Qs336343AkEA1/UbsENrQ42GN0nbczx14Yo4C3Ds/9Hdyw12JoGg+2JPFYB1vmdZ
xo/xXx0wsZWYSBYEwumVd19gF6szyeBqjwJBAJlK57OIS/bJLEfj8SAT35ofcd8E
GdM145FpybJPv6vWWTUu/846txdkDCRN6afa3P4XEYc9hQ8TlTg252c5NcUCQQC6
0xj5C9onvq919TccIhn43BJQE4l0ZqJxn1uvREV4NDwmZdN2vQI1fSFbH22Ys8nh
0uqblGfxtVj0IO/UnYiRAkAu4helSN55HCZ8pcDITJMgENKFQYW4Rgo9IdYAX2tD
1zP5W2FbdJACExzwYfCunSmDgAKQIg/rNWziqc6raqer
-----END RSA PRIVATE KEY-----
">>.

public_key() ->  <<"-----BEGIN PUBLIC KEY-----
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDI6dGvkKSHB6Q3TE6TKGFR4Nyt
6XH3gc7/LAzvW0aDNGZjkoA7jrMTBd/T0N/R4miBK7XNMI+4Z/gd8OgS0wShPwyq
Fwv8Q54goPr6dAXAQifzwK+eOs+Avu9rrVfT31i8wJeIzpk1aySoYB40ozasTdXg
Q2AHZH0AqU/Rne5GuQIDAQAB
-----END PUBLIC KEY-----
">>.

load_private_key() ->  
     [Entry] = public_key:pem_decode(private_key()),
	 public_key:pem_entry_decode(Entry).
								    	    
load_public_key() ->		 
     [Entry] = public_key:pem_decode(public_key()),
   	 public_key:pem_entry_decode(Entry).








