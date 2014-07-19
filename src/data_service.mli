val target_ids : Http_types.request -> string
(**
   DATA SERVICE.  (Requests from Rails.)
   
   req#uri: /maiele/target_ids.js

     LINE-UP
       lex    = <int>
       domain = ...

     BESTSELLER IMPORTER
       query  = <str>
       domain = (just one)
       limit  = <int>

   -> JSON int ary
*)
