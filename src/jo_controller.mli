type json = string

val response_body : Http_types.request -> Jo_master.search_fun -> json
  (** 
      Query from client:
        GET /maiele/results.js?
          - query  = <search query>
          - domain = all | Games   (def: all)
          - limit  = <int>         (def: 20)
        RESULTS: -> JSON hash ary


      Debugging:
        GET /maiele/text.js?
          - id = <int>
          - domain = games


      GET /maiele/target_ids.js

  *)
