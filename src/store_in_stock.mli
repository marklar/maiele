
val has_inventory_fun : Result.t list -> (Result.t -> bool)
  (**
     From a list of Results, creates a function which
     allows one to ask for any Result whether it's available
     in a store.
  *)
