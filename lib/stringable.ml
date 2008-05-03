module type S = sig
  type stringable

  (* WL sweeks: Someday, of_string should return an option, and we should have
     of_string_exn.
  *)
  val of_string : string -> stringable
  val to_string : stringable -> string
end
