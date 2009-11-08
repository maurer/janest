module type S = sig
  type stringable

  val of_string : string -> stringable
  val to_string : stringable -> string
end
