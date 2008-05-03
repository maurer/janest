module type Infix = sig
  type equatable
  val (=) : equatable -> equatable -> bool
  val (<>) : equatable -> equatable -> bool
end

module type S = sig
  include Infix

  val equal : equatable -> equatable -> bool
end
