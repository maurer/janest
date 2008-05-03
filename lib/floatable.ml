module type S = sig
  type floatable
      
  val of_float : float -> floatable
  val to_float : floatable -> float
end
