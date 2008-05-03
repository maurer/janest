module type S = sig
  type robustly_comparable
  val ( >=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( =. ) : robustly_comparable -> robustly_comparable -> bool
  val ( >. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <>. ) : robustly_comparable -> robustly_comparable -> bool
end  
