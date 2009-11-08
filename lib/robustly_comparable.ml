(* This interface compares float-like objects with a small tolerance.  For example [>.]
   returns false if the floats are almost equal, and [=.] returns true if the floats are
   almost but not quite equal.  The tolerance is intended to be about right for
   human-entered values like prices and seconds. *)

module type S = sig
  type robustly_comparable
  val ( >=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( =. ) : robustly_comparable -> robustly_comparable -> bool
  val ( >. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <>. ) : robustly_comparable -> robustly_comparable -> bool
end
