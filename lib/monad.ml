module type Basic = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type Infix = sig
  type 'a monad

  (** [t >>= f] returns a computation that sequences the computations
      represented by two monad elements.  The resulting computation first does
      [t] to yield a value [v], and then runs the computation returned by [f v].
  *)
  val (>>=) : 'a monad -> ('a -> 'b monad) -> 'b monad

  (** [t >>| f] is [t >>= (fun a -> return (f a))]. *)
  val (>>|) : 'a monad -> ('a -> 'b) -> 'b monad

end

module type S = sig
  (** A monad is an abstraction of the concept of sequencing of computations.
      A value of type 'a monad represents a computation that returns a value
      of type 'a. *)
  include Infix

  module Monad_infix : Infix with type 'a monad = 'a monad

  (** [bind t f] = [t >>= f] *)
  val bind : 'a monad -> ('a -> 'b monad) -> 'b monad

  (** [return v] returns the (trivial) computation that returns v. *)
  val return : 'a -> 'a monad

  (** [map t ~f] is t >>| f. *)
  val map : 'a monad -> f:('a -> 'b) -> 'b monad

  (** [join t] is [t >>= (fun t' -> t')]. *)
  val join : 'a monad monad -> 'a monad

  
  (** [ignore t] = map t ~f:(fun _ -> ()). *)
  val ignore : 'a monad -> unit monad

  
  (** [unit] = [return ()] *)
  val unit : unit monad

end

module Make (M : Basic) : S with type 'a monad = 'a M.t = struct

  let bind = M.bind

  let return = M.return

  module Monad_infix = struct
    type 'a monad = 'a M.t

    let (>>=) = bind

    let (>>|) t f = t >>= (fun a -> return (f a))
  end

  include Monad_infix

  let join t = t >>= (fun t' -> t')

  let map t ~f = t >>| f

  let ignore t = map t ~f:(fun _ -> ())

  let unit = return ()

end

(**
   Multi parameter monad.
   The second parameter get unified across all the computation. This is used
   to encode monads working on a multi parameter data structure like
   ([('a,'b result)]).

   The signature is exactly the same as for [S1] except [S.unit] which got
   pruned.  (We cannot write [let unit = return ()] without forcing the
   ['d] parameter of [('a, 'd) monad] to be covariant.)
*)
module type Basic2 = sig
  type ('a, 'd) t
  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val return : 'a -> ('a, _) t
end

(** Same as Infix, except the monad type has two arguments. The second is always just
    passed through. *)
module type Infix2 = sig
  type ('a, 'd) monad
  val (>>=) : ('a, 'd) monad -> ('a -> ('b, 'd) monad) -> ('b, 'd) monad
  val (>>|) : ('a, 'd) monad -> ('a -> 'b) -> ('b, 'd) monad
end


(** The same as S1 except the monad type has two arguments. The second is always just
    passed through. *)
module type S2 = sig
  include Infix2

  module Monad_infix : Infix2 with type ('a, 'd) monad = ('a, 'd) monad

  val bind : ('a, 'd) monad -> ('a -> ('b, 'd) monad) -> ('b, 'd) monad

  val return : 'a -> ('a, _) monad

  val map : ('a, 'd) monad -> f:('a -> 'b) -> ('b, 'd) monad

  val join : (('a, 'd) monad, 'd) monad -> ('a, 'd) monad

  val ignore : (_, 'd) monad -> (unit, 'd) monad
end


module Make2 (M : Basic2) : S2 with type ('a, 'd) monad = ('a, 'd) M.t = struct

  let bind = M.bind

  let return = M.return

  module Monad_infix = struct
    type ('a,'d) monad = ('a,'d) M.t

    let (>>=) = bind

    let (>>|) t f = t >>= (fun a -> return (f a))
  end

  include Monad_infix

  let join t = t >>= (fun t' -> t')

  let map t ~f = t >>| f

  let ignore t = map t ~f:(fun _ -> ())

end
