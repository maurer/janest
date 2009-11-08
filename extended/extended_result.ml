open Core.Std
module Ok = Result

module Error =
  Monad.Make2
    (struct
      type ('a, 'b) t = ('b,'a) Result.t

      let bind x f = match x with
        | Error x -> f x
        | Ok _ as x -> x

      let return x = Error x
    end)
