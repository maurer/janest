open Core.Std

module Ok: Monad.S2 with type ('a,'err) monad = ('a,'err) Result.t
module Error: Monad.S2 with type ('err,'a) monad = ('a,'err) Result.t
