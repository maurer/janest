open Core.Std
let iff cond v =
  if cond then
    Some v
  else
    None
