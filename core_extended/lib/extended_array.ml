open Core.Std

let foldi ~init ar ~f =
  fst (Array.fold ~init:(init,0) ar ~f:(fun (a,i) x -> f i a x,i+1))

TEST = 40 = foldi ~init:0 [|1;2;3;4;5|] ~f:(fun i a x -> a + i * x)

let random_split ?random_state array ~p =
  let a = Array.copy array in
  if p > 1.0 || p < 0.0 then
    failwith "Array.random_split: p is out of bounds [0 1]";
  let stop = Float.iround_exn ~dir:`Nearest (p *. (float (Array.length a))) in
  if stop = 0 then
    (* in slice a stop of 0 means slicing to the end of the array, which is not what we
       want *)
    ([||], a)
  else
    begin
      Array.permute a ?random_state;
      ((Array.slice a 0 stop), (Array.slice a stop 0))
    end

let random_sub ?random_state array ~p =
  fst (random_split ~p array ?random_state)

module Access_control = struct
  type ('a,'z) any = 'a array with sexp, bin_io
  module Immutable = struct
    type 'a t = ('a,immutable) any with sexp, bin_io
  end
  module Read_only = struct
    type 'a t = ('a,read_only) any with sexp, bin_io
  end
  module Read_write = struct
    type 'a t = ('a,read_write) any with sexp, bin_io
  end
  type 'a t = 'a Immutable.t with sexp, bin_io

  let of_array = Fn.id
  let of_array_copy = Array.copy
  let to_array_copy = Array.copy


  include (Array : sig
    val create : len:int -> 'a -> ('a,_) any
    val init : int -> f:(int -> 'a) -> ('a,_) any
    val get : ('a,_) any -> int -> 'a
    val set : 'a Read_write.t -> int -> 'a -> unit

    val append: ('a,_) any -> ('a,_) any -> ('a,_) any
    val copy : ('a,_) any -> ('a,_) any
    val map : f:('a -> 'b) -> ('a,_) any -> ('b,_) any
    val mapi : f:(int -> 'a -> 'b) -> ('a,_) any -> ('b,_) any
    val iteri : f:(int -> 'a -> unit) -> ('a,_) any -> unit
    val filter_opt : ('a option,_) any -> ('a,_) any
    val filter_map : ('a,_) any -> f:('a -> 'b option) -> ('b,_) any
    val filter_mapi : ('a,_) any -> f:(int -> 'a -> 'b option) -> ('b,_) any
    val map2_exn : ('a,_) any -> ('b,_) any -> f:('a -> 'b -> 'c) -> ('c,_) any
    val findi : ('a,_) any -> f:(int -> 'a -> bool) -> (int * 'a) option
    val blito : (('a,_) any, 'a Read_write.t) Blit.blito

    val permute : ?random_state:Random.State.t -> _ Read_write.t -> unit
    val fill : 'a Read_write.t -> pos:int -> len:int -> 'a -> unit
    val of_list : 'a list -> ('a,_) any

    include Container.S1_phantom
      with type ('a, 'phantom) t := ('a, 'phantom) any
  end)
end
