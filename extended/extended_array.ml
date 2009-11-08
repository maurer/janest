open Core.Std

let foldi ~init ar ~f =
  fst (Array.fold_left ~init:(init,0) ar ~f:(fun (a,i) x -> f i a x,i+1))

let random_split ?random_state ~p array =
  let a = Array.copy array in
  if p > 1.0 || p < 0.0 then
    failwith "Array.random_split: p is out of bounds [0 1]";
  let stop = Float.iround_exn (p *. (float (Array.length a))) in
  if stop = 0 then
    (* in slice a stop of 0 means slicing to the end of the array, which is not what we
       want *)
    ([||], a)
  else
    begin
      Array.permute ?random_state a;
      ((Array.slice a 0 stop), (Array.slice a stop 0))
    end

let random_sub ?random_state ~p array =
  fst (random_split ?random_state array ~p)
