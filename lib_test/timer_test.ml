open Core.Std

open OUnit;;

let raises_exception f =
  try f (); false
  with e -> true

let null_handler _ _ = ()
let exception_handler _ _ = failwith "printing this exception does not indicate a bug"

let pause span = 
  try ignore (Unix.select [] [] [] (Time.Span.to_sec span) )
  with _ -> ()

let permute array =
  let array = Array.copy array in
  for i = Array.length array - 1 downto 1 do
    Array.swap array i (Random.int i)
  done;
  array

let rec forever f =
  f ();
  forever f
  
let test =
  "timer" >:::
    [ "abusive user" >::
        (fun () ->
           "create" @? (raises_exception (Timer.create ~tick_unit:(Time.Span.of_sec 0.)));
           "add" @?
             begin
               let timer = Timer.create () in
               raises_exception
                 (fun () -> Timer.add timer null_handler (Time.Span.of_sec (-1.)))
             end;
           "remove twice" @?
             begin
               let timer = Timer.create () in
               let ev = Timer.add timer null_handler (Time.Span.of_sec 0.1) in
               Timer.remove ev;
               Timer.remove ev;
               true
             end;
           "malicious handler" @?
             begin
               let timer = Timer.create () in
               let _ev = Timer.add timer exception_handler (Time.Span.of_sec 0.1) in
               pause (Time.Span.of_sec 0.3);
               true
             end;
        );
      "ordering of events" >::
        (fun () ->
           let timer = Timer.create () in
           let delays = Array.init 100 ~f:(fun i -> (float i)/.100.) in
           let permuted_delays = permute delays in
           let queue = Squeue.create 100 in
           let add_handler _ time = Squeue.push queue (Time.to_float time) in
           Array.iter permuted_delays
             ~f:(fun delay ->
                   ignore (Timer.add timer add_handler (Time.Span.of_sec delay))
                );
           pause (Time.Span.of_sec 1.2);
           let last = ref (-1.) in
           while Squeue.length queue > 0 do
             let next = Squeue.pop queue in
             "in order" @? (next >=. !last);
             last := next
           done
        );
      "deactivation" >::
        (fun () ->
           let timer = Timer.create () in
           "initially activated" @? Timer.is_activated timer;
           let dont_call_handler _ _ = ("handler called stupidly" @? false) in
           ignore (Timer.add timer dont_call_handler (Time.Span.of_sec 0.25));
           Timer.deactivate timer;
           "can be deactivated" @? not (Timer.is_activated timer);
           pause (Time.Span.of_sec 0.5)
        )
    ]
