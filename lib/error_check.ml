open Sexplib.Conv

module Toggle = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      mutable success_last: bool;
    }
  let create ~assertion ~fail_alert ~success_alert =
    {
      fail_alert    = fail_alert;
      success_alert = success_alert;
      assertion     = assertion;
      success_last  = true;
    }

  let check ec param =
    if ec.assertion param then
      (* assertion succeeded *)
      if ec.success_last then
        (* and it succeeded last time too -- no change *)
        None
      else
        (* it did not succeed last time -- change to success *)
        begin
          ec.success_last <- true;
          Some (ec.success_alert param)
        end
    else
      (* assertion failed *)
      if ec.success_last then
        (* but succeeded last time -- change to failure *)
        begin
          ec.success_last <- false;
          Some (ec.fail_alert param)
        end
      else
        (* but failed last time -- no change *)
        None

  let state ec = ec.success_last
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

module ToggleN = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      final_fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      max_fail_alerts: int;
      mutable num_fail_alerts: int;
    }
  let create ~assertion ~fail_alert ~final_fail_alert ~success_alert ~max_fail_alerts =
    {
      assertion        = assertion;
      fail_alert       = fail_alert;
      final_fail_alert = final_fail_alert;
      success_alert    = success_alert;
      max_fail_alerts  = max_fail_alerts;
      num_fail_alerts  = 0;
    }

  let check ec param =
    if ec.assertion param then
      (* assertion succeeded *)
      if ec.num_fail_alerts = 0 then
        (* and it succeeded last time too -- no change *)
        None
      else
        (* it did not succeed last time -- change to success *)
        begin
          ec.num_fail_alerts <- 0;
          Some (ec.success_alert param)
        end
    else
      (* assertion failed *)
      if ec.num_fail_alerts = ec.max_fail_alerts then
        (* we're at max_fail_alerts now *)
        begin
          ec.num_fail_alerts <- ec.num_fail_alerts + 1;
          Some (ec.final_fail_alert param)
        end
      else if ec.num_fail_alerts < ec.max_fail_alerts then
        (* haven't reached max fail alerts yet *)
        begin
          ec.num_fail_alerts <- ec.num_fail_alerts + 1;
          Some (ec.fail_alert param)
        end
      else
        (* we're beyond max_fail_alerts *)
        None

  let state ec = ec.num_fail_alerts = 0
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

module Timer = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      min_alert_interval: Time.Span.t;
      mutable last_fail_alert_time : Time.t option;
    }
  let create ~assertion ~fail_alert ~success_alert ~min_alert_interval =
    {
      fail_alert = fail_alert;
      success_alert = success_alert;
      assertion = assertion;
      min_alert_interval = min_alert_interval;
      last_fail_alert_time = None;
    }
  let check ec param now =
    if ec.assertion param then
      (* assertion succeeded *)
      match ec.last_fail_alert_time with
      | None ->
          (* and it succeeded last time too -- no change *)
          None
      | Some _ ->
          (* it did not succeed last time -- change to success *)
          begin
            ec.last_fail_alert_time <- None;
            Some (ec.success_alert param)
          end
    else
      (* assertion failed *)
      let fail_alert () =
        ec.last_fail_alert_time <- Some now;
        Some (ec.fail_alert param)
      in
      match ec.last_fail_alert_time with
      | None -> fail_alert ()
      | Some t ->
          if Time.abs_diff now t > ec.min_alert_interval then
            (* enough time has passed since last failure, so alert *)
            fail_alert ()
          else
            None
  let state ec = ec.last_fail_alert_time = None
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

(* WL OG: ability to "tighten" as well would be nice *)
module Step = struct
  type ('threshold, 'param, 'alert) t =
    {
      initial_threshold: 'threshold;
      mutable threshold: 'threshold;
      loosen: ('param -> threshold:'threshold -> 'threshold);
      assertion: ('param -> threshold:'threshold -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
    }
  let create ~threshold ~loosen ~assertion ~fail_alert ~success_alert =
    {
      initial_threshold = threshold;
      threshold = threshold;
      loosen = loosen;
      fail_alert = fail_alert;
      success_alert = success_alert;
      assertion = assertion;
    }

  let check ec param =
    if ec.assertion param ~threshold:ec.initial_threshold then (* assertion succeeded *)
      if ec.threshold = ec.initial_threshold then
        (* and it succeeded before -- do nothing *)
        None
      else
        (* but it did not succeed before *)
        begin
          ec.threshold <- ec.initial_threshold;
          Some (ec.success_alert param)
        end
    else (* assertion failed *)
      if ec.assertion param ~threshold:ec.threshold then
        (* but it's not enough worse than before *)
        None
      else
        (* and it's worse than before *)
        begin
          let loosened = ec.loosen param ~threshold:ec.threshold in
          assert (loosened <> ec.initial_threshold);
          ec.threshold <- loosened;
          Some (ec.fail_alert param)
        end
  let state ec = ec.threshold = ec.initial_threshold
  let sexp_of_t _ _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end
