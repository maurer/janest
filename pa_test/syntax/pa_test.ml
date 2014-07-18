open Camlp4.PreCast

let compare =
  try Syntax.Quotation.find "compare" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_compare to be loaded"
let sexp_of =
  try Syntax.Quotation.find "sexp_of" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_sexp_conv to be loaded"

let sexp_list_expr_of_loc loc =
  <:expr@loc<
    [ Sexplib.Sexp.List [
        Sexplib.Sexp.Atom "Loc";
        Sexplib.Sexp.Atom (
          Pa_test_lib.Runtime.make_location_string
            ~pos_fname:$str:Loc.file_name loc$
            ~pos_lnum:$`int:Loc.start_line loc$
            ~pos_cnum:$`int:Loc.start_off loc$
            ~pos_bol:$`int:Loc.start_bol loc$
        );
      ] ::
      match here with
        [ [] -> []
        | _ ->
          [ Sexplib.Sexp.List [
            Sexplib.Sexp.Atom "Stack";
            Sexplib.Conv.sexp_of_list Pa_test_lib.Runtime.sexp_of_loc here;
          ] ] ] ] >>

let () =
  Syntax.Quotation.add "test_pred"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message predicate t ->
       if not (predicate t) then begin
         let message =
           match message with
           [ None -> ""
           | Some s -> s ^ ": "
           ]
           ^ "predicate failed"
         in
         Pa_test_lib.Runtime.failwith message
           (Sexplib.Sexp.List [
             Sexplib.Sexp.List [
               Sexplib.Sexp.Atom "Value"; $sexpifier$ t;
             ] :: $sexp_list_expr_of_loc loc$ ])
       end else ()
      >>
    )


let () =
  Syntax.Quotation.add "test_eq"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let comparator = compare loc loc_name_opt cnt_str in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message ?equal t1 t2 ->
       let sexpifier = $sexpifier$ in
       let pass =
         match equal with
         [ None ->
           match $comparator$ t1 t2 with
           [ 0 -> True
           | _ -> False ]
         | Some f -> f t1 t2 ] in
       if not pass then begin
         let message =
           match message with
           [ None -> ""
           | Some s -> s ^ ": "
           ]
           ^ "comparison failed"
         in
         Pa_test_lib.Runtime.failwith message
           (Sexplib.Sexp.List [
             sexpifier t1;
             Sexplib.Sexp.Atom "vs";
             sexpifier t2
             :: $sexp_list_expr_of_loc loc$ ])
       end else ()
      >>
    )

let () =
  Syntax.Quotation.add "test_result"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let comparator = compare loc loc_name_opt cnt_str in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message ?equal got ~expect ->
       let sexpifier = $sexpifier$ in
       let pass =
         match equal with
         [ None ->
           match $comparator$ got expect with
           [ 0 -> True
           | _ -> False ]
         | Some f -> f got expect ] in
       if not pass then begin
         let message =
           match message with
           [ None -> ""
           | Some s -> s ^ ": "
           ]
           ^ "got unexpected result"
         in
         Pa_test_lib.Runtime.failwith message
           (Sexplib.Sexp.List [
             Sexplib.Sexp.List [Sexplib.Sexp.Atom "got"; sexpifier got];
             Sexplib.Sexp.List [Sexplib.Sexp.Atom "expected"; sexpifier expect];
             :: $sexp_list_expr_of_loc loc$ ])
       end else ()
      >>
    )
