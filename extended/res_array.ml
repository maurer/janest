(*pp cpp $ARCH_FLAGS *)

open Bin_prot

type 'a t = 'a Res.Array.t

let bin_write_t_ bin_write_el sptr eptr ra =
  let n = Res.Array.length ra in
  let pn = Nat0.unsafe_of_int n in
  let els_sptr = Unsafe_write_c.bin_write_nat0 sptr eptr pn in
  let els_sptr_ref = ref els_sptr in
  for i = 0 to n - 1 do
    let el = Res.Array.get ra i in
    let new_els_sptr = bin_write_el !els_sptr_ref eptr el in
    els_sptr_ref := new_els_sptr
  done;
  !els_sptr_ref

let bin_write_t bin_write_el buf ~pos ra =
  let start, sptr, eptr = Write_c.unsafe_get_init buf ~pos in
  let cur = bin_write_t_ bin_write_el sptr eptr ra in
  Unsafe_common.get_safe_buf_pos buf ~start ~cur

let bin_read_t_ bin_read_el sptr_ptr eptr =
  let sptr = Unsafe_common.get_sptr_ptr_sptr sptr_ptr in
  let len = (Unsafe_read_c.bin_read_nat0 sptr_ptr eptr :> int) in
  if len = 0 then Res.Array.empty ()
  else (
    if len > Sys.max_array_length then (
      Unsafe_common.set_sptr_ptr_sptr sptr_ptr sptr;
      raise (
        Unsafe_read_c.Error Bin_prot.Common.ReadError.Array_too_long));
    let first = bin_read_el sptr_ptr eptr in
    let ra = Res.Array.make len first in
    for i = 1 to len - 1 do
      let el = bin_read_el sptr_ptr eptr in
      Res.Array.set ra i el
    done;
    ra)

let bin_read_t__ _bin_read_el _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "Core.Common.Res_array"

let at_end buf sptr_ptr pos_ref el =
  let pos = Unsafe_common.dealloc_sptr_ptr buf sptr_ptr in
  pos_ref := pos;
  el

let bin_read_t bin_read_el buf ~pos_ref =
  let sptr_ptr, eptr = Unsafe_common.get_read_init buf ~pos_ref in
  let el =
    try bin_read_t_ bin_read_el sptr_ptr eptr with
    | Unsafe_read_c.Error read_err -> Read_c.handle_error buf sptr_ptr read_err
    | exc -> Read_c.handle_exc buf sptr_ptr exc
  in
  at_end buf sptr_ptr pos_ref el

let bin_size_array_loop bin_size_el ra ~total_len ~n =
  let total_len_ref = ref total_len in
  for i = 0 to n - 1 do
    let el = Res.Array.get ra i in
    total_len_ref := !total_len_ref + bin_size_el el
  done;
  !total_len_ref

let bin_size_len len =
  let plen = Nat0.unsafe_of_int len in
  Bin_prot.Size.bin_size_nat0 plen

let bin_size_t bin_size_el ra =
  let n = Res.Array.length ra in
  let total_len = bin_size_len n in
  bin_size_array_loop bin_size_el ra ~total_len ~n

open Type_class

let bin_writer_t bin_writer_el =
  {
    size = (fun v -> bin_size_t bin_writer_el.size v);
    write = (fun buf ~pos v ->
      bin_write_t bin_writer_el.unsafe_write buf ~pos v);
    unsafe_write = (fun sptr eptr v ->
      bin_write_t_ bin_writer_el.unsafe_write sptr eptr v);
  }

let bin_reader_t bin_reader_el =
  {
    read = (fun buf ~pos_ref ->
      bin_read_t bin_reader_el.unsafe_read buf ~pos_ref);
    unsafe_read = (fun sptr_ptr eptr ->
      bin_read_t_ bin_reader_el.unsafe_read sptr_ptr eptr);
    unsafe_vtag_read = (fun _sptr_ptr _eptr _vtag ->
      raise (Unsafe_read_c.raise_variant_wrong_type "Core.Res_array.t"));
  }

let bin_t bin_el =
  {
    writer = bin_writer_t bin_el.writer;
    reader = bin_reader_t bin_el.reader;
  }
