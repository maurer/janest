open Interfaces
include Sexplib.Sexp_intf.S

include Sexpable with type sexpable = t
include Binable with type binable = t

module Sexp_option : sig
  open Bin_prot

  val bin_size_sexp_option : ('a, 'a option) Size.sizer1
  val bin_write_sexp_option :('a, 'a option) Map_to_safe.writer1
  val bin_write_sexp_option_ :('a, 'a option) Unsafe_write_c.writer1
  val bin_read_sexp_option : ('a, 'a option) Map_to_safe.reader1
  val bin_read_sexp_option_ : ('a, 'a option) Unsafe_read_c.reader1
  val bin_read_sexp_option__ : ('a, int -> 'a option) Unsafe_read_c.reader1
  val bin_writer_sexp_option : ('a, 'a option) Type_class.S1.writer
  val bin_reader_sexp_option : ('a, 'a option) Type_class.S1.reader
  val bin_sexp_option : ('a, 'a option) Type_class.S1.t
end

module Sexp_list : sig
  open Bin_prot

  val bin_size_sexp_list : ('a, 'a list) Size.sizer1
  val bin_write_sexp_list :('a, 'a list) Map_to_safe.writer1
  val bin_write_sexp_list_ :('a, 'a list) Unsafe_write_c.writer1
  val bin_read_sexp_list : ('a, 'a list) Map_to_safe.reader1
  val bin_read_sexp_list_ : ('a, 'a list) Unsafe_read_c.reader1
  val bin_read_sexp_list__ : ('a, int -> 'a list) Unsafe_read_c.reader1
  val bin_writer_sexp_list : ('a, 'a list) Type_class.S1.writer
  val bin_reader_sexp_list : ('a, 'a list) Type_class.S1.reader
  val bin_sexp_list : ('a, 'a list) Type_class.S1.t
end
