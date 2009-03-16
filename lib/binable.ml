open Bin_prot

module type S = sig
  type binable

  val bin_size_t : binable Size.sizer
  val bin_write_t : binable Map_to_safe.writer
  val bin_write_t_ : binable Unsafe_write_c.writer
  val bin_read_t : binable Read_ml.reader
  val bin_read_t_ : binable Unsafe_read_c.reader
  val bin_read_t__ : (int -> binable) Unsafe_read_c.reader
end

module type S1 = sig
  type 'a binable

  val bin_size_t : ('a, 'a binable) Size.sizer1
  val bin_write_t :('a, 'a binable) Map_to_safe.writer1
  val bin_write_t_ :('a, 'a binable) Unsafe_write_c.writer1
  val bin_read_t : ('a, 'a binable) Map_to_safe.reader1
  val bin_read_t_ : ('a, 'a binable) Unsafe_read_c.reader1
  val bin_read_t__ : ('a, int -> 'a binable) Unsafe_read_c.reader1
end

module type S2 = sig
  type ('a, 'b) binable

  val bin_size_t : ('a, 'b, ('a, 'b) binable) Size.sizer2
  val bin_write_t :('a, 'b, ('a, 'b) binable) Map_to_safe.writer2
  val bin_write_t_ :('a, 'b, ('a, 'b) binable) Unsafe_write_c.writer2
  val bin_read_t : ('a, 'b, ('a, 'b) binable) Map_to_safe.reader2
  val bin_read_t_ : ('a, 'b, ('a, 'b) binable) Unsafe_read_c.reader2
  val bin_read_t__ : ('a, 'b, int -> ('a, 'b) binable) Unsafe_read_c.reader2
end
