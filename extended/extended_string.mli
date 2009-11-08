open Core.Std

(**
   Extensions to [Core.Core_String] .
*)

(**
   [collate s1 s2] sorts string in an order that's is usaully more suited
   for human consumption by treating ints specificaly:
   (e.g. it will output: [["rfc1.txt";"rfc822.txt";"rfc2086.txt"]]).

   It works by splitting the strings in numerical and non numerical chunks and
   comparing chunks two by two from left to right (and starting on a non
   numerical chunks):
   - Non_numerical chunks are compared using lexicographical ordering.
   - Numerical chunks are compared based on the values of the represented ints
   and the number of trailing zeros.

   It is a total order.
*)
val collate : string -> string -> int

(* CRv104 till for till move back to core*)
(**
   [unescaped s] is the inverse operation of [escaped]: it takes a string where
   all the special characters are escaped following the lexical convention of
   OCaml and returns an unescaped copy.
   The [strict] switch is on by default and makes the function treat illegal
   backslashes as errors.
   When [strict] is [false] every illegal backslash except escaped numeral
   greater than [255] is copied literally. The aforementioned numerals still
   raise errors. This mimics the behaviour of the ocaml lexer.
*)
val unescaped : ?strict:bool -> string -> string

(**
   Same as [unescaped] but instead of raising [Failure _] returns an error
   message with the position in the string in case of failure.
*)
val unescaped_res : ?strict:bool -> string -> (string,(int*string)) Core.Result.t

(** [squeeze str] reduces all sequences of spaces, newlines, tables, and
 * carriage returns to single spaces.
 *)

val squeeze : string -> string

(** [is_substring ~substring t] returns [true] if substring is a substring
 * of t.
 *)
val is_substring : substring:string -> string -> bool
