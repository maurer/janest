(** An extension of the standard StringLabels. If you open Core.Std, you'll get
    these in the String module. *)

type t = string

include Binable.S with type binable = t
include Comparable.S with type comparable = t
include Container.S0 with type container = t with type elt = char
include Hashable.S with type hashable = t
include Setable.S with type setable = t
include Sexpable.S with type sexpable = t
include Stringable.S with type stringable = t

(* From the standard StringLabels *)
external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
external create : int -> string = "caml_create_string"
val make : int -> char -> string
val copy : string -> string
val sub : string -> pos:int -> len:int -> string
val fill : string -> pos:int -> len:int -> char -> unit
val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
val concat : sep:t -> string list -> string
val escaped : string -> string
val index : string -> char -> int
val rindex : string -> char -> int
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
val contains : string -> char -> bool
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
val compare: string -> string -> int

val init : int -> f:(int -> char) -> t

(**
   [unescaped s] is the inverse operation of [escaped]: it takes a string where
   all the special characters are escaped following the lexical convention of
   OCaml and returns an unescaped copy.
   The [strict] switch is on by default and makes the function treat illegal
   backslashes as errors.
   When [strict] is [false] every illegal backslash except escaped numeral
   greater than [255] is copied literally. The aforementioned numerals still
   raise errors. Thsi mimics the behaviour of the ocaml lexer.
*)
val unescaped : ?strict:bool -> t -> t

(**
   Same as [unescaped] but instead of raising [Failure _] returns an error
   message with the position in the string in case of failure.
*)
val unescaped_res : ?strict:bool -> t -> (t,(int*t)) Result.t

(** [slice s start stop] gets a slice of [s] between [start] and [stop] where
 * [start] and [stop] are normalized. *)
val slice : string -> int -> int -> string

(** [nget s i] Gets the char at normalized position [i] in [s]. *)
val nget : string -> int -> char

(** [nset s i c] Sets the char at normalized position [i] to [c]. *)
val nset : string -> int -> char -> unit

(* CRv2 sweeks: I wonder if better names for [check_{pre,suf}fix] would be
   [is_{pre,suf}fix].  I also think that one argument should be labeled.

     val is_suffix : string -> suf:t -> bool
     val is_prefix : string -> suf:t -> bool
*)

(** [check_suffix s suff] returns [true] if the string [s] ends with the suffix
    [suff] *)
val check_suffix : string -> string -> bool

(** [check_prefix s pref] returns [true] if the string [s] starts with the prefix [pref] *)
val check_prefix : string -> string -> bool

(* CRv2 tvaroquaux there's a split2 but not split.... ths seems odd.*)
(** if [string] contains the character [char], then [split2_exn string char]
    returns a pair containing [string] split around the first appearance of
    [char] (from the left)
    @raise Not_found When [char] cannot be found in [string]
*)
val split2_exn : string -> char -> string * string

(** if [string] contains the character [char], then [rsplit2_exn string char]
    returns a pair containing [string] split around the first appearance of
    [char] (from the right)
    @raise Not_found When [char] cannot be found in [string]
*)
val rsplit2_exn : string -> char -> string * string

(** [split2 line delim] optionally returns [line] split into two strings around the
  * first appearance of [delim] from the left *)
val split2 : string -> char -> (string * string) option

(** [rsplit2 line delim] optionally returns [line] split into two strings around the
  * first appearance of [delim] from the right *)
val rsplit2 : string -> char -> (string * string) option

(** [split_on_char str delim] @return a list of all substrings of [str]
    that are separated by [delim]. *)
val split_on_char : string -> char -> string list

(** [split_on_pipe str] @return a list of all substrings of [str]
    that are separated by '|'. *)
val split_on_pipe : string -> string list

(** [split_on_dot str] @return a list of all substrings of [str]
    that are separated by '.'. *)
val split_on_dot : string -> string list

(** [split_on_comma str] @return a list of all substrings of [str]
    that are separated by ','. *)
val split_on_comma : string -> string list

(** [split_on_chars str delims] @return a list of all substrings of [str] that are
    separated by any number of the chars from [delims]  *)
val split_on_chars : t -> char list -> t list

(** [findi string ~f] returns the index [i] of the first character in [t] satisfying
    [f i t.[i]]. *)
val findi : string -> f:(int -> char -> bool) -> int option

(** [rfindi string ~f] returns the index [i] of the last character in [t] satisfying
    [f i t.[i]]. *)
val findi : string -> f:(int -> char -> bool) -> int option

(** [lstrip str] returns a new string with consecutive white space (tabs, spaces, newlines,
    and carriage returns) stripped from the beginning of [str]. *)
val lstrip : string -> string

(** [rstrip str] returns a new string with consecutive white space (tabs, spaces, newlines,
    and carriage returns) stripped from the end of [str]. *)
val rstrip : string -> string

(** [strip str] returns a new string with consecutive white space (tabs, spaces, newlines,
    and carriage returns) stripped from the beginning and end of str. *)
val strip : string -> string

(** [map f s] applies [f] to each character in [s], and returns the resulting string. *)
val map : f : (char -> char) -> string -> string

(** Like [map], but allows replacement of a single character with zero or two or more
    characters. *)
val translate : f : (char -> string) -> string -> string

(** [tr target replacement s] replaces every instance of [target] in [s] with
    [replacement]. *)
val tr : target : char -> replacement : char -> string -> string

(** [drop_suffix t suff] returns a copy [t] without the trailing [suff]
    @raise Invalid_argument is [suff] is not a suffix [t]
*)
val chop_suffix : t -> t -> t

(** [drop_prefix t suff] returns a copy [t] without the trailing [pref]
    @raise Invalid_argument is [pref] is not a suffix [t]
*)
val chop_prefix : t -> t -> t

(* CRv2 sweeks: Perhaps [{keep,drop}_{suffix,prefix}_n] should be called
   [{r,}{keep,drop}_n]? *)
(** [keep_suffix_n string n] returns the suffix of [t] of length [n] *)
val keep_suffix_n : string -> int -> string

(** [keep_prefix_n string n] returns the prefix of [t] of length [n] *)
val keep_prefix_n : string -> int -> string

(** [drop_suffix_n string n] drops the suffix of [t] of length [n] *)
val drop_suffix_n : string -> int -> string

(** [drop_prefix_n string n] drops the prefix of [t] of length [n] *)
val drop_prefix_n : string -> int -> string

(** [concat_array sep ar] like {!String.concat}, but operates on arrays *)
val concat_array : sep : string -> string array -> string

(** slightly faster hash function on strings *)
val hash : string -> int

(** fast equality function on strings, doesn't use compare_val *)
val equal : string -> string -> bool

(** This has to be public for interactive top-levels. *)
val pp : Format.formatter -> string -> unit

(** [is_empty t] returns [true] iff [t] is empty (i.e. its length is 0). *)
val is_empty : string -> bool

module Infix : sig
  val ( </> ) : string -> int * int -> string
end

val of_char : char -> t
