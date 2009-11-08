(**
   String escaping.

   Operations for escaping and unescaping strings, with paramaterized escape
   and escapeworthy characters.
*)



(** [escape_gen escapeworthy_map escape_char s] returns an escaped string based on
    [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then all occurences of
    [c1] are replaced by [escape_char] concatenated to [c2]. *)
val escape_gen :
  escapeworthy_map:(char * char) list -> escape_char:char -> string -> string

(** [escape escapeworthy escape_char s] is
    [escape_gen ~escapeworthy_map:(List.combine escapeworthy escapeworthy)
    ~escape_char]. *)
val escape : escapeworthy:char list -> escape_char:char -> string -> string

(** [escape_one_orig ~escapeworthy ~escape_char s] escapes character
    [escapeworthy] with [escape_char] in string [s].  The function
    returns the original string if no character had to be escaped. *)
val escape_one_orig :
  escapeworthy : char -> escape_char : char -> string -> string

(** [escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char s]
    escapes characters [escapeworthy1] and [escapeworthy2] with
    [escape_char] in string [s].  The function returns the original
    string if no character had to be escaped. *)
val escape_two_orig :
  escapeworthy1 : char -> escapeworthy2 : char -> escape_char : char -> string
  -> string

(** [unescape_gen] is the inverse operation of [escape_gen], assuming an inverse
    map is given.  That is, [unescape_gen map escape_char s] returns an escaped string
    based on [s] as follows: if [(c1,c2)] is in [map], then all occurrences of
    [escape_char][c1] are replaced by [c2]. *)
val unescape_gen :
  map:(char * char) list -> escape_char:char -> string -> string

(** [unescape escape_char s] is [unescape_gen ~map:\[\] ~escape_char str] *)
val unescape : escape_char:char -> string -> string
