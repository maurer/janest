open Core.Std
open OUnit;;
module Escaping = Core_extended.Escaping
let test =
  "jane_string" >:::
    [ "escape_gen" >::
        (fun () ->
          let escape = Escaping.escape_gen
            ~escapeworthy_map:[('%','p');('^','c')] ~escape_char:'_'
          in
          "nothing" @? ((escape "foo") = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_pbar");
          "baz" @? (escape "^foo%" = "_cfoo_p");
        );
      "escape" >::
        (fun () ->
          let escape = Escaping.escape
            ~escapeworthy:['%';'^'] ~escape_char:'_' in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "^foo%" = "_^foo_%");
        );
      "escape_one_orig" >::
        (fun () ->
          let escape = Escaping.escape_one_orig
            ~escapeworthy:'%' ~escape_char:'_' in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "%foo%" = "_%foo_%");
        );
      "escape_two_orig" >::
        (fun () ->
          let escape = Escaping.escape_two_orig
            ~escapeworthy1:'%' ~escapeworthy2:'^' ~escape_char:'_'
          in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "^foo%" = "_^foo_%");
        );
      "unescape_gen" >::
        (fun () ->
          let unescape = Escaping.unescape_gen
            ~map:['p','%';'c','^'] ~escape_char:'_' in
          "nothing" @? (unescape "foo" = "foo");
          "foo" @? (unescape "__" = "_");
          "bar" @? (unescape "foo_pbar" = "foo%bar");
          "baz" @? (unescape "_cfoo_p" = "^foo%");
        );
      "unescape" >::
        (fun () ->
          let unescape = Escaping.unescape ~escape_char:'_' in
          "nothing" @? (unescape "foo" = "foo");
          "foo" @? (unescape "__" = "_");
          "bar" @? (unescape "foo_%bar" = "foo%bar");
          "baz" @? (unescape "_^foo_%" = "^foo%");
        );
    ]
