## 109.60.00

- Compatibility with warning 7 (method override)

## 109.35.00

- New syntax extension to get improved location information on calls
  to `failwiths`.

    Added `failwithp`, which is like `failwiths` but takes a `_here_`
    argument:

    ```ocaml
    val failwithp : Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _
    ```

    `pa_fail` automatically converts `failwiths` into `failwithp _here_`

    If you don't want to compile with `pa_fail`, then you can manually
    write `failwithp _here_`.  Otherwise, running the same source
    through `pa_fail` gets you location information for free.

