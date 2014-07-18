open Camlp4.PreCast

let map = object
  inherit Ast.map as super

  method! expr : Ast.expr -> Ast.expr = function
  | Ast.ExId (loc, Ast.IdLid (_, "failwiths")) ->
    <:expr@loc< failwiths ~here:$Pa_here.ast_of_loc loc$ >>
  | e -> super#expr e
end

let () =
  AstFilters.register_str_item_filter map#str_item;
  AstFilters.register_topphrase_filter map#str_item
