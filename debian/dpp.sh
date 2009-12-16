#!/bin/sh

lib/pp.sh $@ | camlp4 '-I' '/usr/lib/ocaml/camlp4' '-I' '/usr/lib/ocaml/type-conv' '-I' '/usr/lib/ocaml' '-I' '/usr/lib/ocaml/sexplib' '-I' '/usr/lib/ocaml/fieldslib' '-I' '/usr/lib/ocaml/bin_prot' '-parser' 'o' '-parser' 'op' '-printer' 'p' 'pa_type_conv.cmo' 'unix.cma' 'bigarray.cma' 'nums.cma' 'sexplib.cma' 'pa_sexp_conv.cmo' 'fieldslib.cma' 'pa_fields_conv.cmo' 'bin_prot.cma' 'pa_bin_prot.cmo'
