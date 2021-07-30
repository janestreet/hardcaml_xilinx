open Base

type t =
  | Auto
  | Block
  | Distributed
  | Ultra
[@@deriving sexp_of]

let to_xpm_args t =
  match sexp_of_t t with
  | Atom s -> String.lowercase s
  | _ -> assert false
;;
