type t =
  | Auto
  | Block
  | Distributed
  | Ultra
[@@deriving sexp_of]

val to_xpm_args : t -> string
