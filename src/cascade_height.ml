open Base

type t =
  | Inferred
  | Specified of int
[@@deriving sexp_of]

let to_xpm_args = function
  | Inferred -> 0
  | Specified cascade_height ->
    if cascade_height <= 0 || cascade_height > 64
    then raise_s [%message (cascade_height : int) "cascade height must be between 1-64"];
    cascade_height
;;
