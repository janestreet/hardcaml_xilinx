type t =
  | Common_clock
  | Independent_clock
[@@deriving sexp_of]

let to_xpm_args = function
  | Common_clock -> "common_clock"
  | Independent_clock -> "independent_clock"
;;
