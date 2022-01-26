open! Base
open! Hardcaml

type t =
  | Distributed
  | Blockram of Collision_mode.t
  | Ultraram
[@@deriving sexp_of]

let to_xpm_parameter = function
  | Distributed -> "distributed"
  | Blockram _ -> "block"
  | Ultraram -> "ultra"
;;
