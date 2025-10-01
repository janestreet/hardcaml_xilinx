open! Base
open! Hardcaml

type t =
  | Distributed
  | Blockram of Collision_mode.t
  | Ultraram of Safer_uram_collision_inference.t
[@@deriving sexp_of]

let to_xpm_parameter = function
  | Distributed -> "distributed"
  | Blockram _ -> "block"
  | Ultraram _ -> "ultra"
;;
