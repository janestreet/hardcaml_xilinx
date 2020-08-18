open! Import

type t =
  | Rtl
  | Auto
  | Distributed
  | Blockram
  | Ultraram

let to_string = function
  | Rtl -> "hardcaml_rtl_ram"
  | Auto -> "auto"
  | Distributed -> "distributed"
  | Blockram -> "block"
  | Ultraram -> "ultra"
;;
