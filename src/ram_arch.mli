(** Xilinx RAM primitive types.

    This is specified for xpm macro generation. Note that there exists an [Auto] mode that
    is not included here. It is not clear what RAM behaviour to model in that case and we
    run the risk of simulation/synthesis mismatches. *)

open Base
open! Hardcaml

type t =
  | Distributed
  (** XPM macro - distributed LUT RAM. Collision mode is Read_before_write. *)
  | Blockram of Collision_mode.t (** XPM macro - RAMB36E2 *)
  | Ultraram of Safer_uram_collision_inference.t
  (** XPM macro - UltraRAM with optional wrapper to manage collision mode inference. *)
[@@deriving sexp_of]

val to_xpm_parameter : t -> string
