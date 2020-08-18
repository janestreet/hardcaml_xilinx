open! Import

type t =
  | Rtl (** Hardcaml RTL model *)
  | Auto (** XPM macro - synthesizer chooses implementation *)
  | Distributed (** XPM macro - distributed LUT RAM *)
  | Blockram (** XPM macro - RAMB36E2 *)
  | Ultraram (** XPM macro - UltraRAM *)

val to_string : t -> string
