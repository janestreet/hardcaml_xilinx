open! Base
open! Hardcaml

type 'a t =
  { address : 'a
  ; data : 'a
  ; read_enable : 'a
  ; write_enable : 'a
  }
[@@deriving hardcaml]
