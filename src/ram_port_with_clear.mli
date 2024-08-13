(** State machine for clearing a RAM via one of it's ports. *)

open! Base
open! Hardcaml

(** This wraps a Ram_port.t with a small state machine that on [clear] sets all memory
    location values to the [clear_to] value. While the memory is being cleared, the output
    clear_busy will be high. Writes and reads while in this state will be ignored.

    The port signals require an extra mutliplexer delay for this function.

    Note that use of other ports on the RAM for writing is discouraged during the clear
    operation. For maximum safety one can guard the write enable to the other port with

    [{other_port.write_enable &: ~:port_with_clear.clear_busy}]
*)
type 'a t =
  { port : 'a Ram_port.t
  ; clear_busy : 'a
  }
[@@deriving hardcaml]

val create
  :  scope:Scope.t
  -> clear_to:Signal.t
  -> clear:Signal.t
  -> clock:Signal.t
  -> size:int
  -> port:Signal.t Ram_port.t
  -> Signal.t t
