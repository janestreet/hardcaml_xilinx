(* Asynchronous (dual clock) FIFO which may be instantiated using a simulation only
   Hardcaml model or a Xilinx primitive FIFO. *)

open Base
open Hardcaml

(** Create a synchronous FIFO with the given capacity. If [build_mode] is [Simulation] a
    hardcaml model is generated. Otherwise a XPM primitive FIFO is instantiated.

    If the [overflow_check] (resp [underflow_check)]) logic is enabled a write will not
    occur when the fifo is full (resp empty).

    [showahead] reduces the fifo read latency from 1 to 0 cycles relative to [rd].

    In simulation mode, the write clock is ignored and the FIFO is effectively
    synchronous. *)
val create
  :  ?showahead:bool (** default is [false] **)
  -> ?build_mode:Build_mode.t (* default is [Synthesis] *)
  -> ?fifo_memory_type:Fifo_memory_type.t
  (** See [Xpm_fifo_async] parameters in [xpm.ml] for default. *)
  -> ?nearly_full:int
  -> ?nearly_empty:int
  -> ?scope:Scope.t
  -> unit
  -> capacity:int
  -> read_clock:Signal.t
  -> write_clock:Signal.t
  -> clear:Signal.t
  -> write:Signal.t
  -> d:Signal.t
  -> read:Signal.t
  -> Signal.t Fifo.t
