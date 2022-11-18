(* Synchronous (single clock) FIFO which may be instantiated using a synthesizable
   Hardcaml model or a (slightely) more efficient Xilinx primitive FIFO. *)

open Base
open Hardcaml

(** Create a synchronous FIFO with the given capacity. If [build_mode] is [Simulation] a
    hardcaml model is generated (note, though, the model is synthesizable). Otherwise a
    XPM primitive FIFO is instantiated.

    If the [overflow_check] (resp [underflow_check)]) logic is enabled a write will not
    occur when the fifo is full (resp read when empty).

    [showahead] reduces the fifo read latency from 1 to 0 cycles relative to [rd]. *)
val create
  :  ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> ?build_mode:Build_mode.t (** default is [Synthesis] *)
  -> ?scope:Scope.t
  -> ?fifo_memory_type:Fifo_memory_type.t
  (** See [Xpm_fifo_sync] parameters in [xpm.ml] for default. *)
  -> ?instance:string (** Only used in synthesis *)
  -> unit
  -> capacity:int
  -> clock:Signal.t
  -> clear:Signal.t
  -> wr:Signal.t
  -> d:Signal.t
  -> rd:Signal.t
  -> Signal.t Fifo.t
