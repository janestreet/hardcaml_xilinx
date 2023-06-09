(* Synchronous (single clock) FIFO which may be instantiated using a synthesizable
   Hardcaml model or a (slightely) more efficient Xilinx primitive FIFO. *)

open Base
open Hardcaml

(** Create a synchronous FIFO with the given capacity. If [build_mode] is [Simulation] a
    hardcaml model is generated (note, though, the model is synthesizable). Otherwise a
    XPM primitive FIFO is instantiated.

    If the [overflow_check] (resp [underflow_check)]) logic is enabled a write will not
    occur when the fifo is full (resp read when empty).

    [showahead] reduces the fifo read latency from 1 to 0 cycles relative to [rd].
    Optionally the [read_latency] can be increased when [showahead] is false, to add extra
    pipelining to the FIFO output. *)
val create
  :  ?read_latency:int
  (** Default is None which will either set [read_latency] to 0 or 1 if [showahead] is
      true or false respectively. *)
  -> ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> ?build_mode:Build_mode.t (** default is [Synthesis] *)
  -> ?scope:Scope.t
  -> ?fifo_memory_type:Fifo_memory_type.t
  (** See [Xpm_fifo_sync] parameters in [xpm.ml] for default. *)
  -> ?instance:string (** Only used in synthesis *)
  -> ?xpm_version:[ `Xpm_2019_1 | `Xpm_2022_1 ]
  (** Used to decide which XPM version instantiation to use *)
  -> ?cascade_height:int (** default is [0] -> Vivado chooses; only used for Xpm 2022.1 *)
  -> ?nearly_full:int (** usage level at which [nearly_full] will be asserted *)
  -> ?nearly_empty:int (** usage level at which [nearly_empty] will be asserted *)
  -> unit
  -> capacity:int
  -> clock:Signal.t
  -> clear:Signal.t
  -> wr:Signal.t
  -> d:Signal.t
  -> rd:Signal.t
  -> Signal.t Fifo.t
