(** Internal module. Wraps the Xilinx Sync FIFO primitive. *)

open Base
open Hardcaml


(** Wrap the Xilinx FIFO primitive so that the interface matches [Hardcaml.Fifo].

    The exact timing of flags (full, empty etc) do not exactly match between the two
    implementations, however, relative to the data, they are the consistent. *)
val create
  :  ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> unit
  -> capacity:int
  -> clk:Signal.t
  -> clr:Signal.t
  -> wr:Signal.t
  -> d:Signal.t
  -> rd:Signal.t
  -> Signal.t Fifo.t
