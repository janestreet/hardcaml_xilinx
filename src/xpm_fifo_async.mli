(** Internal module. Wraps the Xilinx Async FIFO primitive. This is primarily used for
    cross clock domain translations *)

open Base
open Hardcaml

val create
  :  ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> ?fifo_memory_type:Fifo_memory_type.t
       (** See [Xpm_fifo_async] parameters in [xpm.ml] for default. *)
  -> ?nearly_full:int
       (** usage level at which the [nearly_full] flag is asserted, defaults to
           [capacity - 16] *)
  -> ?nearly_empty:int
       (** usage level at which the [nearly_empty] flag is asserted, defaults to 16 *)
  -> unit
  -> capacity:int
  -> latency:int
  -> wr_clk:Signal.t
  -> rd_clk:Signal.t
  -> clr:Signal.t
       (** [clr] is synchronous to [wr_clk], See ug974 [xpm_fifo_async] port descriptions. *)
  -> wr:Signal.t
  -> d:Signal.t
  -> rd:Signal.t
  -> Signal.t Fifo.t
