(** Internal module. Wraps the Xilinx Async FIFO primitive. This is primarily used for
    cross clock domain translations *)
open! Import

val create
  :  ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> unit
  -> capacity:int
  -> latency:int
  -> wr_clk:Signal.t
  -> rd_clk:Signal.t
  -> clr:Signal.t
  (** [clr] is synchronous to [wr_clk], See ug974 [xpm_fifo_async] port
      descriptions. *)
  -> wr:Signal.t
  -> d:Signal.t
  -> rd:Signal.t
  -> Signal.t Fifo.t
