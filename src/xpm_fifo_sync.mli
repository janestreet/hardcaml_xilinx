(** Internal module. Wraps the Xilinx Sync FIFO primitive. *)

open Base
open Hardcaml

module Xpm_2019_1 : sig
  (** Wrap the Xilinx FIFO primitive so that the interface matches [Hardcaml.Fifo].

      The exact timing of flags (full, empty etc) do not exactly match between the two
      implementations, however, relative to the data, they are the consistent. *)
  val create
    :  ?overflow_check:bool (** default is [true] *)
    -> ?showahead:bool (** default is [false] **)
    -> ?underflow_check:bool (** default is [true] *)
    -> ?fifo_memory_type:Fifo_memory_type.t
         (** See [Xpm_fifo_sync] parameters in [xpm.ml] for default. *)
    -> ?nearly_full:int
         (** usage level at which the [nearly_full] flag is asserted, defaults to
             [capacity - 16] *)
    -> ?nearly_empty:int
         (** usage level at which the [nearly_empty] flag is asserted, defaults to 16 *)
    -> ?instance:string
    -> ?read_latency:int
         (** Default is None which will either set [read_latency] to 0 or 1 if [showahead]
             is true or false respectively. *)
    -> unit
    -> capacity:int
    -> clk:Signal.t
    -> clr:Signal.t
    -> wr:Signal.t
    -> d:Signal.t
    -> rd:Signal.t
    -> Signal.t Fifo.t
end

module Xpm_2022_1 : sig
  (** 2021.1 onwards added the [CASCADE_HEIGHT] parameter, which allows you to manually
      set the cascade height Vivado uses to synthesize the FIFO. *)
  val create
    :  ?overflow_check:bool (** default is [true] *)
    -> ?showahead:bool (** default is [false] **)
    -> ?underflow_check:bool (** default is [true] *)
    -> ?fifo_memory_type:Fifo_memory_type.t
         (** See [Xpm_fifo_sync] parameters in [xpm.ml] for default. *)
    -> ?nearly_full:int
         (** usage level at which the [nearly_full] flag is asserted, defaults to
             [capacity - 16] *)
    -> ?nearly_empty:int
         (** usage level at which the [nearly_empty] flag is asserted, defaults to 16 *)
    -> ?instance:string
    -> ?cascade_height:int (** default is [0] -> Vivado chooses. *)
    -> ?read_latency:int
         (** Default is None which will either set [read_latency] to 0 or 1 if [showahead]
             is true or false respectively. *)
    -> unit
    -> capacity:int
    -> clk:Signal.t
    -> clr:Signal.t
    -> wr:Signal.t
    -> d:Signal.t
    -> rd:Signal.t
    -> Signal.t Fifo.t
end
