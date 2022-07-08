(** Single clock Dual Port Memory *)

open Base
open Hardcaml

(** Create a Xilinx compatible memory. Uses True_dual_port_ram with appropriate parameters
    for implementation. *)
val create
  :  ?read_latency:int (** Default is 1 *)
  -> ?arch:Ram_arch.t (** Default is [Block_ram No_change] *)
  -> ?byte_write_width:Byte_write_width.t (** Default is [Full] *)
  -> ?memory_optimization:bool
  -> ?cascade_height:Cascade_height.t
  (** See [Xpm_memory_tdpram] parameters in [xpm.ml] for default. *)
  -> ?simulation_name:string
  (** In simulation, set the name of the underlying [multiport_memory] node. *)
  -> build_mode:Build_mode.t
  -> unit
  -> clock:Signal.t
  -> clear:Signal.t
  -> size:int
  -> port_a:Signal.t Ram_port.t
  -> port_b:Signal.t Ram_port.t
  -> Signal.t * Signal.t
