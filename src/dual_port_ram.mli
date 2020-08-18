(** Single clock Dual Port Memory *)
open! Import

(** Create a Xilinx compatible memory. Uses True_dual_port_ram with appropriate parameters
    for implementation. *)
val create
  :  ?read_latency:int (** Default is 1 *)
  -> ?arch:Ram_arch.t (** Default is [Rtl] *)
  -> ?byte_write_width:Byte_write_width.t (** Default is [Full] *)
  -> unit
  -> clock:Signal.t
  -> clear:Signal.t
  -> size:int
  -> port_a:Signal.t Ram_port.t
  -> port_b:Signal.t Ram_port.t
  -> Signal.t * Signal.t
