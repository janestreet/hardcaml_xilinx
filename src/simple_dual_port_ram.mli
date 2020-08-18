(** Simple Dual Port Memory. 1 port is for writing, the other for reading. *)
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
  -> write_address:Signal.t
  -> write_enable:Signal.t
  -> data:Signal.t
  -> read_address:Signal.t
  -> read_enable:Signal.t
  -> Signal.t
