(** True Dual Port Memory with independent clocks for ports [a] and [b]. *)
open! Import

(** Create a Xilinx compatible memory. When [Arch=Rtl] a Hardcaml RTL model is generated.
    Otherwise the XPM memory generator is used by generating a [xpm_memory_tdpram]
    instantiation with appropriate parameters.

    The RAM models [no_change] mode for read on write behaviour. That is when writing,
    reads are disabled and the output ports will not change. This is consistent with
    UltraRAM behaviour.
*)
val create
  :  ?read_latency:int (** Default is 1 *)
  -> ?arch:Ram_arch.t (** Default is [Rtl] *)
  -> ?byte_write_width:Byte_write_width.t (** Default is [Full] *)
  -> unit
  -> clock_a:Signal.t
  -> clock_b:Signal.t
  -> clear_a:Signal.t
  -> clear_b:Signal.t
  -> size:int
  -> port_a:Signal.t Ram_port.t
  -> port_b:Signal.t Ram_port.t
  -> Signal.t * Signal.t
