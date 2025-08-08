(** Xilinx compatible dual port memory implemented with the XPM memory macro generator for
    synthesis. *)

open Base
open Hardcaml

type 'signal create =
  ?address_collision_model:Address_collision.Model.t
  -> ?read_latency:int (** Default is 1 *)
  -> ?arch:Ram_arch.t (** Default is [Blockram No_change] *)
  -> ?byte_write_width:Byte_write_width.t (** Default is [Full] *)
  -> ?memory_optimization:bool
  -> ?cascade_height:Cascade_height.t
  -> ?clocking_mode:Clocking_mode.t
       (** By default, we make a best-guess effort to choose the correct clocking model -
           if the two clocks have the same [Uid.t], we pick [Common_clock]; otherwise, we
           pick [Independent_clock] *)
  -> ?simulation_name:string
       (** In simulation, set the name of the underlying [multiport_memory] node. *)
  -> build_mode:Build_mode.t
  -> unit
  -> clock_a:'signal
  -> clock_b:'signal
  -> clear_a:'signal
  -> clear_b:'signal
  -> size:int (** The number of port A width words. *)
  -> port_a:'signal Ram_port.t
  -> port_b:'signal Ram_port.t
  -> 'signal * 'signal

module type True_dual_port_ram = sig
  type nonrec 'signal create = 'signal create

  (** Create a Xilinx compatible memory. For [Synthesis] the XPM memory generator is used
      by generating a [xpm_memory_tdpram] instantiation with appropriate parameters. For
      [Simulation] hardcaml multiport memories are used to model the Xilinx memory
      behaviour.

      The interface to the RAM differs subtly from the core primitives - we use separate
      read and write enables, rather than a single enable and write signal. The mapping is
      as follows:

      - write_enable = enable & write
      - read_enable = enable

      The main difference occurs when write_enable and read_enable are both high. In the
      [No_change] collision mode, the read will not occur.

      The documentation explains what happens across ports on address collisions, and in
      some cases this leads to [X]s on the output of one port of the other. Hardcaml
      cannot express [X]s, so we build some custom logic to put "something unexpected" on
      the output ports (see [Address_collision.Model.t]).

      Please see the table at the top of the implementation file for more information.

      Ultraram has it's own subtle behaviour on address collision and this is modelled in
      hardcaml by putting the ports in opposite read/write first modes.

      Distributed RAM is set up to work the same as BlockRAM.

      There is a verilog testbench in [test_hdl] which works with the [xsim_modelling]
      application to test the subtle address collision cases. *)
  val create : Signal.t create

  (** Same as [create] but with clocked signals. Ports a and b may be in different clock
      domains. *)
  module Clocked : sig
    val create : Clocked_signal.t create
  end
end
