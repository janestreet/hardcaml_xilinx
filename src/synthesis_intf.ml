(** Basic Xilinx FPGA primitives *)

open Base
open Hardcaml

module type Combinational_primitives = sig
  include Comb.S

  val lut : int64 -> t -> t
  val muxcy : t -> t -> t -> t
  val inv : t -> t
  val xorcy : t -> t -> t
  val muxf5 : t -> t -> t -> t
  val muxf6 : t -> t -> t -> t
  val muxf7 : t -> t -> t -> t
  val muxf8 : t -> t -> t -> t
  val mult_and : t -> t -> t
end

module type Sequential_primitives = sig
  val fdce : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
  val fdpe : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
  val ram1s : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
end

module type Xilinx_primitives = sig
  type lut_equation

  include Comb.S

  val x_lut : lut_equation -> t -> t
  val x_map : lut_equation -> t list -> t
  val x_and : t -> t -> t
  val x_or : t -> t -> t
  val x_xor : t -> t -> t
  val x_not : t -> t

  val x_reduce_carry
    :  bool
    -> (lut_equation -> lut_equation -> lut_equation)
    -> t
    -> t
    -> t
    -> t

  val x_and_reduce : t -> t
  val x_or_reduce : t -> t
  val x_reduce_tree : (lut_equation -> lut_equation -> lut_equation) -> t -> t
  val x_add_carry : lut_equation -> t -> t -> t -> t * t
  val x_add : t -> t -> t
  val x_sub : t -> t -> t
  val x_mux_add_carry : lut_equation -> t -> t -> t * t -> t -> t * t

  (** [x_mux_add x (a, a') b] gives [(x ? a : a') + b] *)
  val x_mux_add : t -> t * t -> t -> t

  (** [x_mux_sub x a (b, b')] gives [a - (x ? b : b')] *)
  val x_mux_sub : t -> t -> t * t -> t

  val x_eq : t -> t -> t
  val x_lt : t -> t -> t
  val x_mux : t -> t list -> t
  val x_mulu : t -> t -> t
  val x_muls : t -> t -> t
end

module type Synthesis = sig
  module type Combinational_primitives = Combinational_primitives
  module type Sequential_primitives = Sequential_primitives

  (** Allow expressions to generate LUT init values *)
  module Lut_equation : sig
    type t

    val i0 : t
    val i1 : t
    val i2 : t
    val i3 : t
    val i4 : t
    val i5 : t
    val gnd : t
    val vdd : t
    val ( &: ) : t -> t -> t
    val ( |: ) : t -> t -> t
    val ( ^: ) : t -> t -> t
    val ( ~: ) : t -> t
    val ( ==: ) : t -> t -> t
    val ( <>: ) : t -> t -> t
    val eval : int -> t -> int64
  end

  (** Hardcaml based models of Xilinx primitives *)
  module Hardcaml_combinational_primitives (Comb : Comb.S) :
    Combinational_primitives with type t = Comb.t

  module Hardcaml_sequential_primitives : Sequential_primitives

  (** Unisim library based Xilinx primitives *)
  module Unisim_combinational_primitives : Combinational_primitives with type t = Signal.t

  module Unisim_sequential_primitives : Sequential_primitives

  module type Xilinx_primitives =
    Xilinx_primitives with type lut_equation := Lut_equation.t

  module type Lut_size = sig
    val max_lut : int
  end

  module Lut4 : Lut_size
  module Lut6 : Lut_size

  module Make_xilinx_primitives (X : Combinational_primitives) (L : Lut_size) :
    Xilinx_primitives with type t = X.t

  module Make_comb_primitives (Synth : Xilinx_primitives) :
    Comb.Primitives with type t = Synth.t

  module Make_sequential (Synth : Xilinx_primitives with type t = Signal.t) : sig
    val reg
      :  ?enable:Signal.t
      -> ?reset_to:Signal.t
      -> ?clear:Signal.t
      -> ?clear_to:Signal.t
      -> Reg_spec.t
      -> Signal.t
      -> Signal.t
  end
end
