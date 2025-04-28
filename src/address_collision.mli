(** Handling of address collisions between ports in native Xilinx RAM technologies. *)

open Base
open Hardcaml

module Protection : sig
  type t =
    | None__there_be_dragons
    (** Do nothing - the dragons here are X's, and we do see inconsistent output in
        hardware *)
    | Control_enables
    (** Perform address comparison and control the read/write enables to output the
        correct values *)
    | Mux_output_ports
    (** Multiplex the 2 output ports of the RAM to select the correct value. Note that
        both ports are read regardless of collisions to avoid logic on the input side *)
  [@@deriving sexp_of]
end

(** What to output when there is an address collision *)
module Model : sig
  type t =
    | None__there_be_dragons (** Do nothing **)
    | Const of int (** Drive the output port to a constant value **)
    | Counter (** Output a free running counter **)
    | Graycode
    (** Output a free running gray code counter (possibly looks a little more random than
        a straight up counter) **)
    | Lfsr
    (** Output a free running LFSR -- see implementation for details -- it's roughly
        random *)
  [@@deriving sexp_of]

  (** Model the collision on the output bus of the RAM *)
  val q : t -> Signal.Reg_spec.t -> address_collision:Signal.t -> Signal.t -> Signal.t
end
