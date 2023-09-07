open Base
open Hardcaml

module type Config = sig
  val log_num_words : int
  val bits_per_word : int
  val log_scale_between_ports : int
  val read_latency : int
  val collision_mode : Collision_mode.t option
  val byte_write_width : Byte_write_width.t option
  val clocking_mode : Clocking_mode.t option
end

module type S = sig
  (** number of bits in the write address. *)
  val write_address_bits : int

  (** number of bits in the read address. *)
  val read_address_bits : int

  (** number of bits in the write bus. *)
  val write_data_bits : int

  (** number of bits in the read bus. *)
  val read_data_bits : int

  (** the number of bits in the write enable. *)
  val write_enable_bits : int

  module I : sig
    type 'a t =
      { write_clock : 'a
      ; write_clear : 'a
      ; write_address : 'a
      ; write_data : 'a
      ; write_enable : 'a
      ; read_clock : 'a
      ; read_clear : 'a
      ; read_address : 'a
      ; read_enable : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> build_mode:Build_mode.t -> Interface.Create_fn(I)(O).t

  val hierarchical
    :  ?instance:string
    -> Scope.t
    -> build_mode:Build_mode.t
    -> Interface.Create_fn(I)(O).t
end

(** RAM for which the the inputs and output data busses are different widths by some power
    of 2 factor. Assumes a write-before-read ordering between the ports. *)
module type Ram_with_resizing = sig
  module type Config = Config
  module type S = S

  module Make (Config : Config) : S
end
