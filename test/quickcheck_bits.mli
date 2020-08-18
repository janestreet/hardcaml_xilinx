open Base
open Base_quickcheck

module type S = sig
  include Hardcaml.Comb.S with type t = Hardcaml.Bits.t

  val quickcheck_generator : t Generator.t
  val quickcheck_shrinker : t Shrinker.t
  val quickcheck_observer : t Observer.t
end

(** Produce random bit vectors with widths = size. *)
module Bits_at_size : S

(** Produce random bit vectors with widths randomly between [1<=width<=size], where size
    is the randomised quickcheck control parameter. *)
module Bits_upto_size : S

(** Fix the size argument.

    [bits_with_size (module Bit_at_size) 4] will always generate 4 bit vectors.

    [bits_with_size (module Bit_upto_size) 4] will generate upto 4 bit vectors.

    When using the ppx, we can easily generate things like

    [type t = Bits_at_size.t * Bits_at_size]

    Here both bits will have the same width (the size control parameter is the same when
    passed to each generator of the pair).
*)
val bits_with_size : (module S) -> int -> (module S)
