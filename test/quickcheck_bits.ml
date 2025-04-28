open Base
open Base_quickcheck

module type S = sig
  include Hardcaml.Comb.S with type t = Hardcaml.Bits.t

  val quickcheck_generator : t Generator.t
  val quickcheck_shrinker : t Shrinker.t
  val quickcheck_observer : t Observer.t
end

let valid_width ~width = max 1 width

let random_width ~size ~random =
  Splittable_random.int random ~lo:1 ~hi:(valid_width ~width:size)
;;

let random_bits ~width ~random =
  let rec f width =
    let rand width =
      Hardcaml.Bits.of_int_trunc
        ~width
        (Splittable_random.int random ~lo:0 ~hi:((1 lsl width) - 1))
    in
    if width <= 16 then rand width else Hardcaml.Bits.( @: ) (rand 16) (f (width - 16))
  in
  f width
;;

module Bits_at_size = struct
  include Hardcaml.Bits

  let quickcheck_generator =
    Generator.create (fun ~size ~random ->
      let width = valid_width ~width:size in
      random_bits ~width ~random)
  ;;

  let quickcheck_shrinker = Shrinker.atomic
  let quickcheck_observer = Observer.create (fun _ ~size:_ ~hash -> hash)
end

module Bits_upto_size = struct
  include Bits_at_size

  let quickcheck_generator =
    Generator.create (fun ~size ~random ->
      let width = random_width ~size ~random in
      random_bits ~width ~random)
  ;;
end

let bits_with_size (module Bits : S) n =
  (module struct
    include Bits

    let quickcheck_generator = Generator.with_size quickcheck_generator ~size:n
  end : S)
;;
