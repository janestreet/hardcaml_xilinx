(* API representing basic Xilinx primitives *)

open Base
open Hardcaml
open! Synthesis_intf

module type Combinational_primitives = Combinational_primitives
module type Sequential_primitives = Sequential_primitives

(* algebra for building LUT equations *)
module Lut_equation = struct
  type t =
    | Gnd
    | Vdd
    | Input of int64
    | And of t * t
    | Or of t * t
    | Xor of t * t
    | Not of t

  let i0 = Input 0L
  let i1 = Input 1L
  let i2 = Input 2L
  let i3 = Input 3L
  let i4 = Input 4L
  let i5 = Input 5L
  let gnd = Gnd
  let vdd = Vdd
  let ( &: ) a b = And (a, b)
  let ( |: ) a b = Or (a, b)
  let ( ^: ) a b = Xor (a, b)
  let ( ~: ) a = Not a
  let ( <>: ) a b = a ^: b
  let ( ==: ) a b = ~:(a <>: b)
  let ( >>. ) a b = Int64.shift_right_logical a (Int64.to_int_exn b)
  let ( <<. ) a b = Int64.shift_left a (Int64.to_int_exn b)
  let ( &. ) = Int64.( land )
  let ( |. ) = Int64.( lor )
  let ( ^. ) = Int64.( lxor )
  let ( ~. ) = Int64.lnot
  let ( +. ) = Int64.( + )

  let eval n v =
    let n = Int64.of_int n in
    let rec eval n = function
      | Gnd -> 0L
      | Vdd -> 1L
      | Input a -> n >>. a &. 1L
      | And (a, b) -> eval n a &. eval n b
      | Or (a, b) -> eval n a |. eval n b
      | Xor (a, b) -> eval n a ^. eval n b
      | Not a -> ~.(eval n a) &. 1L
    in
    let rec evaln m w =
      if Int64.equal m (1L <<. n) then w else evaln (m +. 1L) (eval m v <<. m |. w)
    in
    evaln 0L 0L
  ;;
end

(* Hardcaml implementation of Xilinx API *)
module Hardcaml_combinational_primitives (Comb : Comb.S) = struct
  module Comb = Comb
  include Comb

  let lut v sel =
    let n = 1 lsl width sel in
    let rec build i =
      if i = n
      then []
      else (
        let d =
          if not (Int64.equal (Int64.( land ) v (Int64.shift_left 1L i)) 0L)
          then vdd
          else gnd
        in
        d :: build (i + 1))
    in
    mux sel (build 0)
  ;;

  let muxcy ci di sel = mux2 sel ci di
  let inv a = ~:a
  let xorcy ci li = ci ^: li
  let muxf5 f t s = mux2 s t f
  let muxf6 f t s = mux2 s t f
  let muxf7 f t s = mux2 s t f
  let muxf8 f t s = mux2 s t f
  let mult_and a b = a &: b
end

module Hardcaml_sequential_primitives = struct
  open Signal

  let fdce c ce clr d =
    reg (Reg_spec.create () ~clock:c ~reset:clr) ~reset_to:gnd ~enable:ce d
  ;;

  let fdpe c ce pre d =
    reg (Reg_spec.create () ~clock:c ~reset:pre) ~reset_to:vdd ~enable:ce d
  ;;

  let ram1s a d clk we =
    memory
      (1 lsl width a)
      ~write_port:
        { write_clock = clk; write_enable = we; write_address = a; write_data = d }
      ~read_address:a
  ;;
end

(* unisim based implementation of Xilinx API *)
module Unisim_combinational_primitives = struct
  include Signal

  let inv a =
    Map.find_exn
      (Instantiation.create () ~name:"INV" ~inputs:[ "I", a ] ~outputs:[ "O", 1 ])
      "O"
  ;;

  let lut v sel =
    let w = width sel in
    let w' = Int.to_string w in
    let init =
      Int64.to_string v
      |> Bits.of_decimal_string ~width:(1 lsl w)
      |> Bits.reverse
      |> Bits.to_string
    in
    Map.find_exn
      (Instantiation.create
         ()
         ~name:("LUT" ^ w')
         ~parameters:[ Parameter.create ~name:"INIT" ~value:(String init) ]
         ~inputs:(List.mapi (bits_lsb sel) ~f:(fun i b -> "I" ^ Int.to_string i, b))
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let muxcy ci di sel =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MUXCY"
         ~inputs:[ "CI", ci; "DI", di; "S", sel ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let xorcy ci li =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"XORCY"
         ~inputs:[ "CI", ci; "LI", li ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let muxf5 f t s =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MUXF5"
         ~inputs:[ "I0", f; "I1", t; "S", s ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let muxf6 f t s =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MUXF6"
         ~inputs:[ "I0", f; "I1", t; "S", s ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let muxf7 f t s =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MUXF7"
         ~inputs:[ "I0", f; "I1", t; "S", s ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let muxf8 f t s =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MUXF8"
         ~inputs:[ "I0", f; "I1", t; "S", s ]
         ~outputs:[ "O", 1 ])
      "O"
  ;;

  let mult_and a b =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"MULT_AND"
         ~inputs:[ "I0", a; "I1", b ]
         ~outputs:[ "LO", 1 ])
      "LO"
  ;;
end

module Unisim_sequential_primitives = struct
  open Signal

  let fdce c ce clr d =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"FDCE"
         ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "0") ]
         ~inputs:[ "C", c; "CE", ce; "CLR", clr; "D", d ]
         ~outputs:[ "Q", 1 ])
      "Q"
  ;;

  let fdpe c ce pre d =
    Map.find_exn
      (Instantiation.create
         ()
         ~name:"FDPE"
         ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "1") ]
         ~inputs:[ "C", c; "CE", ce; "D", d; "PRE", pre ]
         ~outputs:[ "Q", 1 ])
      "Q"
  ;;

  let ram1s a d clk we =
    let width = width a in
    let size = 1 lsl width in
    let a = List.mapi (bits_lsb a) ~f:(fun i s -> "A" ^ Int.to_string i, s) in
    Map.find_exn
      (Instantiation.create
         ()
         ~name:("RAM" ^ Int.to_string size ^ "X1S")
         ~parameters:
           [ Parameter.create
               ~name:"INIT"
               ~value:
                 (String (Constant.of_int ~width:size 0 |> Constant.to_binary_string))
           ]
         ~inputs:([ "D", d; "WE", we; "WCLK", clk ] @ a)
         ~outputs:[ "Q", 1 ])
      "Q"
  ;;
end

module type Xilinx_primitives = Xilinx_primitives with type lut_equation := Lut_equation.t

(* max lut size. *)
module type Lut_size = sig
  val max_lut : int
end

module Lut4 = struct
  let max_lut = 4
end

module Lut6 = struct
  let max_lut = 6
end

(* Build API of Xilinx primitives *)
module Make_xilinx_primitives (Comb : Combinational_primitives) (Lut_size : Lut_size) =
struct
  include Comb
  open Lut_size
  open Comb

  let x_lut f s =
    let i = Lut_equation.eval (width s) f in
    lut i s
  ;;

  let x_map f l =
    let w = List.hd_exn l |> width in
    let lut n = x_lut f (List.map l ~f:(fun s -> s.:[n, n]) |> concat_lsb) in
    let rec build n = if n = w then [] else lut n :: build (n + 1) in
    concat_lsb (build 0)
  ;;

  let x_and a b = x_map Lut_equation.(i0 &: i1) [ a; b ]
  let x_or a b = x_map Lut_equation.(i0 |: i1) [ a; b ]
  let x_xor a b = x_map Lut_equation.(i0 ^: i1) [ a; b ]
  let x_not a = bits_msb a |> List.map ~f:inv |> concat_msb
  let inputs n = List.take Lut_equation.[ i0; i1; i2; i3; i4; i5 ] n

  let reduce_inputs op n =
    let args = inputs n in
    List.reduce_exn args ~f:(fun acc a -> op acc a)
  ;;

  let rec x_reduce_carry inv op mux_din carry_in a =
    let n = min max_lut (width a) in
    let op' = reduce_inputs op n in
    let op' = if inv then Lut_equation.( ~: ) op' else op' in
    let lut = x_lut op' a.:[n - 1, 0] in
    let carry_out = muxcy carry_in mux_din lut in
    if n = width a
    then carry_out
    else x_reduce_carry inv op mux_din carry_out a.:[width a - 1, n]
  ;;

  let x_and_reduce a = x_reduce_carry false Lut_equation.( &: ) gnd vdd a
  let x_or_reduce a = x_reduce_carry true Lut_equation.( |: ) vdd gnd a

  let rec x_reduce_tree op a =
    let rec level a =
      let n = min max_lut (width a) in
      let op' = reduce_inputs op n in
      let lut = x_lut op' a.:[n - 1, 0] in
      if n = width a then lut else level a.:[width a - 1, n] @: lut
    in
    if width a = 1 then a else x_reduce_tree op (level a)
  ;;

  let x_add_carry op c a b =
    let lut c a b =
      let o = x_lut op (a @: b) in
      let s = xorcy c o in
      let c = muxcy c b o in
      c, s
    in
    let r, c =
      List.fold2_exn (bits_lsb a) (bits_lsb b) ~init:([], c) ~f:(fun (r, c) a b ->
        let c, s = lut c a b in
        s :: r, c)
    in
    c, concat_msb r
  ;;

  let x_add a b = snd (x_add_carry Lut_equation.(i0 ^: i1) gnd a b)
  let x_sub a b = snd (x_add_carry Lut_equation.(~:(i0 ^: i1)) vdd b a)

  let x_mux_add_carry op c x (a, a') b =
    let lut op x c (a, a') b =
      let o = x_lut op (x @: b @: a' @: a) in
      let s = xorcy c o in
      let c = muxcy c b o in
      c, s
    in
    let zip a b = List.map2_exn a b ~f:(fun a b -> a, b) in
    let r, c =
      List.fold2_exn
        (zip (bits_lsb a) (bits_lsb a'))
        (bits_lsb b)
        ~init:([], c)
        ~f:(fun (r, c) (a, a') b ->
          let c, s = lut op x c (a, a') b in
          s :: r, c)
    in
    c, concat_msb r
  ;;

  let x_mux_add x (a, a') b =
    let add_lut_op = Lut_equation.((i0 &: i3 |: (i1 &: ~:i3)) ^: i2) in
    snd (x_mux_add_carry add_lut_op gnd x (a, a') b)
  ;;

  let x_mux_sub x a (b, b') =
    let sub_lut_op = Lut_equation.(~:((i0 &: i3 |: (i1 &: ~:i3)) ^: i2)) in
    snd (x_mux_add_carry sub_lut_op vdd x (b, b') a)
  ;;

  let x_eq a b =
    let open Lut_equation in
    let rec eq l =
      match l with
      | [] -> []
      | a :: b :: t -> ~:(a ^: b) :: eq t
      | _ -> failwith "x_eq expecting even length list"
    in
    let eq l = List.fold (eq l) ~init:vdd ~f:( &: ) in
    let eq_lut a b =
      match width a with
      | 1 -> x_lut (eq [ i0; i1 ]) (b @: a)
      | 2 -> x_lut (eq [ i0; i2; i1; i3 ]) (b @: a)
      | 3 -> x_lut (eq [ i0; i3; i1; i4; i2; i5 ]) (b @: a)
      | _ -> failwith "x_eq invalid signal width"
    in
    let size = max_lut / 2 in
    let rec mk a b =
      assert (width a = width b);
      if width a <= size
      then [ eq_lut a b ]
      else
        eq_lut a.:[size - 1, 0] b.:[size - 1, 0]
        :: mk a.:[width a - 1, size] b.:[width b - 1, size]
    in
    let c = mk a b in
    List.fold c ~init:Comb.vdd ~f:(fun cin c -> muxcy cin Comb.gnd c)
  ;;

  let x_lt a b = fst (x_add_carry Lut_equation.(~:(i0 ^: i1)) gnd a b)

  (* muxes - Lut_equation6 version doesnt work... *)

  (* basic lut4/6 structures *)
  let x_lut4_mux2 sel d0 d1 =
    x_lut Lut_equation.(~:i0 &: i1 |: (i0 &: i2)) (d1 @: d0 @: sel)
  ;;

  let x_lut6_mux4 sel d0 d1 d2 d3 =
    x_lut
      Lut_equation.(
        ~:i1 &: ~:i0 &: i2 |: (~:i1 &: i0 &: i3) |: (i1 &: ~:i0 &: i4) |: (i1 &: i0 &: i5))
      (d3 @: d2 @: d1 @: d0 @: sel)
  ;;

  let split n d =
    let rec f m d l =
      if n = m
      then List.rev l, d
      else (
        match d with
        | [] -> List.rev l, []
        | h :: t -> f (m + 1) t (h :: l))
    in
    f 0 d []
  ;;

  let x_mux_2 s d def =
    match d with
    | [] -> def
    | [ d ] -> x_lut4_mux2 s d def
    | [ d0; d1 ] -> x_lut4_mux2 s d0 d1
    | _ -> failwith "x_mux2"
  ;;

  let x_mux_4 s d def =
    match d with
    | [] -> def
    | [ d ] -> x_lut4_mux2 s d def
    | [ d0; d1 ] -> x_lut4_mux2 s d0 d1
    | [ d0; d1; d2 ] -> x_lut6_mux4 s d0 d1 d2 def
    | [ d0; d1; d2; d3 ] -> x_lut6_mux4 s d0 d1 d2 d3
    | _ -> failwith "x_mux4"
  ;;

  let rec x_mux_n n mf s d def =
    if n <= 4 && max_lut >= 6
    then x_mux_4 s d def
    else if n <= 2
    then x_mux_2 s d def
    else (
      let a, b = split (n / 2) d in
      (List.hd_exn mf)
        (msb s)
        (x_mux_n (n / 2) (List.tl_exn mf) (lsbs s) a def)
        (x_mux_n (n / 2) (List.tl_exn mf) (lsbs s) b def))
  ;;

  let muxfn n =
    match n with
    | 5 -> assert false
    | 4 -> [ muxf8; muxf7; muxf6; muxf5 ]
    | 3 -> [ muxf7; muxf6; muxf5 ]
    | 2 -> [ muxf6; muxf5 ]
    | 1 -> [ muxf5 ]
    | _ -> []
  ;;

  (* This assumes that all arch's have muxf5/6/7/8, but they dont.  V5 seems to only have
     muxf7/8 ??? *)
  let x_mux_bit s d =
    let l_max, l_off = if max_lut >= 6 then 6, 2 else 5, 1 in
    let def = List.hd_exn (List.rev d) in
    let rec build s d =
      let l = width s in
      let l = min l_max l in
      let n = 1 lsl l in
      let muxfn = muxfn (l - l_off) in
      let rec build2 s d =
        match d with
        | [] -> []
        | _ ->
          let a, b = split (1 lsl l) d in
          x_mux_n n muxfn s a def :: build2 s b
      in
      let d = build2 s.:[l - 1, 0] d in
      if l = width s then List.hd_exn d else build s.:[width s - 1, l] d
    in
    build s d
  ;;

  let x_mux s d =
    let w = width (List.hd_exn d) in
    let rec mux_bits i =
      if i = w
      then []
      else (
        let d = List.map d ~f:(fun s -> s.:(i)) in
        x_mux_bit s d :: mux_bits (i + 1))
    in
    mux_bits 0 |> List.rev |> concat_msb
  ;;

  let x_mul_lut a0 a1 b0 b1 carry =
    let o = x_lut Lut_equation.((i0 &: i1) ^: (i2 &: i3)) (b0 @: a1 @: b1 @: a0) in
    let a = mult_and a0 b1 in
    let c = muxcy carry a o in
    let s = xorcy carry o in
    c, s
  ;;

  let x_mul_2 ~ex a b =
    let a1 = concat_msb [ ex a; ex a; a ] |> bits_lsb in
    let a0 = concat_msb [ ex a; a; gnd ] |> bits_lsb in
    let rec build a0 a1 b0 b1 c =
      match a0, a1 with
      | [], [] -> []
      | [ a0 ], [ a1 ] -> [ snd (x_mul_lut a0 a1 b0 b1 c) ]
      | a0 :: a0t, a1 :: a1t ->
        let c, s = x_mul_lut a0 a1 b0 b1 c in
        s :: build a0t a1t b0 b1 c
      | _ -> failwith "x_mul_2"
    in
    build a0 a1 b.:(0) b.:(1) gnd |> concat_lsb
  ;;

  let x_mul_1 ~ex a b =
    let a = concat_msb [ ex a; ex a; a ] in
    x_and a (repeat b ~count:(width a))
  ;;

  let rec build_products ~ex i a b =
    match width b with
    | 1 -> [ i, x_mul_1 ~ex a b ]
    | 2 -> [ i, x_mul_2 ~ex a b.:[1, 0] ]
    | _ -> (i, x_mul_2 ~ex a b.:[1, 0]) :: build_products ~ex (i + 2) a (msbs (msbs b))
  ;;

  (* multiplier *)
  let x_mul sign a b =
    let out_width = width a + width b in
    let ex a = if sign then msb a else gnd in
    let add_with_carry a b =
      let width = 1 + max (width a) (width b) in
      let a, b =
        if sign
        then sresize a ~width, sresize b ~width
        else uresize a ~width, uresize b ~width
      in
      let _, sum = x_add_carry Lut_equation.(i0 ^: i1) gnd a b in
      sum
    in
    let adder_tree products =
      let ( +: ) (i, a) (j, b) =
        let min = min i j in
        let pad_right a i =
          let i = i - min in
          if i = 0 then a else a @: zero i
        in
        min, add_with_carry (pad_right a i) (pad_right b j)
      in
      Bits.tree ~arity:2 ~f:(Bits.reduce ~f:( +: )) products
    in
    (snd (adder_tree (build_products ~ex 0 a b))).:[out_width - 1, 0]
  ;;

  let x_mulu a b = x_mul false a b

  let x_muls a b =
    (* note; use x_mux_sub below instead *)
    match width b with
    | 0 -> failwith "x_muls 'b' is empty"
    | 1 ->
      let z = zero (width a + width b) in
      x_mux b [ z; x_sub z (msb a @: a) ]
    | _ ->
      let m = x_mul true a (lsbs b) in
      x_sub
        (msb m @: m)
        (x_mux (msb b) [ zero (width a + width b); msb a @: a @: zero (width b - 1) ])
  ;;
end

(* Generate full Comb.S API for Xilinx primitives *)
module Make_comb_primitives (Synth : Xilinx_primitives) = struct
  include Synth

  let ( &: ) = Synth.x_and
  let ( |: ) = Synth.x_or
  let ( ^: ) = Synth.x_xor
  let ( ~: ) = Synth.x_not
  let ( +: ) = Synth.x_add
  let ( -: ) = Synth.x_sub
  let ( ==: ) = Synth.x_eq
  let ( <: ) = Synth.x_lt
  let ( *: ) = Synth.x_mulu
  let ( *+ ) = Synth.x_muls
  let mux = Synth.x_mux
end

module Make_sequential (Synth : Xilinx_primitives with type t = Signal.t) = struct
  module Comb_primitives = Make_comb_primitives (Synth)
  module Comb = Comb.Make (Comb_primitives)
  open Comb

  let reg (reg_spec : Signal.Type.register) ~enable d =
    let reset_value i =
      if is_empty reg_spec.reset_to then false else is_vdd reg_spec.reset_to.:[i, i]
    in
    let reset =
      if is_empty reg_spec.spec.reset
      then gnd
      else (
        match reg_spec.spec.reset_edge with
        | Falling -> ~:(reg_spec.spec.reset)
        | Rising -> reg_spec.spec.reset)
    in
    let clear = if is_empty reg_spec.spec.clear then gnd else reg_spec.spec.clear in
    let clock =
      match reg_spec.spec.clock_edge with
      | Falling -> ~:(reg_spec.spec.clock)
      | Rising -> reg_spec.spec.clock
    in
    let enable = if is_empty enable then vdd else enable in
    let enable, d =
      if is_empty reg_spec.spec.clear
      then enable, d
      else if is_empty reg_spec.clear_to
      then enable |: clear, mux2 clear (zero (width d)) d
      else enable |: clear, mux2 clear reg_spec.clear_to d
    in
    List.mapi (bits_lsb d) ~f:(fun i d ->
      if reset_value i
      then Unisim_sequential_primitives.fdpe clock enable reset d
      else Unisim_sequential_primitives.fdce clock enable reset d)
    |> concat_lsb
  ;;
end
