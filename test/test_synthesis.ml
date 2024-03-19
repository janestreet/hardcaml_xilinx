open! Import
open Synthesis
open Base_quickcheck
open Quickcheck_bits

let eval6 x = Lut_equation.eval 6 x |> Hardcaml.Bits.of_int64 ~width:64

let%expect_test "lut eqns" =
  let eqn = Lut_equation.(i0 &: i1 &: i2 &: i3 &: i4 &: i5 |> eval6) in
  print_s [%message (eqn : Bits.t)];
  [%expect {| (eqn 1000000000000000000000000000000000000000000000000000000000000000) |}];
  let eqn = Lut_equation.(i0 |: i1 |: i2 |: i3 |: i4 |: i5 |> eval6) in
  print_s [%message (eqn : Bits.t)];
  [%expect {| (eqn 1111111111111111111111111111111111111111111111111111111111111110) |}];
  let eqn = Lut_equation.(i0 &: i1 &: i2 |> eval6) in
  print_s [%message (eqn : Bits.t)];
  [%expect {| (eqn 1000000010000000100000001000000010000000100000001000000010000000) |}];
  let eqn = Lut_equation.(i0 |: i1 |: i2 |: i3 |> eval6) in
  print_s [%message (eqn : Bits.t)];
  [%expect {| (eqn 1111111111111110111111111111111011111111111111101111111111111110) |}]
;;

module Test_xilinx_primitives (Lut_size : Lut_size) = struct
  module X = Make_xilinx_primitives (Hardcaml_combinational_primitives (Bits)) (Lut_size)

  let%expect_test "Xilinx primitives" =
    let x_add = X.x_add (Bits.of_string "1100") (Bits.of_string "0011") in
    print_s [%message (x_add : Bits.t)];
    [%expect {| (x_add 1111) |}]
  ;;

  let%expect_test "quickcheck" =
    let binop_same_arg_width ~f_bits ~f_xilinx =
      Test.run_exn
        (module struct
          type t = Bits_at_size.t * Bits_at_size.t [@@deriving quickcheck, sexp_of]
        end)
        ~f:(fun (d1, d2) ->
          let bits = f_bits d1 d2 in
          let xilinx = f_xilinx d1 d2 in
          if not (Bits.equal bits xilinx)
          then raise_s [%message (bits : Bits.t) (xilinx : Bits.t)])
    in
    binop_same_arg_width ~f_bits:Bits.( +: ) ~f_xilinx:X.x_add;
    binop_same_arg_width ~f_bits:Bits.( -: ) ~f_xilinx:X.x_sub;
    binop_same_arg_width ~f_bits:Bits.( &: ) ~f_xilinx:X.x_and;
    binop_same_arg_width ~f_bits:Bits.( |: ) ~f_xilinx:X.x_or;
    binop_same_arg_width ~f_bits:Bits.( ^: ) ~f_xilinx:X.x_xor;
    binop_same_arg_width ~f_bits:Bits.( ==: ) ~f_xilinx:X.x_eq;
    binop_same_arg_width ~f_bits:Bits.( <: ) ~f_xilinx:X.x_lt;
    let binop_different_arg_width ~num_tests ~f_bits ~f_xilinx =
      let config = { Test.default_config with test_count = num_tests } in
      Test.run_exn
        ~config
        (module struct
          type t = Bits_upto_size.t * Bits_upto_size.t [@@deriving quickcheck, sexp_of]
        end)
        ~f:(fun (d1, d2) ->
          let bits = f_bits d1 d2 in
          let xilinx = f_xilinx d1 d2 in
          if not (Bits.equal bits xilinx)
          then raise_s [%message (bits : Bits.t) (xilinx : Bits.t)])
    in
    (* Mulitplication is a large circuit and takes time to compute.  Limit the test length. *)
    let num_tests = 1000 in
    binop_different_arg_width ~num_tests ~f_bits:Bits.( *: ) ~f_xilinx:X.x_mulu;
    binop_different_arg_width ~num_tests ~f_bits:Bits.( *+ ) ~f_xilinx:X.x_muls
  ;;

  let%expect_test "x_mux_add/sub" =
    Test.run_exn
      (module struct
        module Bits1 = (val bits_with_size (module Bits_at_size) 1)

        type t = (Bits1.t * (Bits_at_size.t * Bits_at_size.t)) * Bits_at_size.t
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun ((sel, (a, a')), b) ->
        let bits = Bits.(mux2 sel a a' +: b) in
        let xilinx = X.x_mux_add sel (a, a') b in
        if not (Bits.equal bits xilinx)
        then raise_s [%message (bits : Bits.t) (xilinx : Bits.t)]);
    Test.run_exn
      (module struct
        module Bits1 = (val bits_with_size (module Bits_at_size) 1)

        type t = Bits_at_size.t * (Bits1.t * (Bits_at_size.t * Bits_at_size.t))
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, (sel, (b, b'))) ->
        let bits = Bits.(a -: mux2 sel b b') in
        let xilinx = X.x_mux_sub sel a (b, b') in
        if not (Bits.equal bits xilinx)
        then raise_s [%message (bits : Bits.t) (xilinx : Bits.t)])
  ;;
end

module _ = Test_xilinx_primitives (Lut4)
module _ = Test_xilinx_primitives (Lut6)

let%expect_test "verilog" =
  let open Signal in
  let module X =
    Make_xilinx_primitives (Hardcaml_combinational_primitives (Signal)) (Lut4)
  in
  [ output "o" (X.x_xor (input "i1" 2) (input "i2" 2)) ]
  |> Circuit.create_exn ~name:"x"
  |> Rtl.print Verilog;
  [%expect
    {|
    module x (
        i1,
        i2,
        o
    );

        input [1:0] i1;
        input [1:0] i2;
        output [1:0] o;

        wire _11;
        wire _10;
        wire [1:0] _12;
        reg _13;
        wire vdd;
        wire gnd;
        wire _5;
        wire _4;
        wire [1:0] _6;
        reg _9;
        wire [1:0] _14;
        assign _11 = i1[0:0];
        assign _10 = i2[0:0];
        assign _12 = { _10,
                       _11 };
        always @* begin
            case (_12)
            0:
                _13 <= gnd;
            1:
                _13 <= vdd;
            2:
                _13 <= vdd;
            default:
                _13 <= gnd;
            endcase
        end
        assign vdd = 1'b1;
        assign gnd = 1'b0;
        assign _5 = i1[1:1];
        assign _4 = i2[1:1];
        assign _6 = { _4,
                      _5 };
        always @* begin
            case (_6)
            0:
                _9 <= gnd;
            1:
                _9 <= vdd;
            2:
                _9 <= vdd;
            default:
                _9 <= gnd;
            endcase
        end
        assign _14 = { _9,
                       _13 };
        assign o = _14;

    endmodule
    |}];
  let module X = Make_xilinx_primitives (Unisim_combinational_primitives) (Lut4) in
  [ output "o" (X.x_xor (input "i1" 2) (input "i2" 2)) ]
  |> Circuit.create_exn ~name:"x"
  |> Rtl.print Verilog;
  [%expect
    {|
    module x (
        i1,
        i2,
        o
    );

        input [1:0] i1;
        input [1:0] i2;
        output [1:0] o;

        wire _10;
        wire _7;
        wire _6;
        wire [1:0] _8;
        wire _9;
        wire _12;
        wire _1;
        wire _17;
        wire _14;
        wire _13;
        wire [1:0] _15;
        wire _16;
        wire _19;
        wire _4;
        wire [1:0] _20;
        assign _10 = _8[1:1];
        assign _7 = i1[0:0];
        assign _6 = i2[0:0];
        assign _8 = { _6,
                      _7 };
        assign _9 = _8[0:0];
        LUT2
            #( .INIT("0110") )
            the_LUT2
            ( .I0(_9),
              .I1(_10),
              .O(_12) );
        assign _1 = _12;
        assign _17 = _15[1:1];
        assign _14 = i1[1:1];
        assign _13 = i2[1:1];
        assign _15 = { _13,
                       _14 };
        assign _16 = _15[0:0];
        LUT2
            #( .INIT("0110") )
            the_LUT2_0
            ( .I0(_16),
              .I1(_17),
              .O(_19) );
        assign _4 = _19;
        assign _20 = { _4,
                       _1 };
        assign o = _20;

    endmodule
    |}]
;;
