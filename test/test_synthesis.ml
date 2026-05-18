open! Import
open Synthesis
open Base_quickcheck
open Quickcheck_bits

let eval6 x = Lut_equation.eval 6 x |> Hardcaml.Bits.of_int64_trunc ~width:64

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
    (* Mulitplication is a large circuit and takes time to compute. Limit the test length. *)
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

module%test Test_LUT4 = Test_xilinx_primitives (Lut4)
module%test Test_LUT6 = Test_xilinx_primitives (Lut6)

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

        wire signal_select;
        wire signal_select_1;
        wire [1:0] signal_cat;
        reg signal_mux;
        wire vdd;
        wire gnd;
        wire signal_select_2;
        wire signal_select_3;
        wire [1:0] signal_cat_1;
        reg signal_mux_1;
        wire [1:0] signal_cat_2;
        assign signal_select = i1[0:0];
        assign signal_select_1 = i2[0:0];
        assign signal_cat = { signal_select_1,
                              signal_select };
        always @* begin
            case (signal_cat)
            0:
                signal_mux <= gnd;
            1:
                signal_mux <= vdd;
            2:
                signal_mux <= vdd;
            default:
                signal_mux <= gnd;
            endcase
        end
        assign vdd = 1'b1;
        assign gnd = 1'b0;
        assign signal_select_2 = i1[1:1];
        assign signal_select_3 = i2[1:1];
        assign signal_cat_1 = { signal_select_3,
                                signal_select_2 };
        always @* begin
            case (signal_cat_1)
            0:
                signal_mux_1 <= gnd;
            1:
                signal_mux_1 <= vdd;
            2:
                signal_mux_1 <= vdd;
            default:
                signal_mux_1 <= gnd;
            endcase
        end
        assign signal_cat_2 = { signal_mux_1,
                                signal_mux };
        assign o = signal_cat_2;

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

        wire signal_select;
        wire signal_select_1;
        wire signal_select_2;
        wire [1:0] signal_cat;
        wire signal_select_3;
        wire signal_inst;
        wire signal_wire;
        wire signal_select_4;
        wire signal_select_5;
        wire signal_select_6;
        wire [1:0] signal_cat_1;
        wire signal_select_7;
        wire signal_inst_1;
        wire signal_wire_1;
        wire [1:0] signal_cat_2;
        assign signal_select = signal_cat[1:1];
        assign signal_select_1 = i1[0:0];
        assign signal_select_2 = i2[0:0];
        assign signal_cat = { signal_select_2,
                              signal_select_1 };
        assign signal_select_3 = signal_cat[0:0];
        LUT2
            #( .INIT("0110") )
            the_LUT2
            ( .I0(signal_select_3),
              .I1(signal_select),
              .O(signal_inst) );
        assign signal_wire = signal_inst;
        assign signal_select_4 = signal_cat_1[1:1];
        assign signal_select_5 = i1[1:1];
        assign signal_select_6 = i2[1:1];
        assign signal_cat_1 = { signal_select_6,
                                signal_select_5 };
        assign signal_select_7 = signal_cat_1[0:0];
        LUT2
            #( .INIT("0110") )
            the_LUT2_1
            ( .I0(signal_select_7),
              .I1(signal_select_4),
              .O(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        assign signal_cat_2 = { signal_wire_1,
                                signal_wire };
        assign o = signal_cat_2;

    endmodule
    |}]
;;
