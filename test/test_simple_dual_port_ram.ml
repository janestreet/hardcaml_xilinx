open! Import
open Signal
open Hardcaml_waveterm

let addr_bits = 8
let data_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; write_enable : 'a
    ; write_address : 'a [@bits addr_bits]
    ; write_data : 'a [@bits data_bits]
    ; read_enable : 'a
    ; read_address : 'a [@bits addr_bits]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { q_1 : 'a [@bits data_bits]
    ; q_2 : 'a [@bits data_bits]
    }
  [@@deriving hardcaml]
end

(* Create a pair of simple dual port ram's with 1 and 2 cycles read latency. *)
let create
  ?arch
  ?protection
  ({ clock; write_enable; write_address; write_data; read_enable; read_address } : _ I.t)
  =
  let create read_latency =
    Simple_dual_port_ram.create
      ~address_collision_model:(Const 666)
      ?address_collision_protection:protection
      ~build_mode:Simulation
      ~read_latency
      ?arch
      ()
      ~clock
      ~clear:gnd
      ~size:(1 lsl addr_bits)
      ~write_address
      ~write_enable
      ~data:write_data
      ~read_address
      ~read_enable
  in
  { O.q_1 = create 1; q_2 = create 2 }
;;

module Sim = Cyclesim.With_interface (I) (O)

let ( <-- ) a b = a := Bits.of_int_trunc ~width:(Bits.width !a) b

let test (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  (* write, then read 2 elements in the ram *)
  inputs.write_enable := Bits.vdd;
  for i = 0 to 2 do
    inputs.write_address <-- i;
    inputs.write_data <-- i;
    Cyclesim.cycle sim
  done;
  inputs.write_enable := Bits.gnd;
  inputs.read_enable := Bits.vdd;
  for i = 0 to 2 do
    inputs.read_address <-- i;
    Cyclesim.cycle sim
  done;
  inputs.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  (* address collision *)
  inputs.write_address <-- 1;
  inputs.read_address <-- 1;
  inputs.write_enable := Bits.vdd;
  inputs.write_data <-- 100;
  inputs.read_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.write_enable := Bits.gnd;
  inputs.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim
;;

(* In the following tests, we are interested in the very last output on [q] where we force
   an address collsion.  We use [666] to display a collision. *)

let print waves =
  Waveform.print
    ~wave_width:2
    ~display_width:86
    ~display_rules:
      (List.concat
         [ (let module I = Display_rules.With_interface (I) in
           I.default ~wave_format:(Bit_or Unsigned_int) ())
         ; (let module O = Display_rules.With_interface (O) in
           O.default ~wave_format:(Bit_or Unsigned_int) ())
         ])
    waves
;;

let print_q waves =
  Waveform.print
    ~wave_width:2
    ~display_width:86
    ~display_rules:
      [ Display_rule.port_name_is "q_1" ~wave_format:Unsigned_int
      ; Display_rule.port_name_is "q_2" ~wave_format:Unsigned_int
      ]
    waves
;;

(* Show the full test - write and read a few elements, then force an address collision. *)
let%expect_test "collision mode modelling" =
  let sim = Sim.create create in
  let waves, sim = Waveform.create sim in
  test sim;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │write_enable      ││──────────────────┐                       ┌─────┐               │
    │                  ││                  └───────────────────────┘     └───────────────│
    │                  ││──────┬─────┬─────────────────────────────┬─────────────────────│
    │write_address     ││ 0    │1    │2                            │1                    │
    │                  ││──────┴─────┴─────────────────────────────┴─────────────────────│
    │                  ││──────┬─────┬─────────────────────────────┬─────────────────────│
    │write_data        ││ 0    │1    │2                            │100                  │
    │                  ││──────┴─────┴─────────────────────────────┴─────────────────────│
    │read_enable       ││                  ┌─────────────────┐     ┌─────┐               │
    │                  ││──────────────────┘                 └─────┘     └───────────────│
    │                  ││────────────────────────┬─────┬───────────┬─────────────────────│
    │read_address      ││ 0                      │1    │2          │1                    │
    │                  ││────────────────────────┴─────┴───────────┴─────────────────────│
    │                  ││──────────────────────────────┬─────┬───────────┬─────┬─────────│
    │q_1               ││ 0                            │1    │2          │666  │1        │
    │                  ││──────────────────────────────┴─────┴───────────┴─────┴─────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────┬───│
    │q_2               ││ 0                                  │1    │2          │666  │1  │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

(* Show collisions when where they can occur *)

let test_q ?arch ?protection () =
  let sim = Sim.create (create ?arch ?protection) in
  let waves, sim = Waveform.create sim in
  test sim;
  print_q waves
;;

let%expect_test "Blockram No_change - show collision" =
  test_q ~arch:(Blockram No_change) ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬─────┬─────────│
    │q_1               ││ 0                            │1    │2          │666  │1        │
    │                  ││──────────────────────────────┴─────┴───────────┴─────┴─────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────┬───│
    │q_2               ││ 0                                  │1    │2          │666  │1  │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Blockram Write_before_read - show collision" =
  test_q ~arch:(Blockram Write_before_read) ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬─────┬─────────│
    │q_1               ││ 0                            │1    │2          │666  │100      │
    │                  ││──────────────────────────────┴─────┴───────────┴─────┴─────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────┬───│
    │q_2               ││ 0                                  │1    │2          │666  │100│
    │                  ││────────────────────────────────────┴─────┴───────────┴─────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Other ram types - no collision" =
  test_q ~arch:(Blockram Read_before_write) ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬───────────────│
    │q_1               ││ 0                            │1    │2          │1              │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────────│
    │q_2               ││ 0                                  │1    │2          │1        │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}];
  test_q ~arch:Ultraram ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬───────────────│
    │q_1               ││ 0                            │1    │2          │100            │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────────│
    │q_2               ││ 0                                  │1    │2          │100      │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}];
  test_q ~arch:Distributed ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬───────────────│
    │q_1               ││ 0                            │1    │2          │1              │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────────│
    │q_2               ││ 0                                  │1    │2          │1        │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

(* Mitigations *)

let%expect_test "Blockram Write_before_read - Control_enabless" =
  test_q ~arch:(Blockram Write_before_read) ~protection:Control_enables ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬───────────────│
    │q_1               ││ 0                            │1    │2          │100            │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────────│
    │q_2               ││ 0                                  │1    │2          │100      │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Blockram Write_before_read - Mux_output_ports" =
  test_q ~arch:(Blockram Write_before_read) ~protection:Mux_output_ports ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────┬───────────────│
    │q_1               ││ 0                            │1    │2          │100            │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────────│
    │                  ││────────────────────────────────────┬─────┬───────────┬─────────│
    │q_2               ││ 0                                  │1    │2          │100      │
    │                  ││────────────────────────────────────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Blockram No_change - Control_enabless" =
  test_q ~arch:(Blockram No_change) ~protection:Control_enables ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────────────────────│
    │q_1               ││ 0                            │1    │2                          │
    │                  ││──────────────────────────────┴─────┴───────────────────────────│
    │                  ││────────────────────────────────────┬─────┬─────────────────────│
    │q_2               ││ 0                                  │1    │2                    │
    │                  ││────────────────────────────────────┴─────┴─────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Blockram No_change - Mux_output_ports" =
  test_q ~arch:(Blockram No_change) ~protection:Mux_output_ports ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────┬─────┬───────────────────────────│
    │q_1               ││ 0                            │1    │2                          │
    │                  ││──────────────────────────────┴─────┴───────────────────────────│
    │                  ││────────────────────────────────────┬─────┬─────────────────────│
    │q_2               ││ 0                                  │1    │2                    │
    │                  ││────────────────────────────────────┴─────┴─────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;
