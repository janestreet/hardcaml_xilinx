(** Tests for the Hardcaml model of the Xilinx XPM 'tdpram' generator, and in particular
    the output pipe and modes used with Ultraram. *)

open! Import
open Hardcaml_waveterm
module Port = Ram_port

let create_circuit
      ?(create = Dual_port_ram.create)
      ?(byte_write_width = Byte_write_width.Full)
      ()
      ~read_latency
      ~address_bits
      ~data_bits
  =
  let port_sizes =
    let write_enable =
      match byte_write_width with
      | Full -> 1
      | B8 -> data_bits / 8
      | B9 -> data_bits / 9
    in
    { Port.address = address_bits; data = data_bits; read_enable = 1; write_enable }
  in
  let qa, qb =
    create
      ~read_latency
      ~arch:Ultraram
      ~build_mode:Simulation
      ()
      ~clock:(Signal.input "clock" 1)
      ~clear:(Signal.input "clear" 1)
      ~size:(1 lsl address_bits)
      ~byte_write_width
      ~port_a:Port.(map2 (map port_names ~f:(( ^ ) "a_")) port_sizes ~f:Signal.input)
      ~port_b:Port.(map2 (map port_names ~f:(( ^ ) "b_")) port_sizes ~f:Signal.input)
  in
  Circuit.create_exn ~name:"ram" [ Signal.output "qa" qa; Signal.output "qb" qb ]
;;

let create_sim ?byte_write_width ~read_latency ~address_bits ~data_bits () =
  let circuit =
    create_circuit ?byte_write_width () ~read_latency ~address_bits ~data_bits
  in
  let sim = Cyclesim.create circuit in
  let wave, sim = Waveform.create sim in
  let clear =
    try Cyclesim.in_port sim "clear" with
    | _ -> ref Bits.empty
  in
  let port_a =
    Port.(map port_names ~f:(fun name -> Cyclesim.in_port sim ("a_" ^ name)))
  in
  let port_b =
    Port.(map port_names ~f:(fun name -> Cyclesim.in_port sim ("b_" ^ name)))
  in
  let qa = Cyclesim.out_port sim "qa" in
  let qb = Cyclesim.out_port sim "qb" in
  (wave, sim), clear, (port_a, qa), (port_b, qb)
;;

let%expect_test "basic write then read, single cycle latency" =
  let (waves, sim), clear, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim ~read_latency:1 ~address_bits:8 ~data_bits:32 ()
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  a.write_enable := Bits.vdd;
  a.address := Bits.of_int ~width:8 10;
  a.data := Bits.of_int ~width:32 100;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:8 10;
  Cyclesim.cycle sim;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~display_height:32 ~display_width:88 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │                  ││────────┬───────────────────────────────────────────────────────  │
    │a_address         ││ 00     │0A                                                       │
    │                  ││────────┴───────────────────────────────────────────────────────  │
    │                  ││────────┬───────────────────────────────────────────────────────  │
    │a_data            ││ 000000.│00000064                                                 │
    │                  ││────────┴───────────────────────────────────────────────────────  │
    │a_read_enable     ││                                                                  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │a_write_enable    ││        ┌───────┐                                                 │
    │                  ││────────┘       └───────────────────────────────────────────────  │
    │                  ││────────────────────────────────┬───────────────────────────────  │
    │b_address         ││ 00                             │0A                               │
    │                  ││────────────────────────────────┴───────────────────────────────  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │b_data            ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────────  │
    │b_read_enable     ││                                ┌───────┐                         │
    │                  ││────────────────────────────────┘       └───────────────────────  │
    │b_write_enable    ││                                                                  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │qa                ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────────  │
    │                  ││────────────────────────────────────────┬───────────────────────  │
    │qb                ││ 00000000                               │00000064                 │
    │                  ││────────────────────────────────────────┴───────────────────────  │
    │                  ││                                                                  │
    │                  ││                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    77d862a70af8690f65b233f5989e8942 |}]
;;

let%expect_test "basic write then read, 2 cycle latency" =
  let (waves, sim), clear, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim ~read_latency:2 ~address_bits:8 ~data_bits:32 ()
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  a.write_enable := Bits.vdd;
  a.address := Bits.of_int ~width:8 10;
  a.data := Bits.of_int ~width:32 100;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:8 10;
  Cyclesim.cycle sim;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~display_height:32 ~display_width:88 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clear             ││────────┐                                                         │
    │                  ││        └───────────────────────────────────────────────────────  │
    │                  ││────────┬───────────────────────────────────────────────────────  │
    │a_address         ││ 00     │0A                                                       │
    │                  ││────────┴───────────────────────────────────────────────────────  │
    │                  ││────────┬───────────────────────────────────────────────────────  │
    │a_data            ││ 000000.│00000064                                                 │
    │                  ││────────┴───────────────────────────────────────────────────────  │
    │a_read_enable     ││                                                                  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │a_write_enable    ││        ┌───────┐                                                 │
    │                  ││────────┘       └───────────────────────────────────────────────  │
    │                  ││────────────────────────────────┬───────────────────────────────  │
    │b_address         ││ 00                             │0A                               │
    │                  ││────────────────────────────────┴───────────────────────────────  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │b_data            ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────────  │
    │b_read_enable     ││                                ┌───────┐                         │
    │                  ││────────────────────────────────┘       └───────────────────────  │
    │b_write_enable    ││                                                                  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │                  ││────────────────────────────────────────────────────────────────  │
    │qa                ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────────  │
    │                  ││────────────────────────────────────────────────┬───────────────  │
    │qb                ││ 00000000                                       │00000064         │
    │                  ││────────────────────────────────────────────────┴───────────────  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    3bb5d8790813caa2b503ac550ebb97e2 |}]
;;

let%expect_test "write and read same cycle, 2 cycle latency" =
  let (waves, sim), clear, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim ~read_latency:2 ~address_bits:8 ~data_bits:32 ()
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  a.write_enable := Bits.vdd;
  a.read_enable := Bits.vdd;
  (* reads dont occur because we model "no change" mode. *)
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:8 10;
  a.address := Bits.of_int ~width:8 10;
  a.data := Bits.of_int ~width:32 100;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  a.read_enable := Bits.gnd;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~display_height:32 ~display_width:88 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clear             ││────────┐                                                         │
    │                  ││        └───────────────────────────────────────                  │
    │                  ││────────┬───────────────────────────────────────                  │
    │a_address         ││ 00     │0A                                                       │
    │                  ││────────┴───────────────────────────────────────                  │
    │                  ││────────┬───────────────────────────────────────                  │
    │a_data            ││ 000000.│00000064                                                 │
    │                  ││────────┴───────────────────────────────────────                  │
    │a_read_enable     ││        ┌───────────────┐                                         │
    │                  ││────────┘               └───────────────────────                  │
    │a_write_enable    ││        ┌───────┐                                                 │
    │                  ││────────┘       └───────────────────────────────                  │
    │                  ││────────┬───────────────────────────────────────                  │
    │b_address         ││ 00     │0A                                                       │
    │                  ││────────┴───────────────────────────────────────                  │
    │                  ││────────────────────────────────────────────────                  │
    │b_data            ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────                  │
    │b_read_enable     ││        ┌───────────────┐                                         │
    │                  ││────────┘               └───────────────────────                  │
    │b_write_enable    ││                                                                  │
    │                  ││────────────────────────────────────────────────                  │
    │                  ││────────────────────────────────┬───────────────                  │
    │qa                ││ 00000000                       │00000064                         │
    │                  ││────────────────────────────────┴───────────────                  │
    │                  ││────────────────────────┬───────────────────────                  │
    │qb                ││ 00000000               │00000064                                 │
    │                  ││────────────────────────┴───────────────────────                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    d52c5114d280caacb2bfd5637fc514ba |}]
;;

let%expect_test "byte write width" =
  let (waves, sim), clear, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim
      ~byte_write_width:Byte_write_width.B8
      ~read_latency:1
      ~address_bits:8
      ~data_bits:32
      ()
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  a.write_enable := Bits.of_string "1001";
  a.address := Bits.of_int ~width:8 10;
  a.data := Bits.of_int ~width:32 0xAABB_CCDD;
  Cyclesim.cycle sim;
  a.write_enable := Bits.of_string "0110";
  a.address := Bits.of_int ~width:8 10;
  a.data := Bits.of_int ~width:32 0xFF11_22EE;
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:8 10;
  Cyclesim.cycle sim;
  a.write_enable := Bits.of_string "0000";
  Cyclesim.cycle sim;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~wave_width:4 ~display_height:32 ~display_width:88 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐│
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └│
    │                  ││──────────┬─────────────────────────────────────────────────      │
    │a_address         ││ 00       │0A                                                     │
    │                  ││──────────┴─────────────────────────────────────────────────      │
    │                  ││──────────┬─────────┬───────────────────────────────────────      │
    │a_data            ││ 00000000 │AABBCCDD │FF1122EE                                     │
    │                  ││──────────┴─────────┴───────────────────────────────────────      │
    │a_read_enable     ││                                                                  │
    │                  ││────────────────────────────────────────────────────────────      │
    │                  ││──────────┬─────────┬─────────┬─────────────────────────────      │
    │a_write_enable    ││ 0        │9        │6        │0                                  │
    │                  ││──────────┴─────────┴─────────┴─────────────────────────────      │
    │                  ││────────────────────┬───────────────────────────────────────      │
    │b_address         ││ 00                 │0A                                           │
    │                  ││────────────────────┴───────────────────────────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_data            ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_read_enable     ││                    ┌───────────────────┐                         │
    │                  ││────────────────────┘                   └───────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_write_enable    ││ 0                                                                │
    │                  ││────────────────────────────────────────────────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │qa                ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────      │
    │                  ││──────────────────────────────┬─────────────────────────────      │
    │qb                ││ 00000000                     │AA1122DD                           │
    │                  ││──────────────────────────────┴─────────────────────────────      │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    11a5518d67715e5bc3e1f177834e8d87 |}]
;;
