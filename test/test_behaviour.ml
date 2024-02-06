(** Tests for the Hardcaml model of the Xilinx XPM 'tdpram' generator, and in particular
    the output pipe and modes used with Ultraram. *)

open! Import
open Hardcaml_waveterm
module Port = Ram_port

let create_circuit
  ?(create = Dual_port_ram.create)
  ?(byte_write_width = Byte_write_width.Full)
  ?data_bits_b
  ()
  ~read_latency
  ~address_bits_a
  ~data_bits_a
  =
  let data_bits_b = Option.value data_bits_b ~default:data_bits_a in
  let address_bits_b =
    Int.ceil_log2 ((1 lsl address_bits_a) * data_bits_a / data_bits_b)
  in
  let port_sizes data_bits address_bits =
    let write_enable =
      match byte_write_width with
      | Full -> 1
      | B8 -> data_bits / 8
      | B9 -> data_bits / 9
    in
    { Port.address = address_bits; data = data_bits; read_enable = 1; write_enable }
  in
  let port_sizes_a = port_sizes data_bits_a address_bits_a in
  let port_sizes_b = port_sizes data_bits_b address_bits_b in
  let qa, qb =
    create
      ~memory_optimization:false
      ~cascade_height:Inferred
      ~read_latency
      ~arch:Ultraram
      ~build_mode:Simulation
      ()
      ~clock:(Signal.input "clock" 1)
      ~clear:(Signal.input "clear" 1)
      ~size:(1 lsl address_bits_a)
      ~byte_write_width
      ~port_a:Port.(map2 (map port_names ~f:(( ^ ) "a_")) port_sizes_a ~f:Signal.input)
      ~port_b:Port.(map2 (map port_names ~f:(( ^ ) "b_")) port_sizes_b ~f:Signal.input)
  in
  Circuit.create_exn ~name:"ram" [ Signal.output "qa" qa; Signal.output "qb" qb ]
;;

let create_sim ?byte_write_width ?data_bits_b ~read_latency ~address_bits ~data_bits () =
  let circuit =
    create_circuit
      ?byte_write_width
      ?data_bits_b
      ()
      ~read_latency
      ~address_bits_a:address_bits
      ~data_bits_a:data_bits
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
    c0d208f19d24171eb01e097dc9f75478 |}]
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
    cb14bf2c63e44399c712e0fb898cc6da |}]
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
    1c29616893d2ca469a8a5301f3e59de0 |}]
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
    e7d23bedd7ad173110c58e043f661a3c |}]
;;

type write =
  { write_enable : string
  ; write_address : int
  ; write_data : int
  }

let test_resizing ?(display_width = 88) ~write_bits ~read_bits ~writes ~reads () =
  let write_address_bits = 8 in
  let read_address_bits =
    write_address_bits + (Int.ceil_log2 write_bits - Int.ceil_log2 read_bits)
  in
  let (waves, sim), clear, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim
      ~byte_write_width:Byte_write_width.B8
      ~read_latency:1
      ~address_bits:write_address_bits
      ~data_bits:write_bits
      ~data_bits_b:read_bits
      ()
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  for i = 0 to Int.max (Array.length reads) (Array.length writes - 1) do
    (* write port *)
    if i < Array.length writes
    then (
      a.write_enable := Bits.of_string writes.(i).write_enable;
      a.address := Bits.of_int ~width:write_address_bits writes.(i).write_address;
      a.data := Bits.of_int ~width:write_bits writes.(i).write_data)
    else a.write_enable := Bits.zero (write_bits / 8);
    (* read port *)
    if i >= 1 && i <= Array.length reads
    then (
      b.read_enable := Bits.vdd;
      b.address := Bits.of_int ~width:read_address_bits reads.(i - 1))
    else b.read_enable := Bits.gnd;
    Cyclesim.cycle sim
  done;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~wave_width:4 ~display_height:34 ~display_width waves
;;

let%expect_test "resize ram with byte write enables; read/write = 1/4" =
  test_resizing
    ()
    ~write_bits:32
    ~read_bits:8
    ~writes:
      [| { write_enable = "1001"; write_address = 10; write_data = 0xAABB_CCDD }
       ; { write_enable = "0110"; write_address = 10; write_data = 0xFF11_22EE }
      |]
    ~reads:[| 40; 41; 42; 43 |];
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐│
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └│
    │clear             ││──────────┐                                                       │
    │                  ││          └───────────────────────────────────────────────────────│
    │                  ││──────────┬───────────────────────────────────────────────────────│
    │a_address         ││ 00       │0A                                                     │
    │                  ││──────────┴───────────────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────────────────────────────────────────│
    │a_data            ││ 00000000 │AABBCCDD │FF1122EE                                     │
    │                  ││──────────┴─────────┴─────────────────────────────────────────────│
    │a_read_enable     ││                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬───────────────────────────────────│
    │a_write_enable    ││ 0        │9        │6        │0                                  │
    │                  ││──────────┴─────────┴─────────┴───────────────────────────────────│
    │                  ││────────────────────┬─────────┬─────────┬─────────┬───────────────│
    │b_address         ││ 000                │028      │029      │02A      │02B            │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴───────────────│
    │                  ││──────────────────────────────────────────────────────────────────│
    │b_data            ││ 00                                                               │
    │                  ││──────────────────────────────────────────────────────────────────│
    │b_read_enable     ││                    ┌───────────────────────────────────────┐     │
    │                  ││────────────────────┘                                       └─────│
    │b_write_enable    ││                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────│
    │qa                ││ 00000000                                                         │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────┬─────────┬─────────┬─────────┬─────│
    │qb                ││ 00                           │DD       │22       │11       │AA   │
    │                  ││──────────────────────────────┴─────────┴─────────┴─────────┴─────│
    │                  ││                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    586051185cc93f2a909088645d0519b3 |}]
;;

let%expect_test "resize ram with byte write enables; read/write = 1/2" =
  test_resizing
    ()
    ~write_bits:32
    ~read_bits:16
    ~writes:
      [| { write_enable = "1001"; write_address = 10; write_data = 0xAABB_CCDD }
       ; { write_enable = "0110"; write_address = 10; write_data = 0xFF11_22EE }
      |]
    ~reads:[| 20; 21 |];
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐│
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └│
    │clear             ││──────────┐                                                       │
    │                  ││          └─────────────────────────────────────────────────      │
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
    │                  ││────────────────────┬─────────┬─────────────────────────────      │
    │b_address         ││ 000                │014      │015                                │
    │                  ││────────────────────┴─────────┴─────────────────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_data            ││ 0000                                                             │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_read_enable     ││                    ┌───────────────────┐                         │
    │                  ││────────────────────┘                   └───────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │b_write_enable    ││ 0                                                                │
    │                  ││────────────────────────────────────────────────────────────      │
    │                  ││────────────────────────────────────────────────────────────      │
    │qa                ││ 00000000                                                         │
    │                  ││────────────────────────────────────────────────────────────      │
    │                  ││──────────────────────────────┬─────────┬───────────────────      │
    │qb                ││ 0000                         │22DD     │AA11                     │
    │                  ││──────────────────────────────┴─────────┴───────────────────      │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    ebe4a270fb50d7b3d96f4e039bd16b68 |}]
;;

let%expect_test "resize ram with byte write enables; read/write = 2" =
  test_resizing
    ()
    ~write_bits:16
    ~read_bits:32
    ~writes:
      [| { write_enable = "10"; write_address = 20; write_data = 0xAABB }
       ; { write_enable = "01"; write_address = 20; write_data = 0xFF11 }
       ; { write_enable = "01"; write_address = 21; write_data = 0xCCDD }
       ; { write_enable = "10"; write_address = 21; write_data = 0x22EE }
      |]
    ~reads:[| 10; 10; 10 |];
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐│
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └│
    │clear             ││──────────┐                                                       │
    │                  ││          └───────────────────────────────────────────────────────│
    │                  ││──────────┬───────────────────┬───────────────────────────────────│
    │a_address         ││ 00       │14                 │15                                 │
    │                  ││──────────┴───────────────────┴───────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────────────────────│
    │a_data            ││ 0000     │AABB     │FF11     │CCDD     │22EE                     │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────────────────────│
    │a_read_enable     ││                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────┬───────────────────┬─────────────────────────│
    │a_write_enable    ││ 0        │2        │1                  │2                        │
    │                  ││──────────┴─────────┴───────────────────┴─────────────────────────│
    │                  ││────────────────────┬─────────────────────────────────────────────│
    │b_address         ││ 00                 │0A                                           │
    │                  ││────────────────────┴─────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────│
    │b_data            ││ 00000000                                                         │
    │                  ││──────────────────────────────────────────────────────────────────│
    │b_read_enable     ││                    ┌─────────────────────────────┐               │
    │                  ││────────────────────┘                             └───────────────│
    │                  ││──────────────────────────────────────────────────────────────────│
    │b_write_enable    ││ 0                                                                │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────│
    │qa                ││ 0000                                                             │
    │                  ││──────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────┬─────────┬─────────┬───────────────│
    │qb                ││ 00000000                     │0000AA11 │00DDAA11 │22DDAA11       │
    │                  ││──────────────────────────────┴─────────┴─────────┴───────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    fcd4ccdb023964c7a33cc171be79aade |}]
;;

let%expect_test "resize ram with byte write enables; read/write = 4" =
  test_resizing
    ()
    ~display_width:120
    ~write_bits:8
    ~read_bits:32
    ~writes:
      [| { write_enable = "1"; write_address = 40; write_data = 0xAA }
       ; { write_enable = "0"; write_address = 40; write_data = 0xFF }
       ; { write_enable = "0"; write_address = 41; write_data = 0xBB }
       ; { write_enable = "1"; write_address = 41; write_data = 0x11 }
       ; { write_enable = "0"; write_address = 42; write_data = 0xCC }
       ; { write_enable = "1"; write_address = 42; write_data = 0x22 }
       ; { write_enable = "1"; write_address = 43; write_data = 0xDD }
       ; { write_enable = "0"; write_address = 43; write_data = 0xEE }
      |]
    ~reads:[| 10; 10; 10; 10; 10; 10; 10 |];
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
    │clear             ││──────────┐                                                                                       │
    │                  ││          └───────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬───────────────────┬───────────────────┬───────────────────┬───────────────────────────│
    │a_address         ││ 00       │28                 │29                 │2A                 │2B                         │
    │                  ││──────────┴───────────────────┴───────────────────┴───────────────────┴───────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────────────│
    │a_data            ││ 00       │AA       │FF       │BB       │11       │CC       │22       │DD       │EE               │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────────────│
    │a_read_enable     ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │a_write_enable    ││          ┌─────────┐                   ┌─────────┐         ┌───────────────────┐                 │
    │                  ││──────────┘         └───────────────────┘         └─────────┘                   └─────────────────│
    │                  ││────────────────────┬─────────────────────────────────────────────────────────────────────────────│
    │b_address         ││ 00                 │0A                                                                           │
    │                  ││────────────────────┴─────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │b_data            ││ 00000000                                                                                         │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │b_read_enable     ││                    ┌─────────────────────────────────────────────────────────────────────┐       │
    │                  ││────────────────────┘                                                                     └───────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │b_write_enable    ││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │qa                ││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────┬───────────────────┬───────────────────┬─────────┬─────────────────│
    │qb                ││ 00000000                     │000000AA           │000011AA           │002211AA │DD2211AA         │
    │                  ││──────────────────────────────┴───────────────────┴───────────────────┴─────────┴─────────────────│
    │                  ││                                                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    16cee7840350faf435d6d335676367ae |}]
;;
