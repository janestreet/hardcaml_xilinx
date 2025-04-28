open! Import
open Hardcaml_waveterm

let address_bits = 10
let data_bits = 32

module O_ports = struct
  type 'a t =
    { a : 'a [@bits data_bits]
    ; b : 'a [@bits data_bits]
    }
  [@@deriving hardcaml]
end

module O_rams = struct
  type 'a t =
    { drw : 'a O_ports.t
    ; brw : 'a O_ports.t
    ; bwr : 'a O_ports.t
    ; bnc : 'a O_ports.t
    ; unc : 'a O_ports.t
    }
  [@@deriving hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { sim : 'a O_rams.t
    ; xpm : 'a O_rams.t
    ; ok : 'a [@bits 10]
    }
  [@@deriving hardcaml ~rtlmangle:true]
end

let create_ram ~arch ~build_mode ~clock ~clear ~port_a ~port_b =
  let byte_write_width = Byte_write_width.Full in
  let a, b =
    True_dual_port_ram.create
      ~address_collision_model:(Const 666)
      ~read_latency:1
      ~arch
      ~build_mode
      ~memory_optimization:false
      ~cascade_height:Inferred
      ()
      ~clock_a:clock
      ~clock_b:clock
      ~clear_a:clear
      ~clear_b:clear
      ~size:(1 lsl address_bits)
      ~byte_write_width
      ~port_a
      ~port_b
  in
  { O_ports.a; b }
;;

(* Create a RAM of each supported type. *)
let create_rams ~build_mode ~clock ~clear ~port_a ~port_b =
  let create_ram ~arch = create_ram ~arch ~build_mode ~clock ~clear ~port_a ~port_b in
  { O_rams.drw = create_ram ~arch:Distributed
  ; O_rams.brw = create_ram ~arch:(Blockram Read_before_write)
  ; O_rams.bwr = create_ram ~arch:(Blockram Write_before_read)
  ; O_rams.bnc = create_ram ~arch:(Blockram No_change)
  ; O_rams.unc = create_ram ~arch:Ultraram
  }
;;

(* Create RAMs in XPM and hardcaml simulation variants. *)
let compare_rams ~clock ~clear ~port_a ~port_b =
  let open Signal in
  let sim = create_rams ~build_mode:Simulation ~clock ~clear ~port_a ~port_b in
  let xpm = create_rams ~build_mode:Synthesis ~clock ~clear ~port_a ~port_b in
  let ok =
    O_rams.map2 sim xpm ~f:(fun sim xpm -> sim ==: xpm) |> O_rams.to_list |> concat_lsb
  in
  assert (width ok = 10);
  { O.sim; xpm; ok }
;;

(* Construct the inputs ports to the ram simulation. *)
let create (type a) ~(f : clock:_ -> clear:_ -> port_a:_ -> port_b:_ -> a) : a =
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let port_sizes =
    { Ram_port.address = address_bits
    ; data = data_bits
    ; read_enable = 1
    ; write_enable = 1
    }
  in
  let port_a =
    Ram_port.(map2 (map port_names ~f:(fun x -> x ^ "_a")) port_sizes ~f:Signal.input)
  in
  let port_b =
    Ram_port.(map2 (map port_names ~f:(fun x -> x ^ "_b")) port_sizes ~f:Signal.input)
  in
  f ~clock ~clear ~port_a ~port_b
;;

let create_circuit () =
  let rams = create ~f:(create_rams ~build_mode:Simulation) in
  Circuit.create_exn
    ~name:"ram"
    (O_rams.map2 O_rams.port_names rams ~f:Signal.output |> O_rams.to_list)
;;

module Display_rules = struct
  let clock = Display_rule.port_name_is "clock" ~wave_format:Bit

  let port_a =
    Display_rule.port_name_matches
      Re.Posix.(compile (re ".*_a$"))
      ~wave_format:(Bit_or Int)
  ;;

  let port_b =
    Display_rule.port_name_matches
      Re.Posix.(compile (re ".*_b$"))
      ~wave_format:(Bit_or Int)
  ;;
end

let create_sim () =
  let circuit = create_circuit () in
  let sim = Cyclesim.create circuit in
  let wave, sim = Waveform.create sim in
  let _clear =
    try Cyclesim.in_port sim "clear" with
    | _ -> ref Bits.empty
  in
  let port_a =
    Ram_port.(map port_names ~f:(fun name -> Cyclesim.in_port sim (name ^ "_a")))
  in
  let port_b =
    Ram_port.(map port_names ~f:(fun name -> Cyclesim.in_port sim (name ^ "_b")))
  in
  (wave, sim), port_a, port_b
;;

let%expect_test "address collision for basic RTL models" =
  let (waves, sim), (a : _ Ram_port.t), (_b : _ Ram_port.t) = create_sim () in
  a.write_enable := Bits.vdd;
  a.address := Bits.of_int_trunc ~width:address_bits 3;
  a.data := Bits.of_int_trunc ~width:data_bits 8;
  Cyclesim.cycle sim;
  a.read_enable := Bits.vdd;
  a.data := Bits.of_int_trunc ~width:data_bits 9;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect ~display_width:50 ~display_rules:Display_rules.[ clock; port_a ] waves;
  [%expect
    {|
    ┌Signals───┐┌Waves───────────────────────────────┐
    │clock     ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │          ││    └───┘   └───┘   └───┘   └───┘   │
    │          ││────────────────────────────────    │
    │address_a ││ 3                                  │
    │          ││────────────────────────────────    │
    │          ││────────┬───────────────────────    │
    │data_a    ││ 8      │9                          │
    │          ││────────┴───────────────────────    │
    │read_enabl││        ┌───────────────────────    │
    │          ││────────┘                           │
    │write_enab││────────────────┐                   │
    │          ││                └───────────────    │
    │          ││────────────────────────┬───────    │
    │bnc_a     ││ 0                      │9          │
    │          ││────────────────────────┴───────    │
    │          ││────────────────┬───────┬───────    │
    │brw_a     ││ 0              │8      │9          │
    │          ││────────────────┴───────┴───────    │
    │          ││────────┬───────┬───────────────    │
    │bwr_a     ││ 0      │8      │9                  │
    │          ││────────┴───────┴───────────────    │
    │          ││────────────────┬───────┬───────    │
    │drw_a     ││ 0              │8      │9          │
    │          ││────────────────┴───────┴───────    │
    │          ││────────────────────────┬───────    │
    │unc_a     ││ 0                      │9          │
    │          ││────────────────────────┴───────    │
    └──────────┘└────────────────────────────────────┘
    c19b13da5309446a73618c912a590a45
    |}]
;;

let%expect_test "address collision across ports for basic RTL models" =
  let (waves, sim), (a : _ Ram_port.t), (b : _ Ram_port.t) = create_sim () in
  a.write_enable := Bits.vdd;
  a.address := Bits.of_int_trunc ~width:address_bits 3;
  b.address := Bits.of_int_trunc ~width:address_bits 3;
  a.data := Bits.of_int_trunc ~width:data_bits 8;
  b.read_enable := Bits.vdd;
  Cyclesim.cycle sim;
  a.data := Bits.of_int_trunc ~width:data_bits 9;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect
    ~display_width:50
    ~display_rules:Display_rules.[ clock; port_a; port_b ]
    waves;
  [%expect
    {|
    ┌Signals───┐┌Waves───────────────────────────────┐
    │clock     ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │          ││    └───┘   └───┘   └───┘   └───┘   │
    │          ││────────────────────────────────    │
    │address_a ││ 3                                  │
    │          ││────────────────────────────────    │
    │          ││────────┬───────────────────────    │
    │data_a    ││ 8      │9                          │
    │          ││────────┴───────────────────────    │
    │read_enabl││                                    │
    │          ││────────────────────────────────    │
    │write_enab││────────────────┐                   │
    │          ││                └───────────────    │
    │          ││────────────────────────────────    │
    │bnc_a     ││ 0                                  │
    │          ││────────────────────────────────    │
    │          ││────────────────┬───────────────    │
    │brw_a     ││ 0              │8                  │
    │          ││────────────────┴───────────────    │
    │          ││────────┬───────┬───────────────    │
    │bwr_a     ││ 0      │8      │9                  │
    │          ││────────┴───────┴───────────────    │
    │          ││────────────────┬───────────────    │
    │drw_a     ││ 0              │8                  │
    │          ││────────────────┴───────────────    │
    │          ││────────────────────────────────    │
    │unc_a     ││ 0                                  │
    │          ││────────────────────────────────    │
    │          ││────────────────────────────────    │
    │address_b ││ 3                                  │
    │          ││────────────────────────────────    │
    │          ││────────────────────────────────    │
    │data_b    ││ 0                                  │
    │          ││────────────────────────────────    │
    │read_enabl││────────────────────────────────    │
    │          ││                                    │
    │write_enab││                                    │
    │          ││────────────────────────────────    │
    │          ││────────┬───────────────┬───────    │
    │bnc_b     ││ 0      │666            │9          │
    │          ││────────┴───────────────┴───────    │
    │          ││────────────────┬───────┬───────    │
    │brw_b     ││ 0              │8      │9          │
    │          ││────────────────┴───────┴───────    │
    │          ││────────┬───────────────┬───────    │
    │bwr_b     ││ 0      │666            │9          │
    │          ││────────┴───────────────┴───────    │
    │          ││────────────────┬───────┬───────    │
    │drw_b     ││ 0              │8      │9          │
    │          ││────────────────┴───────┴───────    │
    │          ││────────┬───────┬───────────────    │
    │unc_b     ││ 0      │8      │9                  │
    │          ││────────┴───────┴───────────────    │
    └──────────┘└────────────────────────────────────┘
    c063462e635cc442aee6d2bce2c3b371
    |}]
;;

(* This expect test models the hardcaml only part of the collision test performed with
   Vivado xsim in the [test_hdl] directory. The same code is used to generate the circuit.

   We are checking all supported ram architectures and collision modes simultaneously
   here.

   BE VERY WARY OF CHANGES TO THIS WAVEFORM. If it changes, you must re-run the verilog
   simulations to ensure things still match up.

   The verilog simulation outputs a bitvector which compares the simulation and xpm
   models. Roughly speaking once the test is initialized it should always be all ones.
   There are two cases where it is not:

   - Blockram cross port writes lead to X's.  We cannot model these in hardcaml currently.
   - Ultrarams use a 1ps delay on the B-port. Every time the b port changes there is a
     brief glitch compared to the hardcaml model.
*)
let%expect_test "Vivado XSIM testbench model" =
  let (waves, sim), (a : _ Ram_port.t), (b : _ Ram_port.t) = create_sim () in
  (* write from port a *)
  a.address := Bits.of_int_trunc ~width:address_bits 1;
  b.address := Bits.of_int_trunc ~width:address_bits 1;
  a.data := Bits.of_int_trunc ~width:data_bits 8;
  b.data := Bits.of_int_trunc ~width:data_bits 9;
  a.read_enable := Bits.vdd;
  b.read_enable := Bits.vdd;
  a.write_enable := Bits.vdd;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  a.read_enable := Bits.gnd;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  (* write from port b *)
  a.address := Bits.of_int_trunc ~width:address_bits 2;
  b.address := Bits.of_int_trunc ~width:address_bits 2;
  a.data := Bits.of_int_trunc ~width:data_bits 18;
  b.data := Bits.of_int_trunc ~width:data_bits 19;
  a.read_enable := Bits.vdd;
  b.read_enable := Bits.vdd;
  b.write_enable := Bits.vdd;
  Cyclesim.cycle sim;
  b.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  a.read_enable := Bits.gnd;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.expect
    ~display_width:90
    ~display_rules:Display_rules.[ clock; port_a; port_b ]
    waves;
  (* BE VERY WARY OF CHANGES TO THIS WAVEFORM.  *)
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │                  ││────────────────────────────────────────┬───────────────────────────│
    │address_a         ││ 1                                      │2                          │
    │                  ││────────────────────────────────────────┴───────────────────────────│
    │                  ││────────────────────────────────────────┬───────────────────────────│
    │data_a            ││ 8                                      │18                         │
    │                  ││────────────────────────────────────────┴───────────────────────────│
    │read_enable_a     ││────────────────┐                       ┌───────────────┐           │
    │                  ││                └───────────────────────┘               └───────────│
    │write_enable_a    ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────────│
    │                  ││────────────────┬───────────────────────────────┬───────┬───────────│
    │bnc_a             ││ 0              │8                              │666    │19         │
    │                  ││────────────────┴───────────────────────────────┴───────┴───────────│
    │                  ││────────────────┬───────────────────────────────┬───────┬───────────│
    │brw_a             ││ 0              │8                              │0      │19         │
    │                  ││────────────────┴───────────────────────────────┴───────┴───────────│
    │                  ││────────┬───────────────────────────────────────┬───────┬───────────│
    │bwr_a             ││ 0      │8                                      │666    │19         │
    │                  ││────────┴───────────────────────────────────────┴───────┴───────────│
    │                  ││────────────────┬───────────────────────────────┬───────────────────│
    │drw_a             ││ 0              │8                              │0                  │
    │                  ││────────────────┴───────────────────────────────┴───────────────────│
    │                  ││────────────────┬───────────────────────────────┬───────┬───────────│
    │unc_a             ││ 0              │8                              │0      │19         │
    │                  ││────────────────┴───────────────────────────────┴───────┴───────────│
    │                  ││────────────────────────────────────────┬───────────────────────────│
    │address_b         ││ 1                                      │2                          │
    │                  ││────────────────────────────────────────┴───────────────────────────│
    │                  ││────────────────────────────────────────┬───────────────────────────│
    │data_b            ││ 9                                      │19                         │
    │                  ││────────────────────────────────────────┴───────────────────────────│
    │read_enable_b     ││────────────────┐                       ┌───────────────┐           │
    │                  ││                └───────────────────────┘               └───────────│
    │write_enable_b    ││                                        ┌───────┐                   │
    │                  ││────────────────────────────────────────┘       └───────────────────│
    │                  ││────────┬───────┬───────────────────────────────────────┬───────────│
    │bnc_b             ││ 0      │666    │8                                      │19         │
    │                  ││────────┴───────┴───────────────────────────────────────┴───────────│
    │                  ││────────────────┬───────────────────────────────┬───────┬───────────│
    │brw_b             ││ 0              │8                              │0      │19         │
    │                  ││────────────────┴───────────────────────────────┴───────┴───────────│
    │                  ││────────┬───────┬───────────────────────────────┬───────────────────│
    │bwr_b             ││ 0      │666    │8                              │19                 │
    │                  ││────────┴───────┴───────────────────────────────┴───────────────────│
    │                  ││────────────────┬───────────────────────────────┬───────────────────│
    │drw_b             ││ 0              │8                              │0                  │
    │                  ││────────────────┴───────────────────────────────┴───────────────────│
    │                  ││────────┬───────────────────────────────────────────────┬───────────│
    │unc_b             ││ 0      │8                                              │19         │
    │                  ││────────┴───────────────────────────────────────────────┴───────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    e69b62fa650e805244002b234bcba7a0
    |}]
;;
