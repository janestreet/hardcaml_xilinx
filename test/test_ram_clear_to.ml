(** Tests for the RAM clear_to wrapper. *)

open! Import
open Hardcaml_waveterm
module Port = Ram_port

let create_circuit
  ?(create = True_dual_port_ram.create)
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
  let clear = Signal.input "clear" 1 in
  let clear_to = Signal.input "clear_to" data_bits in
  let size = 1 lsl address_bits in
  let port_with_clear port =
    Ram_port_with_clear.create ~clear_to ~clear ~clock:Signal.vdd ~size ~port
  in
  let port_a =
    port_with_clear
      Port.(map2 (map port_names ~f:(( ^ ) "a_")) port_sizes ~f:Signal.input)
  in
  let port_b =
    port_with_clear
      Port.(map2 (map port_names ~f:(( ^ ) "b_")) port_sizes ~f:Signal.input)
  in
  let q_a, q_b =
    create
      ~memory_optimization:false
      ~cascade_height:Inferred
      ~read_latency
      ~arch:Ultraram
      ~build_mode:Simulation
      ()
      ~clock_a:(Signal.input "clock_a" 1)
      ~clock_b:(Signal.input "clock_b" 1)
      ~clear_a:clear
      ~clear_b:clear
      ~size
      ~byte_write_width
      ~port_a:port_a.port
      ~port_b:port_b.port
  in
  Circuit.create_exn
    ~name:"ram"
    [ Signal.output "qa" q_a
    ; Signal.output "clear_busy_a" port_a.clear_busy
    ; Signal.output "qb" q_b
    ; Signal.output "clear_busy_b" port_b.clear_busy
    ]
;;

let create_sim ?byte_write_width ~read_latency ~address_bits ~data_bits () =
  let circuit =
    create_circuit ?byte_write_width () ~read_latency ~address_bits ~data_bits
  in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let wave, sim = Waveform.create sim in
  let port_a =
    Port.(map port_names ~f:(fun name -> Cyclesim.in_port sim ("a_" ^ name)))
  in
  let port_b =
    Port.(map port_names ~f:(fun name -> Cyclesim.in_port sim ("b_" ^ name)))
  in
  let qa = Cyclesim.out_port sim "qa" in
  let qb = Cyclesim.out_port sim "qb" in
  ( (wave, sim)
  , Cyclesim.in_port sim "clear"
  , Cyclesim.in_port sim "clear_to"
  , (port_a, qa)
  , (port_b, qb) )
;;

let%expect_test "clear followed by basic write then read, single cycle latency, then \
                 another clear"
  =
  let address_bits = 3 in
  let (waves, sim), clear, clear_to, ((a : _ Port.t), _qa), ((b : _ Port.t), _qb) =
    create_sim ~read_latency:1 ~address_bits ~data_bits:32 ()
  in
  clear_to := Bits.of_int ~width:32 0xBEEF;
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  for _ = 0 to 1 lsl address_bits do
    Cyclesim.cycle sim
  done;
  a.write_enable := Bits.vdd;
  a.address := Bits.of_int ~width:address_bits 10;
  a.data := Bits.of_int ~width:32 100;
  Cyclesim.cycle sim;
  a.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:address_bits 10;
  Cyclesim.cycle sim;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  (* Read an address that should be cleared to BEEF *)
  b.read_enable := Bits.vdd;
  b.address := Bits.of_int ~width:address_bits 1;
  Cyclesim.cycle sim;
  b.read_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  clear_to := Bits.zero 32;
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  for _ = 0 to 1 lsl address_bits do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~display_height:65 ~display_width:85 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves──────────────────────────────────────────────────────────┐
    │clear             ││──┐                                 ┌─┐                        │
    │                  ││  └─────────────────────────────────┘ └─────────────────       │
    │                  ││────────────────────┬───────────────────────────────────       │
    │a_address         ││ 0                  │2                                         │
    │                  ││────────────────────┴───────────────────────────────────       │
    │                  ││────────────────────┬───────────────────────────────────       │
    │a_data            ││ 00000000           │00000064                                  │
    │                  ││────────────────────┴───────────────────────────────────       │
    │a_read_enable     ││                                                               │
    │                  ││────────────────────────────────────────────────────────       │
    │a_write_enable    ││                    ┌─┐                                        │
    │                  ││────────────────────┘ └─────────────────────────────────       │
    │                  ││────────────────────────┬───┬───────────────────────────       │
    │b_address         ││ 0                      │2  │1                                 │
    │                  ││────────────────────────┴───┴───────────────────────────       │
    │                  ││────────────────────────────────────────────────────────       │
    │b_data            ││ 00000000                                                      │
    │                  ││────────────────────────────────────────────────────────       │
    │b_read_enable     ││                        ┌─┐ ┌─┐                                │
    │                  ││────────────────────────┘ └─┘ └─────────────────────────       │
    │b_write_enable    ││                                                               │
    │                  ││────────────────────────────────────────────────────────       │
    │                  ││────────────────────────────────────┬───────────────────       │
    │clear_to          ││ 0000BEEF                           │00000000                  │
    │                  ││────────────────────────────────────┴───────────────────       │
    │clock_a           ││                                                               │
    │                  ││────────────────────────────────────────────────────────       │
    │clock_b           ││                                                               │
    │                  ││────────────────────────────────────────────────────────       │
    │clear_busy_a      ││──────────────────┐                   ┌───────────────┐        │
    │                  ││                  └───────────────────┘               └─       │
    │clear_busy_b      ││──────────────────┐                   ┌───────────────┐        │
    │                  ││                  └───────────────────┘               └─       │
    │                  ││────────────────────────────────────────────────────────       │
    │qa                ││ 00000000                                                      │
    │                  ││────────────────────────────────────────────────────────       │
    │                  ││──┬───────────────────────┬───┬───────────┬─────────────       │
    │qb                ││ .│0000BEEF               │00.│0000BEEF   │00000000            │
    │                  ││──┴───────────────────────┴───┴───────────┴─────────────       │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─────────────────┬─┬─┬─┬─┬─┬─┬─┬─┬─       │
    │int_address       ││ 0  │1│2│3│4│5│6│7│0│2                │0│1│2│3│4│5│6│7│2       │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─────────────────┴─┴─┴─┴─┴─┴─┴─┴─┴─       │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─────┬───┬─────────┬─┬─┬─┬─┬─┬─┬─┬─┬─       │
    │int_address_0     ││ 0  │1│2│3│4│5│6│7│0    │2  │1        │0│1│2│3│4│5│6│7│1       │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─────┴───┴─────────┴─┴─┴─┴─┴─┴─┴─┴─┴─       │
    │                  ││──────────────────┬─┬─────────────────┬───────────────┬─       │
    │int_data          ││ 0000BEEF         │.│00000064         │00000000       │.       │
    │                  ││──────────────────┴─┴─────────────────┴───────────────┴─       │
    │                  ││──────────────────┬─────────────────────────────────────       │
    │int_data_0        ││ 0000BEEF         │00000000                                    │
    │                  ││──────────────────┴─────────────────────────────────────       │
    │int_read_enable   ││                        ┌─┐ ┌─┐                                │
    │                  ││────────────────────────┘ └─┘ └─────────────────────────       │
    │int_read_enable_0 ││                                                               │
    │                  ││────────────────────────────────────────────────────────       │
    │int_write_enable  ││──────────────────┐                   ┌───────────────┐        │
    │                  ││                  └───────────────────┘               └─       │
    │int_write_enable_0││──────────────────┐ ┌─┐               ┌───────────────┐        │
    │                  ││                  └─┘ └───────────────┘               └─       │
    │state             ││                  ┌───────────────────┐               ┌─       │
    │                  ││──────────────────┘                   └───────────────┘        │
    │state_0           ││                  ┌───────────────────┐               ┌─       │
    │                  ││──────────────────┘                   └───────────────┘        │
    └──────────────────┘└───────────────────────────────────────────────────────────────┘
    86cd0021d1ffa0b0662d57a8c16f873b |}]
;;
