open! Core
open! Hardcaml
open Hardcaml_waveterm
module Ram_with_resizing = Hardcaml_xilinx.Ram_with_resizing

module For_rtl_sim = struct
  module Config = struct
    (* Fiddle with the following parameters and run a sim to see the behaviour. *)
    let log_num_words = 6
    let bits_per_word = 16
    let log_scale_between_ports = -1
    let read_latency = 1
    let collision_mode = None
    let byte_write_width = Some Hardcaml_xilinx.Byte_write_width.B8
    let clocking_mode = None
  end

  module Ram = Ram_with_resizing.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
  end

  open Signal

  let create scope (i : _ Ram.I.t) =
    let sim = Ram.hierarchical ~build_mode:Simulation scope i in
    let xpm = Ram.hierarchical ~build_mode:Synthesis scope i in
    { O.q = sim.q ==: xpm.q }
  ;;

  module Step = struct
    type 'a t =
      { write_enable : 'a [@bits Ram.write_enable_bits]
      ; read_enable : 'a
      ; write_address : 'a [@bits Ram.write_address_bits]
      ; read_address : 'a [@bits Ram.read_address_bits]
      ; write_data : 'a [@bits Ram.write_data_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let step ?write_enable ?read_enable ?write_address ?read_address ?write_data () =
    { Step.write_enable; read_enable; write_address; read_address; write_data }
  ;;

  let steps ~clock ~clear (l : int option Step.t list) =
    let rec f (prev : int Step.t) l =
      match l with
      | [] -> []
      | { Step.write_enable; read_enable; write_address; read_address; write_data } :: t
        ->
        let d =
          { Step.write_enable = Option.value ~default:0 write_enable
          ; read_enable = Option.value ~default:0 read_enable
          ; write_address = Option.value ~default:prev.write_address write_address
          ; read_address = Option.value ~default:prev.read_address read_address
          ; write_data = Option.value ~default:prev.write_data write_data
          }
        in
        { Step.write_enable =
            Signal.of_int ~width:Step.port_widths.write_enable d.write_enable
        ; read_enable = Signal.of_int ~width:1 d.read_enable
        ; write_address =
            Signal.of_int ~width:Step.port_widths.write_address d.write_address
        ; read_address = Signal.of_int ~width:Step.port_widths.read_address d.read_address
        ; write_data = Signal.of_int ~width:Step.port_widths.write_data d.write_data
        }
        :: f d t
    in
    f
      { write_enable = 0
      ; read_enable = 0
      ; write_address = 0
      ; read_address = 0
      ; write_data = 0
      }
      l
    |> Step.Of_signal.mux
         (reg_fb
            (Reg_spec.create ~clock ~clear ())
            ~width:(Int.ceil_log2 (List.length l))
            ~f:(fun d -> d +:. 1)
          -- "ROM_ADDRESS")
  ;;

  let testbench scope (i : _ I.t) =
    let max_write_enable = (1 lsl Ram.write_enable_bits) - 1 in
    let write_enable, read_enable = max_write_enable, 1 in
    let p =
      steps
        ~clock:i.clock
        ~clear:i.clear
        (List.concat
           [ (* initialize first 1st word. *)
             List.init
               (if Config.log_scale_between_ports > 0
                then 1 lsl Config.log_scale_between_ports
                else 1)
               ~f:(fun addr -> step ~write_enable ~write_address:addr ~write_data:0 ())
           ; [ step ~read_enable ~read_address:0 () ]
           ; List.init (1 lsl Config.log_num_words) ~f:(fun i ->
               step ~write_enable ~write_address:i ~write_data:i ())
           ; List.init
               (1 lsl (Config.log_num_words - Config.log_scale_between_ports))
               ~f:(fun i -> step ~read_enable ~read_address:i ())
           ; List.init 1000 ~f:(fun _ ->
               step
                 ~write_enable:(Random.int (max_write_enable + 1))
                 ~read_enable:(Random.int 2)
                 ~write_address:(Random.int (1 lsl Config.log_num_words))
                 ~read_address:
                   (Random.int
                      (1 lsl (Config.log_num_words - Config.log_scale_between_ports)))
                 ~write_data:(Random.int (1 lsl Config.bits_per_word))
                 ())
           ])
    in
    create
      scope
      { Ram.I.write_clock = i.clock
      ; write_clear = i.clear
      ; write_enable = p.write_enable
      ; write_address = p.write_address
      ; write_data = p.write_data
      ; read_clock = i.clock
      ; read_clear = i.clear
      ; read_enable = p.read_enable
      ; read_address = p.read_address
      }
  ;;

  let generate () =
    let module Circuit = Circuit.With_interface (I) (O) in
    let scope = Scope.create ~flatten_design:false () in
    let circ = Circuit.create_exn ~name:"rams_with_resizing" (testbench scope) in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circ
  ;;
end

let ( <-. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test
  ?(strobe_enable = true)
  ?(bits_per_word = 4)
  ?byte_write_width
  ?(read_latency = 1)
  log_scale_between_ports
  =
  assert (bits_per_word = 4 || bits_per_word = 8);
  let log_num_words = 3 in
  let module Ram =
    Ram_with_resizing.Make (struct
      let bits_per_word = bits_per_word
      let log_scale_between_ports = log_scale_between_ports
      let log_num_words = log_num_words
      let read_latency = read_latency
      let collision_mode = None
      let byte_write_width = byte_write_width
      let clocking_mode = None
    end)
  in
  print_s
    [%message
      (Ram.write_address_bits : int)
        (Ram.read_address_bits : int)
        (Ram.write_data_bits : int)
        (Ram.read_data_bits : int)
        (Ram.write_enable_bits : int)];
  let module Sim = Cyclesim.With_interface (Ram.I) (Ram.O) in
  let sim =
    Sim.create (Ram.create ~build_mode:Simulation (Scope.create ~flatten_design:true ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  (* write *)
  let scale_up =
    1 lsl if log_scale_between_ports >= 0 then 0 else -log_scale_between_ports
  in
  print_s [%message (scale_up : int)];
  for i = 0 to ((1 lsl log_num_words) / scale_up) - 1 do
    inputs.write_enable
    <-.
    if strobe_enable
    then 1 lsl (i % Ram.write_enable_bits)
    else (1 lsl Ram.write_enable_bits) - 1;
    inputs.write_address <-. i;
    inputs.write_data <-. 0x4321 + (i * 0x11111111 * scale_up * (bits_per_word / 4));
    Cyclesim.cycle sim
  done;
  inputs.write_enable <-. 0;
  (* read *)
  let scale_up =
    1 lsl if log_scale_between_ports <= 0 then 0 else log_scale_between_ports
  in
  inputs.read_enable <-. 1;
  for i = 0 to ((1 lsl log_num_words) / scale_up) - 1 do
    inputs.read_address <-. i;
    Cyclesim.cycle sim
  done;
  inputs.read_enable <-. 0;
  for _ = 0 to read_latency do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~display_width:90 ~display_height:24 ~wave_width:1 waves
;;

let%expect_test "no scale" =
  test 0;
  [%expect
    {|
((Ram.write_address_bits 3) (Ram.read_address_bits 3) (Ram.write_data_bits 4)
 (Ram.read_data_bits 4) (Ram.write_enable_bits 1))
(scale_up 1)
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││────────────────────────────────────┬───┬───┬───┬───┬───┬───┬───────│
│read_address      ││ 0                                  │1  │2  │3  │4  │5  │6  │7      │
│                  ││────────────────────────────────────┴───┴───┴───┴───┴───┴───┴───────│
│read_clock        ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│read_enable       ││                                ┌───────────────────────────────┐   │
│                  ││────────────────────────────────┘                               └───│
│                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────────────────────│
│write_address     ││ 0  │1  │2  │3  │4  │5  │6  │7                                      │
│                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────────────────────│
│write_clock       ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────────────────────│
│write_data        ││ 1  │2  │3  │4  │5  │6  │7  │8                                      │
│                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────────────────────│
│write_enable      ││────────────────────────────────┐                                   │
│                  ││                                └───────────────────────────────────│
│                  ││────────────────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───│
│q                 ││ 0                                  │1  │2  │3  │4  │5  │6  │7  │8  │
│                  ││────────────────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───│
│                  ││                                                                    │
│                  ││                                                                    │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
84553838962977b14b84bebd9258c2fa |}]
;;

let%expect_test "write port wider x2" =
  test (-1);
  [%expect
    {|
((Ram.write_address_bits 2) (Ram.read_address_bits 3) (Ram.write_data_bits 8)
 (Ram.read_data_bits 4) (Ram.write_enable_bits 1))
(scale_up 2)
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───────────            │
│read_address      ││ 0                  │1  │2  │3  │4  │5  │6  │7                      │
│                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───────────            │
│read_clear        ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│read_clock        ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│read_enable       ││                ┌───────────────────────────────┐                   │
│                  ││────────────────┘                               └───────            │
│                  ││────┬───┬───┬───────────────────────────────────────────            │
│write_address     ││ 0  │1  │2  │3                                                      │
│                  ││────┴───┴───┴───────────────────────────────────────────            │
│write_clock       ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│                  ││────┬───┬───┬───────────────────────────────────────────            │
│write_data        ││ 21 │43 │65 │87                                                     │
│                  ││────┴───┴───┴───────────────────────────────────────────            │
│write_enable      ││────────────────┐                                                   │
│                  ││                └───────────────────────────────────────            │
│                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───┬───────            │
│q                 ││ 0                  │1  │2  │3  │4  │5  │6  │7  │8                  │
│                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───┴───────            │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
69674b1ec7a08adcbf409a93d31559aa |}]
;;

let%expect_test "write port wider x4" =
  test (-2);
  [%expect
    {|
    ((Ram.write_address_bits 1) (Ram.read_address_bits 3)
     (Ram.write_data_bits 16) (Ram.read_data_bits 4) (Ram.write_enable_bits 1))
    (scale_up 4)
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───────────                    │
    │read_address      ││ 0          │1  │2  │3  │4  │5  │6  │7                              │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───────────                    │
    │read_clear        ││                                                                    │
    │                  ││────────────────────────────────────────────────                    │
    │read_clock        ││                                                                    │
    │                  ││────────────────────────────────────────────────                    │
    │read_enable       ││        ┌───────────────────────────────┐                           │
    │                  ││────────┘                               └───────                    │
    │write_address     ││    ┌───────────────────────────────────────────                    │
    │                  ││────┘                                                               │
    │write_clock       ││                                                                    │
    │                  ││────────────────────────────────────────────────                    │
    │                  ││────┬───────────────────────────────────────────                    │
    │write_data        ││ 43.│8765                                                           │
    │                  ││────┴───────────────────────────────────────────                    │
    │write_enable      ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────                    │
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───┬───────                    │
    │q                 ││ 0          │1  │2  │3  │4  │5  │6  │7  │8                          │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───┴───────                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    2e01476e50dc38e7f1244fa02315818d |}]
;;

let%expect_test "write port wider x8 - raises" =
  Expect_test_helpers_base.require_does_raise [%here] (fun () -> test (-3));
  [%expect
    {|
    ("Cannot construct RAM with < 2 write entries"
     (write_address_bits 0)
     (read_address_bits  3)
     (write_data_bits    32)
     (read_data_bits     4)) |}]
;;

let%expect_test "read port wider x2" =
  test 1;
  [%expect
    {|
((Ram.write_address_bits 3) (Ram.read_address_bits 2) (Ram.write_data_bits 4)
 (Ram.read_data_bits 8) (Ram.write_enable_bits 1))
(scale_up 1)
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││────────────────────────────────────┬───┬───┬───────────            │
│read_address      ││ 0                                  │1  │2  │3                      │
│                  ││────────────────────────────────────┴───┴───┴───────────            │
│read_clock        ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│read_enable       ││                                ┌───────────────┐                   │
│                  ││────────────────────────────────┘               └───────            │
│                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────────            │
│write_address     ││ 0  │1  │2  │3  │4  │5  │6  │7                                      │
│                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────────            │
│write_clock       ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────────            │
│write_data        ││ 1  │2  │3  │4  │5  │6  │7  │8                                      │
│                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────────            │
│write_enable      ││────────────────────────────────┐                                   │
│                  ││                                └───────────────────────            │
│                  ││────────────────────────────────────┬───┬───┬───┬───────            │
│q                 ││ 00                                 │21 │43 │65 │87                 │
│                  ││────────────────────────────────────┴───┴───┴───┴───────            │
│                  ││                                                                    │
│                  ││                                                                    │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
ef5b1716c1a6241d58c15cd2cc3768f4 |}]
;;

let%expect_test "read port wider x4" =
  test 2;
  [%expect
    {|
    ((Ram.write_address_bits 3) (Ram.read_address_bits 1) (Ram.write_data_bits 4)
     (Ram.read_data_bits 16) (Ram.write_enable_bits 1))
    (scale_up 1)
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │read_address      ││                                    ┌───────────                    │
    │                  ││────────────────────────────────────┘                               │
    │read_clock        ││                                                                    │
    │                  ││────────────────────────────────────────────────                    │
    │read_enable       ││                                ┌───────┐                           │
    │                  ││────────────────────────────────┘       └───────                    │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────                    │
    │write_address     ││ 0  │1  │2  │3  │4  │5  │6  │7                                      │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────                    │
    │write_clock       ││                                                                    │
    │                  ││────────────────────────────────────────────────                    │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────                    │
    │write_data        ││ 1  │2  │3  │4  │5  │6  │7  │8                                      │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────                    │
    │write_enable      ││────────────────────────────────┐                                   │
    │                  ││                                └───────────────                    │
    │                  ││────────────────────────────────────┬───┬───────                    │
    │q                 ││ 0000                               │43.│8765                       │
    │                  ││────────────────────────────────────┴───┴───────                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    196bc610bf41bf3d5312d7b47793912d |}]
;;

let%expect_test "read port wider x4, read_latency=2" =
  test ~read_latency:2 2;
  [%expect
    {|
    ((Ram.write_address_bits 3) (Ram.read_address_bits 1) (Ram.write_data_bits 4)
     (Ram.read_data_bits 16) (Ram.write_enable_bits 1))
    (scale_up 1)
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │read_address      ││                                    ┌───────────────                │
    │                  ││────────────────────────────────────┘                               │
    │read_clear        ││                                                                    │
    │                  ││────────────────────────────────────────────────────                │
    │read_clock        ││                                                                    │
    │                  ││────────────────────────────────────────────────────                │
    │read_enable       ││                                ┌───────┐                           │
    │                  ││────────────────────────────────┘       └───────────                │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────                │
    │write_address     ││ 0  │1  │2  │3  │4  │5  │6  │7                                      │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────                │
    │write_clock       ││                                                                    │
    │                  ││────────────────────────────────────────────────────                │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────                │
    │write_data        ││ 1  │2  │3  │4  │5  │6  │7  │8                                      │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────                │
    │write_enable      ││────────────────────────────────┐                                   │
    │                  ││                                └───────────────────                │
    │                  ││────────────────────────────────────────┬───┬───────                │
    │q                 ││ 0000                                   │43.│8765                   │
    │                  ││────────────────────────────────────────┴───┴───────                │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    fa3bdc9586abba1234097b4216989deb |}]
;;

let%expect_test "read port wider x8 - raises" =
  Expect_test_helpers_base.require_does_raise [%here] (fun () -> test 3);
  [%expect
    {|
    ("Cannot construct RAM with < 2 read entries"
     (write_address_bits 3)
     (read_address_bits  0)
     (write_data_bits    4)
     (read_data_bits     32)) |}]
;;

let%expect_test "write port wider x2, with byte enables" =
  test ~bits_per_word:8 ~byte_write_width:B8 (-1);
  [%expect
    {|
    ((Ram.write_address_bits 2) (Ram.read_address_bits 3)
     (Ram.write_data_bits 16) (Ram.read_data_bits 8) (Ram.write_enable_bits 2))
    (scale_up 2)
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───────────            │
    │read_address      ││ 0                  │1  │2  │3  │4  │5  │6  │7                      │
    │                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───────────            │
    │read_clear        ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │read_clock        ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │read_enable       ││                ┌───────────────────────────────┐                   │
    │                  ││────────────────┘                               └───────            │
    │                  ││────┬───┬───┬───────────────────────────────────────────            │
    │write_address     ││ 0  │1  │2  │3                                                      │
    │                  ││────┴───┴───┴───────────────────────────────────────────            │
    │write_clock       ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │                  ││────┬───┬───┬───────────────────────────────────────────            │
    │write_data        ││ 43.│87.│CB.│0FED                                                   │
    │                  ││────┴───┴───┴───────────────────────────────────────────            │
    │                  ││────┬───┬───┬───┬───────────────────────────────────────            │
    │write_enable      ││ 1  │2  │1  │2  │0                                                  │
    │                  ││────┴───┴───┴───┴───────────────────────────────────────            │
    │                  ││────────────────────┬───┬───────┬───┬───┬───────┬───────            │
    │q                 ││ 00                 │21 │00     │87 │A9 │00     │0F                 │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    a5df70a41a15da42fa14ba59249b0683 |}]
;;

let%expect_test "write port wider x2, with byte enables all high" =
  test ~strobe_enable:false ~bits_per_word:8 ~byte_write_width:B8 (-1);
  [%expect
    {|
    ((Ram.write_address_bits 2) (Ram.read_address_bits 3)
     (Ram.write_data_bits 16) (Ram.read_data_bits 8) (Ram.write_enable_bits 2))
    (scale_up 2)
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───────────            │
    │read_address      ││ 0                  │1  │2  │3  │4  │5  │6  │7                      │
    │                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───────────            │
    │read_clear        ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │read_clock        ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │read_enable       ││                ┌───────────────────────────────┐                   │
    │                  ││────────────────┘                               └───────            │
    │                  ││────┬───┬───┬───────────────────────────────────────────            │
    │write_address     ││ 0  │1  │2  │3                                                      │
    │                  ││────┴───┴───┴───────────────────────────────────────────            │
    │write_clock       ││                                                                    │
    │                  ││────────────────────────────────────────────────────────            │
    │                  ││────┬───┬───┬───────────────────────────────────────────            │
    │write_data        ││ 43.│87.│CB.│0FED                                                   │
    │                  ││────┴───┴───┴───────────────────────────────────────────            │
    │                  ││────────────────┬───────────────────────────────────────            │
    │write_enable      ││ 3              │0                                                  │
    │                  ││────────────────┴───────────────────────────────────────            │
    │                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───┬───────            │
    │q                 ││ 00                 │21 │43 │65 │87 │A9 │CB │ED │0F                 │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    2b8c50b44901d80fe241e4e9f0e05a2d |}]
;;
