open Base
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_xilinx

module Data = struct
  type 'a t =
    { bar : 'a [@bits 8]
    ; foo : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

let memory_config =
  { Memory_builder.Config.underlying_memories =
      [ { data_width = 7
        ; how_to_instantiate_ram = Xpm Ultraram
        ; cascade_height = Specified 1
        ; simulation_name = None
        }
      ; { data_width = 7
        ; how_to_instantiate_ram = Xpm Ultraram
        ; cascade_height = Specified 1
        ; simulation_name = None
        }
      ; { data_width = 3
        ; how_to_instantiate_ram = Xpm Ultraram
        ; cascade_height = Specified 1
        ; simulation_name = None
        }
      ; { data_width = 15
        ; how_to_instantiate_ram = Xpm Ultraram
        ; cascade_height = Specified 1
        ; simulation_name = None
        }
      ]
  ; underlying_ram_read_latency = 1
  ; vertical_dimension = 256
  ; horizontal_dimension = 2
  ; combinational_output = false
  }
;;

module Config = (val Memory_builder.Config.as_module memory_config)
module Write_port = Memory_builder.Write_port_2d.Specialize_with_config (Data) (Config)
module Read_port = Memory_builder.Read_port_2d.Specialize_with_config (Config)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; write_port : 'a Write_port.t [@rtlprefix "wr_"]
    ; read_port : 'a Read_port.t [@rtlprefix "rd_"]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { read_data : 'a Data.t [@rtlprefix "rd_"] } [@@deriving hardcaml]
end

let create scope (i : _ I.t) =
  let data_memory =
    let open Memory_builder.Create (Data) in
    create
      ~instance:"memory"
      ~build_mode:Simulation
      ~config:memory_config
      ~scope
      ~clock:i.clock
      ~clear:i.clear
      ()
  in
  Memory_builder.set_write_port_2d data_memory A i.write_port;
  let read_data = Memory_builder.set_read_port_2d data_memory B i.read_port in
  Memory_builder.complete data_memory;
  { O.read_data }
;;

module Sim = Hardcaml.Cyclesim.With_interface (I) (O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (create scope) in
  Hardcaml_waveterm.Waveform.create sim
;;

let ( @<--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test ~write_vectors ~read_indices =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  List.iter write_vectors ~f:(fun (vertical_index, write_vector) ->
    List.iter2_exn inputs.write_port.data write_vector ~f:(fun dst (src : _ Data.t) ->
      dst.foo := src.foo;
      dst.bar := src.bar);
    inputs.write_port.enable := Bits.vdd;
    inputs.write_port.vertical_index @<--. vertical_index;
    Cyclesim.cycle sim);
  inputs.write_port.enable := Bits.gnd;
  List.iter read_indices ~f:(fun (horizontal_index, vertical_index) ->
    inputs.read_port.enable := Bits.vdd;
    inputs.read_port.horizontal_index @<--. horizontal_index;
    inputs.read_port.vertical_index @<--. vertical_index;
    Cyclesim.cycle sim);
  inputs.read_port.enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print ~display_height:40 ~wave_width:2 waves
;;

let%expect_test "" =
  let i8 = Bits.of_int ~width:8 in
  let write_vectors =
    [ 0, [ { Data.bar = i8 0xAB; foo = i8 0xCD }; { Data.bar = i8 0x12; foo = i8 0x34 } ]
    ; 1, [ { Data.bar = i8 0xEE; foo = i8 0xFF }; { Data.bar = i8 0x56; foo = i8 0x78 } ]
    ]
  in
  let read_indices = [ 0, 0; 0, 1; 1, 0; 1, 1 ] in
  test ~write_vectors ~read_indices;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │rd_enable      ││            ┌───────────────────────┐              │
    │               ││────────────┘                       └──────────────│
    │rd_horizontal_i││                        ┌──────────────────────────│
    │               ││────────────────────────┘                          │
    │               ││──────────────────┬─────┬─────┬────────────────────│
    │rd_vertical_ind││ 00               │01   │00   │01                  │
    │               ││──────────────────┴─────┴─────┴────────────────────│
    │               ││──────┬────────────────────────────────────────────│
    │wr_bar0        ││ AB   │EE                                          │
    │               ││──────┴────────────────────────────────────────────│
    │               ││──────┬────────────────────────────────────────────│
    │wr_bar1        ││ 12   │56                                          │
    │               ││──────┴────────────────────────────────────────────│
    │wr_enable      ││────────────┐                                      │
    │               ││            └──────────────────────────────────────│
    │               ││──────┬────────────────────────────────────────────│
    │wr_foo0        ││ CD   │FF                                          │
    │               ││──────┴────────────────────────────────────────────│
    │               ││──────┬────────────────────────────────────────────│
    │wr_foo1        ││ 34   │78                                          │
    │               ││──────┴────────────────────────────────────────────│
    │               ││──────┬────────────────────────────────────────────│
    │wr_vertical_ind││ 00   │01                                          │
    │               ││──────┴────────────────────────────────────────────│
    │               ││────────────────────────┬─────┬─────┬─────┬────────│
    │rd_bar         ││ 00                     │AB   │EE   │12   │56      │
    │               ││────────────────────────┴─────┴─────┴─────┴────────│
    │               ││────────────────────────┬─────┬─────┬─────┬────────│
    │rd_foo         ││ 00                     │CD   │FF   │34   │78      │
    │               ││────────────────────────┴─────┴─────┴─────┴────────│
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
