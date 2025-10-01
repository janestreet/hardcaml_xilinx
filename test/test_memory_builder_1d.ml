open Base
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_xilinx

module Data = struct
  module T = struct
    type 'a t =
      { foo : 'a [@bits 8]
      ; bar : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  include T

  let num_bits = fold ~f:( + ) ~init:0 port_widths
end

module Make (The_config : Memory_builder.Config.S) = struct
  let memory_config = The_config.t

  module Config = (val Memory_builder.Config.as_module memory_config)
  module Write_port = Memory_builder.Write_port_1d.Specialize_with_config (Data) (Config)
  module Read_port = Memory_builder.Read_port_1d.Specialize_with_config (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_port : 'a Write_port.t [@rtlprefix "wr_"]
      ; read_port : 'a Read_port.t [@rtlprefix "rd_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t = { read_data : 'a Data.t [@rtlprefix "rd_"] }
    [@@deriving hardcaml ~rtlmangle:false]
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
    Memory_builder.set_write_port_1d data_memory A i.write_port;
    let read_data = Memory_builder.set_read_port_1d data_memory A i.read_port in
    Memory_builder.complete data_memory;
    { O.read_data }
  ;;

  module Sim = Hardcaml.Cyclesim.With_interface (I) (O)

  let create_sim () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Sim.create (create scope) in
    Hardcaml_waveterm.Waveform.create sim
  ;;

  let test () =
    let waves, sim = create_sim () in
    let inputs = Cyclesim.inputs sim in
    inputs.write_port.data.foo := Bits.of_int_trunc ~width:8 0xAA;
    inputs.write_port.data.bar := Bits.of_int_trunc ~width:8 0xBB;
    inputs.write_port.enable := Bits.vdd;
    inputs.write_port.address := Bits.of_int_trunc ~width:8 0xFF;
    Cyclesim.cycle sim;
    inputs.write_port.enable := Bits.gnd;
    inputs.read_port.enable := Bits.vdd;
    inputs.read_port.address := Bits.of_int_trunc ~width:8 0xFF;
    Cyclesim.cycle sim;
    inputs.read_port.enable := Bits.gnd;
    Cyclesim.cycle sim;
    Waveform.print waves
  ;;
end

module%test [@name "a single ultraram"] _ = struct
  let memory_config =
    Memory_builder.Config.create_simple_1d_config
      ~how_to_instantiate_ram:(Xpm (Ultraram Let_vivado_decide))
      ~depth:256
      ~num_bits_per_entry:Data.num_bits
      ~ram_read_latency:1
      ~simulation_name:None
  ;;

  include Make ((val Memory_builder.Config.as_module memory_config))

  let%expect_test "" =
    test ();
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────┬───────────────                           │
      │rd_address     ││ 00     │FF                                        │
      │               ││────────┴───────────────                           │
      │rd_enable      ││        ┌───────┐                                  │
      │               ││────────┘       └───────                           │
      │               ││────────────────────────                           │
      │wr_address     ││ FF                                                │
      │               ││────────────────────────                           │
      │               ││────────────────────────                           │
      │wr_bar         ││ BB                                                │
      │               ││────────────────────────                           │
      │wr_enable      ││────────┐                                          │
      │               ││        └───────────────                           │
      │               ││────────────────────────                           │
      │wr_foo         ││ AA                                                │
      │               ││────────────────────────                           │
      │               ││────────────────┬───────                           │
      │rd_bar         ││ 00             │BB                                │
      │               ││────────────────┴───────                           │
      │               ││────────────────┬───────                           │
      │rd_foo         ││ 00             │AA                                │
      │               ││────────────────┴───────                           │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end

module%test [@name "3 single ultraram"] _ = struct
  let memory_config =
    { Memory_builder.Config.underlying_memories =
        [ { data_width = 7
          ; how_to_instantiate_ram = Xpm (Ultraram Let_vivado_decide)
          ; cascade_height = Specified 1
          ; simulation_name = None
          }
        ; { data_width = 7
          ; how_to_instantiate_ram = Xpm (Ultraram Let_vivado_decide)
          ; cascade_height = Specified 1
          ; simulation_name = None
          }
        ; { data_width = 2
          ; how_to_instantiate_ram = Xpm (Ultraram Let_vivado_decide)
          ; cascade_height = Specified 1
          ; simulation_name = None
          }
        ]
    ; underlying_ram_read_latency = 1
    ; vertical_dimension = 256
    ; horizontal_dimension = 1
    ; combinational_output = true
    }
  ;;

  include Make ((val Memory_builder.Config.as_module memory_config))

  let%expect_test "" =
    test ();
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────┬───────────────                           │
      │rd_address     ││ 00     │FF                                        │
      │               ││────────┴───────────────                           │
      │rd_enable      ││        ┌───────┐                                  │
      │               ││────────┘       └───────                           │
      │               ││────────────────────────                           │
      │wr_address     ││ FF                                                │
      │               ││────────────────────────                           │
      │               ││────────────────────────                           │
      │wr_bar         ││ BB                                                │
      │               ││────────────────────────                           │
      │wr_enable      ││────────┐                                          │
      │               ││        └───────────────                           │
      │               ││────────────────────────                           │
      │wr_foo         ││ AA                                                │
      │               ││────────────────────────                           │
      │               ││────────────────┬───────                           │
      │rd_bar         ││ 00             │BB                                │
      │               ││────────────────┴───────                           │
      │               ││────────────────┬───────                           │
      │rd_foo         ││ 00             │AA                                │
      │               ││────────────────┴───────                           │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end
