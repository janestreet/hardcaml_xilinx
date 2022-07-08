open Base
open Hardcaml
open Signal
module Tdpram = Xpm.Xpm_memory_tdpram

(* Block RAM - address collision behaviour.  UG573, table 1-3, common clocks.

   For a given mode on port a and b, and read/write enables on each port, what is the
   resulting value on the data out ports, and stored in memory?

   {v
   port a     port b   wea       web        doa   dob   mem
  RF/WF/NC | RF/WF/NC | 0       | 0       | OLD | OLD | NC
  RF       | RF/WF/NC | 1 (DIA) | 0       | OLD | OLD | DIA
  WF       | RF/WF/NC | 1 (DIA) | 0       | DIA | X   | DIA
  NC       | RF/WF/NC | 1 (DIA) | 0       | NC  | X   | DIA
  RF/WF/NC | RF       | 0       | 1 (DIB) | OLD | OLD | DIB
  RF/WF/NC | WF       | 0       | 1 (DIB) | X   | DIB | DIB
  RF/WF/NC | NC       | 0       | 1 (DIB) | X   | NC  | DIB
  RF/WF/NC | RF/WF/NC | 1       | 1       | X   | X   | X
v}

   RF     = Read first
   WF     = Write first
   NC     = No change
   OLD    = Old values stored in memory
   DIA/B  = Data in A or B
   X      = Invalid
   we[ab] = write when [1], read when [0]
   In all cases the addresses ports a and b are the same value.
*)

(* Ultra RAM

   These work differently. They have 2 port [a] and [b]. The RAM is "double pumped" - that
   is it works at twice the nominal clock rate and performs the [a] operation followed by
   the [b] operation.

   On a [write] operation, the output data on the same port is unchanged. Somewhat similar
   to [no_change] mode.

   Across ports, the behavior depends on the ordering of ports ie Write [a], will be
   reflected on read [b]. But not the other way round.
*)

let any t = tree ~arity:2 ~f:(reduce ~f:( |: )) (Signal.bits_msb t)

let collision_mode (arch : Ram_arch.t) : Collision_mode.t =
  match arch with
  | Distributed -> Read_before_write
  | Blockram mode -> mode
  | Ultraram -> No_change
;;

let create_xpm
      ~read_latency
      ~arch
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size
      ~byte_write_width
      ~(port_a : _ Ram_port.t)
      ~(port_b : _ Ram_port.t)
      ~cascade_height:arg_cascade_height
      ~memory_optimization:arg_memory_optimization
  =
  let byte_write_width =
    match byte_write_width with
    | Byte_write_width.B8 -> 8
    | B9 -> 9
    | Full -> width port_a.data
  in
  let module Params = struct
    include Tdpram.P

    let memory_optimization =
      match arg_memory_optimization with
      | None -> memory_optimization
      | Some false -> "false"
      | Some true -> "true"
    ;;

    let cascade_height =
      match arg_cascade_height with
      | None -> cascade_height
      | Some arg_cascade_height -> Cascade_height.to_xpm_args arg_cascade_height
    ;;

    let width = width port_a.data
    let addr_bits = Bits.address_bits_for size
    let write_data_width_a = width
    let write_data_width_b = width
    let byte_write_width_a = byte_write_width
    let byte_write_width_b = byte_write_width
    let read_data_width_a = width
    let read_data_width_b = width
    let addr_width_a = addr_bits
    let addr_width_b = addr_bits
    let memory_size = width * size
    let memory_primitive = Ram_arch.to_xpm_parameter arch
    let read_latency_a = read_latency
    let read_latency_b = read_latency
    let use_mem_init = 0
    let write_mode_a = Collision_mode.to_xpm_parameter (collision_mode arch)
    let write_mode_b = write_mode_a
  end
  in
  let write_enable_width =
    assert (width port_a.data % byte_write_width = 0);
    width port_a.data / byte_write_width
  in
  assert (read_latency > 0);
  assert (width port_a.data = width port_b.data);
  assert (Params.addr_bits = width port_a.address);
  assert (Params.addr_bits = width port_b.address);
  assert (write_enable_width = width port_a.write_enable);
  assert (write_enable_width = width port_b.write_enable);
  let module RAM = Tdpram.Make (Params) in
  let ena = any port_a.write_enable |: port_a.read_enable in
  let enb = any port_b.write_enable |: port_b.read_enable in
  let regce clock en =
    let spec = Reg_spec.create () ~clock in
    match read_latency with
    | 1 -> vdd
    | n -> pipeline spec ~enable:vdd ~n:(n - 1) en
  in
  let ram : _ RAM.O.t =
    RAM.create
      { RAM.I.clka (* Port A *) = clock_a
      ; rsta = clear_a
      ; regcea = regce clock_a port_a.read_enable
      ; ena
      ; wea = port_a.write_enable
      ; dina = port_a.data
      ; addra = port_a.address
      ; injectsbiterra = gnd
      ; injectdbiterra = gnd (* Port B *)
      ; clkb = clock_b
      ; rstb = clear_b
      ; regceb = regce clock_b port_b.read_enable
      ; enb
      ; web = port_b.write_enable
      ; dinb = port_b.data
      ; addrb = port_b.address
      ; injectsbiterrb = gnd
      ; injectdbiterrb = gnd
      ; sleep = gnd
      }
  in
  ram.douta, ram.doutb
;;

let rec output_pipe ~clock ~clear ~latency ~enable d =
  let spec = Reg_spec.create () ~clock in
  let spec_c = Reg_spec.create () ~clock ~clear in
  match latency with
  | 0 -> d
  | 1 -> reg spec_c ~enable:(reg spec ~enable:vdd enable) d
  | _ ->
    output_pipe
      ~clock
      ~clear
      ~latency:(latency - 1)
      ~enable:(reg spec ~enable:vdd enable)
      (reg spec ~enable:vdd d)
;;

(* This is very similar to rams built with [Ram.create]. The main difference is when
   modelling ultrarams. To get the correct behaviour for a write on one port and read on
   the other port, we must put port [a] into [Read_before_write] mode, and port [b] into
   [Write_before_read] mode. *)
let create_base_rtl_ram
      ~simulation_name
      ~(arch : Ram_arch.t)
      ~clock_a
      ~clock_b
      ~size
      ~(port_a : _ Ram_port.t)
      ~(port_b : _ Ram_port.t)
  =
  let reg clock enable = reg (Reg_spec.create ~clock ()) ~enable in
  let read_enable (port : _ Ram_port.t) =
    match collision_mode arch with
    | No_change -> port.read_enable &: ~:(port.write_enable)
    | Read_before_write | Write_before_read -> port.read_enable |: port.write_enable
  in
  let reg_a = reg clock_a (read_enable port_a) in
  let reg_b = reg clock_b (read_enable port_b) in
  let f_read_address, f_q =
    match arch with
    | Ultraram -> [| Fn.id; reg_b |], [| reg_a; Fn.id |]
    | Distributed | Blockram (Read_before_write | No_change) ->
      [| Fn.id; Fn.id |], [| reg_a; reg_b |]
    | Blockram Write_before_read -> [| reg_a; reg_b |], [| Fn.id; Fn.id |]
  in
  let q =
    Signal.multiport_memory
      size
      ?name:simulation_name
      ~write_ports:
        [| { write_clock = clock_a
           ; write_enable = port_a.write_enable
           ; write_address = port_a.address
           ; write_data = port_a.data
           }
         ; { write_clock = clock_b
           ; write_enable =
               (match arch with
                | Distributed -> gnd
                (* Distributed RAM will not write on port B. *)
                | Blockram _ | Ultraram -> port_b.write_enable)
           ; write_address = port_b.address
           ; write_data = port_b.data
           }
        |]
      ~read_addresses:
        (Array.map2_exn f_read_address [| port_a.address; port_b.address |] ~f:(fun f a ->
           f a))
  in
  Array.map2_exn f_q q ~f:(fun f q -> f q)
;;

let create_rtl'
      ~simulation_name
      ~read_latency
      ~arch
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size
      ~(port_a : _ Ram_port.t)
      ~(port_b : _ Ram_port.t)
  =
  assert (read_latency > 0);
  let q =
    create_base_rtl_ram ~simulation_name ~arch ~clock_a ~clock_b ~size ~port_a ~port_b
  in
  ( output_pipe
      ~clock:clock_a
      ~clear:clear_a
      ~latency:(read_latency - 1)
      ~enable:port_a.read_enable
      q.(0)
  , output_pipe
      ~clock:clock_b
      ~clear:clear_b
      ~latency:(read_latency - 1)
      ~enable:port_b.read_enable
      q.(1) )
;;

(* Instantiate the core rtl ram multiple times so that it can support byte enables.*)
let create_rtl
      ~simulation_name
      ~read_latency
      ~arch
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size
      ~(byte_write_width : Byte_write_width.t)
      ~(port_a : _ Ram_port.t)
      ~(port_b : _ Ram_port.t)
  =
  let split_port (port : _ Ram_port.t) =
    let split_port byte_width =
      let data = split_lsb ~part_width:byte_width port.data in
      let write_enable = bits_lsb port.write_enable in
      List.map2_exn data write_enable ~f:(fun data write_enable ->
        { port with data; write_enable })
    in
    match byte_write_width with
    | Full -> [ port ]
    | B8 -> split_port 8
    | B9 -> split_port 9
  in
  let qs =
    List.map2_exn (split_port port_a) (split_port port_b) ~f:(fun port_a port_b ->
      create_rtl'
        ~simulation_name
        ~read_latency
        ~arch
        ~clock_a
        ~clock_b
        ~clear_a
        ~clear_b
        ~size
        ~port_a
        ~port_b)
  in
  let qa, qb = List.unzip qs in
  concat_lsb qa, concat_lsb qb
;;

let create
      ?(read_latency = 1)
      ?(arch = Ram_arch.Blockram No_change)
      ?(byte_write_width = Byte_write_width.Full)
      ?memory_optimization
      ?cascade_height
      ?simulation_name
      ~(build_mode : Build_mode.t)
      ()
  =
  match build_mode with
  | Simulation -> create_rtl ~simulation_name ~read_latency ~arch ~byte_write_width
  | Synthesis ->
    create_xpm ~read_latency ~arch ~byte_write_width ~cascade_height ~memory_optimization
;;
