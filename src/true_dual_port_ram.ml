open! Import
open Signal
module Tdpram = Xpm.Xpm_memory_tdpram


let any t = tree ~arity:2 ~f:(reduce ~f:( |: )) (Signal.bits_msb t)

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
  =
  let byte_write_width =
    match byte_write_width with
    | Byte_write_width.B8 -> 8
    | B9 -> 9
    | Full -> width port_a.data
  in
  let module Params = struct
    include Tdpram.P

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
    let memory_primitive = Ram_arch.to_string arch
    let read_latency_a = read_latency
    let read_latency_b = read_latency
    let use_mem_init = 0
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

let create_rtl'
      ~read_latency
      ~arch:_
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
    Ram.create
      ~collision_mode:Read_before_write
      ~size
      ~write_ports:
        [| { write_clock = clock_a
           ; write_enable = port_a.write_enable
           ; write_address = port_a.address
           ; write_data = port_a.data
           }
         ; { write_clock = clock_b
           ; write_enable = port_b.write_enable
           ; write_address = port_b.address
           ; write_data = port_b.data
           }
        |]
      ~read_ports:
        [| { read_clock = clock_a
           ; read_enable = port_a.read_enable |: port_a.write_enable
           ; read_address = port_a.address
           }
         ; { read_clock = clock_b
           ; read_enable = port_b.read_enable |: port_b.write_enable
           ; read_address = port_b.address
           }
        |]
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

let create_rtl
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
      ?(arch = Ram_arch.Rtl)
      ?(byte_write_width = Byte_write_width.Full)
      ()
  =
  match arch with
  | Rtl -> create_rtl ~read_latency ~arch ~byte_write_width
  | _ -> create_xpm ~read_latency ~arch ~byte_write_width
;;
