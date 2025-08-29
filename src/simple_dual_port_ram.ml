open Base
open Hardcaml
open Signal

let create
  ?scope
  ?(address_collision_protection = Address_collision.Protection.None__there_be_dragons)
  ?(address_collision_model = Address_collision.Model.None__there_be_dragons)
  ?(read_latency = 1)
  ?(arch = Ram_arch.Blockram No_change)
  ?(byte_write_width = Byte_write_width.Full)
  ?memory_optimization
  ?cascade_height
  ?simulation_name
  ~(build_mode : Build_mode.t)
  ()
  ~clock
  ~clear
  ~size
  ~write_address
  ~write_enable
  ~data
  ~read_address
  ~read_enable
  =
  assert (read_latency >= 1);
  let create ~port_a ~port_b =
    Dual_port_ram.create
      ?scope
      ~address_collision_model:
        (match build_mode with
         | Simulation -> address_collision_model
         | Synthesis -> None__there_be_dragons)
      ~read_latency
      ~arch
      ?memory_optimization
      ?cascade_height
      ?simulation_name
      ~byte_write_width
      ~build_mode
      ()
      ~clock
      ~clear
      ~size
      ~port_a
      ~port_b
  in
  let zero_write_enable =
    match byte_write_width with
    | Full -> gnd
    | B8 ->
      assert (width data % 8 = 0);
      zero (width data / 8)
    | B9 ->
      assert (width data % 9 = 0);
      zero (width data / 9)
  in
  let spec = Reg_spec.create ~clock () in
  let address_collision = write_address ==: read_address in
  let address_collision_reg = pipeline spec ~n:read_latency address_collision in
  match arch, address_collision_protection with
  | (Distributed | Ultraram _ | Blockram Read_before_write), _ | _, None__there_be_dragons
    ->
    let _, q =
      create
        ~port_a:
          { Ram_port.write_enable; address = write_address; read_enable = gnd; data }
        ~port_b:
          { Ram_port.write_enable = zero_write_enable
          ; address = read_address
          ; read_enable
          ; data = zero (width data)
          }
    in
    q
  | Blockram Write_before_read, Control_enables ->
    let _, q =
      let write_enable_a = mux2 address_collision zero_write_enable write_enable in
      let write_enable_b = mux2 address_collision write_enable zero_write_enable in
      create
        ~port_a:
          { Ram_port.write_enable = write_enable_a
          ; address = write_address
          ; read_enable = gnd
          ; data
          }
        ~port_b:
          { Ram_port.write_enable = write_enable_b
          ; address = read_address
          ; read_enable
          ; data
          }
    in
    q
  | Blockram No_change, Control_enables ->
    let _, q =
      let read_enable = mux2 (address_collision &: write_enable) gnd read_enable in
      create
        ~port_a:
          { Ram_port.write_enable; address = write_address; read_enable = gnd; data }
        ~port_b:
          { Ram_port.write_enable = zero_write_enable
          ; address = read_address
          ; read_enable
          ; data
          }
    in
    q
  | Blockram (Write_before_read | No_change), Mux_output_ports ->
    let qa, qb =
      create
        ~port_a:{ Ram_port.write_enable; address = write_address; read_enable; data }
        ~port_b:
          { Ram_port.write_enable = zero_write_enable
          ; address = read_address
          ; read_enable
          ; data
          }
    in
    mux2 address_collision_reg qa qb
;;
