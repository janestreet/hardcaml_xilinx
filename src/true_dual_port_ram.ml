open Base
open Hardcaml
open Signal
module Tdpram = Xpm_2019_1.Xpm_memory_tdpram

type 'signal create = 'signal True_dual_port_ram_intf.create

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
  | Ultraram _ -> No_change
;;

module Size_calculations = struct
  type t =
    { size_a : int
    ; size_b : int
    ; width_a : int
    ; width_b : int
    }
  [@@deriving sexp_of]

  let create ~size ~(port_a : _ Ram_port.t) ~(port_b : _ Ram_port.t) =
    let width_a = width port_a.data in
    let width_b = width port_b.data in
    let size_a = size in
    if size_a * width_a % width_b <> 0
    then
      raise_s
        [%message
          "[size] is the number of port A words in the RAM. It must be chosen so that \
           there is an integer number of port B words in the RAM as well."
            (size_a : int)
            (width_a : int)
            (width_b : int)];
    let size_b = size_a * width_a / width_b in
    { width_a; width_b; size_a; size_b }
  ;;
end

let create_xpm
  ?scope
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
  ~clocking_mode:arg_clocking_mode
  ()
  =
  let byte_write_width (port : _ Ram_port.t) =
    match byte_write_width with
    | Byte_write_width.B8 -> 8
    | B9 -> 9
    | Full -> width port.data
  in
  let { Size_calculations.size_a; size_b; width_a; width_b } =
    Size_calculations.create ~size ~port_a ~port_b
  in
  let addr_bits_a = Bits.address_bits_for size_a in
  let addr_bits_b = Bits.address_bits_for size_b in
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

    let clocking_mode = Clocking_mode.to_xpm_args arg_clocking_mode
    let write_data_width_a = width_a
    let write_data_width_b = width_b
    let byte_write_width_a = byte_write_width port_a
    let byte_write_width_b = byte_write_width port_b
    let read_data_width_a = width_a
    let read_data_width_b = width_b
    let addr_width_a = addr_bits_a
    let addr_width_b = addr_bits_b
    let memory_size = width_a * size_a
    let memory_primitive = Ram_arch.to_xpm_parameter arch
    let read_latency_a = read_latency
    let read_latency_b = read_latency
    let use_mem_init = 0
    let write_mode_a = Collision_mode.to_xpm_parameter (collision_mode arch)
    let write_mode_b = write_mode_a
  end
  in
  let write_enable_width (port : _ Ram_port.t) =
    let byte_write_width = byte_write_width port in
    assert (width port.data % byte_write_width = 0);
    width port.data / byte_write_width
  in
  assert (read_latency > 0);
  assert (Params.addr_width_a = width port_a.address);
  assert (Params.addr_width_b = width port_b.address);
  assert (write_enable_width port_a = width port_a.write_enable);
  assert (write_enable_width port_b = width port_b.write_enable);
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
    let module Hier = Hierarchy.In_scope (RAM.I) (RAM.O) in
    let ram_create_wrapper =
      match arch, scope with
      | Ultraram Wrap_in_module_with_keep_directives, Some scope ->
        let keep s = add_attribute s (Rtl_attribute.Vivado.keep true) in
        Hier.hierarchical ~name:"xpm_tdpram_wrapper" ~scope (fun _ i ->
          RAM.create (RAM.I.map i ~f:keep) |> RAM.O.map ~f:keep)
      | Ultraram Wrap_in_module_with_keep_directives, None ->
        raise_s
          [%message
            "You must provide a scope for [Ultraram Wrap_in_module_with_keep_directives]"]
      | _ -> fun i -> RAM.create i
    in
    ram_create_wrapper
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
    | No_change -> port.read_enable &: (port.write_enable ==:. 0)
    | Read_before_write | Write_before_read ->
      port.read_enable |: (port.write_enable <>:. 0)
  in
  let reg_a = reg clock_a (read_enable port_a) in
  let reg_b = reg clock_b (read_enable port_b) in
  let f_read_address, f_q =
    match arch with
    | Ultraram _ -> [| Fn.id; reg_b |], [| reg_a; Fn.id |]
    | Distributed | Blockram (Read_before_write | No_change) ->
      [| Fn.id; Fn.id |], [| reg_a; reg_b |]
    | Blockram Write_before_read -> [| reg_a; reg_b |], [| Fn.id; Fn.id |]
  in
  let q =
    multiport_memory
      ~enable_modelling_features:true
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
                | Blockram _ | Ultraram _ -> port_b.write_enable)
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

(* Address collisions only occur for [Blockram (No_change|Write_before_read)]. We also
   dont attempt to model anything if we are not using common clocks. This is fine for
   cyclesim as it's what it supports. For the event driven simulator, I dont think you can
   model this at the RTL level anyway - look at the Xilinx Unisim BRAM implementations -
   they are behavioural. *)
let resolve_address_collision_model
  ~(clocking_mode : Clocking_mode.t)
  ~(arch : Ram_arch.t)
  ~(address_collision_model : Address_collision.Model.t)
  =
  match clocking_mode with
  | Independent_clock -> Address_collision.Model.None__there_be_dragons
  | Common_clock ->
    (match arch with
     | Blockram (No_change | Write_before_read) -> address_collision_model
     | Blockram Read_before_write | Distributed | Ultraram _ -> None__there_be_dragons)
;;

let model_common_clock_address_collisions
  ~address_collision_model
  ~clocking_mode
  ~arch
  ~clock
  ~read_latency
  ~(port_a : _ Ram_port.t)
  ~(port_b : _ Ram_port.t)
  ~qa
  ~qb
  =
  let address_collision_model =
    resolve_address_collision_model ~clocking_mode ~arch ~address_collision_model
  in
  let spec = Reg_spec.create ~clock () in
  let pipe = pipeline spec ~n:read_latency in
  let address_collision =
    let compare_top a b =
      let wa, wb = width a, width b in
      let width = Int.min wa wb in
      sel_top ~width a ==: sel_top b ~width
    in
    compare_top port_a.address port_b.address
  in
  let address_collision_reg = pipe address_collision in
  let port_a_reg = Ram_port.map port_a ~f:pipe in
  let port_b_reg = Ram_port.map port_b ~f:pipe in
  let address_collision_a =
    address_collision_reg &: (port_b_reg.write_enable <>:. 0) &: port_a_reg.read_enable
  in
  let address_collision_b =
    address_collision_reg &: (port_a_reg.write_enable <>:. 0) &: port_b_reg.read_enable
  in
  ( Address_collision.Model.q
      address_collision_model
      spec
      ~address_collision:address_collision_a
      qa
  , Address_collision.Model.q
      address_collision_model
      spec
      ~address_collision:address_collision_b
      qb )
;;

let create_rtl_with_collision_model
  ~address_collision_model
  ~simulation_name
  ~clocking_mode
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
  let qa, qb =
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
  in
  model_common_clock_address_collisions
    ~address_collision_model
    ~clocking_mode
    ~arch
    ~clock:clock_a
    ~read_latency
    ~port_a
    ~port_b
    ~qa
    ~qb
;;

let raise_port_widths_not_exact_multiple max_width min_width =
  raise_s
    [%message
      "max port data width must be an exact integer multiple of min port data width. \
       (update the simulation model if something else is required)"
        (max_width : int)
        (min_width : int)]
;;

let raise_ratio_between_port_widths_not_power_of_2 scale =
  raise_s
    [%message
      "ratio between port widths must be a power of 2. (update the simulation model if \
       non-power-of-2 scale is required)"
        (scale : int)]
;;

let raise_port_width_not_divisible_by_byte_size ~width_a ~width_b ~byte_write_width =
  raise_s
    [%message
      "port widths must be divisible by the byte length if using byte enables."
        (width_a : int)
        (width_b : int)
        (byte_write_width : Byte_write_width.t)]
;;

let raise_invalid_read_latency read_latency =
  raise_s [%message "Read latency must be greater than 0" (read_latency : int)]
;;

let create_rtl
  ~address_collision_model
  ~clocking_mode
  ~simulation_name
  ~read_latency
  ~(arch : Ram_arch.t)
  ~clock_a
  ~clock_b
  ~clear_a
  ~clear_b
  ~size
  ~(byte_write_width : Byte_write_width.t)
  ~(port_a : _ Ram_port.t)
  ~(port_b : _ Ram_port.t)
  =
  let { Size_calculations.size_a; size_b; width_a; width_b } =
    Size_calculations.create ~size ~port_a ~port_b
  in
  let min_width = Int.min width_a width_b in
  let max_width = Int.max width_a width_b in
  if max_width % min_width <> 0
  then raise_port_widths_not_exact_multiple max_width min_width;
  let scale = max_width / min_width in
  if not (Int.is_pow2 scale) then raise_ratio_between_port_widths_not_power_of_2 scale;
  (match byte_write_width with
   | B9 ->
     if not (max_width % 9 = 0 && min_width % 9 = 0)
     then raise_port_width_not_divisible_by_byte_size ~width_a ~width_b ~byte_write_width
   | B8 ->
     if not (max_width % 8 = 0 && min_width % 8 = 0)
     then raise_port_width_not_divisible_by_byte_size ~width_a ~width_b ~byte_write_width
   | Full -> ());
  if read_latency < 1 then raise_invalid_read_latency read_latency;
  let qa, qb =
    (* The width of the resulting RAM is the wider of the two port widths. So, the depth
       is given by the corresponding depth, which will be the smaller of the two sizes. *)
    let min_size = Int.min size_a size_b in
    create_rtl_with_collision_model
      ~address_collision_model
      ~clocking_mode
      ~simulation_name
      ~read_latency
      ~arch
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size:min_size
      ~port_a
      ~port_b
  in
  qa, qb
;;

let create
  ?scope
  ?(address_collision_model = Address_collision.Model.None__there_be_dragons)
  ?(read_latency = 1)
  ?(arch = Ram_arch.Blockram No_change)
  ?(byte_write_width = Byte_write_width.Full)
  ?memory_optimization
  ?cascade_height
  ?clocking_mode
  ?simulation_name
  ~(build_mode : Build_mode.t)
  ()
  ~clock_a
  ~clock_b
  ~clear_a
  ~clear_b
  ~size
  ~port_a
  ~port_b
  =
  let clocking_mode =
    Option.value
      clocking_mode
      ~default:
        (if Signal.Type.Uid.equal (Signal.uid clock_a) (Signal.uid clock_b)
         then Clocking_mode.Common_clock
         else Clocking_mode.Independent_clock)
  in
  match build_mode with
  | Simulation ->
    create_rtl
      ~address_collision_model
      ~clocking_mode
      ~simulation_name
      ~read_latency
      ~arch
      ~byte_write_width
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size
      ~port_a
      ~port_b
  | Synthesis ->
    create_xpm
      ?scope
      ~read_latency
      ~arch
      ~byte_write_width
      ~cascade_height
      ~memory_optimization
      ~clocking_mode
      ~clock_a
      ~clock_b
      ~clear_a
      ~clear_b
      ~size
      ~port_a
      ~port_b
      ()
;;

module Clocked = struct
  let create
    ?scope
    ?address_collision_model
    ?read_latency
    ?arch
    ?byte_write_width
    ?memory_optimization
    ?cascade_height
    ?clocking_mode
    ?simulation_name
    ~(build_mode : Build_mode.t)
    ()
    ~clock_a
    ~clock_b
    ~clear_a
    ~clear_b
    ~size
    ~port_a
    ~port_b
    =
    let dom_a = Clocked_signal.get_domain clock_a in
    let dom_b = Clocked_signal.get_domain clock_b in
    let qa, qb =
      create
        ?scope
        ?address_collision_model
        ?read_latency
        ?arch
        ?byte_write_width
        ?memory_optimization
        ?cascade_height
        ?clocking_mode
        ?simulation_name
        ~build_mode:(build_mode : Build_mode.t)
        ()
        ~clock_a:(Clocked_signal.unwrap_signal ~dom:dom_a clock_a)
        ~clock_b:(Clocked_signal.unwrap_signal ~dom:dom_b clock_b)
        ~clear_a:(Clocked_signal.unwrap_signal ~dom:dom_a clear_a)
        ~clear_b:(Clocked_signal.unwrap_signal ~dom:dom_b clear_b)
        ~size
        ~port_a:(Ram_port.map port_a ~f:(Clocked_signal.unwrap_signal ~dom:dom_a))
        ~port_b:(Ram_port.map port_b ~f:(Clocked_signal.unwrap_signal ~dom:dom_b))
    in
    Clocked_signal.to_clocked ~dom:dom_a qa, Clocked_signal.to_clocked ~dom:dom_b qb
  ;;
end
