open Base
open Hardcaml

module For_deriving = struct
  module type Sexp_of_m = sig end
end

module Config = struct
  type inferred_memory =
    { rtl_attributes : Rtl_attribute.t list option
    ; rw_order : [ `Wbr | `Rbw ]
    }
  [@@deriving sexp_of]

  type how_to_instantiate_ram =
    | Xpm of Ram_arch.t
    | Inferred of inferred_memory
  [@@deriving sexp_of]

  type underlying_memory =
    { data_width : int
    ; cascade_height : Cascade_height.t
    ; how_to_instantiate_ram : how_to_instantiate_ram
    ; simulation_name : string option
    }
  [@@deriving sexp_of, fields ~getters]

  type t =
    { underlying_memories : underlying_memory list
    ; underlying_ram_read_latency : int
    ; vertical_dimension : int
    ; horizontal_dimension : int
    ; combinational_output : bool
    }
  [@@deriving sexp_of, fields ~getters]

  let create_simple_1d_config
    ~depth
    ~num_bits_per_entry
    ~ram_read_latency
    ~how_to_instantiate_ram
    ~simulation_name
    =
    { underlying_memories =
        [ { how_to_instantiate_ram
          ; cascade_height = Cascade_height.Specified 1
          ; data_width = num_bits_per_entry
          ; simulation_name
          }
        ]
    ; underlying_ram_read_latency = ram_read_latency
    ; vertical_dimension = depth
    ; horizontal_dimension = 1
    ; combinational_output = true
    }
  ;;

  let log2_vertical_dimension (t : t) = Int.ceil_log2 t.vertical_dimension
  let log2_horizontal_dimension (t : t) = Int.ceil_log2 t.horizontal_dimension
  let vertical_index_width (t : t) = Int.max 1 (log2_vertical_dimension t)
  let horizontal_index_width (t : t) = Int.max 1 (log2_horizontal_dimension t)

  let read_latency (t : t) =
    t.underlying_ram_read_latency + if t.combinational_output then 0 else 1
  ;;

  let underlying_memories_data_width (t : t) =
    List.sum (module Int) t.underlying_memories ~f:data_width
  ;;

  module type S = sig
    val t : t
  end

  let as_module t : (module S) =
    (module struct
      let t = t
    end)
  ;;
end

let instantiate_underlying_memory
  ~clock
  ~clear
  ~scope
  ~build_mode
  ~write_address_a
  ~write_enable_a
  ~write_data_a
  ~write_address_b
  ~write_enable_b
  ~write_data_b
  ~read_address_a
  ~read_address_b
  ~read_enable_a
  ~read_enable_b
  ~(config : Config.t)
  =
  let ( -- ) = Scope.naming scope in
  if Signal.width write_data_a <> Signal.width write_data_b
  then raise_s [%message "write_data on both ports must be the same width"];
  let get_write_segment write_data =
    let pos = ref 0 in
    fun width ->
      let ret = write_data.Signal.:[!pos + width - 1, !pos] in
      pos := !pos + width;
      ret
  in
  let get_write_segment_a = get_write_segment write_data_a in
  let get_write_segment_b = get_write_segment write_data_b in
  let read_datas =
    let address_a =
      Signal.mux2 write_enable_a write_address_a read_address_a -- "address_a"
    in
    let address_b =
      Signal.mux2 write_enable_b write_address_b read_address_b -- "address_b"
    in
    List.map config.underlying_memories ~f:(fun entry ->
      let simulation_name = Option.map ~f:(Scope.name scope) entry.simulation_name in
      match entry.how_to_instantiate_ram with
      | Xpm arch ->
        Dual_port_ram.create
          ?simulation_name
          ~address_collision_model:Counter
          ~clock
          ~clear
          ~arch
          ~read_latency:config.underlying_ram_read_latency
          ~cascade_height:entry.cascade_height
          ~build_mode
          ~size:(1 lsl Signal.width read_address_a)
          ~port_a:
            { address = address_a
            ; data = get_write_segment_a entry.data_width
            ; read_enable = read_enable_a
            ; write_enable = write_enable_a
            }
          ~port_b:
            { address = address_b
            ; data = get_write_segment_b entry.data_width
            ; read_enable = read_enable_b
            ; write_enable = write_enable_b
            }
          ()
      | Inferred { rtl_attributes; rw_order } ->
        let create =
          match rw_order with
          | `Rbw -> Signal.ram_rbw
          | `Wbr -> Signal.ram_wbr
        in
        let rdata =
          create
            ?name:simulation_name
            ?attributes:rtl_attributes
            ~write_port:
              { write_clock = clock
              ; write_address = address_a
              ; write_enable = write_enable_a
              ; write_data = write_data_a
              }
            ~read_port:
              { read_clock = clock
              ; read_address = address_b
              ; read_enable = read_enable_b
              }
            (1 lsl Signal.width read_address_b)
          |> Signal.pipeline
               (Reg_spec.create ~clock ())
               ~n:(config.underlying_ram_read_latency - 1)
        in
        Signal.zero (Signal.width rdata), rdata)
  in
  ( Signal.concat_msb (List.rev_map ~f:fst read_datas)
  , Signal.concat_msb (List.rev_map ~f:snd read_datas) )
;;

let validate_index_width name f config outer_index =
  let expected_width = f config in
  let failed =
    if expected_width = 0
    then Signal.width outer_index > 1
    else Signal.width outer_index <> expected_width
  in
  if failed
  then (
    let error = Printf.sprintf "%s index does not match expected width" name in
    raise_s
      [%message
        error ~actual_width:(Signal.width outer_index : int) (expected_width : int)])
;;

let validate_horizontal_index_width =
  validate_index_width "horizontal" Config.log2_horizontal_dimension
;;

let validate_vertical_index_width =
  validate_index_width "vertical" Config.log2_vertical_dimension
;;

module type Widths_1d = sig
  val address_width : int
end

module Read_port_1d = struct
  module Pre = struct
    type 'a t =
      { address : 'a
      ; enable : 'a
      }
    [@@deriving hardcaml]
  end

  include Pre

  module type S = Hardcaml.Interface.S with type 'a t = 'a t

  module Specialize (X : Widths_1d) = struct
    module Pre = struct
      include Pre

      let port_names_and_widths =
        { address = "address", X.address_width; enable = port_names_and_widths.enable }
      ;;
    end

    include Pre
    include Hardcaml.Interface.Make (Pre)
  end

  module Specialize_with_config (The_config : Config.S) = Specialize (struct
      let address_width = Config.vertical_index_width The_config.t
    end)
end

module Read_port_2d = struct
  module Pre = struct
    type 'a t =
      { horizontal_index : 'a
      ; vertical_index : 'a
      ; enable : 'a
      }
    [@@deriving hardcaml]
  end

  type 'a t = 'a Pre.t =
    { horizontal_index : 'a
    ; vertical_index : 'a
    ; enable : 'a
    }
  [@@deriving sexp_of]

  module Specialize_with_config (The_config : Config.S) = struct
    let config = The_config.t

    module Pre = struct
      include Pre

      let port_names_and_widths =
        { horizontal_index = "horizontal_index", Config.horizontal_index_width config
        ; vertical_index = "vertical_index", Config.vertical_index_width config
        ; enable = port_names_and_widths.enable
        }
      ;;
    end

    include Pre
    include Hardcaml.Interface.Make (Pre)
  end
end

module type Widths_2d = sig
  val vertical_index_width : int
  val horizontal_dimension : int
end

module Write_port_1d = struct
  type ('a, 'write_data) t =
    { address : 'a
    ; enable : 'a
    ; data : 'write_data
    }
  [@@deriving sexp_of]

  module type S = sig
    type 'a write_data

    include Hardcaml.Interface.S with type 'a t = ('a, 'a write_data) t
  end

  module Specialize (M : Hardcaml.Interface.S) (X : Widths_1d) = struct
    module Repr = struct
      type 'a t =
        { address : 'a [@bits X.address_width]
        ; enable : 'a [@bits 1]
        ; data : 'a M.t
        }
      [@@deriving hardcaml]
    end

    module Pre = struct
      type nonrec 'a t = ('a, 'a M.t) t

      let to_repr t : _ Repr.t =
        { Repr.address = t.address; enable = t.enable; data = t.data }
      ;;

      let of_repr (repr : _ Repr.t) : _ t =
        { address = repr.address; enable = repr.enable; data = repr.data }
      ;;

      let sexp_of_t sexp_of_a t = Repr.sexp_of_t sexp_of_a (to_repr t)
      let map t ~f = of_repr (Repr.map (to_repr t) ~f)
      let iter t ~f = Repr.iter (to_repr t) ~f
      let to_list t = Repr.to_list (to_repr t)
      let map2 a b ~f = of_repr (Repr.map2 (to_repr a) (to_repr b) ~f)
      let iter2 a b ~f = Repr.iter2 (to_repr a) (to_repr b) ~f
      let port_names_and_widths = of_repr Repr.port_names_and_widths
    end

    include Pre
    include Interface.Make (Pre)
  end

  module Specialize_with_config (M : Hardcaml.Interface.S) (X : Config.S) =
    Specialize
      (M)
      (struct
        let address_width = Config.vertical_index_width X.t
      end)

  module M (M : T1) = struct
    type nonrec 'a t = ('a, 'a M.t) t

    module type S = S with type 'a write_data := 'a M.t
  end

  let sexp_of_m__t (module _ : For_deriving.Sexp_of_m) sexp_of_a t =
    [%sexp_of: (a, _) t] t
  ;;
end

module Write_port_2d = struct
  type ('a, 'write_data) t =
    { vertical_index : 'a
    ; enable : 'a
    ; data : 'write_data list
    }
  [@@deriving sexp_of]

  let map (t : _ t) ~f_write_data ~f =
    { vertical_index = f t.vertical_index
    ; enable = f t.enable
    ; data = List.map t.data ~f:f_write_data
    }
  ;;

  let and_enable ~with_ t =
    let open Signal in
    { t with enable = t.enable &: with_ }
  ;;

  module type S = sig
    type 'a write_data

    include Hardcaml.Interface.S with type 'a t = ('a, 'a write_data) t
  end

  module Specialize (M : Hardcaml.Interface.S) (X : Widths_2d) = struct
    module Repr = struct
      type 'a t =
        { vertical_index : 'a [@bits X.vertical_index_width]
        ; enable : 'a [@bits 1]
        ; data : 'a M.t list [@length X.horizontal_dimension]
        }
      [@@deriving hardcaml]
    end

    module Pre = struct
      type nonrec 'a t = ('a, 'a M.t) t

      let to_repr t : _ Repr.t =
        { Repr.vertical_index = t.vertical_index; enable = t.enable; data = t.data }
      ;;

      let of_repr (repr : _ Repr.t) : _ t =
        { vertical_index = repr.vertical_index; enable = repr.enable; data = repr.data }
      ;;

      let sexp_of_t sexp_of_a t = Repr.sexp_of_t sexp_of_a (to_repr t)
      let map t ~f = of_repr (Repr.map (to_repr t) ~f)
      let iter t ~f = Repr.iter (to_repr t) ~f
      let to_list t = Repr.to_list (to_repr t)
      let map2 a b ~f = of_repr (Repr.map2 (to_repr a) (to_repr b) ~f)
      let iter2 a b ~f = Repr.iter2 (to_repr a) (to_repr b) ~f
      let port_names_and_widths = of_repr Repr.port_names_and_widths
    end

    include Pre
    include Interface.Make (Pre)
  end

  module Specialize_with_config (M : Hardcaml.Interface.S) (X : Config.S) =
    Specialize
      (M)
      (struct
        let vertical_index_width = Config.vertical_index_width X.t
        let horizontal_dimension = Config.horizontal_dimension X.t
      end)

  module M (M : T1) = struct
    type nonrec 'a t = ('a, 'a M.t) t

    module type S = S with type 'a write_data := 'a M.t
  end

  let sexp_of_m__t (module _ : For_deriving.Sexp_of_m) sexp_of_a t =
    [%sexp_of: (a, _) t] t
  ;;
end

module Component (M : Hardcaml.Interface.S) (The_config : Config.S) = struct
  let config = The_config.t
  let num_bits_per_entry = M.fold ~f:( + ) ~init:0 M.port_widths

  let required_underlying_memories_data_width =
    Config.horizontal_dimension config * num_bits_per_entry
  ;;

  let underlying_memories_data_width = Config.underlying_memories_data_width config

  let () =
    if required_underlying_memories_data_width <> underlying_memories_data_width
    then
      raise_s
        [%message
          "Mismatch between required underlying memories data width and actual"
            (underlying_memories_data_width : int)
            (required_underlying_memories_data_width : int)
            (config : Config.t)];
    if num_bits_per_entry <= 0
    then
      raise_s
        [%message "num_bits_per_entry must be greater than 0" (num_bits_per_entry : int)]
  ;;

  module Write_port_2d = Write_port_2d.Specialize_with_config (M) (The_config)
  module Read_port_2d = Read_port_2d.Specialize_with_config (The_config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_port_a : 'a Write_port_2d.t [@rtlprefix "wra_"]
      ; write_port_b : 'a Write_port_2d.t [@rtlprefix "wrb_"]
      ; read_port_a : 'a Read_port_2d.t [@rtlprefix "rda_"]
      ; read_port_b : 'a Read_port_2d.t [@rtlprefix "rdb_"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { read_data_a : 'a M.t [@rtlprefix "rd_a_"]
      ; read_data_b : 'a M.t [@rtlprefix "rd_b_"]
      }
    [@@deriving hardcaml]
  end

  let post_process_rd_data ~clock ~read_enable ~horizontal_index read_data =
    if Signal.width read_data <> underlying_memories_data_width
    then
      raise_s
        [%message
          "read data width mismatch"
            ~read_data_width:(Signal.width read_data : int)
            (underlying_memories_data_width : int)];
    let spec_no_clear = Reg_spec.create ~clock () in
    List.init config.horizontal_dimension ~f:(fun i ->
      let high = ((i + 1) * num_bits_per_entry) - 1 in
      let low = i * num_bits_per_entry in
      read_data.Signal.:[high, low])
    |> (function
          | [ hd ] -> hd
          | cases ->
            let select_horizontal =
              Signal.pipeline
                ~n:config.underlying_ram_read_latency
                spec_no_clear
                horizontal_index
            in
            Signal.mux select_horizontal cases)
    |> (fun x ->
         if config.combinational_output
         then x
         else (
           let enable =
             Signal.pipeline
               ~n:config.underlying_ram_read_latency
               spec_no_clear
               read_enable
           in
           Signal.reg ~enable spec_no_clear x))
    |> M.Of_signal.unpack
  ;;

  let pack_write_data write_data =
    Signal.concat_msb (List.rev_map ~f:M.Of_signal.pack write_data)
  ;;

  let create ~build_mode (scope : Scope.t) (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let underlying_memory =
      let write_address_a = i.write_port_a.vertical_index in
      let write_address_b = i.write_port_b.vertical_index in
      let read_address_a = i.read_port_a.vertical_index in
      let read_address_b = i.read_port_b.vertical_index in
      let write_data_a = pack_write_data i.write_port_a.data -- "write_data_a" in
      instantiate_underlying_memory
        ~clock:i.clock
        ~clear:i.clear
        ~scope
        ~build_mode
        ~write_address_a
        ~write_enable_a:(i.write_port_a.enable -- "write_enable_a")
        ~write_data_a
        ~write_address_b
        ~write_enable_b:(i.write_port_b.enable -- "write_enable_b")
        ~write_data_b:(pack_write_data i.write_port_b.data -- "write_data_b")
        ~read_address_a
        ~read_enable_a:(i.read_port_a.enable -- "read_enable_a")
        ~read_address_b
        ~read_enable_b:(i.read_port_b.enable -- "read_enable_b")
        ~config
    in
    { O.read_data_a =
        post_process_rd_data
          ~clock:i.clock
          ~horizontal_index:i.read_port_a.horizontal_index
          ~read_enable:i.read_port_a.enable
          (fst underlying_memory -- "read_data_a")
    ; read_data_b =
        post_process_rd_data
          ~clock:i.clock
          ~horizontal_index:i.read_port_b.horizontal_index
          ~read_enable:i.read_port_b.enable
          (snd underlying_memory -- "read_data_b")
    }
  ;;

  let hierarchical ?instance ~name ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name ~scope (create ~build_mode) i
  ;;
end

module Port_label = struct
  type t =
    | A
    | B
  [@@deriving sexp_of, hash, compare, enumerate]
end

let wire_if_nonzero n = if n = 0 then None else Some (Signal.wire n)

module Read_port_wires = struct
  type t =
    { read_enable : Signal.t
    ; horizontal_index : Signal.t option
    ; vertical_index : Signal.t option
    }

  let create (config : Config.t) =
    { read_enable = Signal.wire 1
    ; horizontal_index = wire_if_nonzero (Config.log2_horizontal_dimension config)
    ; vertical_index = wire_if_nonzero (Config.log2_vertical_dimension config)
    }
  ;;
end

module Write_port_wires = struct
  type t =
    { write_enable : Signal.t
    ; vertical_index : Signal.t option
    ; write_data : Signal.t list
    }

  let create ~num_bits_per_entry (config : Config.t) =
    { write_enable = Signal.wire 1
    ; vertical_index = wire_if_nonzero (Config.vertical_index_width config)
    ; write_data =
        List.init (Config.horizontal_dimension config) ~f:(fun _ ->
          Signal.wire num_bits_per_entry)
    }
  ;;
end

type 'a t =
  { a_of_signal : Signal.t -> 'a
  ; signal_of_a : 'a -> Signal.t
  ; read_ports : [ `Unassigned of Read_port_wires.t | `Assigned ] Hashtbl.M(Port_label).t
  ; write_ports :
      [ `Unassigned of Write_port_wires.t | `Assigned ] Hashtbl.M(Port_label).t
  ; read_datas : 'a Hashtbl.M(Port_label).t
  ; mutable is_complete : bool
  ; config : Config.t
  }

module Create (M : Hardcaml.Interface.S) = struct
  let num_bits_per_entry = M.fold ~init:0 ~f:( + ) M.port_widths

  let create
    ?(name = "memory_builder_component")
    ~instance
    ~build_mode
    ~(config : Config.t)
    ~scope
    ~clock
    ~clear
    ()
    =
    let create_read_ports () = Read_port_wires.create config in
    let create_write_ports () = Write_port_wires.create ~num_bits_per_entry config in
    let module Config = (val Config.as_module config) in
    let module Component = Component (M) (Config) in
    let read_port_a = create_read_ports () in
    let read_port_b = create_read_ports () in
    let write_port_a = create_write_ports () in
    let write_port_b = create_write_ports () in
    let component =
      Component.hierarchical
        ~instance
        ~name
        ~build_mode
        scope
        { clock
        ; clear
        ; write_port_a =
            { enable = write_port_a.write_enable
            ; vertical_index =
                Option.value ~default:Signal.gnd write_port_a.vertical_index
            ; data = List.map write_port_a.write_data ~f:M.Of_signal.unpack
            }
        ; write_port_b =
            { enable = write_port_b.write_enable
            ; vertical_index =
                Option.value ~default:Signal.gnd write_port_b.vertical_index
            ; data = List.map write_port_b.write_data ~f:M.Of_signal.unpack
            }
        ; read_port_a =
            { enable = read_port_a.read_enable
            ; vertical_index = Option.value ~default:Signal.gnd read_port_a.vertical_index
            ; horizontal_index =
                Option.value ~default:Signal.gnd read_port_a.horizontal_index
            }
        ; read_port_b =
            { enable = read_port_b.read_enable
            ; vertical_index = Option.value ~default:Signal.gnd read_port_b.vertical_index
            ; horizontal_index =
                Option.value ~default:Signal.gnd read_port_b.horizontal_index
            }
        }
    in
    let create_hashtbl ~a ~b = Hashtbl.of_alist_exn (module Port_label) [ A, a; B, b ] in
    { a_of_signal = M.Of_signal.unpack
    ; signal_of_a = M.Of_signal.pack
    ; read_ports =
        create_hashtbl ~a:(`Unassigned read_port_a) ~b:(`Unassigned read_port_b)
    ; write_ports =
        create_hashtbl ~a:(`Unassigned write_port_a) ~b:(`Unassigned write_port_b)
    ; read_datas = create_hashtbl ~a:component.read_data_a ~b:component.read_data_b
    ; is_complete = false
    ; config
    }
  ;;

  let create_simple_1d
    ?name
    ?simulation_name
    ~instance
    ~build_mode
    ~depth
    ~ram_read_latency
    ~how_to_instantiate_ram
    ~scope
    ~clock
    ~clear
    ()
    =
    let config =
      Config.create_simple_1d_config
        ~num_bits_per_entry
        ~depth
        ~ram_read_latency
        ~how_to_instantiate_ram
        ~simulation_name
    in
    create ?name ~instance ~build_mode ~config ~scope ~clock ~clear ()
  ;;
end

let ( <==? ) dst src =
  match dst with
  | None ->
    if not (Signal.width src <= 1)
    then
      raise_s
        [%message
          "When outer dimension / vertical dimension is of size 1, must provide empty \
           signal or width 1 signal"]
  | Some dst -> Signal.( <== ) dst src
;;

let validate_not_reading_from_port_A_in_inferred_ram
  (how_to_instantiate_ram : Config.how_to_instantiate_ram)
  (port_label : Port_label.t)
  =
  match how_to_instantiate_ram, port_label with
  | Inferred _, A -> raise_s [%message "Cannot write to port A in inferred RAM!"]
  | _ -> ()
;;

let set_read_port_2d (t : _ t) port_label (read_port : _ Read_port_2d.t) =
  validate_vertical_index_width t.config read_port.vertical_index;
  validate_horizontal_index_width t.config read_port.horizontal_index;
  List.iter t.config.underlying_memories ~f:(fun underlying_memory ->
    validate_not_reading_from_port_A_in_inferred_ram
      underlying_memory.how_to_instantiate_ram
      port_label);
  match Hashtbl.find_exn t.read_ports port_label with
  | `Assigned -> raise_s [%message "Cannot assign a read port twice!"]
  | `Unassigned ports ->
    let open Signal in
    Hashtbl.set ~key:port_label ~data:`Assigned t.read_ports;
    ports.read_enable <== read_port.enable;
    ports.horizontal_index <==? read_port.horizontal_index;
    ports.vertical_index <==? read_port.vertical_index;
    Hashtbl.find_exn t.read_datas port_label
;;

let read_latency (t : _ t) = Config.read_latency t.config

let validate_not_writing_to_port_B_in_distributed_ram
  (how_to_instantiate_ram : Config.how_to_instantiate_ram)
  (port_label : Port_label.t)
  =
  match how_to_instantiate_ram, port_label with
  | Xpm Distributed, B -> raise_s [%message "Cannot write to port B in distributed RAM!"]
  | _ -> ()
;;

let validate_not_writing_to_port_B_in_inferred_ram
  (how_to_instantiate_ram : Config.how_to_instantiate_ram)
  (port_label : Port_label.t)
  =
  match how_to_instantiate_ram, port_label with
  | Inferred _, B -> raise_s [%message "Cannot write to port B in inferred RAM!"]
  | _ -> ()
;;

let set_write_port_2d (t : _ t) port_label (write_port : _ Write_port_2d.t) =
  validate_vertical_index_width t.config write_port.vertical_index;
  match Hashtbl.find_exn t.write_ports port_label with
  | `Assigned ->
    raise_s [%message "Cannot assign a write port twice!" (port_label : Port_label.t)]
  | `Unassigned ports ->
    List.iter t.config.underlying_memories ~f:(fun underlying_memory ->
      validate_not_writing_to_port_B_in_distributed_ram
        underlying_memory.how_to_instantiate_ram
        port_label;
      validate_not_writing_to_port_B_in_inferred_ram
        underlying_memory.how_to_instantiate_ram
        port_label);
    let open Signal in
    Hashtbl.set ~key:port_label ~data:`Assigned t.write_ports;
    ports.write_enable <== write_port.enable;
    ports.vertical_index <==? write_port.vertical_index;
    List.iter2_exn ports.write_data write_port.data ~f:(fun dst src ->
      dst <== t.signal_of_a src)
;;

let zero_out a =
  let open Signal in
  a <== zero (width a)
;;

let complete t =
  if t.is_complete then raise_s [%message "Cannnot call [compelte] twice!"];
  t.is_complete <- true;
  Hashtbl.iter t.read_ports ~f:(function
    | `Assigned -> ()
    | `Unassigned ports ->
      Option.iter ~f:zero_out ports.vertical_index;
      Option.iter ~f:zero_out ports.horizontal_index;
      zero_out ports.read_enable);
  Hashtbl.iter t.write_ports ~f:(function
    | `Assigned -> ()
    | `Unassigned ports ->
      Option.iter ~f:zero_out ports.vertical_index;
      zero_out ports.write_enable;
      List.iter ~f:zero_out ports.write_data)
;;

let assert_is_1d (t : _ t) =
  if Config.horizontal_dimension t.config <> 1
  then
    raise_s [%message "1d function can only operate on 1D configs" (t.config : Config.t)]
;;

let set_read_port_1d t port_label (read_port : _ Read_port_1d.t) =
  assert_is_1d t;
  set_read_port_2d
    t
    port_label
    { horizontal_index = Signal.empty
    ; vertical_index = read_port.address
    ; enable = read_port.enable
    }
;;

let set_write_port_1d t port_label (write_port : (Signal.t, _) Write_port_1d.t) =
  assert_is_1d t;
  set_write_port_2d
    t
    port_label
    { vertical_index = write_port.address
    ; enable = write_port.enable
    ; data = [ write_port.data ]
    }
;;

module M (X : T1) = struct
  type nonrec t = Signal.t X.t t
end
