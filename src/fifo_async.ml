open! Base
open Hardcaml

let create
  ?(showahead = false)
  ?(build_mode = Build_mode.Synthesis)
  ?fifo_memory_type
  ?nearly_full
  ?nearly_empty
  ~scope
  ()
  ~capacity
  ~read_clock
  ~write_clock
  ~clear
  ~write
  ~d
  ~read
  =
  Option.iter fifo_memory_type ~f:(fun fifo_memory_type ->
    match fifo_memory_type with
    | Fifo_memory_type.Ultra ->
      raise_s [%message "Cannot create async fifo with [Ultra] memory type."]
    | Block | Auto | Distributed -> ());
  match (build_mode : Build_mode.t) with
  | Simulation ->
    (* Our async fifo implementation currently only supports showahead mode; in the
       non-showahead case, we just stub in a regular fifo for simulations with an
       arbitrary clock chosen. *)
    if showahead
    then
      let open Signal in
      let module Async_fifo =
        Async_fifo.Make (struct
          let width = Signal.width d
          let log2_depth = Int.ceil_log2 capacity
          let optimize_for_same_clock_rate_and_always_reading = false
        end)
      in
      let prog_full_thresh = nearly_full in
      let prog_empty_thresh = nearly_empty in
      let async_fifo =
        Async_fifo.For_testing.create_with_synchronous_clear_semantics_for_simulation_only
          ?prog_full_thresh
          ?prog_empty_thresh
          ~scope
          { clock_write = write_clock
          ; clock_read = read_clock
          ; reset_write = clear
          ; reset_read = clear
          ; data_in = d
          ; write_enable = write
          ; read_enable = read
          }
      in
      { Fifo.q = async_fifo.data_out
      ; full = async_fifo.full
      ; empty = ~:(async_fifo.valid)
      ; nearly_full = async_fifo.prog_full
      ; nearly_empty = async_fifo.prog_empty
      ; used = gnd
      ; rd_rst_busy = gnd
      ; wr_rst_busy = gnd
      }
    else
      Fifo.create
        ~scope
        ?nearly_full
        ?nearly_empty
        ~showahead
        ()
        ~capacity
        ~clock:read_clock
        ~clear
        ~rd:read
        ~wr:write
        ~d
  | Synthesis ->
    Xpm_fifo_async.create
      ?fifo_memory_type
      ?nearly_full
      ?nearly_empty
      ~showahead
      ()
      ~latency:(if showahead then 0 else 1)
      ~capacity
      ~rd_clk:read_clock
      ~wr_clk:write_clock
      ~clr:clear
      ~wr:write
      ~rd:read
      ~d
;;

module With_interface (X : Hardcaml.Interface.S) = struct
  open Signal

  module I = struct
    type 'a t =
      { read_clock : 'a
      ; read_clear : 'a
      ; write_clock : 'a
      ; write_clear : 'a
      ; d : 'a X.t [@rtlprefix "d$"]
      ; wr : 'a
      ; rd : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module X_with_valid = With_valid.Wrap.Make (X)

  module O = struct
    type 'a t =
      { q : 'a X_with_valid.t [@rtlprefix "q$"]
      ; overflow : 'a
      ; underflow : 'a
      ; full : 'a
      ; nearly_full : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let always_reg ( -- ) spec ~width ~name =
    let v = Always.Variable.reg spec ~width in
    ignore (v.value -- name : Signal.t);
    v
  ;;

  let sticky_error ( -- ) spec ~not_ready ~valid ~name =
    let error = always_reg ( -- ) spec ~width:1 ~name in
    Always.(compile [ when_ (not_ready &: valid) [ error <-- vdd ] ]);
    error.value
  ;;

  let create
    scope
    ~build_mode
    ?showahead
    ?fifo_memory_type
    ?nearly_full
    ?nearly_empty
    ~capacity
    ({ read_clock; read_clear; write_clock; write_clear; d; wr; rd } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let write_spec = Reg_spec.create ~clock:write_clock ~clear:write_clear () in
    let read_spec = Reg_spec.create ~clock:read_clock ~clear:read_clear () in
    let fifo =
      create
        ?showahead
        ?fifo_memory_type
        ?nearly_full
        ?nearly_empty
        ~build_mode
        ~scope
        ()
        ~capacity
        ~read_clock
        ~write_clock
        ~clear:write_clear
        ~write:wr
        ~d:(X.Of_signal.pack d)
        ~read:rd
    in
    let overflow =
      sticky_error ( -- ) write_spec ~not_ready:fifo.full ~valid:wr ~name:"overflow"
    in
    let underflow =
      sticky_error ( -- ) read_spec ~not_ready:fifo.empty ~valid:rd ~name:"underflow"
    in
    let q = { With_valid.valid = ~:(fifo.empty); value = X.Of_signal.unpack fifo.q } in
    { O.q; overflow; underflow; full = fifo.full; nearly_full = fifo.nearly_full }
  ;;

  let hierarchical
    ?(instance = "async_fifo_with_interface")
    scope
    ~build_mode
    ?showahead
    ?fifo_memory_type
    ?nearly_full
    ?nearly_empty
    ~capacity
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~instance
      ~scope
      ~name:"async_fifo_with_interface"
      (create
         ~build_mode
         ~capacity
         ?showahead
         ?fifo_memory_type
         ?nearly_full
         ?nearly_empty)
  ;;
end
