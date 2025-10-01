open! Base
open Hardcaml

type 'signal create =
  ?read_latency:int
    (** Default is None which will either set [read_latency] to 0 or 1 if [showahead] is
        true or false respectively. *)
  -> ?overflow_check:bool (** default is [true] *)
  -> ?showahead:bool (** default is [false] **)
  -> ?underflow_check:bool (** default is [true] *)
  -> ?build_mode:Build_mode.t (** default is [Synthesis] *)
  -> ?scope:Scope.t
  -> ?fifo_memory_type:Fifo_memory_type.t
       (** See [Xpm_fifo_sync] parameters in [xpm.ml] for default. *)
  -> ?instance:string (** Only used in synthesis *)
  -> ?xpm_version:[ `Xpm_2019_1 | `Xpm_2022_1 ]
       (** Used to decide which XPM version instantiation to use *)
  -> ?cascade_height:int (** default is [0] -> Vivado chooses; only used for Xpm 2022.1 *)
  -> ?nearly_full:int (** usage level at which [nearly_full] will be asserted *)
  -> ?nearly_empty:int (** usage level at which [nearly_empty] will be asserted *)
  -> unit
  -> capacity:int
  -> clock:'signal
  -> clear:'signal
  -> wr:'signal
  -> d:'signal
  -> rd:'signal
  -> 'signal Fifo.t

let create
  ?read_latency
  ?(overflow_check = true)
  ?(showahead = false)
  ?(underflow_check = true)
  ?(build_mode = Build_mode.Synthesis)
  ?scope
  ?fifo_memory_type
  ?instance
  ?(xpm_version = `Xpm_2019_1)
  ?cascade_height
  ?nearly_full
  ?nearly_empty
  ()
  ~capacity
  ~clock
  ~clear
  ~wr
  ~d
  ~rd
  =
  (* Check if read_latency is set that its value makes sense. *)
  Option.iter read_latency ~f:(fun read_latency ->
    if showahead && read_latency <> 0
    then
      raise_s
        [%message
          "Cannot set showahead = true and read_latency <> 0 for Fifo_sync."
            (read_latency : int)
            (showahead : bool)]);
  match (build_mode : Build_mode.t) with
  | Synthesis ->
    (match xpm_version with
     | `Xpm_2019_1 ->
       Xpm_fifo_sync.Xpm_2019_1.create
         ~overflow_check
         ~underflow_check
         ~showahead
         ?fifo_memory_type
         ?nearly_full
         ?nearly_empty
         ?instance
         ?read_latency
         ()
         ~capacity
         ~clk:clock
         ~clr:clear
         ~rd
         ~wr
         ~d
     | `Xpm_2022_1 ->
       Xpm_fifo_sync.Xpm_2022_1.create
         ~overflow_check
         ~underflow_check
         ~showahead
         ?fifo_memory_type
         ?nearly_full
         ?nearly_empty
         ?instance
         ?cascade_height
         ?read_latency
         ()
         ~capacity
         ~clk:clock
         ~clr:clear
         ~rd
         ~wr
         ~d)
  | Simulation ->
    (* Create a new scope for the FIFO signals if both [scope] and [instance] are provided *)
    let scope =
      match scope, instance with
      | Some scope, Some inst -> Some (Scope.sub_scope scope inst)
      | _, _ -> scope
    in
    Fifo.create
      ?scope
      ~overflow_check
      ~underflow_check
      ~showahead
      ?nearly_full
      ?nearly_empty
      ?read_latency
      ()
      ~capacity
      ~clock
      ~clear
      ~rd
      ~wr
      ~d
;;

module Clocked = struct
  let create
    ?read_latency
    ?overflow_check
    ?showahead
    ?underflow_check
    ?build_mode
    ?scope
    ?fifo_memory_type
    ?instance
    ?xpm_version
    ?cascade_height
    ?nearly_full
    ?nearly_empty
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    create
      ?read_latency
      ?overflow_check
      ?showahead
      ?underflow_check
      ?build_mode
      ?scope
      ?fifo_memory_type
      ?instance
      ?xpm_version
      ?cascade_height
      ?nearly_full
      ?nearly_empty
      ()
      ~capacity
      ~clock:(clock |> Clocked_signal.unwrap_signal ~dom)
      ~clear:(clear |> Clocked_signal.unwrap_signal ~dom)
      ~wr:(wr |> Clocked_signal.unwrap_signal ~dom)
      ~d:(d |> Clocked_signal.unwrap_signal ~dom)
      ~rd:(rd |> Clocked_signal.unwrap_signal ~dom)
    |> Fifo.map ~f:(Clocked_signal.to_clocked ~dom)
  ;;
end

module With_interface (X : Hardcaml.Interface.S) = struct
  open Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
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
    ?read_latency
    ?overflow_check
    ?showahead
    ?underflow_check
    ?fifo_memory_type
    ?xpm_version
    ?cascade_height
    ?nearly_full
    ?nearly_empty
    ~capacity
    ({ clock; clear; d; wr; rd } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear () in
    let fifo =
      create
        ()
        ?read_latency
        ?overflow_check
        ?showahead
        ?underflow_check
        ?fifo_memory_type
        ?xpm_version
        ?cascade_height
        ?nearly_full
        ?nearly_empty
        ~build_mode
        ~scope
        ~capacity
        ~clock
        ~clear
        ~wr
        ~d:(X.Of_signal.pack d)
        ~rd
        ~instance:"fifo"
    in
    let overflow =
      sticky_error ( -- ) spec ~not_ready:fifo.full ~valid:wr ~name:"overflow"
    in
    let underflow =
      sticky_error ( -- ) spec ~not_ready:fifo.empty ~valid:rd ~name:"underflow"
    in
    let q = { With_valid.valid = ~:(fifo.empty); value = X.Of_signal.unpack fifo.q } in
    { O.q; overflow; underflow; full = fifo.full; nearly_full = fifo.nearly_full }
  ;;

  let hierarchical
    ?(instance = "fifo_with_interface")
    scope
    ~build_mode
    ?read_latency
    ?overflow_check
    ?showahead
    ?underflow_check
    ?fifo_memory_type
    ?xpm_version
    ?cascade_height
    ?nearly_full
    ?nearly_empty
    ~capacity
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~instance
      ~scope
      ~name:"fifo_with_interface"
      (create
         ~build_mode
         ~capacity
         ?read_latency
         ?overflow_check
         ?showahead
         ?underflow_check
         ?fifo_memory_type
         ?xpm_version
         ?cascade_height
         ?nearly_full
         ?nearly_empty)
  ;;
end
