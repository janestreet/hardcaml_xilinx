open! Base
open Hardcaml

let create
      ?(showahead = false)
      ?(build_mode = Build_mode.Synthesis)
      ?fifo_memory_type
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
    (* Unfortunately, we cannot properly simulate asynchronous fifos. The choice of
       clock here is arbitrary *)
    Fifo.create ~showahead () ~capacity ~clock:read_clock ~clear ~rd:read ~wr:write ~d
  | Synthesis ->
    Xpm_fifo_async.create
      ?fifo_memory_type
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
