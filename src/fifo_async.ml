open! Base
open Hardcaml

let create
      ?(showahead = false)
      ?(build_mode = Build_mode.Synthesis)
      ()
      ~capacity
      ~read_clock
      ~write_clock
      ~clear
      ~write
      ~d
      ~read
  =
  match (build_mode : Build_mode.t) with
  | Simulation ->
    (* Unfortunately, we cannot properly simulate asynchronous fifos. The choice of
       clock here is arbitrary *)
    Fifo.create ~showahead () ~capacity ~clock:read_clock ~clear ~rd:read ~wr:write ~d
  | Synthesis ->
    Xpm_fifo_async.create
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
