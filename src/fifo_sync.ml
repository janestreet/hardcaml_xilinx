open! Base
open Hardcaml

let create
      ?(overflow_check = true)
      ?(showahead = false)
      ?(underflow_check = true)
      ?(build_mode = Build_mode.Synthesis)
      ?scope
      ?fifo_memory_type
      ?instance
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
  =
  match (build_mode : Build_mode.t) with
  | Synthesis ->
    Xpm_fifo_sync.create
      ~overflow_check
      ~underflow_check
      ~showahead
      ?fifo_memory_type
      ?instance
      ()
      ~capacity
      ~clk:clock
      ~clr:clear
      ~rd
      ~wr
      ~d
  | Simulation ->
    Fifo.create
      ?scope
      ~overflow_check
      ~underflow_check
      ~showahead
      ()
      ~capacity
      ~clock
      ~clear
      ~rd
      ~wr
      ~d
;;
