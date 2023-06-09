open! Base
open Hardcaml

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
