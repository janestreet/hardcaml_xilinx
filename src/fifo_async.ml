open! Base
open Hardcaml

let create
  ?(showahead = false)
  ?(build_mode = Build_mode.Synthesis)
  ?fifo_memory_type
  ?nearly_full
  ?nearly_empty
  ?scope
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
        end)
      in
      let async_fifo =
        Async_fifo.create
          ?scope
          { clock_write = write_clock
          ; clock_read = read_clock
          ; reset_write = clear
          ; reset_read = gnd
          ; data_in = d
          ; write_enable = write
          ; read_enable = read
          }
      in
      { Fifo.q = async_fifo.data_out
      ; full = async_fifo.full
      ; empty = ~:(async_fifo.valid)
      ; nearly_full = gnd
      ; nearly_empty = async_fifo.almost_empty
      ; used = gnd
      ; rd_rst_busy = gnd
      ; wr_rst_busy = gnd
      }
    else
      Fifo.create
        ?scope
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
