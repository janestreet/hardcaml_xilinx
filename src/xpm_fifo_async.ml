open Base
open Hardcaml
open Signal

let create
  ?(overflow_check = true)
  ?(showahead = false)
  ?(underflow_check = true)
  ?fifo_memory_type:arg_fifo_memory_type
  ?nearly_full
  ?(nearly_empty = 16)
  ()
  ~capacity
  ~latency
  ~wr_clk
  ~rd_clk
  ~clr
  ~wr
  ~d
  ~rd
  =
  (* Where is REMOVE_WR_RD_PROT_LOGIC? Its in the docs, but not on the component. *)
  ignore (overflow_check, underflow_check);
  let module X =
    Xpm_2019_1.Xpm_fifo_async.Make (struct
      include Xpm_2019_1.Xpm_fifo_async.P

      let fifo_memory_type =
        Option.map ~f:Fifo_memory_type.to_xpm_args arg_fifo_memory_type
        |> Option.value ~default:fifo_memory_type
      ;;

      let read_mode = if showahead then "fwft" else "std"

      let fifo_read_latency =
        if showahead then assert (latency = 0);
        latency
      ;;

      let write_data_width = width d
      let read_data_width = width d
      let fifo_write_depth = capacity
      let wr_data_count_width = num_bits_to_represent capacity
      let rd_data_count_width = num_bits_to_represent capacity
      let prog_full_thresh = Option.value nearly_full ~default:(capacity - 16)
      let prog_empty_thresh = nearly_empty

      (* There are limitations on the range for [prog_*_thresh], see docs for details.
         The checks are slightly simplified because write_data_width = read_data_width
         for our configuration. *)
      let () =
        let read_mode_val = if String.equal read_mode "std" then 0 else 1 in
        let empty_min_thresh = 3 + (read_mode_val * 2) in
        let empty_max_thresh = fifo_write_depth - 3 - (read_mode_val * 2) in
        let full_min_thresh = empty_min_thresh + cdc_sync_stages in
        let full_max_thresh = empty_max_thresh in
        let check_prog_thresh ~min_thresh ~max_thresh thresh =
          if thresh < min_thresh
          then raise_s [%message "threshold too low!" (thresh : int) (min_thresh : int)];
          if thresh > max_thresh
          then raise_s [%message "threshold too high!" (thresh : int) (max_thresh : int)]
        in
        check_prog_thresh
          ~min_thresh:full_min_thresh
          ~max_thresh:full_max_thresh
          prog_full_thresh;
        check_prog_thresh
          ~min_thresh:empty_min_thresh
          ~max_thresh:empty_max_thresh
          prog_empty_thresh
      ;;
    end)
  in
  let o : _ X.O.t =
    X.create
      { X.I.wr_clk
      ; rd_clk
      ; rst = clr
      ; wr_en = wr
      ; rd_en = rd
      ; din = d
      ; sleep = gnd
      ; injectsbiterr = gnd
      ; injectdbiterr = gnd
      }
  in
  { Fifo.q = o.dout
  ; full = o.full
  ; empty = o.empty
  ; nearly_full = o.prog_full
  ; nearly_empty = o.prog_empty
  ; used = o.wr_data_count
  ; rd_rst_busy = o.rd_rst_busy
  ; wr_rst_busy = o.wr_rst_busy
  }
;;
