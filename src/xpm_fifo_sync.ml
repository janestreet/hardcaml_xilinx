open! Import
open Signal

let create
      ?(overflow_check = true)
      ?(showahead = false)
      ?(underflow_check = true)
      ()
      ~capacity
      ~clk
      ~clr
      ~wr
      ~d
      ~rd
  =
  (* Where is REMOVE_WR_RD_PROT_LOGIC? Its in the docs, but not on the component. *)
  ignore (overflow_check, underflow_check);
  let module XFifo =
    Xpm.Xpm_fifo_sync.Make (struct
      include Xpm.Xpm_fifo_sync.P

      let read_mode = if showahead then "fwft" else "std"
      let fifo_read_latency = if showahead then 0 else 1
      let write_data_width = width d
      let read_data_width = width d
      let fifo_write_depth = capacity
      let wr_data_count_width = num_bits_to_represent capacity
      let rd_data_count_width = num_bits_to_represent capacity
      let prog_full_thresh = capacity - 16
      let prog_empty_thresh = 16
    end)
  in
  let o : _ XFifo.O.t =
    XFifo.create
      { XFifo.I.wr_clk = clk
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
  }
;;
