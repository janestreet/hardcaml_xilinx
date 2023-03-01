open Base
open Hardcaml
open Signal

module Xpm_2019_1 = struct
  let create
        ?(overflow_check = true)
        ?(showahead = false)
        ?(underflow_check = true)
        ?fifo_memory_type:arg_fifo_memory_type
        ?instance
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
      Xpm_2019_1.Xpm_fifo_sync.Make (struct
        include Xpm_2019_1.Xpm_fifo_sync.P

        let fifo_memory_type =
          Option.map ~f:Fifo_memory_type.to_xpm_args arg_fifo_memory_type
          |> Option.value ~default:fifo_memory_type
        ;;

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
        ?instance
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
end

module Xpm_2022_1 = struct
  let create
        ?(overflow_check = true)
        ?(showahead = false)
        ?(underflow_check = true)
        ?fifo_memory_type:arg_fifo_memory_type
        ?instance
        ?cascade_height:arg_cascade_height
        ()
        ~capacity
        ~clk
        ~clr
        ~wr
        ~d
        ~rd
    =
    ignore (overflow_check, underflow_check);
    let module XFifo =
      Xpm_2022_1.Xpm_fifo_sync.Make (struct
        include Xpm_2022_1.Xpm_fifo_sync.P

        let fifo_memory_type =
          Option.map ~f:Fifo_memory_type.to_xpm_args arg_fifo_memory_type
          |> Option.value ~default:fifo_memory_type
        ;;

        let read_mode = if showahead then "fwft" else "std"
        let fifo_read_latency = if showahead then 0 else 1
        let write_data_width = width d
        let read_data_width = width d
        let fifo_write_depth = capacity
        let wr_data_count_width = num_bits_to_represent capacity
        let rd_data_count_width = num_bits_to_represent capacity
        let prog_full_thresh = capacity - 16
        let prog_empty_thresh = 16
        let cascade_height = Option.value arg_cascade_height ~default:cascade_height
      end)
    in
    let o : _ XFifo.O.t =
      XFifo.create
        ?instance
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
end
