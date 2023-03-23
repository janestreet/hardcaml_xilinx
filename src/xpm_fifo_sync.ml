open Base
open Hardcaml
open Signal

module Xpm_2019_1 = struct
  let create
        ?(overflow_check = true)
        ?(showahead = false)
        ?(underflow_check = true)
        ?fifo_memory_type:arg_fifo_memory_type
        ?nearly_full
        ?(nearly_empty = 16)
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
        let prog_full_thresh = Option.value nearly_full ~default:(capacity - 16)
        let prog_empty_thresh = nearly_empty

        (* There are limitations on the range for [prog_*_thresh], see docs for details.
           The checks are slightly simplified because write_data_width = read_data_width
           for our configuration. *)
        let () =
          let read_mode_val = if String.equal read_mode "std" then 0 else 1 in
          let check_prog_thresh thresh =
            assert (thresh >= 3 + (read_mode_val * 2));
            assert (thresh <= fifo_write_depth - 3 - (read_mode_val * 2))
          in
          check_prog_thresh prog_full_thresh;
          check_prog_thresh prog_empty_thresh
        ;;
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
        ?nearly_full
        ?(nearly_empty = 16)
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
        let prog_full_thresh = Option.value nearly_full ~default:(capacity - 16)
        let prog_empty_thresh = nearly_empty
        let cascade_height = Option.value arg_cascade_height ~default:cascade_height

        (* There are limitations on the range for [prog_*_thresh], see docs for details.
           The checks are slightly simplified because write_data_width = read_data_width
           for our configuration. *)
        let () =
          let read_mode_val = if String.equal read_mode "std" then 0 else 1 in
          let check_prog_thresh thresh =
            assert (thresh >= 3 + (read_mode_val * 2));
            assert (thresh <= fifo_write_depth - 3 - (read_mode_val * 2))
          in
          check_prog_thresh prog_full_thresh;
          check_prog_thresh prog_empty_thresh
        ;;
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
