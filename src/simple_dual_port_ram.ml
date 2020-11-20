open Base
open Hardcaml
open Signal

let create
      ?read_latency
      ?arch
      ?(byte_write_width = Byte_write_width.Full)
      ~build_mode
      ()
      ~clock
      ~clear
      ~size
      ~write_address
      ~write_enable
      ~data
      ~read_address
      ~read_enable
  =
  let _, q =
    Dual_port_ram.create
      ?read_latency
      ?arch
      ~byte_write_width
      ~build_mode
      ()
      ~clock
      ~clear
      ~size
      ~port_a:{ Ram_port.write_enable; address = write_address; read_enable = gnd; data }
      ~port_b:
        { Ram_port.write_enable =
            (match byte_write_width with
             | Full -> gnd
             | B8 ->
               assert (width data % 8 = 0);
               zero (width data / 8)
             | B9 ->
               assert (width data % 9 = 0);
               zero (width data / 9))
        ; address = read_address
        ; read_enable
        ; data = zero (width data)
        }
  in
  q
;;
