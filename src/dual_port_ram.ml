open! Import

let create ?read_latency ?arch ?byte_write_width () ~clock ~clear ~size ~port_a ~port_b =
  True_dual_port_ram.create
    ?read_latency
    ?arch
    ?byte_write_width
    ()
    ~clock_a:clock
    ~clock_b:clock
    ~clear_a:clear
    ~clear_b:clear
    ~size
    ~port_a
    ~port_b
;;
