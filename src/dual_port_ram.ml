open! Base
open! Hardcaml

let create
      ?read_latency
      ?arch
      ?byte_write_width
      ?memory_optimization
      ?cascade_height
      ~build_mode
      ()
      ~clock
      ~clear
      ~size
      ~port_a
      ~port_b
  =
  True_dual_port_ram.create
    ?read_latency
    ?arch
    ?byte_write_width
    ~build_mode
    ?memory_optimization
    ?cascade_height
    ()
    ~clock_a:clock
    ~clock_b:clock
    ~clear_a:clear
    ~clear_b:clear
    ~size
    ~port_a
    ~port_b
;;
