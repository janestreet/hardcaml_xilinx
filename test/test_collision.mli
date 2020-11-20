open Hardcaml
open Signal
module O : Interface.S

val compare_rams
  :  clock:t
  -> clear:t
  -> port_a:Signal.t Hardcaml_xilinx.Ram_port.t
  -> port_b:Signal.t Hardcaml_xilinx.Ram_port.t
  -> Signal.t O.t

val create
  :  f:
       (clock:t
        -> clear:t
        -> port_a:Signal.t Hardcaml_xilinx.Ram_port.t
        -> port_b:Signal.t Hardcaml_xilinx.Ram_port.t
        -> 'a)
  -> 'a
