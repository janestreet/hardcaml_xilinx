(* Generate an RTL model we can run XSIM and test the xpm against our hardcaml model. *)
open Hardcaml
open Signal
module Test = Hardcaml_xilinx_test.Test_collision

let () =
  let rams = Test.create ~f:Test.compare_rams in
  Circuit.create_exn
    ~name:"xram_sim_model"
    Test.O.(map2 port_names rams ~f:output |> to_list)
  |> Rtl.print Verilog
;;
