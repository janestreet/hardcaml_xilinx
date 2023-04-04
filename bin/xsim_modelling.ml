(* Generate an RTL model we can run XSIM and test the xpm against our hardcaml model. *)
open Base
open Hardcaml
open Signal
module Test = Hardcaml_xilinx_test.Test_collision

let command_tdpram =
  Command.basic
    ~summary:"true dual port ram module"
    [%map_open.Command
      let () = return () in
      fun () ->
        let rams = Test.create ~f:Test.compare_rams in
        Circuit.create_exn
          ~name:"xram_sim_model"
          Test.O.(map2 port_names rams ~f:output |> to_list)
        |> Rtl.print Verilog]
;;

let command_resizing =
  Command.basic
    ~summary:"ram with resizing"
    [%map_open.Command
      let seed = flag "-seed" (optional int) ~doc:"Set random number generator seed" in
      Option.iter seed ~f:Random.init;
      fun () -> Hardcaml_xilinx_test.Test_ram_with_resizing.For_rtl_sim.generate ()]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Generate RTL for testing against Verilog"
       [ "tdpram", command_tdpram; "resizing", command_resizing ])
;;
