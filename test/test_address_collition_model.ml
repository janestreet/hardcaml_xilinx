open! Import
open Hardcaml_waveterm

let test mode q_width =
  let open Signal in
  let clock = input "clock" 1 in
  let spec = Reg_spec.create ~clock () in
  let q = input "q" q_width in
  let q_collision = Address_collision.Model.q mode spec ~address_collision:vdd q in
  let circ =
    Circuit.create_exn ~name:"address_collisions" [ output "q_collision" q_collision ]
  in
  let sim = Cyclesim.create circ in
  let waves, sim = Waveform.create sim in
  (try
     let q = Cyclesim.in_port sim "q" in
     q := Bits.of_int_trunc ~width:q_width (-1)
   with
   | _ -> ());
  for _ = 0 to 100 do
    Cyclesim.cycle sim
  done;
  waves
;;

let display_rules = Display_rule.[ port_name_is "q_collision" ~wave_format:Unsigned_int ]
let print waves = Waveform.print ~display_rules ~wave_width:0 waves

let%expect_test "1 bit no collision logic" =
  print (test None__there_be_dragons 1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││───────────────────────────────────────────────────│
    │q_collision    ││ 1                                                 │
    │               ││───────────────────────────────────────────────────│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "10 bit no collision logic" =
  print (test None__there_be_dragons 10);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││───────────────────────────────────────────────────│
    │q_collision    ││ 1023                                              │
    │               ││───────────────────────────────────────────────────│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "1 bit counter" =
  print (test Counter 1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0││
    │               ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "10 bit counter" =
  print (test Counter 10);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0│1│2│3│4│5│6│7│8│9│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.││
    │               ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "1 bit gray counter" =
  print (test Graycode 1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0│1│0││
    │               ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "10 bit gray counter" =
  print (test Graycode 10);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0│1│3│2│6│7│5│4│.│.│.│.│.│.│9│8│.│.│.│.│.│.│.│.│.││
    │               ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "1 bit LFSR" =
  print (test Lfsr 1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──────────┬─────┬─┬───┬───┬─┬─┬─┬───────┬─────┬─┬──│
    │q_collision    ││ 0        │1    │0│1  │0  │1│0│1│0      │1    │0│1 │
    │               ││──────────┴─────┴─┴───┴───┴─┴─┴─┴───────┴─────┴─┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "3bit LFSR" =
  print (test Lfsr 3);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││──────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0    │1│3│7│6│5│3│6│4│1│2│5│2│4│0  │1│3│7│6│5│3│6││
    │               ││──────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "200 bit LFSR" =
  print (test Lfsr 200);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬│
    │q_collision    ││ 0  │.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.││
    │               ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
