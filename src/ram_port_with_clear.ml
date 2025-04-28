open! Base
open! Hardcaml

type 'a t =
  { port : 'a Ram_port.t
  ; clear_busy : 'a
  }
[@@deriving hardcaml]

module State = struct
  type t =
    | Clear
    | Done
  [@@deriving sexp_of, enumerate, compare]
end

let create ~scope ~clear_to ~clear ~clock ~size ~(port : _ Ram_port.t) =
  let open Always in
  let open Signal in
  if width clear_to <> width port.data
  then failwith "[Ram_port_with_clear.clear_to] must be the same width as the data port.";
  let enable = vdd in
  let spec = Reg_spec.create ~clock ~clear () in
  let%hw.State_machine sm = State_machine.create ~enable (module State) spec in
  let addr = Variable.reg ~enable spec ~width:(num_bits_to_represent (size - 1)) in
  let ram_port = Ram_port.map port ~f:(fun default -> Variable.wire ~default) in
  Always.(
    compile
      [ sm.switch
          [ ( Clear
            , [ ram_port.write_enable <--. 1
              ; ram_port.read_enable <--. 0
              ; ram_port.data <-- clear_to
              ; ram_port.address <-- addr.value
              ; addr <-- addr.value +:. 1
              ; when_ (addr.value ==:. size - 1) [ sm.set_next Done ]
              ] )
          ; Done, []
          ]
      ]);
  let%hw.Ram_port.Of_signal port = Ram_port.Of_always.value ram_port in
  { port; clear_busy = sm.is Clear }
;;
