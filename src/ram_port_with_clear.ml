open! Base
open! Hardcaml

type 'a t =
  { port : 'a Ram_port.t
  ; clear_busy : 'a
  }
[@@deriving sexp_of, hardcaml]

module State = struct
  type t =
    | Clear
    | Done
  [@@deriving sexp_of, enumerate, compare]
end

let create ~clear_to ~clear ~clock ~size ~port =
  let open Always in
  let open Signal in
  let enable = vdd in
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create ~enable (module State) spec in
  ignore (sm.current -- "state" : _);
  let addr = Variable.reg ~enable spec ~width:(num_bits_to_represent (size - 1)) in
  let ram_port = Ram_port.map port ~f:(fun default -> Variable.wire ~default) in
  compile
    [ sm.switch
        [ ( Clear
          , [ ram_port.write_enable <--. 1
            ; ram_port.read_enable <--. 0
            ; ram_port.data <-- repeat clear_to (width port.data)
            ; ram_port.address <-- addr.value
            ; addr <-- addr.value +:. 1
            ; when_ (addr.value ==:. size - 1) [ sm.set_next Done ]
            ] )
        ; Done, []
        ]
    ];
  { port = Ram_port.(map2 ram_port port_names ~f:(fun w n -> w.value -- ("int_" ^ n)))
  ; clear_busy = sm.is Clear
  }
;;
