open! Base
open! Hardcaml

module type Config = Ram_with_resizing_intf.Config
module type S = Ram_with_resizing_intf.S

module Make (Config : Config) = struct
  open Config
  open Signal

  let () =
    assert (log_num_words > 0);
    assert (bits_per_word > 0)
  ;;

  let write_address_bits, read_address_bits, write_data_bits, read_data_bits =
    if log_scale_between_ports = 0
    then log_num_words, log_num_words, bits_per_word, bits_per_word
    else if log_scale_between_ports > 0
    then
      ( log_num_words
      , log_num_words - log_scale_between_ports
      , bits_per_word
      , bits_per_word * (1 lsl log_scale_between_ports) )
    else
      (* log_scale_between_ports < 0 *)
      ( log_num_words + log_scale_between_ports
      , log_num_words
      , bits_per_word * (1 lsl -log_scale_between_ports)
      , bits_per_word )
  ;;

  let () =
    if write_address_bits <= 0
    then
      raise_s
        [%message
          "Cannot construct RAM with < 2 write entries"
            (write_address_bits : int)
            (read_address_bits : int)
            (write_data_bits : int)
            (read_data_bits : int)];
    if read_address_bits <= 0
    then
      raise_s
        [%message
          "Cannot construct RAM with < 2 read entries"
            (write_address_bits : int)
            (read_address_bits : int)
            (write_data_bits : int)
            (read_data_bits : int)]
  ;;

  let write_enable_bits_of ~write_data_bits =
    let byte_write_width_in_bits =
      match byte_write_width with
      | None -> write_data_bits
      | Some v ->
        (match v with
         | Byte_write_width.B8 -> 8
         | B9 -> 9
         | Full -> write_data_bits)
    in
    assert (write_data_bits % byte_write_width_in_bits = 0);
    write_data_bits / byte_write_width_in_bits
  ;;

  let write_enable_bits = write_enable_bits_of ~write_data_bits

  module I = struct
    type 'a t =
      { write_clock : 'a
      ; write_clear : 'a
      ; write_address : 'a [@bits write_address_bits]
      ; write_data : 'a [@bits write_data_bits]
      ; write_enable : 'a [@bits write_enable_bits]
      ; read_clock : 'a
      ; read_clear : 'a
      ; read_address : 'a [@bits read_address_bits]
      ; read_enable : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits read_data_bits] } [@@deriving hardcaml]
  end

  let create ?address_collision_model _scope ~build_mode (i : _ I.t) =
    let ram =
      True_dual_port_ram.create
        ?address_collision_model
        ~read_latency
        ~build_mode
        ~arch:(Blockram (Option.value ~default:Read_before_write collision_mode))
        ?byte_write_width
        ?clocking_mode
        ()
        ~clock_a:i.write_clock
        ~clock_b:i.read_clock
        ~clear_a:i.write_clear
        ~clear_b:i.read_clear
        ~size:(1 lsl write_address_bits)
        ~port_a:
          { address = i.write_address
          ; data = i.write_data
          ; write_enable = i.write_enable
          ; read_enable = gnd
          }
        ~port_b:
          { address = i.read_address
          ; data = zero read_data_bits
          ; write_enable = zero (write_enable_bits_of ~write_data_bits:read_data_bits)
          ; read_enable = i.read_enable
          }
    in
    { O.q = snd ram }
  ;;

  let hierarchical ?instance ?address_collision_model scope ~build_mode =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical
      ?instance
      ~scope
      ~name:"ram_with_resizing"
      (create ?address_collision_model ~build_mode)
  ;;
end
