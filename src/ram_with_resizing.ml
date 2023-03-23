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

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_address : 'a [@bits write_address_bits]
      ; write_data : 'a [@bits write_data_bits]
      ; write_enable : 'a
      ; read_address : 'a [@bits read_address_bits]
      ; read_enable : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits read_data_bits] } [@@deriving sexp_of, hardcaml]
  end

  let create _scope ~build_mode (i : _ I.t) =
    let ram =
      True_dual_port_ram.create
        ~read_latency
        ~build_mode
        ~arch:(Blockram (Option.value ~default:Read_before_write collision_mode))
        ()
        ~clock_a:i.clock
        ~clock_b:i.clock
        ~clear_a:i.clear
        ~clear_b:i.clear
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
          ; write_enable = gnd
          ; read_enable = i.read_enable
          }
    in
    { O.q = snd ram }
  ;;

  let hierarchical ?instance scope ~build_mode =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ?instance ~scope ~name:"ram_with_resizing" (create ~build_mode)
  ;;
end
