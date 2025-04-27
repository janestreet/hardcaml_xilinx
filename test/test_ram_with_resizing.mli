module For_rtl_sim : sig
  (** Print a verilog circuit which instantiates both the rtl and xpm models and check the
      outputs are the same. *)
  val generate : unit -> unit
end
