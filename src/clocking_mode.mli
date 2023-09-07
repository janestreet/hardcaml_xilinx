(** The clocking mode to use when constructing a true dual port RAM. This is necessary
    when using a BRAM with multiple clocks. *)
type t =
  | Common_clock
  | Independent_clock
[@@deriving sexp_of]

val to_xpm_args : t -> string
