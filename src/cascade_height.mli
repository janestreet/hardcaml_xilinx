(** Height of BRAM cascades chains. This can be explicitly specified to help vivado meet
    timing when it is unnecessarily cascading BRAMs. *)

type t =
  | Inferred
  | Specified of int
[@@deriving sexp_of]

val to_xpm_args : t -> int
