(** Optionally generate XPM URAM instances with an additional wrapper layer to avoid
    issues where Vivado will change the read before write and write before read behavior
    of the RAM causing simulation mismatches. Only seems to occur when READ_LATENCY is
    greater than 1. *)

type t =
  | Wrap_in_module_with_keep_directives
  (** Generate the XPM in a hierarchical wrapper module and place KEEP directives on all
      ports. In testing this stopped Vivado from remapping the A and B ports arbitrarily.

      Only applies when targeting URAMs specifically. *)
  | Let_vivado_decide (** Let Vivado have it's way. *)
[@@deriving sexp_of]
