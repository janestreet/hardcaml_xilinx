type t =
  | Read_before_write
  | Write_before_read
  | No_change
[@@deriving sexp_of]

val to_xpm_parameter : t -> string
