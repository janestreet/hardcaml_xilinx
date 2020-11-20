type t =
  | Read_before_write
  | Write_before_read
  | No_change
[@@deriving sexp_of]

let to_xpm_parameter = function
  | Read_before_write -> "read_first"
  | Write_before_read -> "write_first"
  | No_change -> "no_change"
;;
