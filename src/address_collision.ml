open Base
open Hardcaml
open Signal

module Protection = struct
  type t =
    | None__there_be_dragons
    | Control_enables
    | Mux_output_ports
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    | None__there_be_dragons
    | Const of int
    | Counter
    | Graycode
    | Lfsr
  [@@deriving sexp_of]

  (* We can create LFSR's of width [2..max_lfsr_width] (which is 168).

     For small LFSRs I worry about the missing value ([2^width-1]). By creating an 8 bit
     larger one it will happen only slightly less often than other values (255 v's 256
     times).

     For large LFSRs just [repeat] a single (large) LFSR up to the required width. Not
     totally random, but probably random enough. *)
  let lfsr spec width =
    let create width =
      reg_fb
        spec
        ~width
          (* We use Xnor as it works starting from [0] so doesn't need to be reset. *)
        ~f:(Hardcaml_circuits.Lfsr.create ~config:Fibonacci ~op:Xnor (module Signal))
    in
    let max_width = Hardcaml_circuits.Lfsr.max_lfsr_width in
    if width > max_width
    then (
      (* repeat to required width *)
      let x = create Hardcaml_circuits.Lfsr.max_lfsr_width in
      sel_bottom (repeat x ~count:((width + max_width + 1) / max_width)) ~width)
    else create width
  ;;

  (* Modify the [q] output of a RAM on collision. *)
  let q t spec ~address_collision q =
    match t with
    | None__there_be_dragons -> q
    | Const i -> mux2 address_collision (of_int ~width:(width q) i) q
    | Counter ->
      mux2 address_collision (reg_fb spec ~width:(width q) ~f:(fun d -> d +:. 1)) q
    | Graycode ->
      mux2
        address_collision
        (reg_fb spec ~width:(width q) ~f:(fun d -> d +:. 1) |> binary_to_gray)
        q
    | Lfsr ->
      mux2
        address_collision
        (reg_fb spec ~width:(width q) ~f:(fun d ->
           (* make a bigger LFSR to mitigate the null-value bias - the larger the better,
              but they also take longer to produce non-zero values. *)
           sel_top (lfsr spec (max (width d) 4)) ~width:(width d)))
        q
  ;;
end
