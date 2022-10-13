(** A general-purpose means of representing memories. The [Config.t] type
    allows the user to configure the underlying memory implementations. Eg:
    Using URAM for bits 0-72, and BRAMs for bits 73-80. This module allows
    construction of memories in 1D or 2D Modes. See further documentation
    below.
*)

open Base
open Hardcaml

module For_deriving : sig
  module type Sexp_of_m = sig end
end

(** Configuration for the memory builder. See documentation below for
    elaboration about the purpose of configuration fields.
*)
module Config : sig
  type inferred_memory =
    { rtl_attributes : Rtl_attribute.t list option
    ; rw_order : [ `Wbr | `Rbw ]
    }
  [@@deriving sexp_of, fields]

  type how_to_instantiate_ram =
    | Xpm of Ram_arch.t
    | Inferred of inferred_memory
  [@@deriving sexp_of]

  type underlying_memory =
    { data_width : int
    ; cascade_height : Cascade_height.t
    ; how_to_instantiate_ram : how_to_instantiate_ram
    ; simulation_name : string option
    }
  [@@deriving fields]

  type t =
    { underlying_memories : underlying_memory list
    ; underlying_ram_read_latency : int
    ; vertical_dimension : int
    ; horizontal_dimension : int
    ; combinational_output : bool
    }

  (** The size of the outermost inner dimension. ie: shape[1]  *)
  val vertical_dimension : t -> int

  (** The bit width for the horizontal dimension. Note that this will return 0 when the
      appropriate dimension is zero.
  *)
  val horizontal_index_width : t -> int

  (** Similar to [horizontal_index_width], but for the vertical index *)
  val vertical_index_width : t -> int

  (** Equivalent to
      [underlying_ram_read_latency + if combinational_output then 1 else 0]
  *)
  val read_latency : t -> int

  (** Create the simplest possible 1D configuration with a single RAM architecture. *)
  val create_simple_1d_config
    :  depth:int
    -> num_bits_per_entry:int
    -> ram_read_latency:int
    -> how_to_instantiate_ram:how_to_instantiate_ram
    -> simulation_name:string option
    -> t

  module type S = sig
    val t : t
  end

  val as_module : t -> (module S)
end

type 'a t

module Port_label : sig
  type t =
    | A
    | B
  [@@deriving sexp_of, compare, enumerate]
end

module Create (M : Hardcaml.Interface.S) : sig
  val create_simple_1d
    :  ?name:string
    -> ?simulation_name:string
    -> instance:string
    -> build_mode:Build_mode.t
    -> depth:int
    -> ram_read_latency:int
    -> how_to_instantiate_ram:Config.how_to_instantiate_ram
    -> scope:Scope.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> unit
    -> Signal.t M.t t

  val create
    :  ?name:string
    -> instance:string
    -> build_mode:Build_mode.t
    -> config:Config.t
    -> scope:Scope.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> unit
    -> Signal.t M.t t
end

(** Returns the read latency of the memory, including possibly any
    combinational latency due to muxing.
*)
val read_latency : _ t -> int

(** Must be called strictly after all [set_{read/write}_] functions have been
    called. This assigns to the underlying instantiated memories, and asserts
    that the memory satisfies the config's requirements.
*)
val complete : _ t -> unit

(** {2 Straightforward 1D Memories}

    1-dimensional memories are laid out the way one would expect. A memory
    of depth [d], with columns shared between URAM and BRAM will have the
    following layout:

    |     URAM    |  BRAM  |
    | x[0]                 |
    | x[1]                 |
    | ....                 |
    | x[d-1]               |
    ------------------------

    [Read_port_1d] does not implement Hardcaml.Interface.S, since the [address]
    and [data] are not necessarily known without a config. To get something
    one can use in Hardcaml interfaces, you'll have to specialize it with
    [Specialize].
*)

module type Widths_1d = sig
  val address_width : int
end

(** General purpose 1D read ports. *)
module Read_port_1d : sig
  type 'a t =
    { address : 'a
    ; enable : 'a
    }
  [@@deriving sexp_of]

  module type S = Hardcaml.Interface.S with type 'a t = 'a t

  (** Specialize a [Read_port_1d.t] given widths. *)
  module Specialize (_ : Widths_1d) : S

  (** Specialize a [Read_port_1d.t] given a memory configuration. *)
  module Specialize_with_config (_ : Config.S) : S
end

(** General purpose 1D Write ports. Note that it is possible to write
    ['a Write_port_1d.M(Foo).t] in type declarations in mlis.
*)
module Write_port_1d : sig
  type ('a, 'write_data) t =
    { address : 'a
    ; enable : 'a
    ; data : 'write_data
    }
  [@@deriving sexp_of]

  module type S = sig
    type 'a write_data

    include Hardcaml.Interface.S with type 'a t = ('a, 'a write_data) t
  end

  (** Specialize a [Write_port_1d.t] given widths. *)
  module Specialize (M : Hardcaml.Interface.S) (_ : Widths_1d) :
    S with type 'a write_data := 'a M.t

  (** Specialize a [Write_port_1d.t] given a memory configuration. *)
  module Specialize_with_config (M : Hardcaml.Interface.S) (_ : Config.S) :
    S with type 'a write_data := 'a M.t

  (** Shorthand useful for writing ['a Write_port_1d.M(Foo).t] in type
      signatures. *)
  module M (X : T1) : sig
    type nonrec 'a t = ('a, 'a X.t) t

    module type S = S with type 'a write_data := 'a X.t
  end

  val sexp_of_m__t
    :  (module For_deriving.Sexp_of_m)
    -> ('a -> Sexp.t)
    -> ('a, _) t
    -> Sexp.t
end

(** Assigns the read port for 1D memories. Raises a runtime exception when
    called on a non single dimensional memory.
*)
val set_read_port_1d : 'data t -> Port_label.t -> Signal.t Read_port_1d.t -> 'data

(** Similar to [set_read_port_1d], but for write ports.*)
val set_write_port_1d
  :  'data t
  -> Port_label.t
  -> (Signal.t, 'data) Write_port_1d.t
  -> unit

(** {2 General-Purpose "2D" Memories}

    The more general-purporse memory builders can be used to construct memories
    with the layout [H * V] where every entry is [B] bits wide. One can
    read from any of the entires through the read ports. When writing, however,
    an entire horizontal index must be written to at once, namely x[*, v]. This
    is because the memory is represented using parallel columns of memories
    along the H axis.

    Under the hood, this constructs a memory of depth [V] and width [H * B]
    using the [underlying_memories] constructs provided by the [Config.t].
    When reading (h, v), the memory reads the underlying memory at index [v],
    which returns [H] copies of the the data, and multiplexes them with [h].
    For that reason, [V] must be a power of two, but [H] does not have
    to be.

    Here's a possible underlying layout by using this memory builder, for H=3,
    V=8, implemented with 2 URAMs and 1 BRAMs.

    |     URAM     |   BRAM   |   BRAM   |
    | x[0][0]    | x[1][0]    | x[2][0] |
    | x[0][1]    | x[1][1]    | x[2][1] |
    | x[0][2]    | x[1][2]    | x[2][2] |
    | x[0][3]    | x[1][3]    | x[2][3] |
    | x[0][4]    | x[1][4]    | x[2][4] |
    | x[0][5]    | x[1][5]    | x[2][5] |
    | x[0][6]    | x[1][6]    | x[2][6] |
    | x[0][7]    | x[1][7]    | x[2][7] |
    --------------------------------------

    Since writes to a row are atomic, for a given [v] and [d], all entries at
    x[{0, 1, 2}][d] must be written to simultaneously.

    Note that 1D memories are simply a special case of 2D memories where H = 1.
*)

module type Widths_2d = sig
  val vertical_index_width : int
  val horizontal_dimension : int
end

(** General-purpose Read Ports. *)
module Read_port_2d : sig
  type 'a t =
    { horizontal_index : 'a
    ; vertical_index : 'a
    ; enable : 'a
    }
  [@@deriving sexp_of]

  module Specialize_with_config (_ : Config.S) :
    Hardcaml.Interface.S with type 'a t = 'a t
end

(** General purpose write ports. Note that it is possible to write
    ['a Write_port_2d.M(Foo).t in type declarations in mlis
*)
module Write_port_2d : sig
  type ('a, 'write_data) t =
    { vertical_index : 'a
    ; enable : 'a
    ; data : 'write_data list
    }
  [@@deriving sexp_of]

  val map
    :  ('a, 'data_a) t
    -> f_write_data:('data_a -> 'data_b)
    -> f:('a -> 'b)
    -> ('b, 'data_b) t

  val and_enable : with_:Signal.t -> (Signal.t, 'a) t -> (Signal.t, 'a) t

  module type S = sig
    type 'a write_data

    include Hardcaml.Interface.S with type 'a t = ('a, 'a write_data) t
  end

  (** Specialize a [Read_port_2d.t] given widths. *)
  module Specialize (M : Hardcaml.Interface.S) (_ : Widths_2d) :
    S with type 'a write_data := 'a M.t

  (** Specialize a [Write_port_2d.t] given a memory configuration. *)
  module Specialize_with_config (M : Hardcaml.Interface.S) (_ : Config.S) :
    S with type 'a write_data := 'a M.t

  (** Shorthand useful for writing ['a Write_port_2d.M(Foo).t] in type
      signatures. *)
  module M (M : T1) : sig
    type nonrec 'a t = ('a, 'a M.t) t

    module type S = S with type 'a write_data := 'a M.t
  end

  val sexp_of_m__t
    :  (module For_deriving.Sexp_of_m)
    -> ('a -> Sexp.t)
    -> ('a, 'data) t
    -> Sexp.t
end

(** Set the write port. *)
val set_write_port_2d
  :  'data t
  -> Port_label.t
  -> (Signal.t, 'data) Write_port_2d.t
  -> unit

(** Set the read port. This should only ever be called once. *)
val set_read_port_2d : 'data t -> Port_label.t -> Signal.t Read_port_2d.t -> 'data

(** Shorthand useful for writing Memory_builder.M(Foo).t *)
module M (X : T1) : sig
  type nonrec t = Signal.t X.t t
end
