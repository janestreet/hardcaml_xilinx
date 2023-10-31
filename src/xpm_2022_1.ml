module Xpm_memory_tdpram = struct
  module type P = sig
    val memory_size : int
    val memory_primitive : string
    val clocking_mode : string
    val ecc_mode : string
    val ecc_type : string
    val ecc_bit_range : string
    val memory_init_file : string
    val memory_init_param : string
    val use_mem_init : int
    val use_mem_init_mmi : int
    val wakeup_time : string
    val auto_sleep_time : int
    val message_control : int
    val use_embedded_constraint : int
    val memory_optimization : string
    val cascade_height : int
    val sim_assert_chk : int
    val write_protect : int
    val write_data_width_a : int
    val read_data_width_a : int
    val byte_write_width_a : int
    val addr_width_a : int
    val read_reset_value_a : string
    val read_latency_a : int
    val write_mode_a : string
    val rst_mode_a : string
    val write_data_width_b : int
    val read_data_width_b : int
    val byte_write_width_b : int
    val addr_width_b : int
    val read_reset_value_b : string
    val read_latency_b : int
    val write_mode_b : string
    val rst_mode_b : string
  end

  module P : P = struct
    let memory_size = 2048
    let memory_primitive = "auto"
    let clocking_mode = "common_clock"
    let ecc_mode = "no_ecc"
    let ecc_type = "none"
    let ecc_bit_range = "[7:0]"
    let memory_init_file = "none"
    let memory_init_param = ""
    let use_mem_init = 1
    let use_mem_init_mmi = 0
    let wakeup_time = "disable_sleep"
    let auto_sleep_time = 0
    let message_control = 0
    let use_embedded_constraint = 0
    let memory_optimization = "true"
    let cascade_height = 0
    let sim_assert_chk = 0
    let write_protect = 1
    let write_data_width_a = 32
    let read_data_width_a = 32
    let byte_write_width_a = 32
    let addr_width_a = 6
    let read_reset_value_a = "0"
    let read_latency_a = 2
    let write_mode_a = "no_change"
    let rst_mode_a = "SYNC"
    let write_data_width_b = 32
    let read_data_width_b = 32
    let byte_write_width_b = 32
    let addr_width_b = 6
    let read_reset_value_b = "0"
    let read_latency_b = 2
    let write_mode_b = "no_change"
    let rst_mode_b = "SYNC"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"MEMORY_SIZE" ~value:(Int P.memory_size)
      ; Hardcaml.Parameter.create
          ~name:"MEMORY_PRIMITIVE"
          ~value:(String P.memory_primitive)
      ; Hardcaml.Parameter.create ~name:"CLOCKING_MODE" ~value:(String P.clocking_mode)
      ; Hardcaml.Parameter.create ~name:"ECC_MODE" ~value:(String P.ecc_mode)
      ; Hardcaml.Parameter.create ~name:"ECC_TYPE" ~value:(String P.ecc_type)
      ; Hardcaml.Parameter.create ~name:"ECC_BIT_RANGE" ~value:(String P.ecc_bit_range)
      ; Hardcaml.Parameter.create
          ~name:"MEMORY_INIT_FILE"
          ~value:(String P.memory_init_file)
      ; Hardcaml.Parameter.create
          ~name:"MEMORY_INIT_PARAM"
          ~value:(String P.memory_init_param)
      ; Hardcaml.Parameter.create ~name:"USE_MEM_INIT" ~value:(Int P.use_mem_init)
      ; Hardcaml.Parameter.create ~name:"USE_MEM_INIT_MMI" ~value:(Int P.use_mem_init_mmi)
      ; Hardcaml.Parameter.create ~name:"WAKEUP_TIME" ~value:(String P.wakeup_time)
      ; Hardcaml.Parameter.create ~name:"AUTO_SLEEP_TIME" ~value:(Int P.auto_sleep_time)
      ; Hardcaml.Parameter.create ~name:"MESSAGE_CONTROL" ~value:(Int P.message_control)
      ; Hardcaml.Parameter.create
          ~name:"USE_EMBEDDED_CONSTRAINT"
          ~value:(Int P.use_embedded_constraint)
      ; Hardcaml.Parameter.create
          ~name:"MEMORY_OPTIMIZATION"
          ~value:(String P.memory_optimization)
      ; Hardcaml.Parameter.create ~name:"CASCADE_HEIGHT" ~value:(Int P.cascade_height)
      ; Hardcaml.Parameter.create ~name:"SIM_ASSERT_CHK" ~value:(Int P.sim_assert_chk)
      ; Hardcaml.Parameter.create ~name:"WRITE_PROTECT" ~value:(Int P.write_protect)
      ; Hardcaml.Parameter.create
          ~name:"WRITE_DATA_WIDTH_A"
          ~value:(Int P.write_data_width_a)
      ; Hardcaml.Parameter.create
          ~name:"READ_DATA_WIDTH_A"
          ~value:(Int P.read_data_width_a)
      ; Hardcaml.Parameter.create
          ~name:"BYTE_WRITE_WIDTH_A"
          ~value:(Int P.byte_write_width_a)
      ; Hardcaml.Parameter.create ~name:"ADDR_WIDTH_A" ~value:(Int P.addr_width_a)
      ; Hardcaml.Parameter.create
          ~name:"READ_RESET_VALUE_A"
          ~value:(String P.read_reset_value_a)
      ; Hardcaml.Parameter.create ~name:"READ_LATENCY_A" ~value:(Int P.read_latency_a)
      ; Hardcaml.Parameter.create ~name:"WRITE_MODE_A" ~value:(String P.write_mode_a)
      ; Hardcaml.Parameter.create ~name:"RST_MODE_A" ~value:(String P.rst_mode_a)
      ; Hardcaml.Parameter.create
          ~name:"WRITE_DATA_WIDTH_B"
          ~value:(Int P.write_data_width_b)
      ; Hardcaml.Parameter.create
          ~name:"READ_DATA_WIDTH_B"
          ~value:(Int P.read_data_width_b)
      ; Hardcaml.Parameter.create
          ~name:"BYTE_WRITE_WIDTH_B"
          ~value:(Int P.byte_write_width_b)
      ; Hardcaml.Parameter.create ~name:"ADDR_WIDTH_B" ~value:(Int P.addr_width_b)
      ; Hardcaml.Parameter.create
          ~name:"READ_RESET_VALUE_B"
          ~value:(String P.read_reset_value_b)
      ; Hardcaml.Parameter.create ~name:"READ_LATENCY_B" ~value:(Int P.read_latency_b)
      ; Hardcaml.Parameter.create ~name:"WRITE_MODE_B" ~value:(String P.write_mode_b)
      ; Hardcaml.Parameter.create ~name:"RST_MODE_B" ~value:(String P.rst_mode_b)
      ]
    ;;

    module I = struct
      type 'a t =
        { sleep : 'a [@bits 1] [@rtlname "sleep"]
        ; clka : 'a [@bits 1] [@rtlname "clka"]
        ; rsta : 'a [@bits 1] [@rtlname "rsta"]
        ; ena : 'a [@bits 1] [@rtlname "ena"]
        ; regcea : 'a [@bits 1] [@rtlname "regcea"]
        ; wea : 'a
             [@bits (P.write_data_width_a / P.byte_write_width_a) - 1 - 0 + 1]
             [@rtlname "wea"]
        ; addra : 'a [@bits P.addr_width_a - 1 - 0 + 1] [@rtlname "addra"]
        ; dina : 'a [@bits P.write_data_width_a - 1 - 0 + 1] [@rtlname "dina"]
        ; injectsbiterra : 'a [@bits 1] [@rtlname "injectsbiterra"]
        ; injectdbiterra : 'a [@bits 1] [@rtlname "injectdbiterra"]
        ; clkb : 'a [@bits 1] [@rtlname "clkb"]
        ; rstb : 'a [@bits 1] [@rtlname "rstb"]
        ; enb : 'a [@bits 1] [@rtlname "enb"]
        ; regceb : 'a [@bits 1] [@rtlname "regceb"]
        ; web : 'a
             [@bits (P.write_data_width_b / P.byte_write_width_b) - 1 - 0 + 1]
             [@rtlname "web"]
        ; addrb : 'a [@bits P.addr_width_b - 1 - 0 + 1] [@rtlname "addrb"]
        ; dinb : 'a [@bits P.write_data_width_b - 1 - 0 + 1] [@rtlname "dinb"]
        ; injectsbiterrb : 'a [@bits 1] [@rtlname "injectsbiterrb"]
        ; injectdbiterrb : 'a [@bits 1] [@rtlname "injectdbiterrb"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { douta : 'a [@bits P.read_data_width_a - 1 - 0 + 1] [@rtlname "douta"]
        ; sbiterra : 'a [@bits 1] [@rtlname "sbiterra"]
        ; dbiterra : 'a [@bits 1] [@rtlname "dbiterra"]
        ; doutb : 'a [@bits P.read_data_width_b - 1 - 0 + 1] [@rtlname "doutb"]
        ; sbiterrb : 'a [@bits 1] [@rtlname "sbiterrb"]
        ; dbiterrb : 'a [@bits 1] [@rtlname "dbiterrb"]
        }
      [@@deriving hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create
      ?lib
      ?arch
      ?attributes
      ?instance
      ?(name = "xpm_memory_tdpram")
      ?parameters
      inputs
      =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module Xpm_fifo_async = struct
  module type P = sig
    val fifo_memory_type : string
    val fifo_write_depth : int
    val cascade_height : int
    val related_clocks : int
    val write_data_width : int
    val read_mode : string
    val fifo_read_latency : int
    val full_reset_value : int
    val use_adv_features : string
    val read_data_width : int
    val cdc_sync_stages : int
    val wr_data_count_width : int
    val prog_full_thresh : int
    val rd_data_count_width : int
    val prog_empty_thresh : int
    val dout_reset_value : string
    val ecc_mode : string
    val sim_assert_chk : int
    val wakeup_time : int
  end

  module P : P = struct
    let fifo_memory_type = "auto"
    let fifo_write_depth = 2048
    let cascade_height = 0
    let related_clocks = 0
    let write_data_width = 32
    let read_mode = "std"
    let fifo_read_latency = 1
    let full_reset_value = 0
    let use_adv_features = "0707"
    let read_data_width = 32
    let cdc_sync_stages = 2
    let wr_data_count_width = 1
    let prog_full_thresh = 10
    let rd_data_count_width = 1
    let prog_empty_thresh = 10
    let dout_reset_value = "0"
    let ecc_mode = "no_ecc"
    let sim_assert_chk = 0
    let wakeup_time = 0
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create
          ~name:"FIFO_MEMORY_TYPE"
          ~value:(String P.fifo_memory_type)
      ; Hardcaml.Parameter.create ~name:"FIFO_WRITE_DEPTH" ~value:(Int P.fifo_write_depth)
      ; Hardcaml.Parameter.create ~name:"CASCADE_HEIGHT" ~value:(Int P.cascade_height)
      ; Hardcaml.Parameter.create ~name:"RELATED_CLOCKS" ~value:(Int P.related_clocks)
      ; Hardcaml.Parameter.create ~name:"WRITE_DATA_WIDTH" ~value:(Int P.write_data_width)
      ; Hardcaml.Parameter.create ~name:"READ_MODE" ~value:(String P.read_mode)
      ; Hardcaml.Parameter.create
          ~name:"FIFO_READ_LATENCY"
          ~value:(Int P.fifo_read_latency)
      ; Hardcaml.Parameter.create ~name:"FULL_RESET_VALUE" ~value:(Int P.full_reset_value)
      ; Hardcaml.Parameter.create
          ~name:"USE_ADV_FEATURES"
          ~value:(String P.use_adv_features)
      ; Hardcaml.Parameter.create ~name:"READ_DATA_WIDTH" ~value:(Int P.read_data_width)
      ; Hardcaml.Parameter.create ~name:"CDC_SYNC_STAGES" ~value:(Int P.cdc_sync_stages)
      ; Hardcaml.Parameter.create
          ~name:"WR_DATA_COUNT_WIDTH"
          ~value:(Int P.wr_data_count_width)
      ; Hardcaml.Parameter.create ~name:"PROG_FULL_THRESH" ~value:(Int P.prog_full_thresh)
      ; Hardcaml.Parameter.create
          ~name:"RD_DATA_COUNT_WIDTH"
          ~value:(Int P.rd_data_count_width)
      ; Hardcaml.Parameter.create
          ~name:"PROG_EMPTY_THRESH"
          ~value:(Int P.prog_empty_thresh)
      ; Hardcaml.Parameter.create
          ~name:"DOUT_RESET_VALUE"
          ~value:(String P.dout_reset_value)
      ; Hardcaml.Parameter.create ~name:"ECC_MODE" ~value:(String P.ecc_mode)
      ; Hardcaml.Parameter.create ~name:"SIM_ASSERT_CHK" ~value:(Int P.sim_assert_chk)
      ; Hardcaml.Parameter.create ~name:"WAKEUP_TIME" ~value:(Int P.wakeup_time)
      ]
    ;;

    module I = struct
      type 'a t =
        { sleep : 'a [@bits 1] [@rtlname "sleep"]
        ; rst : 'a [@bits 1] [@rtlname "rst"]
        ; wr_clk : 'a [@bits 1] [@rtlname "wr_clk"]
        ; wr_en : 'a [@bits 1] [@rtlname "wr_en"]
        ; din : 'a [@bits P.write_data_width - 1 - 0 + 1] [@rtlname "din"]
        ; rd_clk : 'a [@bits 1] [@rtlname "rd_clk"]
        ; rd_en : 'a [@bits 1] [@rtlname "rd_en"]
        ; injectsbiterr : 'a [@bits 1] [@rtlname "injectsbiterr"]
        ; injectdbiterr : 'a [@bits 1] [@rtlname "injectdbiterr"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { full : 'a [@bits 1] [@rtlname "full"]
        ; prog_full : 'a [@bits 1] [@rtlname "prog_full"]
        ; wr_data_count : 'a
             [@bits P.wr_data_count_width - 1 - 0 + 1] [@rtlname "wr_data_count"]
        ; overflow : 'a [@bits 1] [@rtlname "overflow"]
        ; wr_rst_busy : 'a [@bits 1] [@rtlname "wr_rst_busy"]
        ; almost_full : 'a [@bits 1] [@rtlname "almost_full"]
        ; wr_ack : 'a [@bits 1] [@rtlname "wr_ack"]
        ; dout : 'a [@bits P.read_data_width - 1 - 0 + 1] [@rtlname "dout"]
        ; empty : 'a [@bits 1] [@rtlname "empty"]
        ; prog_empty : 'a [@bits 1] [@rtlname "prog_empty"]
        ; rd_data_count : 'a
             [@bits P.rd_data_count_width - 1 - 0 + 1] [@rtlname "rd_data_count"]
        ; underflow : 'a [@bits 1] [@rtlname "underflow"]
        ; rd_rst_busy : 'a [@bits 1] [@rtlname "rd_rst_busy"]
        ; almost_empty : 'a [@bits 1] [@rtlname "almost_empty"]
        ; data_valid : 'a [@bits 1] [@rtlname "data_valid"]
        ; sbiterr : 'a [@bits 1] [@rtlname "sbiterr"]
        ; dbiterr : 'a [@bits 1] [@rtlname "dbiterr"]
        }
      [@@deriving hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create
      ?lib
      ?arch
      ?attributes
      ?instance
      ?(name = "xpm_fifo_async")
      ?parameters
      inputs
      =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module Xpm_fifo_sync = struct
  module type P = sig
    val fifo_memory_type : string
    val fifo_write_depth : int
    val cascade_height : int
    val write_data_width : int
    val read_mode : string
    val fifo_read_latency : int
    val full_reset_value : int
    val use_adv_features : string
    val read_data_width : int
    val wr_data_count_width : int
    val prog_full_thresh : int
    val rd_data_count_width : int
    val prog_empty_thresh : int
    val dout_reset_value : string
    val ecc_mode : string
    val sim_assert_chk : int
    val wakeup_time : int
  end

  module P : P = struct
    let fifo_memory_type = "auto"
    let fifo_write_depth = 2048
    let cascade_height = 0
    let write_data_width = 32
    let read_mode = "std"
    let fifo_read_latency = 1
    let full_reset_value = 0
    let use_adv_features = "0707"
    let read_data_width = 32
    let wr_data_count_width = 1
    let prog_full_thresh = 10
    let rd_data_count_width = 1
    let prog_empty_thresh = 10
    let dout_reset_value = "0"
    let ecc_mode = "no_ecc"
    let sim_assert_chk = 0
    let wakeup_time = 0
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create
          ~name:"FIFO_MEMORY_TYPE"
          ~value:(String P.fifo_memory_type)
      ; Hardcaml.Parameter.create ~name:"FIFO_WRITE_DEPTH" ~value:(Int P.fifo_write_depth)
      ; Hardcaml.Parameter.create ~name:"CASCADE_HEIGHT" ~value:(Int P.cascade_height)
      ; Hardcaml.Parameter.create ~name:"WRITE_DATA_WIDTH" ~value:(Int P.write_data_width)
      ; Hardcaml.Parameter.create ~name:"READ_MODE" ~value:(String P.read_mode)
      ; Hardcaml.Parameter.create
          ~name:"FIFO_READ_LATENCY"
          ~value:(Int P.fifo_read_latency)
      ; Hardcaml.Parameter.create ~name:"FULL_RESET_VALUE" ~value:(Int P.full_reset_value)
      ; Hardcaml.Parameter.create
          ~name:"USE_ADV_FEATURES"
          ~value:(String P.use_adv_features)
      ; Hardcaml.Parameter.create ~name:"READ_DATA_WIDTH" ~value:(Int P.read_data_width)
      ; Hardcaml.Parameter.create
          ~name:"WR_DATA_COUNT_WIDTH"
          ~value:(Int P.wr_data_count_width)
      ; Hardcaml.Parameter.create ~name:"PROG_FULL_THRESH" ~value:(Int P.prog_full_thresh)
      ; Hardcaml.Parameter.create
          ~name:"RD_DATA_COUNT_WIDTH"
          ~value:(Int P.rd_data_count_width)
      ; Hardcaml.Parameter.create
          ~name:"PROG_EMPTY_THRESH"
          ~value:(Int P.prog_empty_thresh)
      ; Hardcaml.Parameter.create
          ~name:"DOUT_RESET_VALUE"
          ~value:(String P.dout_reset_value)
      ; Hardcaml.Parameter.create ~name:"ECC_MODE" ~value:(String P.ecc_mode)
      ; Hardcaml.Parameter.create ~name:"SIM_ASSERT_CHK" ~value:(Int P.sim_assert_chk)
      ; Hardcaml.Parameter.create ~name:"WAKEUP_TIME" ~value:(Int P.wakeup_time)
      ]
    ;;

    module I = struct
      type 'a t =
        { sleep : 'a [@bits 1] [@rtlname "sleep"]
        ; rst : 'a [@bits 1] [@rtlname "rst"]
        ; wr_clk : 'a [@bits 1] [@rtlname "wr_clk"]
        ; wr_en : 'a [@bits 1] [@rtlname "wr_en"]
        ; din : 'a [@bits P.write_data_width - 1 - 0 + 1] [@rtlname "din"]
        ; rd_en : 'a [@bits 1] [@rtlname "rd_en"]
        ; injectsbiterr : 'a [@bits 1] [@rtlname "injectsbiterr"]
        ; injectdbiterr : 'a [@bits 1] [@rtlname "injectdbiterr"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { full : 'a [@bits 1] [@rtlname "full"]
        ; prog_full : 'a [@bits 1] [@rtlname "prog_full"]
        ; wr_data_count : 'a
             [@bits P.wr_data_count_width - 1 - 0 + 1] [@rtlname "wr_data_count"]
        ; overflow : 'a [@bits 1] [@rtlname "overflow"]
        ; wr_rst_busy : 'a [@bits 1] [@rtlname "wr_rst_busy"]
        ; almost_full : 'a [@bits 1] [@rtlname "almost_full"]
        ; wr_ack : 'a [@bits 1] [@rtlname "wr_ack"]
        ; dout : 'a [@bits P.read_data_width - 1 - 0 + 1] [@rtlname "dout"]
        ; empty : 'a [@bits 1] [@rtlname "empty"]
        ; prog_empty : 'a [@bits 1] [@rtlname "prog_empty"]
        ; rd_data_count : 'a
             [@bits P.rd_data_count_width - 1 - 0 + 1] [@rtlname "rd_data_count"]
        ; underflow : 'a [@bits 1] [@rtlname "underflow"]
        ; rd_rst_busy : 'a [@bits 1] [@rtlname "rd_rst_busy"]
        ; almost_empty : 'a [@bits 1] [@rtlname "almost_empty"]
        ; data_valid : 'a [@bits 1] [@rtlname "data_valid"]
        ; sbiterr : 'a [@bits 1] [@rtlname "sbiterr"]
        ; dbiterr : 'a [@bits 1] [@rtlname "dbiterr"]
        }
      [@@deriving hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create
      ?lib
      ?arch
      ?attributes
      ?instance
      ?(name = "xpm_fifo_sync")
      ?parameters
      inputs
      =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end
