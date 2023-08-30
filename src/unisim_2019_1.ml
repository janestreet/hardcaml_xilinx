module IBUF = struct
  module type P = sig
    val capacitance : string
    val ibuf_delay_value : string
    val ibuf_low_pwr : string
    val ifd_delay_value : string
    val iostandard : string
  end

  module P : P = struct
    let capacitance = "DONT_CARE"
    let ibuf_delay_value = "0"
    let ibuf_low_pwr = "TRUE"
    let ifd_delay_value = "AUTO"
    let iostandard = "DEFAULT"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"CAPACITANCE" ~value:(String P.capacitance)
      ; Hardcaml.Parameter.create
          ~name:"IBUF_DELAY_VALUE"
          ~value:(String P.ibuf_delay_value)
      ; Hardcaml.Parameter.create ~name:"IBUF_LOW_PWR" ~value:(String P.ibuf_low_pwr)
      ; Hardcaml.Parameter.create
          ~name:"IFD_DELAY_VALUE"
          ~value:(String P.ifd_delay_value)
      ; Hardcaml.Parameter.create ~name:"IOSTANDARD" ~value:(String P.iostandard)
      ]
    ;;

    module I = struct
      type 'a t = { i : 'a [@bits 1] [@rtlname "I"] } [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "IBUF") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module IBUFDS = struct
  module type P = sig
    val capacitance : string
    val diff_term : string
    val dqs_bias : string
    val ibuf_delay_value : string
    val ibuf_low_pwr : string
    val ifd_delay_value : string
    val iostandard : string
  end

  module P : P = struct
    let capacitance = "DONT_CARE"
    let diff_term = "FALSE"
    let dqs_bias = "FALSE"
    let ibuf_delay_value = "0"
    let ibuf_low_pwr = "TRUE"
    let ifd_delay_value = "AUTO"
    let iostandard = "DEFAULT"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"CAPACITANCE" ~value:(String P.capacitance)
      ; Hardcaml.Parameter.create ~name:"DIFF_TERM" ~value:(String P.diff_term)
      ; Hardcaml.Parameter.create ~name:"DQS_BIAS" ~value:(String P.dqs_bias)
      ; Hardcaml.Parameter.create
          ~name:"IBUF_DELAY_VALUE"
          ~value:(String P.ibuf_delay_value)
      ; Hardcaml.Parameter.create ~name:"IBUF_LOW_PWR" ~value:(String P.ibuf_low_pwr)
      ; Hardcaml.Parameter.create
          ~name:"IFD_DELAY_VALUE"
          ~value:(String P.ifd_delay_value)
      ; Hardcaml.Parameter.create ~name:"IOSTANDARD" ~value:(String P.iostandard)
      ]
    ;;

    module I = struct
      type 'a t =
        { i : 'a [@bits 1] [@rtlname "I"]
        ; ib : 'a [@bits 1] [@rtlname "IB"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "IBUFDS") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module IBUFDS_GTE4 = struct
  module type P = sig
    val refclk_en_tx_path : Hardcaml.Logic.Std_logic.t
    val refclk_hrow_ck_sel : Hardcaml.Logic.Std_logic_vector.t
    val refclk_icntl_rx : Hardcaml.Logic.Std_logic_vector.t
  end

  module P : P = struct
    let refclk_en_tx_path = Hardcaml.Logic.Std_logic.L0
    let refclk_hrow_ck_sel = Hardcaml.Logic.Std_logic_vector.of_string "00"
    let refclk_icntl_rx = Hardcaml.Logic.Std_logic_vector.of_string "00"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create
          ~name:"REFCLK_EN_TX_PATH"
          ~value:(Std_logic P.refclk_en_tx_path)
      ; Hardcaml.Parameter.create
          ~name:"REFCLK_HROW_CK_SEL"
          ~value:(Std_logic_vector P.refclk_hrow_ck_sel)
      ; Hardcaml.Parameter.create
          ~name:"REFCLK_ICNTL_RX"
          ~value:(Std_logic_vector P.refclk_icntl_rx)
      ]
    ;;

    module I = struct
      type 'a t =
        { ceb : 'a [@bits 1] [@rtlname "CEB"]
        ; i : 'a [@bits 1] [@rtlname "I"]
        ; ib : 'a [@bits 1] [@rtlname "IB"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { o : 'a [@bits 1] [@rtlname "O"]
        ; odiv2 : 'a [@bits 1] [@rtlname "ODIV2"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "IBUFDS_GTE4") ?parameters inputs
      =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module ICAPE3 = struct
  module type P = sig
    val device_id : Hardcaml.Logic.Bit_vector.t
    val icap_auto_switch : string
    val sim_cfg_file_name : string
  end

  module P : P = struct
    let device_id = Hardcaml.Logic.Bit_vector.of_string "00000011011000101000000010010011"
    let icap_auto_switch = "DISABLE"
    let sim_cfg_file_name = "NONE"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"DEVICE_ID" ~value:(Bit_vector P.device_id)
      ; Hardcaml.Parameter.create
          ~name:"ICAP_AUTO_SWITCH"
          ~value:(String P.icap_auto_switch)
      ; Hardcaml.Parameter.create
          ~name:"SIM_CFG_FILE_NAME"
          ~value:(String P.sim_cfg_file_name)
      ]
    ;;

    module I = struct
      type 'a t =
        { clk : 'a [@bits 1] [@rtlname "CLK"]
        ; csib : 'a [@bits 1] [@rtlname "CSIB"]
        ; i : 'a [@bits 31 - 0 + 1] [@rtlname "I"]
        ; rdwrb : 'a [@bits 1] [@rtlname "RDWRB"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { avail : 'a [@bits 1] [@rtlname "AVAIL"]
        ; o : 'a [@bits 31 - 0 + 1] [@rtlname "O"]
        ; prdone : 'a [@bits 1] [@rtlname "PRDONE"]
        ; prerror : 'a [@bits 1] [@rtlname "PRERROR"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "ICAPE3") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module LUT2 = struct
  module type P = sig
    val init : Hardcaml.Logic.Bit_vector.t
  end

  module P : P = struct
    let init = Hardcaml.Logic.Bit_vector.of_string "0000"
  end

  module Make (P : P) = struct
    let params = [ Hardcaml.Parameter.create ~name:"INIT" ~value:(Bit_vector P.init) ]

    module I = struct
      type 'a t =
        { i0 : 'a [@bits 1] [@rtlname "I0"]
        ; i1 : 'a [@bits 1] [@rtlname "I1"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "LUT2") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end

module STARTUPE3 = struct
  module type P = sig
    val prog_usr : string
    val sim_cclk_freq : float
  end

  module P : P = struct
    let prog_usr = "FALSE"
    let sim_cclk_freq = 0.
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"PROG_USR" ~value:(String P.prog_usr)
      ; Hardcaml.Parameter.create ~name:"SIM_CCLK_FREQ" ~value:(Real P.sim_cclk_freq)
      ]
    ;;

    module I = struct
      type 'a t =
        { do_ : 'a [@bits 3 - 0 + 1] [@rtlname "DO"]
        ; dts : 'a [@bits 3 - 0 + 1] [@rtlname "DTS"]
        ; fcsbo : 'a [@bits 1] [@rtlname "FCSBO"]
        ; fcsbts : 'a [@bits 1] [@rtlname "FCSBTS"]
        ; gsr : 'a [@bits 1] [@rtlname "GSR"]
        ; gts : 'a [@bits 1] [@rtlname "GTS"]
        ; keyclearb : 'a [@bits 1] [@rtlname "KEYCLEARB"]
        ; pack : 'a [@bits 1] [@rtlname "PACK"]
        ; usrcclko : 'a [@bits 1] [@rtlname "USRCCLKO"]
        ; usrcclkts : 'a [@bits 1] [@rtlname "USRCCLKTS"]
        ; usrdoneo : 'a [@bits 1] [@rtlname "USRDONEO"]
        ; usrdonets : 'a [@bits 1] [@rtlname "USRDONETS"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { cfgclk : 'a [@bits 1] [@rtlname "CFGCLK"]
        ; cfgmclk : 'a [@bits 1] [@rtlname "CFGMCLK"]
        ; di : 'a [@bits 3 - 0 + 1] [@rtlname "DI"]
        ; eos : 'a [@bits 1] [@rtlname "EOS"]
        ; preq : 'a [@bits 1] [@rtlname "PREQ"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "STARTUPE3") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end
