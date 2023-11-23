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
      type 'a t = { i : 'a [@bits 1] [@rtlname "I"] } [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving hardcaml]
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving hardcaml]
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { o : 'a [@bits 1] [@rtlname "O"]
        ; odiv2 : 'a [@bits 1] [@rtlname "ODIV2"]
        }
      [@@deriving hardcaml]
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { avail : 'a [@bits 1] [@rtlname "AVAIL"]
        ; o : 'a [@bits 31 - 0 + 1] [@rtlname "O"]
        ; prdone : 'a [@bits 1] [@rtlname "PRDONE"]
        ; prerror : 'a [@bits 1] [@rtlname "PRERROR"]
        }
      [@@deriving hardcaml]
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { o : 'a [@bits 1] [@rtlname "O"] } [@@deriving hardcaml]
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { cfgclk : 'a [@bits 1] [@rtlname "CFGCLK"]
        ; cfgmclk : 'a [@bits 1] [@rtlname "CFGMCLK"]
        ; di : 'a [@bits 3 - 0 + 1] [@rtlname "DI"]
        ; eos : 'a [@bits 1] [@rtlname "EOS"]
        ; preq : 'a [@bits 1] [@rtlname "PREQ"]
        }
      [@@deriving hardcaml]
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

module SYSMONE1 = struct
  module type P = sig
    val init_40 : Hardcaml.Logic.Bit_vector.t
    val init_41 : Hardcaml.Logic.Bit_vector.t
    val init_42 : Hardcaml.Logic.Bit_vector.t
    val init_43 : Hardcaml.Logic.Bit_vector.t
    val init_44 : Hardcaml.Logic.Bit_vector.t
    val init_45 : Hardcaml.Logic.Bit_vector.t
    val init_46 : Hardcaml.Logic.Bit_vector.t
    val init_47 : Hardcaml.Logic.Bit_vector.t
    val init_48 : Hardcaml.Logic.Bit_vector.t
    val init_49 : Hardcaml.Logic.Bit_vector.t
    val init_4a : Hardcaml.Logic.Bit_vector.t
    val init_4b : Hardcaml.Logic.Bit_vector.t
    val init_4c : Hardcaml.Logic.Bit_vector.t
    val init_4d : Hardcaml.Logic.Bit_vector.t
    val init_4e : Hardcaml.Logic.Bit_vector.t
    val init_4f : Hardcaml.Logic.Bit_vector.t
    val init_50 : Hardcaml.Logic.Bit_vector.t
    val init_51 : Hardcaml.Logic.Bit_vector.t
    val init_52 : Hardcaml.Logic.Bit_vector.t
    val init_53 : Hardcaml.Logic.Bit_vector.t
    val init_54 : Hardcaml.Logic.Bit_vector.t
    val init_55 : Hardcaml.Logic.Bit_vector.t
    val init_56 : Hardcaml.Logic.Bit_vector.t
    val init_57 : Hardcaml.Logic.Bit_vector.t
    val init_58 : Hardcaml.Logic.Bit_vector.t
    val init_59 : Hardcaml.Logic.Bit_vector.t
    val init_5a : Hardcaml.Logic.Bit_vector.t
    val init_5b : Hardcaml.Logic.Bit_vector.t
    val init_5c : Hardcaml.Logic.Bit_vector.t
    val init_5d : Hardcaml.Logic.Bit_vector.t
    val init_5e : Hardcaml.Logic.Bit_vector.t
    val init_5f : Hardcaml.Logic.Bit_vector.t
    val init_60 : Hardcaml.Logic.Bit_vector.t
    val init_61 : Hardcaml.Logic.Bit_vector.t
    val init_62 : Hardcaml.Logic.Bit_vector.t
    val init_63 : Hardcaml.Logic.Bit_vector.t
    val init_64 : Hardcaml.Logic.Bit_vector.t
    val init_65 : Hardcaml.Logic.Bit_vector.t
    val init_66 : Hardcaml.Logic.Bit_vector.t
    val init_67 : Hardcaml.Logic.Bit_vector.t
    val init_68 : Hardcaml.Logic.Bit_vector.t
    val init_69 : Hardcaml.Logic.Bit_vector.t
    val init_6a : Hardcaml.Logic.Bit_vector.t
    val init_6b : Hardcaml.Logic.Bit_vector.t
    val init_6c : Hardcaml.Logic.Bit_vector.t
    val init_6d : Hardcaml.Logic.Bit_vector.t
    val init_6e : Hardcaml.Logic.Bit_vector.t
    val init_6f : Hardcaml.Logic.Bit_vector.t
    val init_70 : Hardcaml.Logic.Bit_vector.t
    val init_71 : Hardcaml.Logic.Bit_vector.t
    val init_72 : Hardcaml.Logic.Bit_vector.t
    val init_73 : Hardcaml.Logic.Bit_vector.t
    val init_74 : Hardcaml.Logic.Bit_vector.t
    val init_75 : Hardcaml.Logic.Bit_vector.t
    val init_76 : Hardcaml.Logic.Bit_vector.t
    val init_77 : Hardcaml.Logic.Bit_vector.t
    val init_78 : Hardcaml.Logic.Bit_vector.t
    val init_79 : Hardcaml.Logic.Bit_vector.t
    val init_7a : Hardcaml.Logic.Bit_vector.t
    val init_7b : Hardcaml.Logic.Bit_vector.t
    val init_7c : Hardcaml.Logic.Bit_vector.t
    val init_7d : Hardcaml.Logic.Bit_vector.t
    val init_7e : Hardcaml.Logic.Bit_vector.t
    val init_7f : Hardcaml.Logic.Bit_vector.t
    val is_convstclk_inverted : Hardcaml.Logic.Std_logic.t
    val is_dclk_inverted : Hardcaml.Logic.Std_logic.t
    val sim_monitor_file : string
    val sysmon_vuser0_bank : int
    val sysmon_vuser0_monitor : string
    val sysmon_vuser1_bank : int
    val sysmon_vuser1_monitor : string
    val sysmon_vuser2_bank : int
    val sysmon_vuser2_monitor : string
    val sysmon_vuser3_bank : int
    val sysmon_vuser3_monitor : string
  end

  module P : P = struct
    let init_40 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_41 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_42 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_43 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_44 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_45 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_46 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_47 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_48 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_49 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4a = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4b = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4c = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4d = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4e = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_4f = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_50 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_51 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_52 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_53 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_54 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_55 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_56 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_57 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_58 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_59 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5a = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5b = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5c = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5d = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5e = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_5f = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_60 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_61 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_62 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_63 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_64 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_65 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_66 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_67 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_68 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_69 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6a = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6b = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6c = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6d = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6e = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_6f = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_70 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_71 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_72 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_73 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_74 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_75 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_76 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_77 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_78 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_79 = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7a = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7b = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7c = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7d = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7e = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let init_7f = Hardcaml.Logic.Bit_vector.of_string "0000000000000000"
    let is_convstclk_inverted = Hardcaml.Logic.Std_logic.L0
    let is_dclk_inverted = Hardcaml.Logic.Std_logic.L0
    let sim_monitor_file = "design.txt"
    let sysmon_vuser0_bank = 0
    let sysmon_vuser0_monitor = "NONE"
    let sysmon_vuser1_bank = 0
    let sysmon_vuser1_monitor = "NONE"
    let sysmon_vuser2_bank = 0
    let sysmon_vuser2_monitor = "NONE"
    let sysmon_vuser3_bank = 0
    let sysmon_vuser3_monitor = "NONE"
  end

  module Make (P : P) = struct
    let params =
      [ Hardcaml.Parameter.create ~name:"INIT_40" ~value:(Bit_vector P.init_40)
      ; Hardcaml.Parameter.create ~name:"INIT_41" ~value:(Bit_vector P.init_41)
      ; Hardcaml.Parameter.create ~name:"INIT_42" ~value:(Bit_vector P.init_42)
      ; Hardcaml.Parameter.create ~name:"INIT_43" ~value:(Bit_vector P.init_43)
      ; Hardcaml.Parameter.create ~name:"INIT_44" ~value:(Bit_vector P.init_44)
      ; Hardcaml.Parameter.create ~name:"INIT_45" ~value:(Bit_vector P.init_45)
      ; Hardcaml.Parameter.create ~name:"INIT_46" ~value:(Bit_vector P.init_46)
      ; Hardcaml.Parameter.create ~name:"INIT_47" ~value:(Bit_vector P.init_47)
      ; Hardcaml.Parameter.create ~name:"INIT_48" ~value:(Bit_vector P.init_48)
      ; Hardcaml.Parameter.create ~name:"INIT_49" ~value:(Bit_vector P.init_49)
      ; Hardcaml.Parameter.create ~name:"INIT_4A" ~value:(Bit_vector P.init_4a)
      ; Hardcaml.Parameter.create ~name:"INIT_4B" ~value:(Bit_vector P.init_4b)
      ; Hardcaml.Parameter.create ~name:"INIT_4C" ~value:(Bit_vector P.init_4c)
      ; Hardcaml.Parameter.create ~name:"INIT_4D" ~value:(Bit_vector P.init_4d)
      ; Hardcaml.Parameter.create ~name:"INIT_4E" ~value:(Bit_vector P.init_4e)
      ; Hardcaml.Parameter.create ~name:"INIT_4F" ~value:(Bit_vector P.init_4f)
      ; Hardcaml.Parameter.create ~name:"INIT_50" ~value:(Bit_vector P.init_50)
      ; Hardcaml.Parameter.create ~name:"INIT_51" ~value:(Bit_vector P.init_51)
      ; Hardcaml.Parameter.create ~name:"INIT_52" ~value:(Bit_vector P.init_52)
      ; Hardcaml.Parameter.create ~name:"INIT_53" ~value:(Bit_vector P.init_53)
      ; Hardcaml.Parameter.create ~name:"INIT_54" ~value:(Bit_vector P.init_54)
      ; Hardcaml.Parameter.create ~name:"INIT_55" ~value:(Bit_vector P.init_55)
      ; Hardcaml.Parameter.create ~name:"INIT_56" ~value:(Bit_vector P.init_56)
      ; Hardcaml.Parameter.create ~name:"INIT_57" ~value:(Bit_vector P.init_57)
      ; Hardcaml.Parameter.create ~name:"INIT_58" ~value:(Bit_vector P.init_58)
      ; Hardcaml.Parameter.create ~name:"INIT_59" ~value:(Bit_vector P.init_59)
      ; Hardcaml.Parameter.create ~name:"INIT_5A" ~value:(Bit_vector P.init_5a)
      ; Hardcaml.Parameter.create ~name:"INIT_5B" ~value:(Bit_vector P.init_5b)
      ; Hardcaml.Parameter.create ~name:"INIT_5C" ~value:(Bit_vector P.init_5c)
      ; Hardcaml.Parameter.create ~name:"INIT_5D" ~value:(Bit_vector P.init_5d)
      ; Hardcaml.Parameter.create ~name:"INIT_5E" ~value:(Bit_vector P.init_5e)
      ; Hardcaml.Parameter.create ~name:"INIT_5F" ~value:(Bit_vector P.init_5f)
      ; Hardcaml.Parameter.create ~name:"INIT_60" ~value:(Bit_vector P.init_60)
      ; Hardcaml.Parameter.create ~name:"INIT_61" ~value:(Bit_vector P.init_61)
      ; Hardcaml.Parameter.create ~name:"INIT_62" ~value:(Bit_vector P.init_62)
      ; Hardcaml.Parameter.create ~name:"INIT_63" ~value:(Bit_vector P.init_63)
      ; Hardcaml.Parameter.create ~name:"INIT_64" ~value:(Bit_vector P.init_64)
      ; Hardcaml.Parameter.create ~name:"INIT_65" ~value:(Bit_vector P.init_65)
      ; Hardcaml.Parameter.create ~name:"INIT_66" ~value:(Bit_vector P.init_66)
      ; Hardcaml.Parameter.create ~name:"INIT_67" ~value:(Bit_vector P.init_67)
      ; Hardcaml.Parameter.create ~name:"INIT_68" ~value:(Bit_vector P.init_68)
      ; Hardcaml.Parameter.create ~name:"INIT_69" ~value:(Bit_vector P.init_69)
      ; Hardcaml.Parameter.create ~name:"INIT_6A" ~value:(Bit_vector P.init_6a)
      ; Hardcaml.Parameter.create ~name:"INIT_6B" ~value:(Bit_vector P.init_6b)
      ; Hardcaml.Parameter.create ~name:"INIT_6C" ~value:(Bit_vector P.init_6c)
      ; Hardcaml.Parameter.create ~name:"INIT_6D" ~value:(Bit_vector P.init_6d)
      ; Hardcaml.Parameter.create ~name:"INIT_6E" ~value:(Bit_vector P.init_6e)
      ; Hardcaml.Parameter.create ~name:"INIT_6F" ~value:(Bit_vector P.init_6f)
      ; Hardcaml.Parameter.create ~name:"INIT_70" ~value:(Bit_vector P.init_70)
      ; Hardcaml.Parameter.create ~name:"INIT_71" ~value:(Bit_vector P.init_71)
      ; Hardcaml.Parameter.create ~name:"INIT_72" ~value:(Bit_vector P.init_72)
      ; Hardcaml.Parameter.create ~name:"INIT_73" ~value:(Bit_vector P.init_73)
      ; Hardcaml.Parameter.create ~name:"INIT_74" ~value:(Bit_vector P.init_74)
      ; Hardcaml.Parameter.create ~name:"INIT_75" ~value:(Bit_vector P.init_75)
      ; Hardcaml.Parameter.create ~name:"INIT_76" ~value:(Bit_vector P.init_76)
      ; Hardcaml.Parameter.create ~name:"INIT_77" ~value:(Bit_vector P.init_77)
      ; Hardcaml.Parameter.create ~name:"INIT_78" ~value:(Bit_vector P.init_78)
      ; Hardcaml.Parameter.create ~name:"INIT_79" ~value:(Bit_vector P.init_79)
      ; Hardcaml.Parameter.create ~name:"INIT_7A" ~value:(Bit_vector P.init_7a)
      ; Hardcaml.Parameter.create ~name:"INIT_7B" ~value:(Bit_vector P.init_7b)
      ; Hardcaml.Parameter.create ~name:"INIT_7C" ~value:(Bit_vector P.init_7c)
      ; Hardcaml.Parameter.create ~name:"INIT_7D" ~value:(Bit_vector P.init_7d)
      ; Hardcaml.Parameter.create ~name:"INIT_7E" ~value:(Bit_vector P.init_7e)
      ; Hardcaml.Parameter.create ~name:"INIT_7F" ~value:(Bit_vector P.init_7f)
      ; Hardcaml.Parameter.create
          ~name:"IS_CONVSTCLK_INVERTED"
          ~value:(Std_logic P.is_convstclk_inverted)
      ; Hardcaml.Parameter.create
          ~name:"IS_DCLK_INVERTED"
          ~value:(Std_logic P.is_dclk_inverted)
      ; Hardcaml.Parameter.create
          ~name:"SIM_MONITOR_FILE"
          ~value:(String P.sim_monitor_file)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER0_BANK"
          ~value:(Int P.sysmon_vuser0_bank)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER0_MONITOR"
          ~value:(String P.sysmon_vuser0_monitor)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER1_BANK"
          ~value:(Int P.sysmon_vuser1_bank)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER1_MONITOR"
          ~value:(String P.sysmon_vuser1_monitor)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER2_BANK"
          ~value:(Int P.sysmon_vuser2_bank)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER2_MONITOR"
          ~value:(String P.sysmon_vuser2_monitor)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER3_BANK"
          ~value:(Int P.sysmon_vuser3_bank)
      ; Hardcaml.Parameter.create
          ~name:"SYSMON_VUSER3_MONITOR"
          ~value:(String P.sysmon_vuser3_monitor)
      ]
    ;;

    module I = struct
      type 'a t =
        { convst : 'a [@bits 1] [@rtlname "CONVST"]
        ; convstclk : 'a [@bits 1] [@rtlname "CONVSTCLK"]
        ; daddr : 'a [@bits 7 - 0 + 1] [@rtlname "DADDR"]
        ; dclk : 'a [@bits 1] [@rtlname "DCLK"]
        ; den : 'a [@bits 1] [@rtlname "DEN"]
        ; di : 'a [@bits 15 - 0 + 1] [@rtlname "DI"]
        ; dwe : 'a [@bits 1] [@rtlname "DWE"]
        ; i2c_sclk : 'a [@bits 1] [@rtlname "I2C_SCLK"]
        ; i2c_sda : 'a [@bits 1] [@rtlname "I2C_SDA"]
        ; reset : 'a [@bits 1] [@rtlname "RESET"]
        ; vauxn : 'a [@bits 15 - 0 + 1] [@rtlname "VAUXN"]
        ; vauxp : 'a [@bits 15 - 0 + 1] [@rtlname "VAUXP"]
        ; vn : 'a [@bits 1] [@rtlname "VN"]
        ; vp : 'a [@bits 1] [@rtlname "VP"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { alm : 'a [@bits 15 - 0 + 1] [@rtlname "ALM"]
        ; busy : 'a [@bits 1] [@rtlname "BUSY"]
        ; channel : 'a [@bits 5 - 0 + 1] [@rtlname "CHANNEL"]
        ; do_ : 'a [@bits 15 - 0 + 1] [@rtlname "DO"]
        ; drdy : 'a [@bits 1] [@rtlname "DRDY"]
        ; eoc : 'a [@bits 1] [@rtlname "EOC"]
        ; eos : 'a [@bits 1] [@rtlname "EOS"]
        ; i2c_sclk_ts : 'a [@bits 1] [@rtlname "I2C_SCLK_TS"]
        ; i2c_sda_ts : 'a [@bits 1] [@rtlname "I2C_SDA_TS"]
        ; jtagbusy : 'a [@bits 1] [@rtlname "JTAGBUSY"]
        ; jtaglocked : 'a [@bits 1] [@rtlname "JTAGLOCKED"]
        ; jtagmodified : 'a [@bits 1] [@rtlname "JTAGMODIFIED"]
        ; muxaddr : 'a [@bits 4 - 0 + 1] [@rtlname "MUXADDR"]
        ; ot : 'a [@bits 1] [@rtlname "OT"]
        }
      [@@deriving hardcaml]
    end

    module T = Hardcaml.Interface.Empty

    open struct
      include Hardcaml.Instantiation.With_interface (I) (O)
    end

    let create ?lib ?arch ?attributes ?instance ?(name = "SYSMONE1") ?parameters inputs =
      let parameters = Option.value ~default:params parameters in
      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
    ;;
  end
end
