open! Import
open Signal
open Hardcaml_waveterm

let circuit
  ?(address_width_a = 4)
  ?(data_width_a = 8)
  ?(address_width_b = 4)
  ?(data_width_b = 8)
  ?(byte_enables = false)
  ?size
  ?scope
  arch
  build_mode
  =
  let write_enable_width, byte_write_width =
    if byte_enables
    then
      if data_width_a % 8 = 0 && data_width_b % 8 = 0
      then [| data_width_a / 8; data_width_b / 8 |], Byte_write_width.B8
      else if data_width_a % 9 = 0 && data_width_b % 9 = 0
      then [| data_width_a / 9; data_width_b / 9 |], Byte_write_width.B9
      else
        raise_s
          [%message
            "Invalid byte enable configuration" (data_width_a : int) (data_width_b : int)]
    else [| 1; 1 |], Byte_write_width.Full
  in
  let port port ~address_width ~data_width =
    let port_suffix = [| "a"; "b" |] in
    let input name width = input (name ^ "_" ^ port_suffix.(port)) width in
    { Ram_port.address = input "address" address_width
    ; write_enable = input "write" write_enable_width.(port)
    ; read_enable = input "read" 1
    ; data = input "data" data_width
    }
  in
  let qa, qb =
    True_dual_port_ram.create
      ?scope
      ~memory_optimization:false
      ~cascade_height:Inferred
      ~byte_write_width
      ~arch
      ~build_mode
      ()
      ~clock_a:(input "clock_a" 1)
      ~clock_b:(input "clock_b" 1)
      ~clear_a:(input "clear_a" 1)
      ~clear_b:(input "clear_b" 1)
      ~size:(Option.value size ~default:(1 lsl address_width_a))
      ~port_a:(port 0 ~address_width:address_width_a ~data_width:data_width_a)
      ~port_b:(port 1 ~address_width:address_width_b ~data_width:data_width_b)
  in
  Circuit.create_exn ~name:"true_dual_port_ram" [ output "qa" qa; output "qb" qb ]
;;

let print_verilog
  ?address_width_a
  ?data_width_a
  ?address_width_b
  ?data_width_b
  ?size
  arch
  build_mode
  =
  let scope = Scope.create () in
  Rtl.print
    ~database:(Scope.circuit_database scope)
    Verilog
    (circuit
       ?address_width_a
       ?data_width_a
       ?address_width_b
       ?data_width_b
       ?size
       ~scope
       arch
       build_mode)
;;

let%expect_test "True_dual_port_ram" =
  print_verilog Distributed Synthesis;
  [%expect
    {|
    module true_dual_port_ram (
        data_b,
        address_b,
        read_b,
        write_b,
        clear_b,
        clock_b,
        data_a,
        address_a,
        read_a,
        write_a,
        clear_a,
        clock_a,
        qa,
        qb
    );

        input [7:0] data_b;
        input [3:0] address_b;
        input read_b;
        input write_b;
        input clear_b;
        input clock_b;
        input [7:0] data_a;
        input [3:0] address_a;
        input read_a;
        input write_a;
        input clear_a;
        input clock_a;
        output [7:0] qa;
        output [7:0] qb;

        wire [7:0] signal_select;
        wire signal_or;
        wire vdd;
        wire signal_or_1;
        wire gnd;
        wire [19:0] signal_inst;
        wire [7:0] signal_select_1;
        assign signal_select = signal_inst[17:10];
        assign signal_or = write_b | read_b;
        assign vdd = 1'b1;
        assign signal_or_1 = write_a | read_a;
        assign gnd = 1'b0;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128),
               .MEMORY_PRIMITIVE("distributed"),
               .CLOCKING_MODE("independent_clock"),
               .ECC_MODE("no_ecc"),
               .MEMORY_INIT_FILE("none"),
               .MEMORY_INIT_PARAM(""),
               .USE_MEM_INIT(0),
               .WAKEUP_TIME("disable_sleep"),
               .AUTO_SLEEP_TIME(0),
               .MESSAGE_CONTROL(0),
               .USE_EMBEDDED_CONSTRAINT(0),
               .MEMORY_OPTIMIZATION("false"),
               .CASCADE_HEIGHT(0),
               .SIM_ASSERT_CHK(0),
               .WRITE_DATA_WIDTH_A(8),
               .READ_DATA_WIDTH_A(8),
               .BYTE_WRITE_WIDTH_A(8),
               .ADDR_WIDTH_A(4),
               .READ_RESET_VALUE_A("0"),
               .READ_LATENCY_A(1),
               .WRITE_MODE_A("read_first"),
               .RST_MODE_A("SYNC"),
               .WRITE_DATA_WIDTH_B(8),
               .READ_DATA_WIDTH_B(8),
               .BYTE_WRITE_WIDTH_B(8),
               .ADDR_WIDTH_B(4),
               .READ_RESET_VALUE_B("0"),
               .READ_LATENCY_B(1),
               .WRITE_MODE_B("read_first"),
               .RST_MODE_B("SYNC") )
            the_xpm_memory_tdpram
            ( .sleep(gnd),
              .clka(clock_a),
              .rsta(clear_a),
              .ena(signal_or_1),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(signal_or),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(signal_inst[7:0]),
              .sbiterra(signal_inst[8:8]),
              .dbiterra(signal_inst[9:9]),
              .doutb(signal_inst[17:10]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_1 = signal_inst[7:0];
        assign qa = signal_select_1;
        assign qb = signal_select;

    endmodule
    |}];
  print_verilog (Blockram Read_before_write) Synthesis;
  [%expect
    {|
    module true_dual_port_ram (
        data_b,
        address_b,
        read_b,
        write_b,
        clear_b,
        clock_b,
        data_a,
        address_a,
        read_a,
        write_a,
        clear_a,
        clock_a,
        qa,
        qb
    );

        input [7:0] data_b;
        input [3:0] address_b;
        input read_b;
        input write_b;
        input clear_b;
        input clock_b;
        input [7:0] data_a;
        input [3:0] address_a;
        input read_a;
        input write_a;
        input clear_a;
        input clock_a;
        output [7:0] qa;
        output [7:0] qb;

        wire [7:0] signal_select;
        wire signal_or;
        wire vdd;
        wire signal_or_1;
        wire gnd;
        wire [19:0] signal_inst;
        wire [7:0] signal_select_1;
        assign signal_select = signal_inst[17:10];
        assign signal_or = write_b | read_b;
        assign vdd = 1'b1;
        assign signal_or_1 = write_a | read_a;
        assign gnd = 1'b0;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128),
               .MEMORY_PRIMITIVE("block"),
               .CLOCKING_MODE("independent_clock"),
               .ECC_MODE("no_ecc"),
               .MEMORY_INIT_FILE("none"),
               .MEMORY_INIT_PARAM(""),
               .USE_MEM_INIT(0),
               .WAKEUP_TIME("disable_sleep"),
               .AUTO_SLEEP_TIME(0),
               .MESSAGE_CONTROL(0),
               .USE_EMBEDDED_CONSTRAINT(0),
               .MEMORY_OPTIMIZATION("false"),
               .CASCADE_HEIGHT(0),
               .SIM_ASSERT_CHK(0),
               .WRITE_DATA_WIDTH_A(8),
               .READ_DATA_WIDTH_A(8),
               .BYTE_WRITE_WIDTH_A(8),
               .ADDR_WIDTH_A(4),
               .READ_RESET_VALUE_A("0"),
               .READ_LATENCY_A(1),
               .WRITE_MODE_A("read_first"),
               .RST_MODE_A("SYNC"),
               .WRITE_DATA_WIDTH_B(8),
               .READ_DATA_WIDTH_B(8),
               .BYTE_WRITE_WIDTH_B(8),
               .ADDR_WIDTH_B(4),
               .READ_RESET_VALUE_B("0"),
               .READ_LATENCY_B(1),
               .WRITE_MODE_B("read_first"),
               .RST_MODE_B("SYNC") )
            the_xpm_memory_tdpram
            ( .sleep(gnd),
              .clka(clock_a),
              .rsta(clear_a),
              .ena(signal_or_1),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(signal_or),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(signal_inst[7:0]),
              .sbiterra(signal_inst[8:8]),
              .dbiterra(signal_inst[9:9]),
              .doutb(signal_inst[17:10]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_1 = signal_inst[7:0];
        assign qa = signal_select_1;
        assign qb = signal_select;

    endmodule
    |}];
  print_verilog (Ultraram Let_vivado_decide) Synthesis;
  [%expect
    {|
    module true_dual_port_ram (
        data_b,
        address_b,
        read_b,
        write_b,
        clear_b,
        clock_b,
        data_a,
        address_a,
        read_a,
        write_a,
        clear_a,
        clock_a,
        qa,
        qb
    );

        input [7:0] data_b;
        input [3:0] address_b;
        input read_b;
        input write_b;
        input clear_b;
        input clock_b;
        input [7:0] data_a;
        input [3:0] address_a;
        input read_a;
        input write_a;
        input clear_a;
        input clock_a;
        output [7:0] qa;
        output [7:0] qb;

        wire [7:0] signal_select;
        wire signal_or;
        wire vdd;
        wire signal_or_1;
        wire gnd;
        wire [19:0] signal_inst;
        wire [7:0] signal_select_1;
        assign signal_select = signal_inst[17:10];
        assign signal_or = write_b | read_b;
        assign vdd = 1'b1;
        assign signal_or_1 = write_a | read_a;
        assign gnd = 1'b0;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128),
               .MEMORY_PRIMITIVE("ultra"),
               .CLOCKING_MODE("independent_clock"),
               .ECC_MODE("no_ecc"),
               .MEMORY_INIT_FILE("none"),
               .MEMORY_INIT_PARAM(""),
               .USE_MEM_INIT(0),
               .WAKEUP_TIME("disable_sleep"),
               .AUTO_SLEEP_TIME(0),
               .MESSAGE_CONTROL(0),
               .USE_EMBEDDED_CONSTRAINT(0),
               .MEMORY_OPTIMIZATION("false"),
               .CASCADE_HEIGHT(0),
               .SIM_ASSERT_CHK(0),
               .WRITE_DATA_WIDTH_A(8),
               .READ_DATA_WIDTH_A(8),
               .BYTE_WRITE_WIDTH_A(8),
               .ADDR_WIDTH_A(4),
               .READ_RESET_VALUE_A("0"),
               .READ_LATENCY_A(1),
               .WRITE_MODE_A("no_change"),
               .RST_MODE_A("SYNC"),
               .WRITE_DATA_WIDTH_B(8),
               .READ_DATA_WIDTH_B(8),
               .BYTE_WRITE_WIDTH_B(8),
               .ADDR_WIDTH_B(4),
               .READ_RESET_VALUE_B("0"),
               .READ_LATENCY_B(1),
               .WRITE_MODE_B("no_change"),
               .RST_MODE_B("SYNC") )
            the_xpm_memory_tdpram
            ( .sleep(gnd),
              .clka(clock_a),
              .rsta(clear_a),
              .ena(signal_or_1),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(signal_or),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(signal_inst[7:0]),
              .sbiterra(signal_inst[8:8]),
              .dbiterra(signal_inst[9:9]),
              .doutb(signal_inst[17:10]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_1 = signal_inst[7:0];
        assign qa = signal_select_1;
        assign qb = signal_select;

    endmodule
    |}];
  print_verilog (Ultraram Wrap_in_module_with_keep_directives) Synthesis;
  [%expect
    {|
    module xpm_tdpram_wrapper (
        injectdbiterrb,
        injectsbiterrb,
        dinb,
        addrb,
        web,
        regceb,
        enb,
        rstb,
        clkb,
        injectdbiterra,
        injectsbiterra,
        dina,
        addra,
        wea,
        regcea,
        ena,
        rsta,
        clka,
        sleep,
        douta,
        sbiterra,
        dbiterra,
        doutb,
        sbiterrb,
        dbiterrb
    );

        input injectdbiterrb;
        input injectsbiterrb;
        input [7:0] dinb;
        input [3:0] addrb;
        input web;
        input regceb;
        input enb;
        input rstb;
        input clkb;
        input injectdbiterra;
        input injectsbiterra;
        input [7:0] dina;
        input [3:0] addra;
        input wea;
        input regcea;
        input ena;
        input rsta;
        input clka;
        input sleep;
        output [7:0] douta;
        output sbiterra;
        output dbiterra;
        output [7:0] doutb;
        output sbiterrb;
        output dbiterrb;

        (* keep="TRUE" *)
        wire signal_select;
        (* keep="TRUE" *)
        wire signal_select_1;
        (* keep="TRUE" *)
        wire [7:0] signal_select_2;
        (* keep="TRUE" *)
        wire signal_select_3;
        (* keep="TRUE" *)
        wire signal_select_4;
        (* keep="TRUE" *)
        wire signal_wire;
        (* keep="TRUE" *)
        wire signal_wire_1;
        (* keep="TRUE" *)
        wire [7:0] signal_wire_2;
        (* keep="TRUE" *)
        wire [3:0] signal_wire_3;
        (* keep="TRUE" *)
        wire signal_wire_4;
        (* keep="TRUE" *)
        wire signal_wire_5;
        (* keep="TRUE" *)
        wire signal_wire_6;
        (* keep="TRUE" *)
        wire signal_wire_7;
        (* keep="TRUE" *)
        wire signal_wire_8;
        (* keep="TRUE" *)
        wire signal_wire_9;
        (* keep="TRUE" *)
        wire signal_wire_10;
        (* keep="TRUE" *)
        wire [7:0] signal_wire_11;
        (* keep="TRUE" *)
        wire [3:0] signal_wire_12;
        (* keep="TRUE" *)
        wire signal_wire_13;
        (* keep="TRUE" *)
        wire signal_wire_14;
        (* keep="TRUE" *)
        wire signal_wire_15;
        (* keep="TRUE" *)
        wire signal_wire_16;
        (* keep="TRUE" *)
        wire signal_wire_17;
        (* keep="TRUE" *)
        wire signal_wire_18;
        wire [19:0] signal_inst;
        (* keep="TRUE" *)
        wire [7:0] signal_select_5;
        assign signal_select = signal_inst[19:19];
        assign signal_select_1 = signal_inst[18:18];
        assign signal_select_2 = signal_inst[17:10];
        assign signal_select_3 = signal_inst[9:9];
        assign signal_select_4 = signal_inst[8:8];
        assign signal_wire = injectdbiterrb;
        assign signal_wire_1 = injectsbiterrb;
        assign signal_wire_2 = dinb;
        assign signal_wire_3 = addrb;
        assign signal_wire_4 = web;
        assign signal_wire_5 = regceb;
        assign signal_wire_6 = enb;
        assign signal_wire_7 = rstb;
        assign signal_wire_8 = clkb;
        assign signal_wire_9 = injectdbiterra;
        assign signal_wire_10 = injectsbiterra;
        assign signal_wire_11 = dina;
        assign signal_wire_12 = addra;
        assign signal_wire_13 = wea;
        assign signal_wire_14 = regcea;
        assign signal_wire_15 = ena;
        assign signal_wire_16 = rsta;
        assign signal_wire_17 = clka;
        assign signal_wire_18 = sleep;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128),
               .MEMORY_PRIMITIVE("ultra"),
               .CLOCKING_MODE("independent_clock"),
               .ECC_MODE("no_ecc"),
               .MEMORY_INIT_FILE("none"),
               .MEMORY_INIT_PARAM(""),
               .USE_MEM_INIT(0),
               .WAKEUP_TIME("disable_sleep"),
               .AUTO_SLEEP_TIME(0),
               .MESSAGE_CONTROL(0),
               .USE_EMBEDDED_CONSTRAINT(0),
               .MEMORY_OPTIMIZATION("false"),
               .CASCADE_HEIGHT(0),
               .SIM_ASSERT_CHK(0),
               .WRITE_DATA_WIDTH_A(8),
               .READ_DATA_WIDTH_A(8),
               .BYTE_WRITE_WIDTH_A(8),
               .ADDR_WIDTH_A(4),
               .READ_RESET_VALUE_A("0"),
               .READ_LATENCY_A(1),
               .WRITE_MODE_A("no_change"),
               .RST_MODE_A("SYNC"),
               .WRITE_DATA_WIDTH_B(8),
               .READ_DATA_WIDTH_B(8),
               .BYTE_WRITE_WIDTH_B(8),
               .ADDR_WIDTH_B(4),
               .READ_RESET_VALUE_B("0"),
               .READ_LATENCY_B(1),
               .WRITE_MODE_B("no_change"),
               .RST_MODE_B("SYNC") )
            the_xpm_memory_tdpram
            ( .sleep(signal_wire_18),
              .clka(signal_wire_17),
              .rsta(signal_wire_16),
              .ena(signal_wire_15),
              .regcea(signal_wire_14),
              .wea(signal_wire_13),
              .addra(signal_wire_12),
              .dina(signal_wire_11),
              .injectsbiterra(signal_wire_10),
              .injectdbiterra(signal_wire_9),
              .clkb(signal_wire_8),
              .rstb(signal_wire_7),
              .enb(signal_wire_6),
              .regceb(signal_wire_5),
              .web(signal_wire_4),
              .addrb(signal_wire_3),
              .dinb(signal_wire_2),
              .injectsbiterrb(signal_wire_1),
              .injectdbiterrb(signal_wire),
              .douta(signal_inst[7:0]),
              .sbiterra(signal_inst[8:8]),
              .dbiterra(signal_inst[9:9]),
              .doutb(signal_inst[17:10]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_5 = signal_inst[7:0];
        assign douta = signal_select_5;
        assign sbiterra = signal_select_4;
        assign dbiterra = signal_select_3;
        assign doutb = signal_select_2;
        assign sbiterrb = signal_select_1;
        assign dbiterrb = signal_select;

    endmodule
    module true_dual_port_ram (
        data_b,
        address_b,
        read_b,
        write_b,
        clear_b,
        clock_b,
        data_a,
        address_a,
        read_a,
        write_a,
        clear_a,
        clock_a,
        qa,
        qb
    );

        input [7:0] data_b;
        input [3:0] address_b;
        input read_b;
        input write_b;
        input clear_b;
        input clock_b;
        input [7:0] data_a;
        input [3:0] address_a;
        input read_a;
        input write_a;
        input clear_a;
        input clock_a;
        output [7:0] qa;
        output [7:0] qb;

        wire [7:0] signal_select;
        wire signal_or;
        wire vdd;
        wire signal_or_1;
        wire gnd;
        wire [19:0] signal_inst;
        wire [7:0] signal_select_1;
        assign signal_select = signal_inst[17:10];
        assign signal_or = write_b | read_b;
        assign vdd = 1'b1;
        assign signal_or_1 = write_a | read_a;
        assign gnd = 1'b0;
        xpm_tdpram_wrapper
            xpm_tdpram_wrapper
            ( .sleep(gnd),
              .clka(clock_a),
              .rsta(clear_a),
              .ena(signal_or_1),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(signal_or),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(signal_inst[7:0]),
              .sbiterra(signal_inst[8:8]),
              .dbiterra(signal_inst[9:9]),
              .doutb(signal_inst[17:10]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_1 = signal_inst[7:0];
        assign qa = signal_select_1;
        assign qb = signal_select;

    endmodule
    |}];
  (* address width b is wider than the implied address width from port a. *)
  let test_bad_address_width build_mode =
    print_verilog
      ~address_width_a:4
      ~data_width_a:4
      ~address_width_b:4
      ~data_width_b:8
      (Blockram Read_before_write)
      build_mode
  in
  require_does_raise ~hide_positions:true (fun () -> test_bad_address_width Synthesis);
  [%expect {| "Assert_failure true_dual_port_ram.ml:LINE:COL" |}];
  require_does_raise (fun () -> test_bad_address_width Simulation);
  [%expect
    {|
    ("[Signal.multiport_memory] Width of write port data and address are inconsistent with widest port"
     (write_port_data_width     4)
     (write_port_address_width  4)
     (ratio                     1)
     (widest_port_data_width    8)
     (widest_port_address_width 4))
    |}];
  (* data width b does not result in an even number of words in the ram *)
  let test_uneven_sizes build_mode =
    print_verilog
      ~address_width_a:4
      ~data_width_a:4
      ~address_width_b:3
      ~data_width_b:9
      (Blockram Read_before_write)
      build_mode
  in
  require_does_raise (fun () -> test_uneven_sizes Synthesis);
  [%expect
    {|
    ("[size] is the number of port A words in the RAM. It must be chosen so that there is an integer number of port B words in the RAM as well."
     (size_a  16)
     (width_a 4)
     (width_b 9))
    |}];
  require_does_raise (fun () -> test_uneven_sizes Simulation);
  [%expect
    {|
    ("[size] is the number of port A words in the RAM. It must be chosen so that there is an integer number of port B words in the RAM as well."
     (size_a  16)
     (width_a 4)
     (width_b 9))
    |}];
  (* non-power-of-2 scaling - works in synthesis but not simulation *)
  let test_non_pow2_scale build_mode =
    print_verilog
      ~size:15
      ~address_width_a:4
      ~data_width_a:4
      ~address_width_b:3
      ~data_width_b:12
      (Blockram Read_before_write)
      build_mode
  in
  require_does_not_raise (fun () -> test_non_pow2_scale Synthesis);
  [%expect
    {|
    module true_dual_port_ram (
        data_b,
        address_b,
        read_b,
        write_b,
        clear_b,
        clock_b,
        data_a,
        address_a,
        read_a,
        write_a,
        clear_a,
        clock_a,
        qa,
        qb
    );

        input [11:0] data_b;
        input [2:0] address_b;
        input read_b;
        input write_b;
        input clear_b;
        input clock_b;
        input [3:0] data_a;
        input [3:0] address_a;
        input read_a;
        input write_a;
        input clear_a;
        input clock_a;
        output [3:0] qa;
        output [11:0] qb;

        wire [11:0] signal_select;
        wire signal_or;
        wire vdd;
        wire signal_or_1;
        wire gnd;
        wire [19:0] signal_inst;
        wire [3:0] signal_select_1;
        assign signal_select = signal_inst[17:6];
        assign signal_or = write_b | read_b;
        assign vdd = 1'b1;
        assign signal_or_1 = write_a | read_a;
        assign gnd = 1'b0;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(60),
               .MEMORY_PRIMITIVE("block"),
               .CLOCKING_MODE("independent_clock"),
               .ECC_MODE("no_ecc"),
               .MEMORY_INIT_FILE("none"),
               .MEMORY_INIT_PARAM(""),
               .USE_MEM_INIT(0),
               .WAKEUP_TIME("disable_sleep"),
               .AUTO_SLEEP_TIME(0),
               .MESSAGE_CONTROL(0),
               .USE_EMBEDDED_CONSTRAINT(0),
               .MEMORY_OPTIMIZATION("false"),
               .CASCADE_HEIGHT(0),
               .SIM_ASSERT_CHK(0),
               .WRITE_DATA_WIDTH_A(4),
               .READ_DATA_WIDTH_A(4),
               .BYTE_WRITE_WIDTH_A(4),
               .ADDR_WIDTH_A(4),
               .READ_RESET_VALUE_A("0"),
               .READ_LATENCY_A(1),
               .WRITE_MODE_A("read_first"),
               .RST_MODE_A("SYNC"),
               .WRITE_DATA_WIDTH_B(12),
               .READ_DATA_WIDTH_B(12),
               .BYTE_WRITE_WIDTH_B(12),
               .ADDR_WIDTH_B(3),
               .READ_RESET_VALUE_B("0"),
               .READ_LATENCY_B(1),
               .WRITE_MODE_B("read_first"),
               .RST_MODE_B("SYNC") )
            the_xpm_memory_tdpram
            ( .sleep(gnd),
              .clka(clock_a),
              .rsta(clear_a),
              .ena(signal_or_1),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(signal_or),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(signal_inst[3:0]),
              .sbiterra(signal_inst[4:4]),
              .dbiterra(signal_inst[5:5]),
              .doutb(signal_inst[17:6]),
              .sbiterrb(signal_inst[18:18]),
              .dbiterrb(signal_inst[19:19]) );
        assign signal_select_1 = signal_inst[3:0];
        assign qa = signal_select_1;
        assign qb = signal_select;

    endmodule
    |}];
  require_does_raise (fun () -> test_non_pow2_scale Simulation);
  [%expect
    {|
    ("ratio between port widths must be a power of 2. (update the simulation model if non-power-of-2 scale is required)"
     (scale 3))
    |}]
;;

let%expect_test "Elaborate Dual_port_ram in synthesis and simulation modes." =
  let create arch build_mode =
    let port port ~address_width ~data_width =
      let input name width = input (name ^ "_" ^ port) width in
      { Ram_port.address = input "address" address_width
      ; write_enable = input "write" 1
      ; read_enable = input "read" 1
      ; data = input "data" data_width
      }
    in
    let qa, qb =
      Dual_port_ram.create
        ~memory_optimization:false
        ~cascade_height:Inferred
        ~arch
        ~build_mode
        ()
        ~clock:(input "clock" 1)
        ~clear:(input "clear" 1)
        ~size:16
        ~port_a:(port "a" ~address_width:4 ~data_width:8)
        ~port_b:(port "b" ~address_width:4 ~data_width:8)
    in
    ignore
      ([ Circuit.create_exn ~name:"dual_port_ram" [ output "qa" qa; output "qb" qb ] ]
       |> Rtl.create Verilog
       |> Rtl.full_hierarchy
       : Rope.t)
  in
  require_does_not_raise (fun () -> create Distributed Synthesis);
  require_does_not_raise (fun () -> create (Blockram No_change) Synthesis);
  require_does_not_raise (fun () -> create (Ultraram Let_vivado_decide) Synthesis);
  require_does_not_raise (fun () -> create Distributed Simulation);
  require_does_not_raise (fun () -> create (Blockram Write_before_read) Simulation);
  require_does_not_raise (fun () -> create (Ultraram Let_vivado_decide) Simulation);
  [%expect {| |}]
;;

let%expect_test "Elaborate Simple_dual_port_ram in synthesis and simulation modes." =
  let create arch build_mode =
    let address_width = 16 in
    let data_width = 32 in
    let q =
      Simple_dual_port_ram.create
        ~memory_optimization:false
        ~cascade_height:Inferred
        ~arch
        ~build_mode
        ()
        ~clock:(input "clock" 1)
        ~clear:(input "clear" 1)
        ~size:65536
        ~write_address:(input "write_address" address_width)
        ~write_enable:(input "write_enable" 1)
        ~read_address:(input "read_address" address_width)
        ~read_enable:(input "read_enable" 1)
        ~data:(input "data" data_width)
    in
    ignore
      ([ Circuit.create_exn ~name:"simple_dual_port_ram" [ output "q" q ] ]
       |> Rtl.create Verilog
       |> Rtl.full_hierarchy
       : Rope.t)
  in
  require_does_not_raise (fun () -> create Distributed Synthesis);
  require_does_not_raise (fun () -> create (Blockram Read_before_write) Synthesis);
  require_does_not_raise (fun () -> create (Ultraram Let_vivado_decide) Synthesis);
  require_does_not_raise (fun () -> create Distributed Simulation);
  require_does_not_raise (fun () -> create (Blockram No_change) Simulation);
  require_does_not_raise (fun () -> create (Ultraram Let_vivado_decide) Simulation);
  [%expect {| |}]
;;

let%expect_test "byte enables" =
  circuit
    ~address_width_a:4
    ~address_width_b:4
    ~data_width_a:16
    ~data_width_b:16
    ~byte_enables:true
    (Blockram No_change)
    Simulation
  |> Rtl.print Verilog;
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        read_a,
        write_b,
        data_b,
        address_b,
        clock_b,
        write_a,
        data_a,
        clock_a,
        address_a,
        qa,
        qb
    );

        input read_b;
        input read_a;
        input [1:0] write_b;
        input [15:0] data_b;
        input [3:0] address_b;
        input clock_b;
        input [1:0] write_a;
        input [15:0] data_a;
        input clock_a;
        input [3:0] address_a;
        output [15:0] qa;
        output [15:0] qb;

        wire [1:0] signal_const;
        wire signal_eq;
        wire signal_and;
        wire signal_const_1;
        wire [4:0] signal_cat;
        wire [7:0] signal_mem_read_port;
        wire signal_const_2;
        wire [4:0] signal_cat_1;
        wire [7:0] signal_mem_read_port_1;
        wire [15:0] signal_cat_2;
        reg [15:0] signal_reg;
        wire signal_eq_1;
        wire signal_and_1;
        wire [4:0] signal_cat_3;
        wire [7:0] signal_mem_read_port_2;
        wire signal_select;
        wire [7:0] signal_select_1;
        wire [4:0] signal_cat_4;
        wire signal_select_2;
        wire [7:0] signal_select_3;
        wire [4:0] signal_cat_5;
        wire signal_select_4;
        wire [7:0] signal_select_5;
        wire [4:0] signal_cat_6;
        wire signal_select_6;
        wire [7:0] signal_select_7;
        wire [4:0] signal_cat_7;
        reg [7:0] signal_multiport_mem[0:31];
        wire [4:0] signal_cat_8;
        wire [7:0] signal_mem_read_port_3;
        wire [15:0] signal_cat_9;
        reg [15:0] signal_reg_1;
        assign signal_const = 2'b00;
        assign signal_eq = write_b == signal_const;
        assign signal_and = read_b & signal_eq;
        assign signal_const_1 = 1'b0;
        assign signal_cat = { address_b,
                              signal_const_1 };
        assign signal_mem_read_port = signal_multiport_mem[signal_cat];
        assign signal_const_2 = 1'b1;
        assign signal_cat_1 = { address_b,
                                signal_const_2 };
        assign signal_mem_read_port_1 = signal_multiport_mem[signal_cat_1];
        assign signal_cat_2 = { signal_mem_read_port_1,
                                signal_mem_read_port };
        always @(posedge clock_b) begin
            if (signal_and)
                signal_reg <= signal_cat_2;
        end
        assign signal_eq_1 = write_a == signal_const;
        assign signal_and_1 = read_a & signal_eq_1;
        assign signal_cat_3 = { address_a,
                                signal_const_1 };
        assign signal_mem_read_port_2 = signal_multiport_mem[signal_cat_3];
        assign signal_select = write_b[1:1];
        assign signal_select_1 = data_b[15:8];
        assign signal_cat_4 = { address_b,
                                signal_const_2 };
        assign signal_select_2 = write_b[0:0];
        assign signal_select_3 = data_b[7:0];
        assign signal_cat_5 = { address_b,
                                signal_const_1 };
        assign signal_select_4 = write_a[1:1];
        assign signal_select_5 = data_a[15:8];
        assign signal_cat_6 = { address_a,
                                signal_const_2 };
        assign signal_select_6 = write_a[0:0];
        assign signal_select_7 = data_a[7:0];
        assign signal_cat_7 = { address_a,
                                signal_const_1 };
        always @(posedge clock_a) begin
            if (signal_select_6)
                signal_multiport_mem[signal_cat_7] <= signal_select_7;
        end
        always @(posedge clock_a) begin
            if (signal_select_4)
                signal_multiport_mem[signal_cat_6] <= signal_select_5;
        end
        always @(posedge clock_b) begin
            if (signal_select_2)
                signal_multiport_mem[signal_cat_5] <= signal_select_3;
        end
        always @(posedge clock_b) begin
            if (signal_select)
                signal_multiport_mem[signal_cat_4] <= signal_select_1;
        end
        assign signal_cat_8 = { address_a,
                                signal_const_2 };
        assign signal_mem_read_port_3 = signal_multiport_mem[signal_cat_8];
        assign signal_cat_9 = { signal_mem_read_port_3,
                                signal_mem_read_port_2 };
        always @(posedge clock_a) begin
            if (signal_and_1)
                signal_reg_1 <= signal_cat_9;
        end
        assign qa = signal_reg_1;
        assign qb = signal_reg;

    endmodule
    |}];
  circuit
    ~address_width_a:4
    ~address_width_b:4
    ~data_width_a:18
    ~data_width_b:18
    ~byte_enables:true
    (Blockram No_change)
    Simulation
  |> Rtl.print Verilog;
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        read_a,
        write_b,
        data_b,
        address_b,
        clock_b,
        write_a,
        data_a,
        clock_a,
        address_a,
        qa,
        qb
    );

        input read_b;
        input read_a;
        input [1:0] write_b;
        input [17:0] data_b;
        input [3:0] address_b;
        input clock_b;
        input [1:0] write_a;
        input [17:0] data_a;
        input clock_a;
        input [3:0] address_a;
        output [17:0] qa;
        output [17:0] qb;

        wire [1:0] signal_const;
        wire signal_eq;
        wire signal_and;
        wire signal_const_1;
        wire [4:0] signal_cat;
        wire [8:0] signal_mem_read_port;
        wire signal_const_2;
        wire [4:0] signal_cat_1;
        wire [8:0] signal_mem_read_port_1;
        wire [17:0] signal_cat_2;
        reg [17:0] signal_reg;
        wire signal_eq_1;
        wire signal_and_1;
        wire [4:0] signal_cat_3;
        wire [8:0] signal_mem_read_port_2;
        wire signal_select;
        wire [8:0] signal_select_1;
        wire [4:0] signal_cat_4;
        wire signal_select_2;
        wire [8:0] signal_select_3;
        wire [4:0] signal_cat_5;
        wire signal_select_4;
        wire [8:0] signal_select_5;
        wire [4:0] signal_cat_6;
        wire signal_select_6;
        wire [8:0] signal_select_7;
        wire [4:0] signal_cat_7;
        reg [8:0] signal_multiport_mem[0:31];
        wire [4:0] signal_cat_8;
        wire [8:0] signal_mem_read_port_3;
        wire [17:0] signal_cat_9;
        reg [17:0] signal_reg_1;
        assign signal_const = 2'b00;
        assign signal_eq = write_b == signal_const;
        assign signal_and = read_b & signal_eq;
        assign signal_const_1 = 1'b0;
        assign signal_cat = { address_b,
                              signal_const_1 };
        assign signal_mem_read_port = signal_multiport_mem[signal_cat];
        assign signal_const_2 = 1'b1;
        assign signal_cat_1 = { address_b,
                                signal_const_2 };
        assign signal_mem_read_port_1 = signal_multiport_mem[signal_cat_1];
        assign signal_cat_2 = { signal_mem_read_port_1,
                                signal_mem_read_port };
        always @(posedge clock_b) begin
            if (signal_and)
                signal_reg <= signal_cat_2;
        end
        assign signal_eq_1 = write_a == signal_const;
        assign signal_and_1 = read_a & signal_eq_1;
        assign signal_cat_3 = { address_a,
                                signal_const_1 };
        assign signal_mem_read_port_2 = signal_multiport_mem[signal_cat_3];
        assign signal_select = write_b[1:1];
        assign signal_select_1 = data_b[17:9];
        assign signal_cat_4 = { address_b,
                                signal_const_2 };
        assign signal_select_2 = write_b[0:0];
        assign signal_select_3 = data_b[8:0];
        assign signal_cat_5 = { address_b,
                                signal_const_1 };
        assign signal_select_4 = write_a[1:1];
        assign signal_select_5 = data_a[17:9];
        assign signal_cat_6 = { address_a,
                                signal_const_2 };
        assign signal_select_6 = write_a[0:0];
        assign signal_select_7 = data_a[8:0];
        assign signal_cat_7 = { address_a,
                                signal_const_1 };
        always @(posedge clock_a) begin
            if (signal_select_6)
                signal_multiport_mem[signal_cat_7] <= signal_select_7;
        end
        always @(posedge clock_a) begin
            if (signal_select_4)
                signal_multiport_mem[signal_cat_6] <= signal_select_5;
        end
        always @(posedge clock_b) begin
            if (signal_select_2)
                signal_multiport_mem[signal_cat_5] <= signal_select_3;
        end
        always @(posedge clock_b) begin
            if (signal_select)
                signal_multiport_mem[signal_cat_4] <= signal_select_1;
        end
        assign signal_cat_8 = { address_a,
                                signal_const_2 };
        assign signal_mem_read_port_3 = signal_multiport_mem[signal_cat_8];
        assign signal_cat_9 = { signal_mem_read_port_3,
                                signal_mem_read_port_2 };
        always @(posedge clock_a) begin
            if (signal_and_1)
                signal_reg_1 <= signal_cat_9;
        end
        assign qa = signal_reg_1;
        assign qb = signal_reg;

    endmodule
    |}]
;;

let%expect_test "byte enables with resizing" =
  circuit
    ~address_width_a:4
    ~address_width_b:5
    ~data_width_a:16
    ~data_width_b:8
    ~byte_enables:true
    (Blockram No_change)
    Simulation
  |> Rtl.print Verilog;
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        read_a,
        write_b,
        data_b,
        address_b,
        clock_b,
        write_a,
        data_a,
        clock_a,
        address_a,
        qa,
        qb
    );

        input read_b;
        input read_a;
        input write_b;
        input [7:0] data_b;
        input [4:0] address_b;
        input clock_b;
        input [1:0] write_a;
        input [15:0] data_a;
        input clock_a;
        input [3:0] address_a;
        output [15:0] qa;
        output [7:0] qb;

        wire signal_const;
        wire signal_eq;
        wire signal_and;
        wire [7:0] signal_select;
        wire [4:0] signal_cat;
        wire [7:0] signal_mem_read_port;
        wire signal_const_2;
        wire [3:0] signal_select_1;
        wire [4:0] signal_cat_1;
        wire [7:0] signal_mem_read_port_1;
        wire [15:0] signal_cat_2;
        wire [7:0] signal_select_2;
        wire signal_select_3;
        wire [7:0] signal_mux;
        reg [7:0] signal_reg;
        wire [1:0] signal_const_3;
        wire signal_eq_1;
        wire signal_and_1;
        wire [4:0] signal_cat_3;
        wire [7:0] signal_mem_read_port_2;
        wire signal_select_4;
        wire [7:0] signal_select_5;
        wire [4:0] signal_cat_4;
        wire signal_eq_2;
        wire signal_mux_1;
        wire signal_select_6;
        wire signal_eq_3;
        wire signal_mux_2;
        wire [1:0] signal_cat_5;
        wire signal_select_7;
        wire [15:0] signal_cat_6;
        wire [7:0] signal_select_8;
        wire [3:0] signal_select_9;
        wire [4:0] signal_cat_7;
        wire signal_select_10;
        wire [7:0] signal_select_11;
        wire [4:0] signal_cat_8;
        wire signal_select_12;
        wire [7:0] signal_select_13;
        wire [4:0] signal_cat_9;
        reg [7:0] signal_multiport_mem[0:31];
        wire [4:0] signal_cat_10;
        wire [7:0] signal_mem_read_port_3;
        wire [15:0] signal_cat_11;
        reg [15:0] signal_reg_1;
        assign signal_const = 1'b0;
        assign signal_eq = write_b == signal_const;
        assign signal_and = read_b & signal_eq;
        assign signal_select = signal_cat_2[15:8];
        assign signal_cat = { signal_select_1,
                              signal_const };
        assign signal_mem_read_port = signal_multiport_mem[signal_cat];
        assign signal_const_2 = 1'b1;
        assign signal_select_1 = address_b[4:1];
        assign signal_cat_1 = { signal_select_1,
                                signal_const_2 };
        assign signal_mem_read_port_1 = signal_multiport_mem[signal_cat_1];
        assign signal_cat_2 = { signal_mem_read_port_1,
                                signal_mem_read_port };
        assign signal_select_2 = signal_cat_2[7:0];
        assign signal_select_3 = address_b[0:0];
        assign signal_mux = signal_select_3 ? signal_select : signal_select_2;
        always @(posedge clock_b) begin
            if (signal_and)
                signal_reg <= signal_mux;
        end
        assign signal_const_3 = 2'b00;
        assign signal_eq_1 = write_a == signal_const_3;
        assign signal_and_1 = read_a & signal_eq_1;
        assign signal_cat_3 = { address_a,
                                signal_const };
        assign signal_mem_read_port_2 = signal_multiport_mem[signal_cat_3];
        assign signal_select_4 = signal_cat_5[1:1];
        assign signal_select_5 = signal_cat_6[15:8];
        assign signal_cat_4 = { signal_select_9,
                                signal_const_2 };
        assign signal_eq_2 = signal_select_6 == signal_const;
        assign signal_mux_1 = signal_eq_2 ? write_b : signal_const;
        assign signal_select_6 = address_b[0:0];
        assign signal_eq_3 = signal_select_6 == signal_const_2;
        assign signal_mux_2 = signal_eq_3 ? write_b : signal_const;
        assign signal_cat_5 = { signal_mux_2,
                                signal_mux_1 };
        assign signal_select_7 = signal_cat_5[0:0];
        assign signal_cat_6 = { data_b,
                                data_b };
        assign signal_select_8 = signal_cat_6[7:0];
        assign signal_select_9 = address_b[4:1];
        assign signal_cat_7 = { signal_select_9,
                                signal_const };
        assign signal_select_10 = write_a[1:1];
        assign signal_select_11 = data_a[15:8];
        assign signal_cat_8 = { address_a,
                                signal_const_2 };
        assign signal_select_12 = write_a[0:0];
        assign signal_select_13 = data_a[7:0];
        assign signal_cat_9 = { address_a,
                                signal_const };
        always @(posedge clock_a) begin
            if (signal_select_12)
                signal_multiport_mem[signal_cat_9] <= signal_select_13;
        end
        always @(posedge clock_a) begin
            if (signal_select_10)
                signal_multiport_mem[signal_cat_8] <= signal_select_11;
        end
        always @(posedge clock_b) begin
            if (signal_select_7)
                signal_multiport_mem[signal_cat_7] <= signal_select_8;
        end
        always @(posedge clock_b) begin
            if (signal_select_4)
                signal_multiport_mem[signal_cat_4] <= signal_select_5;
        end
        assign signal_cat_10 = { address_a,
                                 signal_const_2 };
        assign signal_mem_read_port_3 = signal_multiport_mem[signal_cat_10];
        assign signal_cat_11 = { signal_mem_read_port_3,
                                 signal_mem_read_port_2 };
        always @(posedge clock_a) begin
            if (signal_and_1)
                signal_reg_1 <= signal_cat_11;
        end
        assign qa = signal_reg_1;
        assign qb = signal_reg;

    endmodule
    |}];
  circuit
    ~address_width_a:4
    ~address_width_b:5
    ~data_width_a:18
    ~data_width_b:9
    ~byte_enables:true
    (Blockram No_change)
    Simulation
  |> Rtl.print Verilog;
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        read_a,
        write_b,
        data_b,
        address_b,
        clock_b,
        write_a,
        data_a,
        clock_a,
        address_a,
        qa,
        qb
    );

        input read_b;
        input read_a;
        input write_b;
        input [8:0] data_b;
        input [4:0] address_b;
        input clock_b;
        input [1:0] write_a;
        input [17:0] data_a;
        input clock_a;
        input [3:0] address_a;
        output [17:0] qa;
        output [8:0] qb;

        wire signal_const;
        wire signal_eq;
        wire signal_and;
        wire [8:0] signal_select;
        wire [4:0] signal_cat;
        wire [8:0] signal_mem_read_port;
        wire signal_const_2;
        wire [3:0] signal_select_1;
        wire [4:0] signal_cat_1;
        wire [8:0] signal_mem_read_port_1;
        wire [17:0] signal_cat_2;
        wire [8:0] signal_select_2;
        wire signal_select_3;
        wire [8:0] signal_mux;
        reg [8:0] signal_reg;
        wire [1:0] signal_const_3;
        wire signal_eq_1;
        wire signal_and_1;
        wire [4:0] signal_cat_3;
        wire [8:0] signal_mem_read_port_2;
        wire signal_select_4;
        wire [8:0] signal_select_5;
        wire [4:0] signal_cat_4;
        wire signal_eq_2;
        wire signal_mux_1;
        wire signal_select_6;
        wire signal_eq_3;
        wire signal_mux_2;
        wire [1:0] signal_cat_5;
        wire signal_select_7;
        wire [17:0] signal_cat_6;
        wire [8:0] signal_select_8;
        wire [3:0] signal_select_9;
        wire [4:0] signal_cat_7;
        wire signal_select_10;
        wire [8:0] signal_select_11;
        wire [4:0] signal_cat_8;
        wire signal_select_12;
        wire [8:0] signal_select_13;
        wire [4:0] signal_cat_9;
        reg [8:0] signal_multiport_mem[0:31];
        wire [4:0] signal_cat_10;
        wire [8:0] signal_mem_read_port_3;
        wire [17:0] signal_cat_11;
        reg [17:0] signal_reg_1;
        assign signal_const = 1'b0;
        assign signal_eq = write_b == signal_const;
        assign signal_and = read_b & signal_eq;
        assign signal_select = signal_cat_2[17:9];
        assign signal_cat = { signal_select_1,
                              signal_const };
        assign signal_mem_read_port = signal_multiport_mem[signal_cat];
        assign signal_const_2 = 1'b1;
        assign signal_select_1 = address_b[4:1];
        assign signal_cat_1 = { signal_select_1,
                                signal_const_2 };
        assign signal_mem_read_port_1 = signal_multiport_mem[signal_cat_1];
        assign signal_cat_2 = { signal_mem_read_port_1,
                                signal_mem_read_port };
        assign signal_select_2 = signal_cat_2[8:0];
        assign signal_select_3 = address_b[0:0];
        assign signal_mux = signal_select_3 ? signal_select : signal_select_2;
        always @(posedge clock_b) begin
            if (signal_and)
                signal_reg <= signal_mux;
        end
        assign signal_const_3 = 2'b00;
        assign signal_eq_1 = write_a == signal_const_3;
        assign signal_and_1 = read_a & signal_eq_1;
        assign signal_cat_3 = { address_a,
                                signal_const };
        assign signal_mem_read_port_2 = signal_multiport_mem[signal_cat_3];
        assign signal_select_4 = signal_cat_5[1:1];
        assign signal_select_5 = signal_cat_6[17:9];
        assign signal_cat_4 = { signal_select_9,
                                signal_const_2 };
        assign signal_eq_2 = signal_select_6 == signal_const;
        assign signal_mux_1 = signal_eq_2 ? write_b : signal_const;
        assign signal_select_6 = address_b[0:0];
        assign signal_eq_3 = signal_select_6 == signal_const_2;
        assign signal_mux_2 = signal_eq_3 ? write_b : signal_const;
        assign signal_cat_5 = { signal_mux_2,
                                signal_mux_1 };
        assign signal_select_7 = signal_cat_5[0:0];
        assign signal_cat_6 = { data_b,
                                data_b };
        assign signal_select_8 = signal_cat_6[8:0];
        assign signal_select_9 = address_b[4:1];
        assign signal_cat_7 = { signal_select_9,
                                signal_const };
        assign signal_select_10 = write_a[1:1];
        assign signal_select_11 = data_a[17:9];
        assign signal_cat_8 = { address_a,
                                signal_const_2 };
        assign signal_select_12 = write_a[0:0];
        assign signal_select_13 = data_a[8:0];
        assign signal_cat_9 = { address_a,
                                signal_const };
        always @(posedge clock_a) begin
            if (signal_select_12)
                signal_multiport_mem[signal_cat_9] <= signal_select_13;
        end
        always @(posedge clock_a) begin
            if (signal_select_10)
                signal_multiport_mem[signal_cat_8] <= signal_select_11;
        end
        always @(posedge clock_b) begin
            if (signal_select_7)
                signal_multiport_mem[signal_cat_7] <= signal_select_8;
        end
        always @(posedge clock_b) begin
            if (signal_select_4)
                signal_multiport_mem[signal_cat_4] <= signal_select_5;
        end
        assign signal_cat_10 = { address_a,
                                 signal_const_2 };
        assign signal_mem_read_port_3 = signal_multiport_mem[signal_cat_10];
        assign signal_cat_11 = { signal_mem_read_port_3,
                                 signal_mem_read_port_2 };
        always @(posedge clock_a) begin
            if (signal_and_1)
                signal_reg_1 <= signal_cat_11;
        end
        assign qa = signal_reg_1;
        assign qb = signal_reg;

    endmodule
    |}]
;;

let get_port sim suffix =
  let get name = Cyclesim.in_port sim (name ^ "_" ^ suffix) in
  ( { Ram_port.address = get "address"
    ; write_enable = get "write"
    ; read_enable = get "read"
    ; data = get "data"
    }
  , Cyclesim.out_port sim ("q" ^ suffix) )
;;

let sim ~address_width_a ~address_width_b ~data_width_a ~data_width_b =
  let circ =
    circuit
      ~address_width_a
      ~address_width_b
      ~data_width_a
      ~data_width_b
      ~byte_enables:true
      (Blockram Read_before_write)
      Simulation
  in
  let sim = Cyclesim.create circ in
  let waves, sim = Cyclesim.Waveform.create sim in
  let a, _qa = get_port sim "a" in
  let b, _qb = get_port sim "b" in
  let open Bits in
  let cycle () =
    Cyclesim.cycle sim;
    a.read_enable <--. 0;
    a.write_enable <--. 0;
    b.read_enable <--. 0;
    b.write_enable <--. 0
  in
  let write (port : _ Ram_port.t) address data enable =
    port.address <--. address;
    port.data <--. data;
    port.write_enable <--. enable
  in
  let read (port : _ Ram_port.t) address =
    port.address <--. address;
    port.read_enable <--. 1
  in
  waves, cycle, a, b, write, read
;;

let%expect_test "simulate 16 -> 32 w/ enables" =
  let waves, cycle, a, b, write, read =
    sim ~address_width_a:5 ~address_width_b:4 ~data_width_a:16 ~data_width_b:32
  in
  write b 6 0xffccbbff 0b0110;
  cycle ();
  write b 6 0xdd0000aa 0b1001;
  read b 6;
  cycle ();
  cycle ();
  read b 6;
  cycle ();
  read a 12;
  cycle ();
  read a 13;
  cycle ();
  write a 13 0x1122 0b11;
  cycle ();
  read a 13;
  read b 6;
  cycle ();
  cycle ();
  cycle ();
  Waveform.expect_exact waves ~wave_width:2;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────────────────────────┬─────┬─────────────────────────────        │
    │address_a         ││ 00                     │0C   │0D                                   │
    │                  ││────────────────────────┴─────┴─────────────────────────────        │
    │                  ││────────────────────────────────────────────────────────────        │
    │address_b         ││ 6                                                                  │
    │                  ││────────────────────────────────────────────────────────────        │
    │clock_a           ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────        │
    │clock_b           ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────        │
    │                  ││────────────────────────────────────┬───────────────────────        │
    │data_a            ││ 0000                               │1122                           │
    │                  ││────────────────────────────────────┴───────────────────────        │
    │                  ││──────┬─────────────────────────────────────────────────────        │
    │data_b            ││ FFCC.│DD0000AA                                                     │
    │                  ││──────┴─────────────────────────────────────────────────────        │
    │read_a            ││                        ┌───────────┐     ┌─────┐                   │
    │                  ││────────────────────────┘           └─────┘     └───────────        │
    │read_b            ││      ┌─────┐     ┌─────┐                 ┌─────┐                   │
    │                  ││──────┘     └─────┘     └─────────────────┘     └───────────        │
    │                  ││────────────────────────────────────┬─────┬─────────────────        │
    │write_a           ││ 0                                  │3    │0                        │
    │                  ││────────────────────────────────────┴─────┴─────────────────        │
    │                  ││──────┬─────┬───────────────────────────────────────────────        │
    │write_b           ││ 6    │9    │0                                                      │
    │                  ││──────┴─────┴───────────────────────────────────────────────        │
    │                  ││──────────────────────────────┬─────┬───────────┬───────────        │
    │qa                ││ 0000                         │BBAA │DDCC       │1122               │
    │                  ││──────────────────────────────┴─────┴───────────┴───────────        │
    │                  ││────────────┬───────────┬───────────────────────┬───────────        │
    │qb                ││ 00000000   │00CCBB00   │DDCCBBAA               │1122BBAA           │
    │                  ││────────────┴───────────┴───────────────────────┴───────────        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    7165a9de0d9be2be0b3a3b8e93e13fd4
    |}]
;;

let%expect_test "simulate 18-> 9 w/ enables" =
  let waves, cycle, a, b, write, read =
    sim ~address_width_a:4 ~address_width_b:5 ~data_width_a:18 ~data_width_b:9
  in
  write a 3 (0x1aa lor (0x1bb lsl 9)) 3;
  cycle ();
  write a 4 (0x1cc lsl 9) 2;
  cycle ();
  read a 4;
  cycle ();
  read a 3;
  cycle ();
  read b 6;
  cycle ();
  read b 7;
  cycle ();
  read b 9;
  cycle ();
  write b 8 0x1dd 1;
  cycle ();
  read b 8;
  read a 4;
  cycle ();
  cycle ();
  Waveform.expect_exact waves ~wave_width:2;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││──────┬───────────┬─────────────────────────────┬───────────        │
    │address_a         ││ 3    │4          │3                            │4                  │
    │                  ││──────┴───────────┴─────────────────────────────┴───────────        │
    │                  ││────────────────────────┬─────┬─────┬─────┬─────────────────        │
    │address_b         ││ 00                     │06   │07   │09   │08                       │
    │                  ││────────────────────────┴─────┴─────┴─────┴─────────────────        │
    │clock_a           ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────        │
    │clock_b           ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────        │
    │                  ││──────┬─────────────────────────────────────────────────────        │
    │data_a            ││ 377AA│39800                                                        │
    │                  ││──────┴─────────────────────────────────────────────────────        │
    │                  ││──────────────────────────────────────────┬─────────────────        │
    │data_b            ││ 000                                      │1DD                      │
    │                  ││──────────────────────────────────────────┴─────────────────        │
    │read_a            ││            ┌───────────┐                       ┌─────┐             │
    │                  ││────────────┘           └───────────────────────┘     └─────        │
    │read_b            ││                        ┌─────────────────┐     ┌─────┐             │
    │                  ││────────────────────────┘                 └─────┘     └─────        │
    │                  ││──────┬─────┬───────────────────────────────────────────────        │
    │write_a           ││ 3    │2    │0                                                      │
    │                  ││──────┴─────┴───────────────────────────────────────────────        │
    │write_b           ││                                          ┌─────┐                   │
    │                  ││──────────────────────────────────────────┘     └───────────        │
    │                  ││──────────────────┬─────┬─────────────────────────────┬─────        │
    │qa                ││ 00000            │39800│377AA                        │399DD        │
    │                  ││──────────────────┴─────┴─────────────────────────────┴─────        │
    │                  ││──────────────────────────────┬─────┬─────┬─────┬─────┬─────        │
    │qb                ││ 000                          │1AA  │1BB  │1CC  │000  │1DD          │
    │                  ││──────────────────────────────┴─────┴─────┴─────┴─────┴─────        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    1fb8ddf75deb39ef58fa54bd8c562025
    |}]
;;
