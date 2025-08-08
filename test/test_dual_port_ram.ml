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
  Rtl.print
    Verilog
    (circuit
       ?address_width_a
       ?data_width_a
       ?address_width_b
       ?data_width_b
       ?size
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

        wire [7:0] _20;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _19;
        wire [7:0] _21;
        assign _20 = _19[17:10];
        assign _18 = write_b | read_b;
        assign vdd = 1'b1;
        assign _16 = write_a | read_a;
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
              .ena(_16),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(_18),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(_19[7:0]),
              .sbiterra(_19[8:8]),
              .dbiterra(_19[9:9]),
              .doutb(_19[17:10]),
              .sbiterrb(_19[18:18]),
              .dbiterrb(_19[19:19]) );
        assign _21 = _19[7:0];
        assign qa = _21;
        assign qb = _20;

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

        wire [7:0] _20;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _19;
        wire [7:0] _21;
        assign _20 = _19[17:10];
        assign _18 = write_b | read_b;
        assign vdd = 1'b1;
        assign _16 = write_a | read_a;
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
              .ena(_16),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(_18),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(_19[7:0]),
              .sbiterra(_19[8:8]),
              .dbiterra(_19[9:9]),
              .doutb(_19[17:10]),
              .sbiterrb(_19[18:18]),
              .dbiterrb(_19[19:19]) );
        assign _21 = _19[7:0];
        assign qa = _21;
        assign qb = _20;

    endmodule
    |}];
  print_verilog Ultraram Synthesis;
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

        wire [7:0] _20;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _19;
        wire [7:0] _21;
        assign _20 = _19[17:10];
        assign _18 = write_b | read_b;
        assign vdd = 1'b1;
        assign _16 = write_a | read_a;
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
              .ena(_16),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(_18),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(_19[7:0]),
              .sbiterra(_19[8:8]),
              .dbiterra(_19[9:9]),
              .doutb(_19[17:10]),
              .sbiterrb(_19[18:18]),
              .dbiterrb(_19[19:19]) );
        assign _21 = _19[7:0];
        assign qa = _21;
        assign qb = _20;

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

        wire [11:0] _20;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _19;
        wire [3:0] _21;
        assign _20 = _19[17:6];
        assign _18 = write_b | read_b;
        assign vdd = 1'b1;
        assign _16 = write_a | read_a;
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
              .ena(_16),
              .regcea(vdd),
              .wea(write_a),
              .addra(address_a),
              .dina(data_a),
              .injectsbiterra(gnd),
              .injectdbiterra(gnd),
              .clkb(clock_b),
              .rstb(clear_b),
              .enb(_18),
              .regceb(vdd),
              .web(write_b),
              .addrb(address_b),
              .dinb(data_b),
              .injectsbiterrb(gnd),
              .injectdbiterrb(gnd),
              .douta(_19[3:0]),
              .sbiterra(_19[4:4]),
              .dbiterra(_19[5:5]),
              .doutb(_19[17:6]),
              .sbiterrb(_19[18:18]),
              .dbiterrb(_19[19:19]) );
        assign _21 = _19[3:0];
        assign qa = _21;
        assign qb = _20;

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
  require_does_not_raise (fun () -> create Ultraram Synthesis);
  require_does_not_raise (fun () -> create Distributed Simulation);
  require_does_not_raise (fun () -> create (Blockram Write_before_read) Simulation);
  require_does_not_raise (fun () -> create Ultraram Simulation);
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
  require_does_not_raise (fun () -> create Ultraram Synthesis);
  require_does_not_raise (fun () -> create Distributed Simulation);
  require_does_not_raise (fun () -> create (Blockram No_change) Simulation);
  require_does_not_raise (fun () -> create Ultraram Simulation);
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

        wire [1:0] _37;
        wire _38;
        wire _39;
        wire _33;
        wire [4:0] _34;
        wire [7:0] _35;
        wire _13;
        wire [4:0] _14;
        wire [7:0] _32;
        wire [15:0] _36;
        reg [15:0] _40;
        wire _49;
        wire _50;
        wire [4:0] _45;
        wire [7:0] _46;
        wire _30;
        wire [7:0] _29;
        wire [4:0] _28;
        wire _26;
        wire [7:0] _25;
        wire [4:0] _24;
        wire _22;
        wire [7:0] _21;
        wire [4:0] _20;
        wire _18;
        wire [7:0] _17;
        wire [4:0] _16;
        reg [7:0] _31[0:31];
        wire [4:0] _42;
        wire [7:0] _43;
        wire [15:0] _47;
        reg [15:0] _51;
        assign _37 = 2'b00;
        assign _38 = write_b == _37;
        assign _39 = read_b & _38;
        assign _33 = 1'b0;
        assign _34 = { address_b,
                       _33 };
        assign _35 = _31[_34];
        assign _13 = 1'b1;
        assign _14 = { address_b,
                       _13 };
        assign _32 = _31[_14];
        assign _36 = { _32,
                       _35 };
        always @(posedge clock_b) begin
            if (_39)
                _40 <= _36;
        end
        assign _49 = write_a == _37;
        assign _50 = read_a & _49;
        assign _45 = { address_a,
                       _33 };
        assign _46 = _31[_45];
        assign _30 = write_b[1:1];
        assign _29 = data_b[15:8];
        assign _28 = { address_b,
                       _13 };
        assign _26 = write_b[0:0];
        assign _25 = data_b[7:0];
        assign _24 = { address_b,
                       _33 };
        assign _22 = write_a[1:1];
        assign _21 = data_a[15:8];
        assign _20 = { address_a,
                       _13 };
        assign _18 = write_a[0:0];
        assign _17 = data_a[7:0];
        assign _16 = { address_a,
                       _33 };
        always @(posedge clock_a) begin
            if (_18)
                _31[_16] <= _17;
        end
        always @(posedge clock_a) begin
            if (_22)
                _31[_20] <= _21;
        end
        always @(posedge clock_b) begin
            if (_26)
                _31[_24] <= _25;
        end
        always @(posedge clock_b) begin
            if (_30)
                _31[_28] <= _29;
        end
        assign _42 = { address_a,
                       _13 };
        assign _43 = _31[_42];
        assign _47 = { _43,
                       _46 };
        always @(posedge clock_a) begin
            if (_50)
                _51 <= _47;
        end
        assign qa = _51;
        assign qb = _40;

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

        wire [1:0] _37;
        wire _38;
        wire _39;
        wire _33;
        wire [4:0] _34;
        wire [8:0] _35;
        wire _13;
        wire [4:0] _14;
        wire [8:0] _32;
        wire [17:0] _36;
        reg [17:0] _40;
        wire _49;
        wire _50;
        wire [4:0] _45;
        wire [8:0] _46;
        wire _30;
        wire [8:0] _29;
        wire [4:0] _28;
        wire _26;
        wire [8:0] _25;
        wire [4:0] _24;
        wire _22;
        wire [8:0] _21;
        wire [4:0] _20;
        wire _18;
        wire [8:0] _17;
        wire [4:0] _16;
        reg [8:0] _31[0:31];
        wire [4:0] _42;
        wire [8:0] _43;
        wire [17:0] _47;
        reg [17:0] _51;
        assign _37 = 2'b00;
        assign _38 = write_b == _37;
        assign _39 = read_b & _38;
        assign _33 = 1'b0;
        assign _34 = { address_b,
                       _33 };
        assign _35 = _31[_34];
        assign _13 = 1'b1;
        assign _14 = { address_b,
                       _13 };
        assign _32 = _31[_14];
        assign _36 = { _32,
                       _35 };
        always @(posedge clock_b) begin
            if (_39)
                _40 <= _36;
        end
        assign _49 = write_a == _37;
        assign _50 = read_a & _49;
        assign _45 = { address_a,
                       _33 };
        assign _46 = _31[_45];
        assign _30 = write_b[1:1];
        assign _29 = data_b[17:9];
        assign _28 = { address_b,
                       _13 };
        assign _26 = write_b[0:0];
        assign _25 = data_b[8:0];
        assign _24 = { address_b,
                       _33 };
        assign _22 = write_a[1:1];
        assign _21 = data_a[17:9];
        assign _20 = { address_a,
                       _13 };
        assign _18 = write_a[0:0];
        assign _17 = data_a[8:0];
        assign _16 = { address_a,
                       _33 };
        always @(posedge clock_a) begin
            if (_18)
                _31[_16] <= _17;
        end
        always @(posedge clock_a) begin
            if (_22)
                _31[_20] <= _21;
        end
        always @(posedge clock_b) begin
            if (_26)
                _31[_24] <= _25;
        end
        always @(posedge clock_b) begin
            if (_30)
                _31[_28] <= _29;
        end
        assign _42 = { address_a,
                       _13 };
        assign _43 = _31[_42];
        assign _47 = { _43,
                       _46 };
        always @(posedge clock_a) begin
            if (_50)
                _51 <= _47;
        end
        assign qa = _51;
        assign qb = _40;

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

        wire _54;
        wire _55;
        wire _56;
        wire [7:0] _52;
        wire [4:0] _48;
        wire [7:0] _49;
        wire _15;
        wire [3:0] _14;
        wire [4:0] _16;
        wire [7:0] _46;
        wire [15:0] _50;
        wire [7:0] _51;
        wire _13;
        wire [7:0] _53;
        reg [7:0] _57;
        wire [1:0] _65;
        wire _66;
        wire _67;
        wire [4:0] _62;
        wire [7:0] _63;
        wire _44;
        wire [7:0] _43;
        wire [4:0] _42;
        wire _36;
        wire _38;
        wire _30;
        wire _32;
        wire _34;
        wire [1:0] _39;
        wire _40;
        wire [15:0] _28;
        wire [7:0] _29;
        wire [3:0] _25;
        wire [4:0] _27;
        wire _24;
        wire [7:0] _23;
        wire [4:0] _22;
        wire _20;
        wire [7:0] _19;
        wire [4:0] _18;
        reg [7:0] _45[0:31];
        wire [4:0] _59;
        wire [7:0] _60;
        wire [15:0] _64;
        reg [15:0] _68;
        assign _54 = 1'b0;
        assign _55 = write_b == _54;
        assign _56 = read_b & _55;
        assign _52 = _50[15:8];
        assign _48 = { _14,
                       _54 };
        assign _49 = _45[_48];
        assign _15 = 1'b1;
        assign _14 = address_b[4:1];
        assign _16 = { _14,
                       _15 };
        assign _46 = _45[_16];
        assign _50 = { _46,
                       _49 };
        assign _51 = _50[7:0];
        assign _13 = address_b[0:0];
        assign _53 = _13 ? _52 : _51;
        always @(posedge clock_b) begin
            if (_56)
                _57 <= _53;
        end
        assign _65 = 2'b00;
        assign _66 = write_a == _65;
        assign _67 = read_a & _66;
        assign _62 = { address_a,
                       _54 };
        assign _63 = _45[_62];
        assign _44 = _39[1:1];
        assign _43 = _28[15:8];
        assign _42 = { _25,
                       _15 };
        assign _36 = _30 == _54;
        assign _38 = _36 ? write_b : _54;
        assign _30 = address_b[0:0];
        assign _32 = _30 == _15;
        assign _34 = _32 ? write_b : _54;
        assign _39 = { _34,
                       _38 };
        assign _40 = _39[0:0];
        assign _28 = { data_b,
                       data_b };
        assign _29 = _28[7:0];
        assign _25 = address_b[4:1];
        assign _27 = { _25,
                       _54 };
        assign _24 = write_a[1:1];
        assign _23 = data_a[15:8];
        assign _22 = { address_a,
                       _15 };
        assign _20 = write_a[0:0];
        assign _19 = data_a[7:0];
        assign _18 = { address_a,
                       _54 };
        always @(posedge clock_a) begin
            if (_20)
                _45[_18] <= _19;
        end
        always @(posedge clock_a) begin
            if (_24)
                _45[_22] <= _23;
        end
        always @(posedge clock_b) begin
            if (_40)
                _45[_27] <= _29;
        end
        always @(posedge clock_b) begin
            if (_44)
                _45[_42] <= _43;
        end
        assign _59 = { address_a,
                       _15 };
        assign _60 = _45[_59];
        assign _64 = { _60,
                       _63 };
        always @(posedge clock_a) begin
            if (_67)
                _68 <= _64;
        end
        assign qa = _68;
        assign qb = _57;

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

        wire _54;
        wire _55;
        wire _56;
        wire [8:0] _52;
        wire [4:0] _48;
        wire [8:0] _49;
        wire _15;
        wire [3:0] _14;
        wire [4:0] _16;
        wire [8:0] _46;
        wire [17:0] _50;
        wire [8:0] _51;
        wire _13;
        wire [8:0] _53;
        reg [8:0] _57;
        wire [1:0] _65;
        wire _66;
        wire _67;
        wire [4:0] _62;
        wire [8:0] _63;
        wire _44;
        wire [8:0] _43;
        wire [4:0] _42;
        wire _36;
        wire _38;
        wire _30;
        wire _32;
        wire _34;
        wire [1:0] _39;
        wire _40;
        wire [17:0] _28;
        wire [8:0] _29;
        wire [3:0] _25;
        wire [4:0] _27;
        wire _24;
        wire [8:0] _23;
        wire [4:0] _22;
        wire _20;
        wire [8:0] _19;
        wire [4:0] _18;
        reg [8:0] _45[0:31];
        wire [4:0] _59;
        wire [8:0] _60;
        wire [17:0] _64;
        reg [17:0] _68;
        assign _54 = 1'b0;
        assign _55 = write_b == _54;
        assign _56 = read_b & _55;
        assign _52 = _50[17:9];
        assign _48 = { _14,
                       _54 };
        assign _49 = _45[_48];
        assign _15 = 1'b1;
        assign _14 = address_b[4:1];
        assign _16 = { _14,
                       _15 };
        assign _46 = _45[_16];
        assign _50 = { _46,
                       _49 };
        assign _51 = _50[8:0];
        assign _13 = address_b[0:0];
        assign _53 = _13 ? _52 : _51;
        always @(posedge clock_b) begin
            if (_56)
                _57 <= _53;
        end
        assign _65 = 2'b00;
        assign _66 = write_a == _65;
        assign _67 = read_a & _66;
        assign _62 = { address_a,
                       _54 };
        assign _63 = _45[_62];
        assign _44 = _39[1:1];
        assign _43 = _28[17:9];
        assign _42 = { _25,
                       _15 };
        assign _36 = _30 == _54;
        assign _38 = _36 ? write_b : _54;
        assign _30 = address_b[0:0];
        assign _32 = _30 == _15;
        assign _34 = _32 ? write_b : _54;
        assign _39 = { _34,
                       _38 };
        assign _40 = _39[0:0];
        assign _28 = { data_b,
                       data_b };
        assign _29 = _28[8:0];
        assign _25 = address_b[4:1];
        assign _27 = { _25,
                       _54 };
        assign _24 = write_a[1:1];
        assign _23 = data_a[17:9];
        assign _22 = { address_a,
                       _15 };
        assign _20 = write_a[0:0];
        assign _19 = data_a[8:0];
        assign _18 = { address_a,
                       _54 };
        always @(posedge clock_a) begin
            if (_20)
                _45[_18] <= _19;
        end
        always @(posedge clock_a) begin
            if (_24)
                _45[_22] <= _23;
        end
        always @(posedge clock_b) begin
            if (_40)
                _45[_27] <= _29;
        end
        always @(posedge clock_b) begin
            if (_44)
                _45[_42] <= _43;
        end
        assign _59 = { address_a,
                       _15 };
        assign _60 = _45[_59];
        assign _64 = { _60,
                       _63 };
        always @(posedge clock_a) begin
            if (_67)
                _68 <= _64;
        end
        assign qa = _68;
        assign qb = _57;

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
  let waves, sim = Waveform.create sim in
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
