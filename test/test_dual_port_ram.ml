open! Import
open Signal

let%expect_test "True_dual_port_ram" =
  let create
    ?(address_width_a = 4)
    ?(data_width_a = 8)
    ?(address_width_b = 4)
    ?(data_width_b = 8)
    ?size
    arch
    build_mode
    =
    let port port ~address_width ~data_width =
      let input name width = input (name ^ "_" ^ port) width in
      { Ram_port.address = input "address" address_width
      ; write_enable = input "write" 1
      ; read_enable = input "read" 1
      ; data = input "data" data_width
      }
    in
    let qa, qb =
      True_dual_port_ram.create
        ~memory_optimization:false
        ~cascade_height:Inferred
        ~arch
        ~build_mode
        ()
        ~clock_a:(input "clock_a" 1)
        ~clock_b:(input "clock_b" 1)
        ~clear_a:(input "clear_a" 1)
        ~clear_b:(input "clear_b" 1)
        ~size:(Option.value size ~default:(1 lsl address_width_a))
        ~port_a:(port "a" ~address_width:address_width_a ~data_width:data_width_a)
        ~port_b:(port "b" ~address_width:address_width_b ~data_width:data_width_b)
    in
    Circuit.create_exn ~name:"true_dual_port_ram" [ output "qa" qa; output "qb" qb ]
    |> Rtl.print Verilog
  in
  create Distributed Synthesis;
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

        wire [7:0] _21;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _20;
        wire [7:0] _22;
        assign _21 = _20[17:10];
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
              .dbiterrb(_20[19:19]),
              .sbiterrb(_20[18:18]),
              .doutb(_20[17:10]),
              .dbiterra(_20[9:9]),
              .sbiterra(_20[8:8]),
              .douta(_20[7:0]) );
        assign _22 = _20[7:0];
        assign qa = _22;
        assign qb = _21;

    endmodule
    |}];
  create (Blockram Read_before_write) Synthesis;
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

        wire [7:0] _21;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _20;
        wire [7:0] _22;
        assign _21 = _20[17:10];
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
              .dbiterrb(_20[19:19]),
              .sbiterrb(_20[18:18]),
              .doutb(_20[17:10]),
              .dbiterra(_20[9:9]),
              .sbiterra(_20[8:8]),
              .douta(_20[7:0]) );
        assign _22 = _20[7:0];
        assign qa = _22;
        assign qb = _21;

    endmodule
    |}];
  create Ultraram Synthesis;
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

        wire [7:0] _21;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _20;
        wire [7:0] _22;
        assign _21 = _20[17:10];
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
              .dbiterrb(_20[19:19]),
              .sbiterrb(_20[18:18]),
              .doutb(_20[17:10]),
              .dbiterra(_20[9:9]),
              .sbiterra(_20[8:8]),
              .douta(_20[7:0]) );
        assign _22 = _20[7:0];
        assign qa = _22;
        assign qb = _21;

    endmodule
    |}];
  (* address width b is wider than the implied address width from port a. *)
  let test_bad_address_width build_mode =
    create
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
    ("[Signal.multiport_memory] width of write address is inconsistent"
     (port                1)
     (write_address_width 4)
     (expected            3))
    |}];
  (* data width b does not result in an even number of words in the ram *)
  let test_uneven_sizes build_mode =
    create
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
    create
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

        wire [11:0] _21;
        wire _18;
        wire vdd;
        wire _16;
        wire gnd;
        wire [19:0] _20;
        wire [3:0] _22;
        assign _21 = _20[17:6];
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
              .dbiterrb(_20[19:19]),
              .sbiterrb(_20[18:18]),
              .doutb(_20[17:6]),
              .dbiterra(_20[5:5]),
              .sbiterra(_20[4:4]),
              .douta(_20[3:0]) );
        assign _22 = _20[3:0];
        assign qa = _22;
        assign qb = _21;

    endmodule
    |}];
  require_does_raise (fun () -> test_non_pow2_scale Simulation);
  [%expect
    {|
    ("ratio between port widths must be a power of 2. (update the simulation model if non-power-of-2 scale is required)"
     (scale 3))
    |}]
;;

let%expect_test "Dual_port_ram" =
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
    Circuit.create_exn ~name:"dual_port_ram" [ output "qa" qa; output "qb" qb ]
    |> Rtl.output Verilog ~output_mode:(To_buffer (Buffer.create 1024))
  in
  require_does_not_raise (fun () -> create Distributed Synthesis);
  require_does_not_raise (fun () -> create (Blockram No_change) Synthesis);
  require_does_not_raise (fun () -> create Ultraram Synthesis);
  require_does_not_raise (fun () -> create Distributed Simulation);
  require_does_not_raise (fun () -> create (Blockram Write_before_read) Simulation);
  require_does_not_raise (fun () -> create Ultraram Simulation);
  [%expect {| |}]
;;

let%expect_test "Simple_dual_port_ram" =
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
    Circuit.create_exn ~name:"simple_dual_port_ram" [ output "q" q ]
    |> Rtl.output Verilog ~output_mode:(To_buffer (Buffer.create 1024))
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
  let create arch =
    let port port ~address_width ~data_width =
      let input name width = input (name ^ "_" ^ port) width in
      { Ram_port.address = input "address" address_width
      ; write_enable = input "write" (data_width / 8)
      ; read_enable = input "read" 1
      ; data = input "data" data_width
      }
    in
    let qa, qb =
      True_dual_port_ram.create
        ~memory_optimization:false
        ~cascade_height:Inferred
        ~arch
        ~build_mode:Simulation
        ()
        ~clock_a:(input "clock_a" 1)
        ~clock_b:(input "clock_b" 1)
        ~clear_a:(input "clear_a" 1)
        ~clear_b:(input "clear_b" 1)
        ~size:16
        ~byte_write_width:B8
        ~port_a:(port "a" ~address_width:4 ~data_width:16)
        ~port_b:(port "b" ~address_width:4 ~data_width:16)
    in
    Circuit.create_exn ~name:"true_dual_port_ram" [ output "qa" qa; output "qb" qb ]
    |> Rtl.print Verilog
  in
  create (Blockram No_change);
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

        wire _32;
        wire _33;
        wire [7:0] _31;
        wire [7:0] _29;
        reg [7:0] _34;
        wire _21;
        wire _22;
        wire [7:0] _18;
        reg [7:0] _23;
        wire [15:0] _35;
        wire _45;
        wire _46;
        wire _27;
        wire [7:0] _26;
        wire _25;
        wire [7:0] _24;
        reg [7:0] _28[0:15];
        wire [7:0] _42;
        reg [7:0] _47;
        wire _39;
        wire _40;
        wire _16;
        wire [7:0] _15;
        wire _14;
        wire [7:0] _13;
        reg [7:0] _17[0:15];
        wire [7:0] _36;
        reg [7:0] _41;
        wire [15:0] _48;
        assign _32 = ~ _27;
        assign _33 = read_b & _32;
        assign _31 = 8'b00000000;
        assign _29 = _28[address_b];
        always @(posedge clock_b) begin
            if (_33)
                _34 <= _29;
        end
        assign _21 = ~ _16;
        assign _22 = read_b & _21;
        assign _18 = _17[address_b];
        always @(posedge clock_b) begin
            if (_22)
                _23 <= _18;
        end
        assign _35 = { _23,
                       _34 };
        assign _45 = ~ _25;
        assign _46 = read_a & _45;
        assign _27 = write_b[0:0];
        assign _26 = data_b[7:0];
        assign _25 = write_a[0:0];
        assign _24 = data_a[7:0];
        always @(posedge clock_a) begin
            if (_25)
                _28[address_a] <= _24;
        end
        always @(posedge clock_b) begin
            if (_27)
                _28[address_b] <= _26;
        end
        assign _42 = _28[address_a];
        always @(posedge clock_a) begin
            if (_46)
                _47 <= _42;
        end
        assign _39 = ~ _14;
        assign _40 = read_a & _39;
        assign _16 = write_b[1:1];
        assign _15 = data_b[15:8];
        assign _14 = write_a[1:1];
        assign _13 = data_a[15:8];
        always @(posedge clock_a) begin
            if (_14)
                _17[address_a] <= _13;
        end
        always @(posedge clock_b) begin
            if (_16)
                _17[address_b] <= _15;
        end
        assign _36 = _17[address_a];
        always @(posedge clock_a) begin
            if (_40)
                _41 <= _36;
        end
        assign _48 = { _41,
                       _47 };
        assign qa = _48;
        assign qb = _35;

    endmodule
    |}]
;;

let%expect_test "byte enables with resizing" =
  let create arch =
    let port port ~address_width ~data_width =
      let input name width = input (name ^ "_" ^ port) width in
      { Ram_port.address = input "address" address_width
      ; write_enable = input "write" (data_width / 8)
      ; read_enable = input "read" 1
      ; data = input "data" data_width
      }
    in
    let qa, qb =
      True_dual_port_ram.create
        ~memory_optimization:false
        ~cascade_height:Inferred
        ~arch
        ~build_mode:Simulation
        ()
        ~clock_a:(input "clock_a" 1)
        ~clock_b:(input "clock_b" 1)
        ~clear_a:(input "clear_a" 1)
        ~clear_b:(input "clear_b" 1)
        ~size:16
        ~byte_write_width:B8
        ~port_a:(port "a" ~address_width:4 ~data_width:16)
        ~port_b:(port "b" ~address_width:5 ~data_width:8)
    in
    Circuit.create_exn ~name:"true_dual_port_ram" [ output "qa" qa; output "qb" qb ]
    |> Rtl.print Verilog
  in
  create (Blockram No_change);
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        clear_b,
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
        input clear_b;
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

        wire _44;
        wire _43;
        wire _45;
        wire [7:0] _42;
        wire [7:0] _40;
        reg [7:0] _46;
        wire _30;
        wire _29;
        wire _31;
        wire [7:0] _26;
        reg [7:0] _32;
        wire _16;
        wire _14;
        reg _17;
        wire [7:0] _47;
        wire _57;
        wire _58;
        wire _21;
        wire _23;
        wire _24;
        wire _20;
        wire [7:0] _19;
        reg [7:0] _25[0:15];
        wire [7:0] _54;
        reg [7:0] _59;
        wire _51;
        wire _52;
        wire _36;
        wire _35;
        wire _37;
        wire _38;
        wire [3:0] _18;
        wire _34;
        wire [7:0] _33;
        reg [7:0] _39[0:15];
        wire [7:0] _48;
        reg [7:0] _53;
        wire [15:0] _60;
        assign _44 = ~ _38;
        assign _43 = read_b & _37;
        assign _45 = _43 & _44;
        assign _42 = 8'b00000000;
        assign _40 = _39[_18];
        always @(posedge clock_b) begin
            if (_45)
                _46 <= _40;
        end
        assign _30 = ~ _24;
        assign _29 = read_b & _23;
        assign _31 = _29 & _30;
        assign _26 = _25[_18];
        always @(posedge clock_b) begin
            if (_31)
                _32 <= _26;
        end
        assign _16 = 1'b0;
        assign _14 = address_b[0:0];
        always @(posedge clock_b) begin
            if (clear_b)
                _17 <= _16;
            else
                if (read_b)
                    _17 <= _14;
        end
        assign _47 = _17 ? _46 : _32;
        assign _57 = ~ _20;
        assign _58 = read_a & _57;
        assign _21 = address_b[0:0];
        assign _23 = _21 == _16;
        assign _24 = write_b & _23;
        assign _20 = write_a[0:0];
        assign _19 = data_a[7:0];
        always @(posedge clock_a) begin
            if (_20)
                _25[address_a] <= _19;
        end
        always @(posedge clock_b) begin
            if (_24)
                _25[_18] <= data_b;
        end
        assign _54 = _25[address_a];
        always @(posedge clock_a) begin
            if (_58)
                _59 <= _54;
        end
        assign _51 = ~ _34;
        assign _52 = read_a & _51;
        assign _36 = 1'b1;
        assign _35 = address_b[0:0];
        assign _37 = _35 == _36;
        assign _38 = write_b & _37;
        assign _18 = address_b[4:1];
        assign _34 = write_a[1:1];
        assign _33 = data_a[15:8];
        always @(posedge clock_a) begin
            if (_34)
                _39[address_a] <= _33;
        end
        always @(posedge clock_b) begin
            if (_38)
                _39[_18] <= data_b;
        end
        assign _48 = _39[address_a];
        always @(posedge clock_a) begin
            if (_52)
                _53 <= _48;
        end
        assign _60 = { _53,
                       _59 };
        assign qa = _60;
        assign qb = _47;

    endmodule
    |}]
;;
