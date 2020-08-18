open! Import
open Signal

let%expect_test "True_dual_port_ram" =
  let create arch =
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
        ~arch
        ()
        ~clock_a:(input "clock_a" 1)
        ~clock_b:(input "clock_b" 1)
        ~clear_a:(input "clear_a" 1)
        ~clear_b:(input "clear_b" 1)
        ~size:16
        ~port_a:(port "a" ~address_width:4 ~data_width:8)
        ~port_b:(port "b" ~address_width:4 ~data_width:8)
    in
    Circuit.create_exn ~name:"true_dual_port_ram" [ output "qa" qa; output "qb" qb ]
    |> Rtl.print Verilog
  in
  create Rtl;
  [%expect
    {|
    module true_dual_port_ram (
        read_b,
        read_a,
        write_b,
        data_b,
        clock_b,
        write_a,
        data_a,
        clock_a,
        address_b,
        address_a,
        qa,
        qb
    );

        input read_b;
        input read_a;
        input write_b;
        input [7:0] data_b;
        input clock_b;
        input write_a;
        input [7:0] data_a;
        input clock_a;
        input [3:0] address_b;
        input [3:0] address_a;
        output [7:0] qa;
        output [7:0] qb;

        /* signal declarations */
        wire _17;
        wire [7:0] _16 = 8'b00000000;
        wire [7:0] _15 = 8'b00000000;
        wire [7:0] _14;
        reg [7:0] _18;
        wire _22;
        wire [7:0] _21 = 8'b00000000;
        wire [7:0] _20 = 8'b00000000;
        reg [7:0] _13[0:15];
        wire [7:0] _19;
        reg [7:0] _23;

        /* logic */
        assign _17 = read_b | write_b;
        assign _14 = _13[address_b];
        always @(posedge clock_b) begin
            if (_17)
                _18 <= _14;
        end
        assign _22 = read_a | write_a;
        always @(posedge clock_a) begin
            if (write_a)
                _13[address_a] <= data_a;
        end
        always @(posedge clock_b) begin
            if (write_b)
                _13[address_b] <= data_b;
        end
        assign _19 = _13[address_a];
        always @(posedge clock_a) begin
            if (_22)
                _23 <= _19;
        end

        /* aliases */

        /* output assignments */
        assign qa = _23;
        assign qb = _18;

    endmodule |}];
  create Auto;
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

        /* signal declarations */
        wire [7:0] _21;
        wire _18;
        wire vdd = 1'b1;
        wire _16;
        wire gnd = 1'b0;
        wire [19:0] _20;
        wire [7:0] _22;

        /* logic */
        assign _21 = _20[17:10];
        assign _18 = write_b | read_b;
        assign _16 = write_a | read_a;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128), .MEMORY_PRIMITIVE("auto"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("false"), .WRITE_DATA_WIDTH_A(8), .READ_DATA_WIDTH_A(8), .BYTE_WRITE_WIDTH_A(8), .ADDR_WIDTH_A(4), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .WRITE_DATA_WIDTH_B(8), .READ_DATA_WIDTH_B(8), .BYTE_WRITE_WIDTH_B(8), .ADDR_WIDTH_B(4), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change") )
            the_xpm_memory_tdpram
            ( .sleep(gnd), .clka(clock_a), .rsta(clear_a), .ena(_16), .regcea(vdd), .wea(write_a), .addra(address_a), .dina(data_a), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(clock_b), .rstb(clear_b), .enb(_18), .regceb(vdd), .web(write_b), .addrb(address_b), .dinb(data_b), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_20[19:19]), .sbiterrb(_20[18:18]), .doutb(_20[17:10]), .dbiterra(_20[9:9]), .sbiterra(_20[8:8]), .douta(_20[7:0]) );
        assign _22 = _20[7:0];

        /* aliases */

        /* output assignments */
        assign qa = _22;
        assign qb = _21;

    endmodule |}];
  create Distributed;
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

        /* signal declarations */
        wire [7:0] _21;
        wire _18;
        wire vdd = 1'b1;
        wire _16;
        wire gnd = 1'b0;
        wire [19:0] _20;
        wire [7:0] _22;

        /* logic */
        assign _21 = _20[17:10];
        assign _18 = write_b | read_b;
        assign _16 = write_a | read_a;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128), .MEMORY_PRIMITIVE("distributed"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("false"), .WRITE_DATA_WIDTH_A(8), .READ_DATA_WIDTH_A(8), .BYTE_WRITE_WIDTH_A(8), .ADDR_WIDTH_A(4), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .WRITE_DATA_WIDTH_B(8), .READ_DATA_WIDTH_B(8), .BYTE_WRITE_WIDTH_B(8), .ADDR_WIDTH_B(4), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change") )
            the_xpm_memory_tdpram
            ( .sleep(gnd), .clka(clock_a), .rsta(clear_a), .ena(_16), .regcea(vdd), .wea(write_a), .addra(address_a), .dina(data_a), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(clock_b), .rstb(clear_b), .enb(_18), .regceb(vdd), .web(write_b), .addrb(address_b), .dinb(data_b), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_20[19:19]), .sbiterrb(_20[18:18]), .doutb(_20[17:10]), .dbiterra(_20[9:9]), .sbiterra(_20[8:8]), .douta(_20[7:0]) );
        assign _22 = _20[7:0];

        /* aliases */

        /* output assignments */
        assign qa = _22;
        assign qb = _21;

    endmodule |}];
  create Blockram;
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

        /* signal declarations */
        wire [7:0] _21;
        wire _18;
        wire vdd = 1'b1;
        wire _16;
        wire gnd = 1'b0;
        wire [19:0] _20;
        wire [7:0] _22;

        /* logic */
        assign _21 = _20[17:10];
        assign _18 = write_b | read_b;
        assign _16 = write_a | read_a;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128), .MEMORY_PRIMITIVE("block"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("false"), .WRITE_DATA_WIDTH_A(8), .READ_DATA_WIDTH_A(8), .BYTE_WRITE_WIDTH_A(8), .ADDR_WIDTH_A(4), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .WRITE_DATA_WIDTH_B(8), .READ_DATA_WIDTH_B(8), .BYTE_WRITE_WIDTH_B(8), .ADDR_WIDTH_B(4), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change") )
            the_xpm_memory_tdpram
            ( .sleep(gnd), .clka(clock_a), .rsta(clear_a), .ena(_16), .regcea(vdd), .wea(write_a), .addra(address_a), .dina(data_a), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(clock_b), .rstb(clear_b), .enb(_18), .regceb(vdd), .web(write_b), .addrb(address_b), .dinb(data_b), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_20[19:19]), .sbiterrb(_20[18:18]), .doutb(_20[17:10]), .dbiterra(_20[9:9]), .sbiterra(_20[8:8]), .douta(_20[7:0]) );
        assign _22 = _20[7:0];

        /* aliases */

        /* output assignments */
        assign qa = _22;
        assign qb = _21;

    endmodule |}];
  create Ultraram;
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

        /* signal declarations */
        wire [7:0] _21;
        wire _18;
        wire vdd = 1'b1;
        wire _16;
        wire gnd = 1'b0;
        wire [19:0] _20;
        wire [7:0] _22;

        /* logic */
        assign _21 = _20[17:10];
        assign _18 = write_b | read_b;
        assign _16 = write_a | read_a;
        xpm_memory_tdpram
            #( .MEMORY_SIZE(128), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("false"), .WRITE_DATA_WIDTH_A(8), .READ_DATA_WIDTH_A(8), .BYTE_WRITE_WIDTH_A(8), .ADDR_WIDTH_A(4), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .WRITE_DATA_WIDTH_B(8), .READ_DATA_WIDTH_B(8), .BYTE_WRITE_WIDTH_B(8), .ADDR_WIDTH_B(4), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change") )
            the_xpm_memory_tdpram
            ( .sleep(gnd), .clka(clock_a), .rsta(clear_a), .ena(_16), .regcea(vdd), .wea(write_a), .addra(address_a), .dina(data_a), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(clock_b), .rstb(clear_b), .enb(_18), .regceb(vdd), .web(write_b), .addrb(address_b), .dinb(data_b), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_20[19:19]), .sbiterrb(_20[18:18]), .doutb(_20[17:10]), .dbiterra(_20[9:9]), .sbiterra(_20[8:8]), .douta(_20[7:0]) );
        assign _22 = _20[7:0];

        /* aliases */

        /* output assignments */
        assign qa = _22;
        assign qb = _21;

    endmodule |}]
;;

let%expect_test "Dual_port_ram" =
  let create arch =
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
        ~arch
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
  require_does_not_raise [%here] (fun () -> create Rtl);
  require_does_not_raise [%here] (fun () -> create Auto);
  require_does_not_raise [%here] (fun () -> create Distributed);
  require_does_not_raise [%here] (fun () -> create Blockram);
  require_does_not_raise [%here] (fun () -> create Ultraram)
;;

let%expect_test "Simple_dual_port_ram" =
  let create arch =
    let address_width = 16 in
    let data_width = 32 in
    let q =
      Simple_dual_port_ram.create
        ~arch
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
  require_does_not_raise [%here] (fun () -> create Rtl);
  require_does_not_raise [%here] (fun () -> create Auto);
  require_does_not_raise [%here] (fun () -> create Distributed);
  require_does_not_raise [%here] (fun () -> create Blockram);
  require_does_not_raise [%here] (fun () -> create Ultraram)
;;
