open! Import
open! Signal

let%expect_test "Rtl" =
  let create_async_fifo build_mode =
    let fifo =
      Fifo_async.create
        ~build_mode
        ()
        ~capacity:16
        ~read_clock:(input "read_clock" 1)
        ~write_clock:(input "write_clock" 1)
        ~clear:(input "clear" 1)
        ~write:(input "write" 1)
        ~d:(input "d" 32)
        ~read:(input "read" 1)
    in
    Circuit.create_exn
      ~name:"fifo"
      [ output "q" fifo.q
      ; output "full" fifo.full
      ; output "empty" fifo.empty
      ; output "nearly_full" fifo.nearly_full
      ; output "nearly_empty" fifo.nearly_empty
      ; output "used" fifo.used
      ]
    |> Rtl.print Verilog
  in
  create_async_fifo Simulation;
  [%expect
    {|
    module fifo (
        d,
        write,
        read,
        clear,
        read_clock,
        q,
        full,
        empty,
        nearly_full,
        nearly_empty,
        used
    );

        input [31:0] d;
        input write;
        input read;
        input clear;
        input read_clock;
        output [31:0] q;
        output full;
        output empty;
        output nearly_full;
        output nearly_empty;
        output [4:0] used;

        /* signal declarations */
        wire _30 = 1'b0;
        wire [4:0] _28 = 5'b00001;
        wire _29;
        reg _32;
        wire _37 = 1'b0;
        wire _36 = 1'b0;
        wire [4:0] _33 = 5'b01111;
        wire _34;
        wire _35;
        reg _38;
        wire [31:0] _64 = 32'b00000000000000000000000000000000;
        wire [31:0] _63 = 32'b00000000000000000000000000000000;
        wire [3:0] _42 = 4'b0000;
        wire [3:0] _41 = 4'b0000;
        wire [3:0] _39 = 4'b0001;
        wire [3:0] WRITE_ADDRESS_NEXT;
        reg [3:0] _43;
        wire [3:0] WRITE_ADDRESS;
        (* RAM_STYLE="block" *)
        reg [31:0] _61[0:15];
        wire vdd = 1'b1;
        wire _54 = 1'b0;
        wire [4:0] _52 = 5'b00000;
        wire [4:0] _24 = 5'b00001;
        wire [4:0] _25;
        wire [4:0] _22 = 5'b00001;
        wire [4:0] _23;
        wire [4:0] _26;
        wire [4:0] _45 = 5'b00000;
        wire [4:0] _44 = 5'b00000;
        reg [4:0] _46;
        wire [4:0] _8;
        wire _50 = 1'b0;
        wire _49 = 1'b0;
        wire [4:0] _47 = 5'b10000;
        wire _48;
        reg _51;
        wire _9;
        wire _19;
        wire WR_INT;
        wire _21;
        wire [4:0] USED_NEXT;
        wire _53;
        reg _55;
        wire _11;
        wire _17;
        wire RD_INT;
        wire [3:0] _59 = 4'b0000;
        wire [3:0] _58 = 4'b0000;
        wire [3:0] _56 = 4'b0001;
        wire [3:0] READ_ADDRESS_NEXT;
        reg [3:0] _60;
        wire [3:0] READ_ADDRESS;
        wire [31:0] _62;
        reg [31:0] _65;

        /* logic */
        assign _29 = USED_NEXT < _28;
        always @(posedge read_clock) begin
            if (clear)
                _32 <= vdd;
            else
                if (_21)
                    _32 <= _29;
        end
        assign _34 = USED_NEXT < _33;
        assign _35 = ~ _34;
        always @(posedge read_clock) begin
            if (clear)
                _38 <= _37;
            else
                if (_21)
                    _38 <= _35;
        end
        assign WRITE_ADDRESS_NEXT = WRITE_ADDRESS + _39;
        always @(posedge read_clock) begin
            if (clear)
                _43 <= _42;
            else
                if (WR_INT)
                    _43 <= WRITE_ADDRESS_NEXT;
        end
        assign WRITE_ADDRESS = _43;
        always @(posedge read_clock) begin
            if (WR_INT)
                _61[WRITE_ADDRESS] <= d;
        end
        assign _25 = _8 - _24;
        assign _23 = _8 + _22;
        assign _26 = RD_INT ? _25 : _23;
        always @(posedge read_clock) begin
            if (clear)
                _46 <= _45;
            else
                if (_21)
                    _46 <= USED_NEXT;
        end
        assign _8 = _46;
        assign _48 = USED_NEXT == _47;
        always @(posedge read_clock) begin
            if (clear)
                _51 <= _50;
            else
                if (_21)
                    _51 <= _48;
        end
        assign _9 = _51;
        assign _19 = ~ _9;
        assign WR_INT = write & _19;
        assign _21 = RD_INT ^ WR_INT;
        assign USED_NEXT = _21 ? _26 : _8;
        assign _53 = USED_NEXT == _52;
        always @(posedge read_clock) begin
            if (clear)
                _55 <= vdd;
            else
                if (_21)
                    _55 <= _53;
        end
        assign _11 = _55;
        assign _17 = ~ _11;
        assign RD_INT = read & _17;
        assign READ_ADDRESS_NEXT = READ_ADDRESS + _56;
        always @(posedge read_clock) begin
            if (clear)
                _60 <= _59;
            else
                if (RD_INT)
                    _60 <= READ_ADDRESS_NEXT;
        end
        assign READ_ADDRESS = _60;
        assign _62 = _61[READ_ADDRESS];
        always @(posedge read_clock) begin
            if (RD_INT)
                _65 <= _62;
        end

        /* aliases */

        /* output assignments */
        assign q = _65;
        assign full = _9;
        assign empty = _11;
        assign nearly_full = _38;
        assign nearly_empty = _32;
        assign used = _8;

    endmodule |}];
  create_async_fifo Synthesis;
  [%expect
    {|
    module fifo (
        read,
        read_clock,
        d,
        write,
        write_clock,
        clear,
        q,
        full,
        empty,
        nearly_full,
        nearly_empty,
        used
    );

        input read;
        input read_clock;
        input [31:0] d;
        input write;
        input write_clock;
        input clear;
        output [31:0] q;
        output full;
        output empty;
        output nearly_full;
        output nearly_empty;
        output [4:0] used;

        /* signal declarations */
        wire [4:0] _16;
        wire _17;
        wire _18;
        wire _19;
        wire _20;
        wire gnd = 1'b0;
        wire [55:0] _15;
        wire [31:0] _21;

        /* logic */
        assign _16 = _15[6:2];
        assign _17 = _15[44:44];
        assign _18 = _15[1:1];
        assign _19 = _15[43:43];
        assign _20 = _15[0:0];
        xpm_fifo_async
            #( .FIFO_MEMORY_TYPE("block"), .FIFO_WRITE_DEPTH(16), .RELATED_CLOCKS(0), .WRITE_DATA_WIDTH(32), .READ_MODE("std"), .FIFO_READ_LATENCY(1), .FULL_RESET_VALUE(0), .USE_ADV_FEATURES("0707"), .READ_DATA_WIDTH(32), .CDC_SYNC_STAGES(2), .WR_DATA_COUNT_WIDTH(5), .PROG_FULL_THRESH(0), .RD_DATA_COUNT_WIDTH(5), .PROG_EMPTY_THRESH(16), .DOUT_RESET_VALUE("0"), .ECC_MODE("no_ecc"), .WAKEUP_TIME(0) )
            the_xpm_fifo_async
            ( .sleep(gnd), .rst(clear), .wr_clk(write_clock), .wr_en(write), .din(d), .rd_clk(read_clock), .rd_en(read), .injectsbiterr(gnd), .injectdbiterr(gnd), .dbiterr(_15[55:55]), .sbiterr(_15[54:54]), .data_valid(_15[53:53]), .almost_empty(_15[52:52]), .rd_rst_busy(_15[51:51]), .underflow(_15[50:50]), .rd_data_count(_15[49:45]), .prog_empty(_15[44:44]), .empty(_15[43:43]), .dout(_15[42:11]), .wr_ack(_15[10:10]), .almost_full(_15[9:9]), .wr_rst_busy(_15[8:8]), .overflow(_15[7:7]), .wr_data_count(_15[6:2]), .prog_full(_15[1:1]), .full(_15[0:0]) );
        assign _21 = _15[42:11];

        /* aliases */

        /* output assignments */
        assign q = _21;
        assign full = _20;
        assign empty = _19;
        assign nearly_full = _18;
        assign nearly_empty = _17;
        assign used = _16;

    endmodule |}]
;;
