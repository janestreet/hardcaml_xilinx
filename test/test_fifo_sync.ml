open! Import
open! Signal

let%expect_test "Rtl" =
  let create_sync_fifo build_mode =
    let fifo =
      Fifo_sync.create
        ~build_mode
        ()
        ~capacity:16
        ~clock:(input "clock" 1)
        ~clear:(input "clear" 1)
        ~wr:(input "wr" 1)
        ~d:(input "d" 32)
        ~rd:(input "rd" 1)
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
  create_sync_fifo Simulation;
  [%expect
    {|
    module fifo (
        d,
        wr,
        rd,
        clear,
        clock,
        q,
        full,
        empty,
        nearly_full,
        nearly_empty,
        used
    );

        input [31:0] d;
        input wr;
        input rd;
        input clear;
        input clock;
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
        always @(posedge clock) begin
            if (clear)
                _32 <= vdd;
            else
                if (_21)
                    _32 <= _29;
        end
        assign _34 = USED_NEXT < _33;
        assign _35 = ~ _34;
        always @(posedge clock) begin
            if (clear)
                _38 <= _37;
            else
                if (_21)
                    _38 <= _35;
        end
        assign WRITE_ADDRESS_NEXT = WRITE_ADDRESS + _39;
        always @(posedge clock) begin
            if (clear)
                _43 <= _42;
            else
                if (WR_INT)
                    _43 <= WRITE_ADDRESS_NEXT;
        end
        assign WRITE_ADDRESS = _43;
        always @(posedge clock) begin
            if (WR_INT)
                _61[WRITE_ADDRESS] <= d;
        end
        assign _25 = _8 - _24;
        assign _23 = _8 + _22;
        assign _26 = RD_INT ? _25 : _23;
        always @(posedge clock) begin
            if (clear)
                _46 <= _45;
            else
                if (_21)
                    _46 <= USED_NEXT;
        end
        assign _8 = _46;
        assign _48 = USED_NEXT == _47;
        always @(posedge clock) begin
            if (clear)
                _51 <= _50;
            else
                if (_21)
                    _51 <= _48;
        end
        assign _9 = _51;
        assign _19 = ~ _9;
        assign WR_INT = wr & _19;
        assign _21 = RD_INT ^ WR_INT;
        assign USED_NEXT = _21 ? _26 : _8;
        assign _53 = USED_NEXT == _52;
        always @(posedge clock) begin
            if (clear)
                _55 <= vdd;
            else
                if (_21)
                    _55 <= _53;
        end
        assign _11 = _55;
        assign _17 = ~ _11;
        assign RD_INT = rd & _17;
        assign READ_ADDRESS_NEXT = READ_ADDRESS + _56;
        always @(posedge clock) begin
            if (clear)
                _60 <= _59;
            else
                if (RD_INT)
                    _60 <= READ_ADDRESS_NEXT;
        end
        assign READ_ADDRESS = _60;
        assign _62 = _61[READ_ADDRESS];
        always @(posedge clock) begin
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
  create_sync_fifo Synthesis;
  [%expect
    {|
    module fifo (
        rd,
        d,
        wr,
        clock,
        clear,
        q,
        full,
        empty,
        nearly_full,
        nearly_empty,
        used
    );

        input rd;
        input [31:0] d;
        input wr;
        input clock;
        input clear;
        output [31:0] q;
        output full;
        output empty;
        output nearly_full;
        output nearly_empty;
        output [4:0] used;

        /* signal declarations */
        wire [4:0] _15;
        wire _16;
        wire _17;
        wire _18;
        wire _19;
        wire gnd = 1'b0;
        wire [55:0] _14;
        wire [31:0] _20;

        /* logic */
        assign _15 = _14[6:2];
        assign _16 = _14[44:44];
        assign _17 = _14[1:1];
        assign _18 = _14[43:43];
        assign _19 = _14[0:0];
        xpm_fifo_sync
            #( .FIFO_MEMORY_TYPE("block"), .FIFO_WRITE_DEPTH(16), .WRITE_DATA_WIDTH(32), .READ_MODE("std"), .FIFO_READ_LATENCY(1), .FULL_RESET_VALUE(0), .USE_ADV_FEATURES("0707"), .READ_DATA_WIDTH(32), .WR_DATA_COUNT_WIDTH(5), .PROG_FULL_THRESH(0), .RD_DATA_COUNT_WIDTH(5), .PROG_EMPTY_THRESH(16), .DOUT_RESET_VALUE("0"), .ECC_MODE("no_ecc"), .WAKEUP_TIME(0) )
            the_xpm_fifo_sync
            ( .sleep(gnd), .rst(clear), .wr_clk(clock), .wr_en(wr), .din(d), .rd_en(rd), .injectsbiterr(gnd), .injectdbiterr(gnd), .dbiterr(_14[55:55]), .sbiterr(_14[54:54]), .data_valid(_14[53:53]), .almost_empty(_14[52:52]), .rd_rst_busy(_14[51:51]), .underflow(_14[50:50]), .rd_data_count(_14[49:45]), .prog_empty(_14[44:44]), .empty(_14[43:43]), .dout(_14[42:11]), .wr_ack(_14[10:10]), .almost_full(_14[9:9]), .wr_rst_busy(_14[8:8]), .overflow(_14[7:7]), .wr_data_count(_14[6:2]), .prog_full(_14[1:1]), .full(_14[0:0]) );
        assign _20 = _14[42:11];

        /* aliases */

        /* output assignments */
        assign q = _20;
        assign full = _19;
        assign empty = _18;
        assign nearly_full = _17;
        assign nearly_empty = _16;
        assign used = _15;

    endmodule |}]
;;
