open! Import
open! Signal

let%expect_test "Rtl" =
  let create_async_fifo build_mode =
    let fifo =
      Fifo_async.create
        ~fifo_memory_type:Block
        ~nearly_full:12
        ~nearly_empty:3
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

        wire _31;
        wire [4:0] _17;
        wire _29;
        wire _30;
        reg _33;
        wire [4:0] _34;
        wire _35;
        wire _36;
        reg _39;
        wire [31:0] _65;
        wire [3:0] _43;
        wire [3:0] _40;
        wire [3:0] WRITE_ADDRESS_NEXT;
        reg [3:0] _44;
        wire [3:0] WRITE_ADDRESS;
        (* RAM_STYLE="block" *)
        reg [31:0] _62[0:15];
        wire vdd;
        wire [4:0] _53;
        wire [4:0] _25;
        wire [4:0] _26;
        wire [4:0] _24;
        wire [4:0] _27;
        reg [4:0] _47;
        wire [4:0] _8;
        wire [4:0] _48;
        wire _49;
        reg _52;
        wire _9;
        wire _20;
        wire WR_INT;
        wire _22;
        wire [4:0] USED_NEXT;
        wire _54;
        reg _56;
        wire _11;
        wire _18;
        wire RD_INT;
        wire [3:0] READ_ADDRESS_NEXT;
        reg [3:0] _61;
        wire [3:0] READ_ADDRESS;
        wire [31:0] _63;
        reg [31:0] _66;
        assign _31 = 1'b0;
        assign _17 = 5'b00011;
        assign _29 = _17 < USED_NEXT;
        assign _30 = ~ _29;
        always @(posedge read_clock) begin
            if (clear)
                _33 <= vdd;
            else
                if (_22)
                    _33 <= _30;
        end
        assign _34 = 5'b01100;
        assign _35 = USED_NEXT < _34;
        assign _36 = ~ _35;
        always @(posedge read_clock) begin
            if (clear)
                _39 <= _31;
            else
                if (_22)
                    _39 <= _36;
        end
        assign _65 = 32'b00000000000000000000000000000000;
        assign _43 = 4'b0000;
        assign _40 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = WRITE_ADDRESS + _40;
        always @(posedge read_clock) begin
            if (clear)
                _44 <= _43;
            else
                if (WR_INT)
                    _44 <= WRITE_ADDRESS_NEXT;
        end
        assign WRITE_ADDRESS = _44;
        always @(posedge read_clock) begin
            if (WR_INT)
                _62[WRITE_ADDRESS] <= d;
        end
        assign vdd = 1'b1;
        assign _53 = 5'b00000;
        assign _25 = 5'b00001;
        assign _26 = _8 - _25;
        assign _24 = _8 + _25;
        assign _27 = RD_INT ? _26 : _24;
        always @(posedge read_clock) begin
            if (clear)
                _47 <= _53;
            else
                if (_22)
                    _47 <= USED_NEXT;
        end
        assign _8 = _47;
        assign _48 = 5'b10000;
        assign _49 = USED_NEXT == _48;
        always @(posedge read_clock) begin
            if (clear)
                _52 <= _31;
            else
                if (_22)
                    _52 <= _49;
        end
        assign _9 = _52;
        assign _20 = ~ _9;
        assign WR_INT = write & _20;
        assign _22 = RD_INT ^ WR_INT;
        assign USED_NEXT = _22 ? _27 : _8;
        assign _54 = USED_NEXT == _53;
        always @(posedge read_clock) begin
            if (clear)
                _56 <= vdd;
            else
                if (_22)
                    _56 <= _54;
        end
        assign _11 = _56;
        assign _18 = ~ _11;
        assign RD_INT = read & _18;
        assign READ_ADDRESS_NEXT = READ_ADDRESS + _40;
        always @(posedge read_clock) begin
            if (clear)
                _61 <= _43;
            else
                if (RD_INT)
                    _61 <= READ_ADDRESS_NEXT;
        end
        assign READ_ADDRESS = _61;
        assign _63 = _62[READ_ADDRESS];
        always @(posedge read_clock) begin
            if (RD_INT)
                _66 <= _63;
        end
        assign q = _66;
        assign full = _9;
        assign empty = _11;
        assign nearly_full = _39;
        assign nearly_empty = _33;
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

        wire [4:0] _16;
        wire _17;
        wire _18;
        wire _19;
        wire _20;
        wire gnd;
        wire [55:0] _15;
        wire [31:0] _21;
        assign _16 = _15[6:2];
        assign _17 = _15[44:44];
        assign _18 = _15[1:1];
        assign _19 = _15[43:43];
        assign _20 = _15[0:0];
        assign gnd = 1'b0;
        xpm_fifo_async
            #( .FIFO_MEMORY_TYPE("block"),
               .FIFO_WRITE_DEPTH(16),
               .RELATED_CLOCKS(0),
               .WRITE_DATA_WIDTH(32),
               .READ_MODE("std"),
               .FIFO_READ_LATENCY(1),
               .FULL_RESET_VALUE(0),
               .USE_ADV_FEATURES("0707"),
               .READ_DATA_WIDTH(32),
               .CDC_SYNC_STAGES(2),
               .WR_DATA_COUNT_WIDTH(5),
               .PROG_FULL_THRESH(12),
               .RD_DATA_COUNT_WIDTH(5),
               .PROG_EMPTY_THRESH(3),
               .DOUT_RESET_VALUE("0"),
               .ECC_MODE("no_ecc"),
               .SIM_ASSERT_CHK(0),
               .WAKEUP_TIME(0) )
            the_xpm_fifo_async
            ( .sleep(gnd),
              .rst(clear),
              .wr_clk(write_clock),
              .wr_en(write),
              .din(d),
              .rd_clk(read_clock),
              .rd_en(read),
              .injectsbiterr(gnd),
              .injectdbiterr(gnd),
              .dbiterr(_15[55:55]),
              .sbiterr(_15[54:54]),
              .data_valid(_15[53:53]),
              .almost_empty(_15[52:52]),
              .rd_rst_busy(_15[51:51]),
              .underflow(_15[50:50]),
              .rd_data_count(_15[49:45]),
              .prog_empty(_15[44:44]),
              .empty(_15[43:43]),
              .dout(_15[42:11]),
              .wr_ack(_15[10:10]),
              .almost_full(_15[9:9]),
              .wr_rst_busy(_15[8:8]),
              .overflow(_15[7:7]),
              .wr_data_count(_15[6:2]),
              .prog_full(_15[1:1]),
              .full(_15[0:0]) );
        assign _21 = _15[42:11];
        assign q = _21;
        assign full = _20;
        assign empty = _19;
        assign nearly_full = _18;
        assign nearly_empty = _17;
        assign used = _16;

    endmodule |}]
;;
