open! Import
open! Signal

let%expect_test "Rtl" =
  let create_sync_fifo build_mode =
    let fifo =
      Fifo_sync.create
        ~fifo_memory_type:Block
        ~build_mode
        ~nearly_empty:3
        ~nearly_full:12
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

        wire vdd;
        wire _32;
        wire [4:0] _17;
        wire _30;
        wire _31;
        reg nearly_empty_0;
        wire [4:0] _35;
        wire _36;
        wire _37;
        reg nearly_full_0;
        wire [31:0] _68;
        wire [3:0] _44;
        wire [3:0] _41;
        wire [3:0] WRITE_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] WRITE_ADDRESS;
        wire [3:0] _7;
        (* RAM_STYLE="block" *)
        reg [31:0] _65[0:15];
        wire [4:0] _54;
        wire [4:0] _26;
        wire [4:0] _27;
        wire [4:0] _25;
        wire [4:0] _28;
        reg [4:0] USED;
        wire [4:0] _8;
        wire [4:0] _49;
        wire _50;
        reg full_0;
        wire _9;
        wire _21;
        wire WR_INT;
        wire _23;
        wire [4:0] USED_NEXT;
        wire _55;
        wire _56;
        reg not_empty;
        wire _11;
        wire _18;
        wire _19;
        wire RD_INT;
        wire [3:0] READ_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] READ_ADDRESS;
        wire [3:0] _15;
        wire [31:0] _66;
        reg [31:0] _69;
        assign vdd = 1'b1;
        assign _32 = 1'b0;
        assign _17 = 5'b00011;
        assign _30 = _17 < USED_NEXT;
        assign _31 = ~ _30;
        always @(posedge clock) begin
            if (clear)
                nearly_empty_0 <= vdd;
            else
                if (_23)
                    nearly_empty_0 <= _31;
        end
        assign _35 = 5'b01100;
        assign _36 = USED_NEXT < _35;
        assign _37 = ~ _36;
        always @(posedge clock) begin
            if (clear)
                nearly_full_0 <= _32;
            else
                if (_23)
                    nearly_full_0 <= _37;
        end
        assign _68 = 32'b00000000000000000000000000000000;
        assign _44 = 4'b0000;
        assign _41 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = _7 + _41;
        always @(posedge clock) begin
            if (clear)
                WRITE_ADDRESS <= _44;
            else
                if (WR_INT)
                    WRITE_ADDRESS <= WRITE_ADDRESS_NEXT;
        end
        assign _7 = WRITE_ADDRESS;
        always @(posedge clock) begin
            if (WR_INT)
                _65[_7] <= d;
        end
        assign _54 = 5'b00000;
        assign _26 = 5'b00001;
        assign _27 = _8 - _26;
        assign _25 = _8 + _26;
        assign _28 = RD_INT ? _27 : _25;
        always @(posedge clock) begin
            if (clear)
                USED <= _54;
            else
                if (_23)
                    USED <= USED_NEXT;
        end
        assign _8 = USED;
        assign _49 = 5'b10000;
        assign _50 = USED_NEXT == _49;
        always @(posedge clock) begin
            if (clear)
                full_0 <= _32;
            else
                if (_23)
                    full_0 <= _50;
        end
        assign _9 = full_0;
        assign _21 = ~ _9;
        assign WR_INT = wr & _21;
        assign _23 = RD_INT ^ WR_INT;
        assign USED_NEXT = _23 ? _28 : _8;
        assign _55 = USED_NEXT == _54;
        assign _56 = ~ _55;
        always @(posedge clock) begin
            if (clear)
                not_empty <= _32;
            else
                if (_23)
                    not_empty <= _56;
        end
        assign _11 = not_empty;
        assign _18 = ~ _11;
        assign _19 = ~ _18;
        assign RD_INT = rd & _19;
        assign READ_ADDRESS_NEXT = _15 + _41;
        always @(posedge clock) begin
            if (clear)
                READ_ADDRESS <= _44;
            else
                if (RD_INT)
                    READ_ADDRESS <= READ_ADDRESS_NEXT;
        end
        assign _15 = READ_ADDRESS;
        assign _66 = _65[_15];
        always @(posedge clock) begin
            if (RD_INT)
                _69 <= _66;
        end
        assign q = _69;
        assign full = _9;
        assign empty = _18;
        assign nearly_full = nearly_full_0;
        assign nearly_empty = nearly_empty_0;
        assign used = _8;

    endmodule
    |}];
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

        wire [4:0] _15;
        wire _16;
        wire _17;
        wire _18;
        wire _19;
        wire gnd;
        wire [55:0] _14;
        wire [31:0] _20;
        assign _15 = _14[6:2];
        assign _16 = _14[44:44];
        assign _17 = _14[1:1];
        assign _18 = _14[43:43];
        assign _19 = _14[0:0];
        assign gnd = 1'b0;
        xpm_fifo_sync
            #( .FIFO_MEMORY_TYPE("block"),
               .FIFO_WRITE_DEPTH(16),
               .WRITE_DATA_WIDTH(32),
               .READ_MODE("std"),
               .FIFO_READ_LATENCY(1),
               .FULL_RESET_VALUE(0),
               .USE_ADV_FEATURES("0707"),
               .READ_DATA_WIDTH(32),
               .WR_DATA_COUNT_WIDTH(5),
               .PROG_FULL_THRESH(12),
               .RD_DATA_COUNT_WIDTH(5),
               .PROG_EMPTY_THRESH(3),
               .DOUT_RESET_VALUE("0"),
               .ECC_MODE("no_ecc"),
               .SIM_ASSERT_CHK(0),
               .WAKEUP_TIME(0) )
            the_xpm_fifo_sync
            ( .sleep(gnd),
              .rst(clear),
              .wr_clk(clock),
              .wr_en(wr),
              .din(d),
              .rd_en(rd),
              .injectsbiterr(gnd),
              .injectdbiterr(gnd),
              .dbiterr(_14[55:55]),
              .sbiterr(_14[54:54]),
              .data_valid(_14[53:53]),
              .almost_empty(_14[52:52]),
              .rd_rst_busy(_14[51:51]),
              .underflow(_14[50:50]),
              .rd_data_count(_14[49:45]),
              .prog_empty(_14[44:44]),
              .empty(_14[43:43]),
              .dout(_14[42:11]),
              .wr_ack(_14[10:10]),
              .almost_full(_14[9:9]),
              .wr_rst_busy(_14[8:8]),
              .overflow(_14[7:7]),
              .wr_data_count(_14[6:2]),
              .prog_full(_14[1:1]),
              .full(_14[0:0]) );
        assign _20 = _14[42:11];
        assign q = _20;
        assign full = _19;
        assign empty = _18;
        assign nearly_full = _17;
        assign nearly_empty = _16;
        assign used = _15;

    endmodule
    |}]
;;
