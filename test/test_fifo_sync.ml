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
        wire [4:0] _21;
        wire _29;
        wire _30;
        reg nearly_empty_0 = 1'b1;
        wire _36;
        wire [4:0] _33;
        wire _34;
        wire _35;
        reg nearly_full_0;
        wire [3:0] _41;
        wire [3:0] _39;
        wire [3:0] WRITE_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] WRITE_ADDRESS;
        wire [3:0] _7;
        (* RAM_STYLE="block" *)
        reg [31:0] _66[0:15];
        wire [4:0] _57;
        wire [4:0] _45;
        wire [4:0] _43;
        wire [4:0] _44;
        reg [4:0] USED_MINUS_1 = 5'b11111;
        wire [4:0] _8;
        wire [4:0] _48;
        reg [4:0] USED_PLUS_1 = 5'b00001;
        wire [4:0] _9;
        wire [4:0] _27;
        reg [4:0] USED;
        wire [4:0] _10;
        wire [4:0] _53;
        wire _54;
        reg full_0;
        wire _11;
        wire _12;
        wire _24;
        wire WR_INT;
        wire _26;
        wire [4:0] USED_NEXT;
        wire _58;
        wire _59;
        reg not_empty;
        wire _14;
        wire _38;
        wire _15;
        wire _22;
        wire RD_INT;
        wire [3:0] READ_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] READ_ADDRESS;
        wire [3:0] _19;
        wire [31:0] _67;
        reg [31:0] _68;
        assign vdd = 1'b1;
        assign _21 = 5'b00011;
        assign _29 = _21 < USED_NEXT;
        assign _30 = ~ _29;
        always @(posedge clock) begin
            if (clear)
                nearly_empty_0 <= vdd;
            else
                if (_26)
                    nearly_empty_0 <= _30;
        end
        assign _36 = 1'b0;
        assign _33 = 5'b01100;
        assign _34 = USED_NEXT < _33;
        assign _35 = ~ _34;
        always @(posedge clock) begin
            if (clear)
                nearly_full_0 <= _36;
            else
                if (_26)
                    nearly_full_0 <= _35;
        end
        assign _41 = 4'b0000;
        assign _39 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = _7 + _39;
        always @(posedge clock) begin
            if (clear)
                WRITE_ADDRESS <= _41;
            else
                if (WR_INT)
                    WRITE_ADDRESS <= WRITE_ADDRESS_NEXT;
        end
        assign _7 = WRITE_ADDRESS;
        always @(posedge clock) begin
            if (WR_INT)
                _66[_7] <= d;
        end
        assign _57 = 5'b00000;
        assign _45 = 5'b11111;
        assign _43 = 5'b00001;
        assign _44 = USED_NEXT - _43;
        always @(posedge clock) begin
            if (clear)
                USED_MINUS_1 <= _45;
            else
                if (_26)
                    USED_MINUS_1 <= _44;
        end
        assign _8 = USED_MINUS_1;
        assign _48 = USED_NEXT + _43;
        always @(posedge clock) begin
            if (clear)
                USED_PLUS_1 <= _43;
            else
                if (_26)
                    USED_PLUS_1 <= _48;
        end
        assign _9 = USED_PLUS_1;
        assign _27 = RD_INT ? _8 : _9;
        always @(posedge clock) begin
            if (clear)
                USED <= _57;
            else
                if (_26)
                    USED <= USED_NEXT;
        end
        assign _10 = USED;
        assign _53 = 5'b10000;
        assign _54 = USED_NEXT == _53;
        always @(posedge clock) begin
            if (clear)
                full_0 <= _36;
            else
                if (_26)
                    full_0 <= _54;
        end
        assign _11 = full_0;
        assign _12 = _11;
        assign _24 = ~ _12;
        assign WR_INT = wr & _24;
        assign _26 = RD_INT ^ WR_INT;
        assign USED_NEXT = _26 ? _27 : _10;
        assign _58 = USED_NEXT == _57;
        assign _59 = ~ _58;
        always @(posedge clock) begin
            if (clear)
                not_empty <= _36;
            else
                if (_26)
                    not_empty <= _59;
        end
        assign _14 = not_empty;
        assign _38 = ~ _14;
        assign _15 = _38;
        assign _22 = ~ _15;
        assign RD_INT = rd & _22;
        assign READ_ADDRESS_NEXT = _19 + _39;
        always @(posedge clock) begin
            if (clear)
                READ_ADDRESS <= _41;
            else
                if (RD_INT)
                    READ_ADDRESS <= READ_ADDRESS_NEXT;
        end
        assign _19 = READ_ADDRESS;
        assign _67 = _66[_19];
        always @(posedge clock) begin
            if (RD_INT)
                _68 <= _67;
        end
        assign q = _68;
        assign full = _11;
        assign empty = _38;
        assign nearly_full = nearly_full_0;
        assign nearly_empty = nearly_empty_0;
        assign used = _10;

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

        wire [4:0] _14;
        wire _15;
        wire _16;
        wire _17;
        wire _18;
        wire gnd;
        wire [55:0] _13;
        wire [31:0] _19;
        assign _14 = _13[6:2];
        assign _15 = _13[44:44];
        assign _16 = _13[1:1];
        assign _17 = _13[43:43];
        assign _18 = _13[0:0];
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
              .full(_13[0:0]),
              .prog_full(_13[1:1]),
              .wr_data_count(_13[6:2]),
              .overflow(_13[7:7]),
              .wr_rst_busy(_13[8:8]),
              .almost_full(_13[9:9]),
              .wr_ack(_13[10:10]),
              .dout(_13[42:11]),
              .empty(_13[43:43]),
              .prog_empty(_13[44:44]),
              .rd_data_count(_13[49:45]),
              .underflow(_13[50:50]),
              .rd_rst_busy(_13[51:51]),
              .almost_empty(_13[52:52]),
              .data_valid(_13[53:53]),
              .sbiterr(_13[54:54]),
              .dbiterr(_13[55:55]) );
        assign _19 = _13[42:11];
        assign q = _19;
        assign full = _18;
        assign empty = _17;
        assign nearly_full = _16;
        assign nearly_empty = _15;
        assign used = _14;

    endmodule
    |}]
;;
