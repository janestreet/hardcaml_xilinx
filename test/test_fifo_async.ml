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

        wire vdd;
        wire [4:0] _19;
        wire _28;
        wire _29;
        reg nearly_empty_0;
        wire _35;
        wire [4:0] _32;
        wire _33;
        wire _34;
        reg nearly_full_0;
        wire [3:0] _39;
        wire [3:0] _37;
        wire [3:0] WRITE_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] WRITE_ADDRESS;
        wire [3:0] _7;
        (* RAM_STYLE="block" *)
        reg [31:0] _64[0:15];
        wire [4:0] _55;
        wire [4:0] _43;
        wire [4:0] _41;
        wire [4:0] _42;
        reg [4:0] USED_MINUS_1;
        wire [4:0] _8;
        wire [4:0] _46;
        reg [4:0] USED_PLUS_1;
        wire [4:0] _9;
        wire [4:0] _26;
        reg [4:0] USED;
        wire [4:0] _10;
        wire [4:0] _51;
        wire _52;
        reg full_0;
        wire _11;
        wire _23;
        wire WR_INT;
        wire _25;
        wire [4:0] USED_NEXT;
        wire _56;
        wire _57;
        reg not_empty;
        wire _13;
        wire _20;
        wire _21;
        wire RD_INT;
        wire [3:0] READ_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] READ_ADDRESS;
        wire [3:0] _17;
        wire [31:0] _65;
        reg [31:0] _66;
        assign vdd = 1'b1;
        assign _19 = 5'b00011;
        assign _28 = _19 < USED_NEXT;
        assign _29 = ~ _28;
        always @(posedge read_clock) begin
            if (clear)
                nearly_empty_0 <= vdd;
            else
                if (_25)
                    nearly_empty_0 <= _29;
        end
        assign _35 = 1'b0;
        assign _32 = 5'b01100;
        assign _33 = USED_NEXT < _32;
        assign _34 = ~ _33;
        always @(posedge read_clock) begin
            if (clear)
                nearly_full_0 <= _35;
            else
                if (_25)
                    nearly_full_0 <= _34;
        end
        assign _39 = 4'b0000;
        assign _37 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = _7 + _37;
        always @(posedge read_clock) begin
            if (clear)
                WRITE_ADDRESS <= _39;
            else
                if (WR_INT)
                    WRITE_ADDRESS <= WRITE_ADDRESS_NEXT;
        end
        assign _7 = WRITE_ADDRESS;
        always @(posedge read_clock) begin
            if (WR_INT)
                _64[_7] <= d;
        end
        assign _55 = 5'b00000;
        assign _43 = 5'b11111;
        assign _41 = 5'b00001;
        assign _42 = USED_NEXT - _41;
        always @(posedge read_clock) begin
            if (clear)
                USED_MINUS_1 <= _43;
            else
                if (_25)
                    USED_MINUS_1 <= _42;
        end
        assign _8 = USED_MINUS_1;
        assign _46 = USED_NEXT + _41;
        always @(posedge read_clock) begin
            if (clear)
                USED_PLUS_1 <= _41;
            else
                if (_25)
                    USED_PLUS_1 <= _46;
        end
        assign _9 = USED_PLUS_1;
        assign _26 = RD_INT ? _8 : _9;
        always @(posedge read_clock) begin
            if (clear)
                USED <= _55;
            else
                if (_25)
                    USED <= USED_NEXT;
        end
        assign _10 = USED;
        assign _51 = 5'b10000;
        assign _52 = USED_NEXT == _51;
        always @(posedge read_clock) begin
            if (clear)
                full_0 <= _35;
            else
                if (_25)
                    full_0 <= _52;
        end
        assign _11 = full_0;
        assign _23 = ~ _11;
        assign WR_INT = write & _23;
        assign _25 = RD_INT ^ WR_INT;
        assign USED_NEXT = _25 ? _26 : _10;
        assign _56 = USED_NEXT == _55;
        assign _57 = ~ _56;
        always @(posedge read_clock) begin
            if (clear)
                not_empty <= _35;
            else
                if (_25)
                    not_empty <= _57;
        end
        assign _13 = not_empty;
        assign _20 = ~ _13;
        assign _21 = ~ _20;
        assign RD_INT = read & _21;
        assign READ_ADDRESS_NEXT = _17 + _37;
        always @(posedge read_clock) begin
            if (clear)
                READ_ADDRESS <= _39;
            else
                if (RD_INT)
                    READ_ADDRESS <= READ_ADDRESS_NEXT;
        end
        assign _17 = READ_ADDRESS;
        assign _65 = _64[_17];
        always @(posedge read_clock) begin
            if (RD_INT)
                _66 <= _65;
        end
        assign q = _66;
        assign full = _11;
        assign empty = _20;
        assign nearly_full = nearly_full_0;
        assign nearly_empty = nearly_empty_0;
        assign used = _10;

    endmodule
    |}];
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
