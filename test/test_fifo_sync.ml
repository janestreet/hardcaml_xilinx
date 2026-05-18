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
        ~scope:(Scope.create ())
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

        wire signal_const;
        wire [4:0] signal_const_1;
        wire signal_lt;
        wire signal_not;
        reg nearly_empty_0 = 1'b1;
        wire signal_const_2;
        wire [4:0] signal_const_3;
        wire signal_lt_1;
        wire signal_not_1;
        reg nearly_full_0;
        wire [3:0] signal_const_4;
        wire [3:0] signal_const_5;
        wire [3:0] WRITE_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] WRITE_ADDRESS;
        wire [3:0] signal_wire;
        (* RAM_STYLE="block" *)
        reg [31:0] signal_multiport_mem[0:15];
        wire [4:0] signal_const_7;
        wire [4:0] signal_const_8;
        wire [4:0] signal_const_9;
        wire [4:0] signal_sub;
        reg [4:0] USED_MINUS_1 = 5'b11111;
        wire [4:0] signal_wire_1;
        wire [4:0] signal_add;
        reg [4:0] USED_PLUS_1 = 5'b00001;
        wire [4:0] signal_wire_2;
        wire [4:0] signal_mux;
        reg [4:0] USED_0;
        wire [4:0] signal_wire_3;
        wire [4:0] signal_const_14;
        wire signal_eq;
        reg full_0;
        wire signal_wire_4;
        wire signal_wire_5;
        wire signal_not_2;
        wire WR_INT;
        wire signal_xor;
        wire [4:0] USED_NEXT;
        wire signal_eq_1;
        wire signal_not_3;
        reg not_empty;
        wire signal_wire_6;
        wire signal_not_4;
        wire signal_wire_7;
        wire signal_not_5;
        wire RD_INT;
        wire [3:0] READ_ADDRESS_NEXT;
        (* extract_reset="FALSE" *)
        reg [3:0] READ_ADDRESS;
        wire [3:0] signal_wire_8;
        wire [31:0] signal_mem_read_port;
        reg [31:0] signal_reg;
        assign signal_const = 1'b1;
        assign signal_const_1 = 5'b00011;
        assign signal_lt = signal_const_1 < USED_NEXT;
        assign signal_not = ~ signal_lt;
        always @(posedge clock) begin
            if (clear)
                nearly_empty_0 <= signal_const;
            else
                if (signal_xor)
                    nearly_empty_0 <= signal_not;
        end
        assign signal_const_2 = 1'b0;
        assign signal_const_3 = 5'b01100;
        assign signal_lt_1 = USED_NEXT < signal_const_3;
        assign signal_not_1 = ~ signal_lt_1;
        always @(posedge clock) begin
            if (clear)
                nearly_full_0 <= signal_const_2;
            else
                if (signal_xor)
                    nearly_full_0 <= signal_not_1;
        end
        assign signal_const_4 = 4'b0000;
        assign signal_const_5 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = signal_wire + signal_const_5;
        always @(posedge clock) begin
            if (clear)
                WRITE_ADDRESS <= signal_const_4;
            else
                if (WR_INT)
                    WRITE_ADDRESS <= WRITE_ADDRESS_NEXT;
        end
        assign signal_wire = WRITE_ADDRESS;
        always @(posedge clock) begin
            if (WR_INT)
                signal_multiport_mem[signal_wire] <= d;
        end
        assign signal_const_7 = 5'b00000;
        assign signal_const_8 = 5'b11111;
        assign signal_const_9 = 5'b00001;
        assign signal_sub = USED_NEXT - signal_const_9;
        always @(posedge clock) begin
            if (clear)
                USED_MINUS_1 <= signal_const_8;
            else
                if (signal_xor)
                    USED_MINUS_1 <= signal_sub;
        end
        assign signal_wire_1 = USED_MINUS_1;
        assign signal_add = USED_NEXT + signal_const_9;
        always @(posedge clock) begin
            if (clear)
                USED_PLUS_1 <= signal_const_9;
            else
                if (signal_xor)
                    USED_PLUS_1 <= signal_add;
        end
        assign signal_wire_2 = USED_PLUS_1;
        assign signal_mux = RD_INT ? signal_wire_1 : signal_wire_2;
        always @(posedge clock) begin
            if (clear)
                USED_0 <= signal_const_7;
            else
                if (signal_xor)
                    USED_0 <= USED_NEXT;
        end
        assign signal_wire_3 = USED_0;
        assign signal_const_14 = 5'b10000;
        assign signal_eq = USED_NEXT == signal_const_14;
        always @(posedge clock) begin
            if (clear)
                full_0 <= signal_const_2;
            else
                if (signal_xor)
                    full_0 <= signal_eq;
        end
        assign signal_wire_4 = full_0;
        assign signal_wire_5 = signal_wire_4;
        assign signal_not_2 = ~ signal_wire_5;
        assign WR_INT = wr & signal_not_2;
        assign signal_xor = RD_INT ^ WR_INT;
        assign USED_NEXT = signal_xor ? signal_mux : signal_wire_3;
        assign signal_eq_1 = USED_NEXT == signal_const_7;
        assign signal_not_3 = ~ signal_eq_1;
        always @(posedge clock) begin
            if (clear)
                not_empty <= signal_const_2;
            else
                if (signal_xor)
                    not_empty <= signal_not_3;
        end
        assign signal_wire_6 = not_empty;
        assign signal_not_4 = ~ signal_wire_6;
        assign signal_wire_7 = signal_not_4;
        assign signal_not_5 = ~ signal_wire_7;
        assign RD_INT = rd & signal_not_5;
        assign READ_ADDRESS_NEXT = signal_wire_8 + signal_const_5;
        always @(posedge clock) begin
            if (clear)
                READ_ADDRESS <= signal_const_4;
            else
                if (RD_INT)
                    READ_ADDRESS <= READ_ADDRESS_NEXT;
        end
        assign signal_wire_8 = READ_ADDRESS;
        assign signal_mem_read_port = signal_multiport_mem[signal_wire_8];
        always @(posedge clock) begin
            if (RD_INT)
                signal_reg <= signal_mem_read_port;
        end
        assign q = signal_reg;
        assign full = signal_wire_4;
        assign empty = signal_not_4;
        assign nearly_full = nearly_full_0;
        assign nearly_empty = nearly_empty_0;
        assign used = signal_wire_3;

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

        wire [4:0] signal_select;
        wire signal_select_1;
        wire signal_select_2;
        wire signal_select_3;
        wire signal_select_4;
        wire gnd;
        wire [55:0] signal_inst;
        wire [31:0] signal_select_5;
        assign signal_select = signal_inst[6:2];
        assign signal_select_1 = signal_inst[44:44];
        assign signal_select_2 = signal_inst[1:1];
        assign signal_select_3 = signal_inst[43:43];
        assign signal_select_4 = signal_inst[0:0];
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
              .full(signal_inst[0:0]),
              .prog_full(signal_inst[1:1]),
              .wr_data_count(signal_inst[6:2]),
              .overflow(signal_inst[7:7]),
              .wr_rst_busy(signal_inst[8:8]),
              .almost_full(signal_inst[9:9]),
              .wr_ack(signal_inst[10:10]),
              .dout(signal_inst[42:11]),
              .empty(signal_inst[43:43]),
              .prog_empty(signal_inst[44:44]),
              .rd_data_count(signal_inst[49:45]),
              .underflow(signal_inst[50:50]),
              .rd_rst_busy(signal_inst[51:51]),
              .almost_empty(signal_inst[52:52]),
              .data_valid(signal_inst[53:53]),
              .sbiterr(signal_inst[54:54]),
              .dbiterr(signal_inst[55:55]) );
        assign signal_select_5 = signal_inst[42:11];
        assign q = signal_select_5;
        assign full = signal_select_4;
        assign empty = signal_select_3;
        assign nearly_full = signal_select_2;
        assign nearly_empty = signal_select_1;
        assign used = signal_select;

    endmodule
    |}]
;;
