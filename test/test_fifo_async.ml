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
        ~scope:(Scope.create ())
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
        always @(posedge read_clock) begin
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
        always @(posedge read_clock) begin
            if (clear)
                nearly_full_0 <= signal_const_2;
            else
                if (signal_xor)
                    nearly_full_0 <= signal_not_1;
        end
        assign signal_const_4 = 4'b0000;
        assign signal_const_5 = 4'b0001;
        assign WRITE_ADDRESS_NEXT = signal_wire + signal_const_5;
        always @(posedge read_clock) begin
            if (clear)
                WRITE_ADDRESS <= signal_const_4;
            else
                if (WR_INT)
                    WRITE_ADDRESS <= WRITE_ADDRESS_NEXT;
        end
        assign signal_wire = WRITE_ADDRESS;
        always @(posedge read_clock) begin
            if (WR_INT)
                signal_multiport_mem[signal_wire] <= d;
        end
        assign signal_const_7 = 5'b00000;
        assign signal_const_8 = 5'b11111;
        assign signal_const_9 = 5'b00001;
        assign signal_sub = USED_NEXT - signal_const_9;
        always @(posedge read_clock) begin
            if (clear)
                USED_MINUS_1 <= signal_const_8;
            else
                if (signal_xor)
                    USED_MINUS_1 <= signal_sub;
        end
        assign signal_wire_1 = USED_MINUS_1;
        assign signal_add = USED_NEXT + signal_const_9;
        always @(posedge read_clock) begin
            if (clear)
                USED_PLUS_1 <= signal_const_9;
            else
                if (signal_xor)
                    USED_PLUS_1 <= signal_add;
        end
        assign signal_wire_2 = USED_PLUS_1;
        assign signal_mux = RD_INT ? signal_wire_1 : signal_wire_2;
        always @(posedge read_clock) begin
            if (clear)
                USED_0 <= signal_const_7;
            else
                if (signal_xor)
                    USED_0 <= USED_NEXT;
        end
        assign signal_wire_3 = USED_0;
        assign signal_const_14 = 5'b10000;
        assign signal_eq = USED_NEXT == signal_const_14;
        always @(posedge read_clock) begin
            if (clear)
                full_0 <= signal_const_2;
            else
                if (signal_xor)
                    full_0 <= signal_eq;
        end
        assign signal_wire_4 = full_0;
        assign signal_wire_5 = signal_wire_4;
        assign signal_not_2 = ~ signal_wire_5;
        assign WR_INT = write & signal_not_2;
        assign signal_xor = RD_INT ^ WR_INT;
        assign USED_NEXT = signal_xor ? signal_mux : signal_wire_3;
        assign signal_eq_1 = USED_NEXT == signal_const_7;
        assign signal_not_3 = ~ signal_eq_1;
        always @(posedge read_clock) begin
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
        assign RD_INT = read & signal_not_5;
        assign READ_ADDRESS_NEXT = signal_wire_8 + signal_const_5;
        always @(posedge read_clock) begin
            if (clear)
                READ_ADDRESS <= signal_const_4;
            else
                if (RD_INT)
                    READ_ADDRESS <= READ_ADDRESS_NEXT;
        end
        assign signal_wire_8 = READ_ADDRESS;
        assign signal_mem_read_port = signal_multiport_mem[signal_wire_8];
        always @(posedge read_clock) begin
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

module With_interface_test_x = struct
  type 'a t =
    { a : 'a [@bits 8]
    ; b : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module With_interface_test_fifo = Fifo_async.With_interface (With_interface_test_x)

(* Multi-clock domain tests using the LWS evsim backend. These tests exercise the async
   FIFO with truly independent clock domains running at different rates, unlike the
   Cyclesim-based tests above which use a single shared clock. *)

module%test [@tags "runtime5-only"] Lws_evsim_tests = struct
  open Core
  open Hardcaml_lws
  module Fifo = With_interface_test_fifo

  type sim_context = Lws_context.M(Fifo.I)(Fifo.O).t

  let create_lws_evsim ~write_clock_time ~read_clock_time =
    let module L = Lws_evsim.With_interface (Fifo.I) (Fifo.O) in
    L.create
      ~config:{ Lws.Config.default with auto_label_hierarchical_ports = false }
      ~backend_specific_config:
        { clocks =
            [ { port_name = "write_clock"; time = write_clock_time; initial_delay = 0 }
            ; { port_name = "read_clock"; time = read_clock_time; initial_delay = 0 }
            ]
        ; evsim_config = Lws_evsim.Evsim.Config.default
        ; waves = true
        ; default_clock_name = Some "write_clock"
        }
      (fun scope input ->
        Fifo.hierarchical
          scope
          ~build_mode:Simulation
          ~showahead:true
          ~capacity:8
          ~nearly_empty:1
          ~nearly_full:6
          input)
  ;;

  let spawn_in_write_clock h sim_context f =
    Lws_context.spawn_in_clock_domain
      h
      sim_context
      ~clock_domain:(`Port_name Fifo.I.port_names.write_clock)
      f
  ;;

  let spawn_in_read_clock h sim_context f =
    Lws_context.spawn_in_clock_domain
      h
      sim_context
      ~clock_domain:(`Port_name Fifo.I.port_names.read_clock)
      f
  ;;

  let%expect_test "Multi-clock domain write then read, fast writer slow reader" =
    let lws = create_lws_evsim ~write_clock_time:1 ~read_clock_time:3 in
    Lws_evsim.run lws (fun h (sim_context : sim_context) ->
      let inputs = sim_context.inputs in
      let outputs = sim_context.outputs in
      (* Reset *)
      inputs.read_clear := Bits.vdd;
      inputs.write_clear := Bits.vdd;
      Lws.step h;
      inputs.read_clear := Bits.gnd;
      inputs.write_clear := Bits.gnd;
      Lws.step h;
      (* Write 5 values in the write clock domain *)
      let write_task =
        spawn_in_write_clock h sim_context (fun h ->
          for n = 1 to 5 do
            inputs.wr := Bits.vdd;
            inputs.d.a := Bits.of_int_trunc ~width:8 (n * 10);
            inputs.d.b := Bits.of_int_trunc ~width:8 (n * 11);
            Lws.step h
          done;
          inputs.wr := Bits.gnd;
          Lws.step h)
      in
      (* Read values in the read clock domain *)
      let read_task =
        spawn_in_read_clock h sim_context (fun h ->
          let received = Queue.create () in
          (* Wait a bit for data to cross the clock domain *)
          Lws.step ~n:2 h;
          for _ = 1 to 8 do
            inputs.rd := Bits.vdd;
            Lws.step h;
            let q_valid = Bits.to_bool !(outputs.after_edge.q.valid) in
            if q_valid
            then (
              let a = Bits.to_int_trunc !(outputs.after_edge.q.value.a) in
              let b = Bits.to_int_trunc !(outputs.after_edge.q.value.b) in
              Queue.enqueue received (a, b))
          done;
          inputs.rd := Bits.gnd;
          Queue.to_list received)
      in
      Lws.wait h write_task;
      let received = Lws.wait h read_task in
      printf "Received %d values:\n" (List.length received);
      List.iter received ~f:(fun (a, b) -> printf "  a=%d b=%d\n" a b);
      Lws_context.Waveform.expect sim_context ~wave_width:0 ~display_width:86);
    [%expect
      {|
      Received 4 values:
        a=20 b=22
        a=30 b=33
        a=40 b=44
        a=50 b=55
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
      │                  ││────────┬───┬───┬───┬───┬───────────────────────────────────────│
      │d$a               ││ 00     │0A │14 │1E │28 │32                                     │
      │                  ││────────┴───┴───┴───┴───┴───────────────────────────────────────│
      │                  ││────────┬───┬───┬───┬───┬───────────────────────────────────────│
      │d$b               ││ 00     │0B │16 │21 │2C │37                                     │
      │                  ││────────┴───┴───┴───┴───┴───────────────────────────────────────│
      │full              ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │nearly_full       ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │overflow          ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │q$valid           ││                                    ┌───────────────────────────│
      │                  ││────────────────────────────────────┘                           │
      │                  ││────────────────────────┬───────────────────────┬───────────┬───│
      │q$value$a         ││ 00                     │0A                     │14         │1E │
      │                  ││────────────────────────┴───────────────────────┴───────────┴───│
      │                  ││────────────────────────┬───────────────────────┬───────────┬───│
      │q$value$b         ││ 00                     │0B                     │16         │21 │
      │                  ││────────────────────────┴───────────────────────┴───────────┴───│
      │rd                ││                                    ┌───────────────────────────│
      │                  ││────────────────────────────────────┘                           │
      │read_clear        ││────┐                                                           │
      │                  ││    └───────────────────────────────────────────────────────────│
      │read_clock        ││──────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌───│
      │                  ││      └─────┘     └─────┘     └─────┘     └─────┘     └─────┘   │
      │underflow         ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │wr                ││        ┌───────────────────┐                                   │
      │                  ││────────┘                   └───────────────────────────────────│
      │write_clear       ││────┐                                                           │
      │                  ││    └───────────────────────────────────────────────────────────│
      │write_clock       ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
      └──────────────────┘└────────────────────────────────────────────────────────────────┘
      88538ed7c6dc8b4e9db040e132596101
      |}]
  ;;

  let%expect_test "Multi-clock domain write then read, slow writer fast reader" =
    let lws = create_lws_evsim ~write_clock_time:3 ~read_clock_time:1 in
    Lws_evsim.run lws (fun h (sim_context : sim_context) ->
      let inputs = sim_context.inputs in
      let outputs = sim_context.outputs in
      (* Reset *)
      inputs.read_clear := Bits.vdd;
      inputs.write_clear := Bits.vdd;
      Lws.step h;
      inputs.read_clear := Bits.gnd;
      inputs.write_clear := Bits.gnd;
      Lws.step h;
      (* Write 4 values in the write clock domain *)
      let write_task =
        spawn_in_write_clock h sim_context (fun h ->
          for n = 1 to 4 do
            inputs.wr := Bits.vdd;
            inputs.d.a := Bits.of_int_trunc ~width:8 (n * 10);
            inputs.d.b := Bits.of_int_trunc ~width:8 (n * 11);
            Lws.step h
          done;
          inputs.wr := Bits.gnd;
          Lws.step h)
      in
      (* Read values in the read clock domain *)
      let read_task =
        spawn_in_read_clock h sim_context (fun h ->
          let received = Queue.create () in
          (* Wait for data to arrive *)
          Lws.step ~n:5 h;
          for _ = 1 to 12 do
            inputs.rd := Bits.vdd;
            Lws.step h;
            let q_valid = Bits.to_bool !(outputs.after_edge.q.valid) in
            if q_valid
            then (
              let a = Bits.to_int_trunc !(outputs.after_edge.q.value.a) in
              let b = Bits.to_int_trunc !(outputs.after_edge.q.value.b) in
              Queue.enqueue received (a, b))
          done;
          inputs.rd := Bits.gnd;
          Queue.to_list received)
      in
      Lws.wait h write_task;
      let received = Lws.wait h read_task in
      printf "Received %d values:\n" (List.length received);
      List.iter received ~f:(fun (a, b) -> printf "  a=%d b=%d\n" a b);
      Lws_context.Waveform.expect sim_context ~wave_width:0 ~display_width:86);
    [%expect
      {|
      Received 3 values:
        a=20 b=22
        a=30 b=33
        a=40 b=44
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
      │                  ││────────────────────────┬───────────┬───────────┬───────────┬───│
      │d$a               ││ 00                     │0A         │14         │1E         │28 │
      │                  ││────────────────────────┴───────────┴───────────┴───────────┴───│
      │                  ││────────────────────────┬───────────┬───────────┬───────────┬───│
      │d$b               ││ 00                     │0B         │16         │21         │2C │
      │                  ││────────────────────────┴───────────┴───────────┴───────────┴───│
      │full              ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │nearly_full       ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │overflow          ││                                                                │
      │                  ││────────────────────────────────────────────────────────────────│
      │q$valid           ││                                            ┌───┐       ┌───┐   │
      │                  ││────────────────────────────────────────────┘   └───────┘   └───│
      │                  ││────────────────────────────────────────┬───────┬───┬───────┬───│
      │q$value$a         ││ 00                                     │0A     │00 │14     │00 │
      │                  ││────────────────────────────────────────┴───────┴───┴───────┴───│
      │                  ││────────────────────────────────────────┬───────┬───┬───────┬───│
      │q$value$b         ││ 00                                     │0B     │00 │16     │00 │
      │                  ││────────────────────────────────────────┴───────┴───┴───────┴───│
      │rd                ││                                            ┌───────────────────│
      │                  ││────────────────────────────────────────────┘                   │
      │read_clear        ││────────────┐                                                   │
      │                  ││            └───────────────────────────────────────────────────│
      │read_clock        ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
      │underflow         ││                                                    ┌───────────│
      │                  ││────────────────────────────────────────────────────┘           │
      │wr                ││                        ┌───────────────────────────────────────│
      │                  ││────────────────────────┘                                       │
      │write_clear       ││────────────┐                                                   │
      │                  ││            └───────────────────────────────────────────────────│
      │write_clock       ││──────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌───│
      │                  ││      └─────┘     └─────┘     └─────┘     └─────┘     └─────┘   │
      └──────────────────┘└────────────────────────────────────────────────────────────────┘
      649def9a55aa4ed600740b2c725a8663
      |}]
  ;;

  let%expect_test "Multi-clock domain overflow and underflow" =
    let lws = create_lws_evsim ~write_clock_time:1 ~read_clock_time:2 in
    Lws_evsim.run lws (fun h (sim_context : sim_context) ->
      let inputs = sim_context.inputs in
      let outputs = sim_context.outputs in
      (* Reset *)
      inputs.read_clear := Bits.vdd;
      inputs.write_clear := Bits.vdd;
      Lws.step h;
      inputs.read_clear := Bits.gnd;
      inputs.write_clear := Bits.gnd;
      Lws.step h;
      (* Overflow: fill the fifo (capacity 8), then write once more *)
      let write_task =
        spawn_in_write_clock h sim_context (fun h ->
          for n = 1 to 9 do
            inputs.wr := Bits.vdd;
            inputs.d.a := Bits.of_int_trunc ~width:8 n;
            inputs.d.b := Bits.of_int_trunc ~width:8 (n + 100);
            Lws.step h
          done;
          inputs.wr := Bits.gnd;
          Lws.step h;
          let overflow = Bits.to_bool !(outputs.after_edge.overflow) in
          printf "overflow after writing 9 to capacity-8 fifo: %b\n" overflow)
      in
      Lws.wait h write_task;
      (* Underflow: drain all entries then read once more *)
      let read_task =
        spawn_in_read_clock h sim_context (fun h ->
          for _ = 1 to 9 do
            inputs.rd := Bits.vdd;
            Lws.step h
          done;
          inputs.rd := Bits.gnd;
          Lws.step h;
          let underflow = Bits.to_bool !(outputs.after_edge.underflow) in
          printf "underflow after reading from empty fifo: %b\n" underflow)
      in
      Lws.wait h read_task);
    [%expect
      {|
      overflow after writing 9 to capacity-8 fifo: true
      underflow after reading from empty fifo: true
      |}]
  ;;
end
