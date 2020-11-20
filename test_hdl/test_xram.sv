`timescale 1ns / 1ps

module test_xram_collision;

   reg        clock=0;

   reg [9:0]  address_a=0;
   reg        read_enable_a=0;
   reg        write_enable_a=0;
   reg [31:0] data_a=0;

   reg [9:0]  address_b=0;
   reg        read_enable_b=0;
   reg        write_enable_b=0;
   reg [31:0] data_b=0;

   wire [31:0] sim_drw_a;
   wire [31:0] sim_drw_b;
   wire [31:0] sim_brw_a;
   wire [31:0] sim_brw_b;
   wire [31:0] sim_bwr_a;
   wire [31:0] sim_bwr_b;
   wire [31:0] sim_bnc_a;
   wire [31:0] sim_bnc_b;
   wire [31:0] sim_unc_a;
   wire [31:0] sim_unc_b;
   wire [31:0] xpm_drw_a;
   wire [31:0] xpm_drw_b;
   wire [31:0] xpm_brw_a;
   wire [31:0] xpm_brw_b;
   wire [31:0] xpm_bwr_a;
   wire [31:0] xpm_bwr_b;
   wire [31:0] xpm_bnc_a;
   wire [31:0] xpm_bnc_b;
   wire [31:0] xpm_unc_a;
   wire [31:0] xpm_unc_b;
   wire [9:0] ok;

   xram_sim_model the_xram
     ( .clock(clock),
       .address_a(address_a),
       .data_a(data_a),
       .write_enable_a(write_enable_a),
       .read_enable_a(read_enable_a),
       .address_b(address_b),
       .data_b(data_b),
       .write_enable_b(write_enable_b),
       .read_enable_b(read_enable_b),
       .sim_drw_a(sim_drw_a),
       .sim_drw_b(sim_drw_b),
       .sim_brw_a(sim_brw_a),
       .sim_brw_b(sim_brw_b),
       .sim_bwr_a(sim_bwr_a),
       .sim_bwr_b(sim_bwr_b),
       .sim_bnc_a(sim_bnc_a),
       .sim_bnc_b(sim_bnc_b),
       .sim_unc_a(sim_unc_a),
       .sim_unc_b(sim_unc_b),
       .xpm_drw_a(xpm_drw_a),
       .xpm_drw_b(xpm_drw_b),
       .xpm_brw_a(xpm_brw_a),
       .xpm_brw_b(xpm_brw_b),
       .xpm_bwr_a(xpm_bwr_a),
       .xpm_bwr_b(xpm_bwr_b),
       .xpm_bnc_a(xpm_bnc_a),
       .xpm_bnc_b(xpm_bnc_b),
       .xpm_unc_a(xpm_unc_a),
       .xpm_unc_b(xpm_unc_b),
       .ok(ok));

   always begin
      clock <= 0; #5;
      clock <= 1; #5;
   end

   integer i;

   initial begin
      $display("Simulation starting!");

      /* Clear the first few entries in the ram */
      @(posedge clock);
      for (i=0; i<4; i=i+1) begin
         address_a <= i;
         data_a <= 255;
         write_enable_a <= 1;
         @(posedge clock);
      end
      write_enable_a <= 0;
      /* clear the output ports */
      read_enable_a <= 1;
      read_enable_b <= 1;
      address_a <= 0;
      address_b <= 0;
      @(posedge clock)
      read_enable_a <= 0;
      read_enable_b <= 0;
      @(posedge clock)

      @(posedge clock)
      /* Write on port a */
      address_a <= 1;
      address_b <= 1;
      data_a <= 8;
      data_b <= 9;
      read_enable_a <= 1;
      read_enable_b <= 1;
      write_enable_a <= 1;
      @(posedge clock);
      write_enable_a <= 0;
      @(posedge clock);
      read_enable_a <= 0;
      read_enable_b <= 0;
      @(posedge clock);
      @(posedge clock);
      /* Write on port b */
      @(posedge clock)
      address_a <= 2;
      address_b <= 2;
      data_a <= 18;
      data_b <= 19;
      read_enable_a <= 1;
      read_enable_b <= 1;
      write_enable_b <= 1;
      @(posedge clock);
      write_enable_b <= 0;
      @(posedge clock);
      read_enable_a <= 0;
      read_enable_b <= 0;
      @(posedge clock);
      @(posedge clock);

      $display("Simulation done!");
    end
endmodule
