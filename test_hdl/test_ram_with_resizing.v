`timescale 1ns/1ns

module test_ram_with_resizing;

  reg clock = 0;
  reg clear = 1;

  wire q;
  
  always #5 clock = ~clock;
  initial #25 clear <= 0;

  rams_with_resizing the_ram_with_resizing (.clock(clock), .clear(clear), .q(q));

endmodule 