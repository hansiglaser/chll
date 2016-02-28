module SimpleSPI (
  input         Reset_n_i,
  input         Clk_i,
  // OpenMSP430 Interface
  input  [13:0] PerAddr_i,
  input  [15:0] PerDIn_i,
  output [15:0] PerDOut_o,
  input   [1:0] PerWr_i,
  input         PerEn_i,
  output        Intr_o,
  // SPI Interface
  output        SCK_o,
  output        MOSI_o,
  input         MISO_i
);

  parameter BaseAddr = 'h180; // := 16#0180#

endmodule // SimpleSPI
