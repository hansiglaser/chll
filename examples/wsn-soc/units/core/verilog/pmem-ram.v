`include "openMSP430_defines.v"

module PMem (
  input               ram_rstn,       // RAM reset (low active)
  input               ram_clk,        // RAM clock
  input [`PMEM_MSB:0] ram_addr,       // RAM address
  input               ram_cen,        // RAM chip enable (low active)
  output       [15:0] ram_dout,       // RAM data output
  input        [15:0] ram_din,        // RAM data input
  input         [1:0] ram_wen         // RAM write enable (low active)
);

  ram #(
    .ADDR_MSB(`PMEM_MSB),
    .MEM_SIZE(`PMEM_SIZE)
  ) ram_0 (
    .ram_clk     (ram_clk),          // Memory clock
    .ram_addr    (ram_addr),         // Memory address
    .ram_cen     (ram_cen),          // Memory chip enable (low active)
    .ram_dout    (ram_dout),         // Memory data output
    .ram_din     (ram_din),          // Memory data input
    .ram_wen     (ram_wen)           // Memory write enable (low active)
  );

endmodule
