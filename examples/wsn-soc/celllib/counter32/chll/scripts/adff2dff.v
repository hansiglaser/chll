(* techmap_celltype = "$adff" *)
module adff2dff (CLK, ARST, D, Q);

parameter WIDTH = 1;
parameter CLK_POLARITY = 1;
parameter ARST_POLARITY = 1;
parameter ARST_VALUE = 0;

input CLK, ARST;
input [WIDTH-1:0] D;
output reg [WIDTH-1:0] Q;

wire [1023:0] _TECHMAP_DO_ = "proc";

wire _TECHMAP_FAIL_ =
    !CLK_POLARITY || ARST_POLARITY;

always @(posedge CLK)
        if (!ARST)
                Q <= ARST_VALUE;
        else
                Q <= D;

endmodule
