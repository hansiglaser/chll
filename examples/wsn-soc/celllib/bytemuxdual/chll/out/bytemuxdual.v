/* Generated by Yosys 0.3.0+ (git sha1 3b52121) */

(* src = "../../verilog/bytemuxdual.v:1" *)
module ByteMuxDual(A_i, B_i, S_i, Y_o);
  (* intersynth_conntype = "Byte" *)
  (* src = "../../verilog/bytemuxdual.v:3" *)
  input [7:0] A_i;
  (* intersynth_conntype = "Byte" *)
  (* src = "../../verilog/bytemuxdual.v:5" *)
  input [7:0] B_i;
  (* intersynth_conntype = "Bit" *)
  (* src = "../../verilog/bytemuxdual.v:7" *)
  input S_i;
  (* intersynth_conntype = "Byte" *)
  (* src = "../../verilog/bytemuxdual.v:9" *)
  output [7:0] Y_o;
  (* src = "../../verilog/bytemuxdual.v:12" *)
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$ternary$../../verilog/bytemuxdual.v:12$1  (
    .A(A_i),
    .B(B_i),
    .S(S_i),
    .Y(Y_o)
  );
endmodule