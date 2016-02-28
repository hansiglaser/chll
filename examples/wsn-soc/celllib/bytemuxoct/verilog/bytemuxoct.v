module ByteMuxOct (
  (* intersynth_conntype = "Byte" *)
  input[7:0] A_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] B_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] C_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] D_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] E_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] F_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] G_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] H_i,
  (* intersynth_conntype = "Bit" *)
  input SAB_i,
  (* intersynth_conntype = "Bit" *)
  input SC_i,
  (* intersynth_conntype = "Bit" *)
  input SD_i,
  (* intersynth_conntype = "Bit" *)
  input SE_i,
  (* intersynth_conntype = "Bit" *)
  input SF_i,
  (* intersynth_conntype = "Bit" *)
  input SG_i,
  (* intersynth_conntype = "Bit" *)
  input SH_i,
  (* intersynth_conntype = "Byte" *)
  output[7:0] Y_o
);

wire [7:0] AB      = SAB_i ? B_i : A_i;
wire [7:0] ABC     = SC_i  ? C_i : AB;
wire [7:0] ABCD    = SD_i  ? D_i : ABC;
wire [7:0] ABCDE   = SE_i  ? E_i : ABCD;
wire [7:0] ABCDEF  = SF_i  ? F_i : ABCDE;
wire [7:0] ABCDEFG = SG_i  ? G_i : ABCDEF;
assign     Y_o     = SH_i  ? H_i : ABCDEFG;

endmodule
