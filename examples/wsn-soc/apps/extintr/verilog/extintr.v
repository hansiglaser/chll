module ExtIntr (
  (* intersynth_port = "ReconfModuleIRQs_s", intersynth_conntype = "Bit" *)
  output ExtIntrOut_o,
  (* intersynth_port = "Inputs_i", intersynth_conntype = "Bit" *)
  input ExtIntrIn_i
);

assign ExtIntrOut_o = ExtIntrIn_i;

endmodule
