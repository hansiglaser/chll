# Generated by netlist.pas

module \SPIFSM

  wire input 0 \Reset_n_i

  wire input 1 \Clk_i

  wire input 2 \In0_i

  wire input 3 \In1_i

  wire input 4 \In2_i

  wire input 5 \In3_i

  wire input 6 \In4_i

  wire input 7 \In5_i

  wire input 8 \In6_i

  wire input 9 \In7_i

  wire output 10 \Out0_o

  wire output 11 \Out1_o

  wire output 12 \Out2_o

  wire output 13 \Out3_o

  wire output 14 \Out4_o

  wire output 15 \Out5_o

  wire output 16 \Out6_o

  wire output 17 \Out7_o

  wire output 18 \Out8_o

  wire output 19 \Out9_o

  wire output 20 \Out10_o

  wire output 21 \Out11_o

  wire output 22 \Out12_o

  wire output 23 \Out13_o

  wire output 24 \Out14_o

  wire input 25 \CfgMode_i

  wire input 26 \CfgClk_i

  wire input 27 \CfgShift_i

  wire input 28 \CfgDataIn_i

  wire output 29 \CfgDataOut_o

  wire width 8 \Input_s

  wire width 15 \Output_s

  wire \ScanClk_s

  wire \ScanDataIn_s

  wire \ScanDataOut_s

  wire \ScanEnable_s

  cell \TRFSM \TRFSM_1
    parameter \InputWidth 8
    parameter \OutputWidth 15
    parameter \StateWidth 5
    parameter \UseResetRow 0
    parameter \NumRows0 5
    parameter \NumRows1 10
    parameter \NumRows2 10
    parameter \NumRows3 5
    parameter \NumRows4 5
    parameter \NumRows5 0
    parameter \NumRows6 0
    parameter \NumRows7 0
    parameter \NumRows8 0
    parameter \NumRows9 0
    connect \Reset_n_i \Reset_n_i
    connect \Clk_i \Clk_i
    connect \Input_i \Input_s
    connect \Output_o \Output_s
    connect \CfgMode_i \CfgMode_i
    connect \CfgClk_i \CfgClk_i
    connect \CfgShift_i \CfgShift_i
    connect \CfgDataIn_i \CfgDataIn_i
    connect \CfgDataOut_o \CfgDataOut_o
    connect \ScanEnable_i \ScanEnable_s
    connect \ScanClk_i \ScanClk_s
    connect \ScanDataIn_i \ScanDataIn_s
    connect \ScanDataOut_o \ScanDataOut_s
  end

  connect \Input_s { \In7_i \In6_i \In5_i \In4_i \In3_i \In2_i \In1_i \In0_i }
  connect \Out0_o \Output_s [0]
  connect \Out1_o \Output_s [1]
  connect \Out2_o \Output_s [2]
  connect \Out3_o \Output_s [3]
  connect \Out4_o \Output_s [4]
  connect \Out5_o \Output_s [5]
  connect \Out6_o \Output_s [6]
  connect \Out7_o \Output_s [7]
  connect \Out8_o \Output_s [8]
  connect \Out9_o \Output_s [9]
  connect \Out10_o \Output_s [10]
  connect \Out11_o \Output_s [11]
  connect \Out12_o \Output_s [12]
  connect \Out13_o \Output_s [13]
  connect \Out14_o \Output_s [14]
  connect \ScanEnable_s 1'0
  connect \ScanClk_s 1'0
  connect \ScanDataIn_s 1'0
end
