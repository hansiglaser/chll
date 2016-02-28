
autoidx 56

attribute \src "../../verilog/counter32.v:1"
module \Counter32

  attribute \src "../../verilog/counter32.v:30"
  wire width 33 $0\Value[32:0]

  attribute \src "../../verilog/counter32.v:45"
  wire width 33 $add$../../verilog/counter32.v:45$4_Y

  wire width 33 $procmux$20_Y

  wire width 33 $procmux$22_Y

  wire width 33 $procmux$25_Y

  attribute \src "../../verilog/counter32.v:47"
  wire width 33 $sub$../../verilog/counter32.v:47$5_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter32.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:19"
  wire width 16 output 9 \DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:21"
  wire width 16 output 10 \DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:13"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:23"
  wire output 11 \Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:15"
  wire width 16 input 7 \PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:17"
  wire width 16 input 8 \PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:9"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:7"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter32.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter32.v:28"
  wire width 33 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:25"
  wire output 12 \Zero_o

  attribute \src "../../verilog/counter32.v:45"
  cell $add $add$../../verilog/counter32.v:45$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \Value [31:0] }
    connect \B 1'1
    connect \Y $add$../../verilog/counter32.v:45$4_Y
  end

  attribute \src "../../verilog/counter32.v:55"
  cell $eq $eq$../../verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value [31:0]
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter32.v:30"
  cell $adff $procdff$39
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 33'000000000000000000000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 33
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[32:0]
    connect \Q \Value
  end

  cell $mux $procmux$20
    parameter \WIDTH 33
    connect \A $add$../../verilog/counter32.v:45$4_Y
    connect \B $sub$../../verilog/counter32.v:47$5_Y
    connect \S \Direction_i
    connect \Y $procmux$20_Y
  end

  cell $mux $procmux$22
    parameter \WIDTH 33
    connect \A \Value
    connect \B $procmux$20_Y
    connect \S \Enable_i
    connect \Y $procmux$22_Y
  end

  cell $mux $procmux$25
    parameter \WIDTH 33
    connect \A $procmux$22_Y
    connect \B { 1'0 \PresetValH_i \PresetValL_i }
    connect \S \Preset_i
    connect \Y $procmux$25_Y
  end

  cell $mux $procmux$28
    parameter \WIDTH 33
    connect \A $procmux$25_Y
    connect \B 33'000000000000000000000000000000000
    connect \S \ResetSig_i
    connect \Y $0\Value[32:0]
  end

  attribute \src "../../verilog/counter32.v:47"
  cell $sub $sub$../../verilog/counter32.v:47$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \Value [31:0] }
    connect \B 1'1
    connect \Y $sub$../../verilog/counter32.v:47$5_Y
  end

  connect \DH_o \Value [31:16]
  connect \DL_o \Value [15:0]
  connect \Overflow_o \Value [32]
end

autoidx 56

attribute \src "../../verilog/counter32_rv1.v:36"
module \Counter32_RV1_Timer

  attribute \src "../../verilog/counter32_rv1.v:47"
  wire width 32 $0\Value[31:0]

  wire width 32 $procmux$31_Y

  attribute \src "../../verilog/counter32_rv1.v:59"
  wire width 32 $sub$../../verilog/counter32_rv1.v:59$10_Y

  attribute \src "../../verilog/counter32_rv1.v:38"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter32_rv1.v:40"
  wire input 4 \Enable_i

  attribute \src "../../verilog/counter32_rv1.v:41"
  wire width 32 input 5 \PresetVal_i

  attribute \src "../../verilog/counter32_rv1.v:39"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter32_rv1.v:37"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter32_rv1.v:45"
  wire width 32 \Value

  attribute \src "../../verilog/counter32_rv1.v:42"
  wire output 6 \Zero_o

  attribute \src "../../verilog/counter32_rv1.v:64"
  cell $eq $eq$../../verilog/counter32_rv1.v:64$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter32_rv1.v:47"
  cell $adff $procdff$40
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 0
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 32
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[31:0]
    connect \Q \Value
  end

  cell $mux $procmux$31
    parameter \WIDTH 32
    connect \A \Value
    connect \B $sub$../../verilog/counter32_rv1.v:59$10_Y
    connect \S \Enable_i
    connect \Y $procmux$31_Y
  end

  cell $mux $procmux$34
    parameter \WIDTH 32
    connect \A $procmux$31_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $0\Value[31:0]
  end

  attribute \src "../../verilog/counter32_rv1.v:59"
  cell $sub $sub$../../verilog/counter32_rv1.v:59$10
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 32
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../verilog/counter32_rv1.v:59$10_Y
  end
end

attribute \src "../../verilog/counter32_rv2.v:37"
module \Counter32_RV2_Timer

  attribute \src "../../verilog/counter32_rv2.v:47"
  wire width 32 $0\Value[31:0]

  attribute \src "../../verilog/counter32_rv2.v:59"
  wire width 32 $sub$../../verilog/counter32_rv2.v:59$15_Y

  attribute \src "../../verilog/counter32_rv2.v:39"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter32_rv2.v:41"
  wire width 32 input 4 \PresetVal_i

  attribute \src "../../verilog/counter32_rv2.v:40"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter32_rv2.v:38"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter32_rv2.v:45"
  wire width 32 \Value

  attribute \src "../../verilog/counter32_rv2.v:42"
  wire output 5 \Zero_o

  attribute \src "../../verilog/counter32_rv2.v:64"
  cell $eq $eq$../../verilog/counter32_rv2.v:64$16
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter32_rv2.v:47"
  cell $adff $procdff$41
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 0
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 32
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[31:0]
    connect \Q \Value
  end

  cell $mux $procmux$37
    parameter \WIDTH 32
    connect \A $sub$../../verilog/counter32_rv2.v:59$15_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $0\Value[31:0]
  end

  attribute \src "../../verilog/counter32_rv2.v:59"
  cell $sub $sub$../../verilog/counter32_rv2.v:59$15
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 32
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../verilog/counter32_rv2.v:59$15_Y
  end
end

autoidx 56

attribute \src "../../verilog/counter32_rv2.v:1"
module \Counter32_RV2_Main

  attribute \src "../../verilog/counter32.v:30"
  wire width 33 $techmap\ThisCounter.$0\Value[32:0]

  wire width 33 $techmap\ThisCounter.$procmux$20_Y

  attribute \src "../../verilog/counter32_rv2.v:3"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter32_rv2.v:12"
  wire width 16 \DH_s

  attribute \src "../../verilog/counter32_rv2.v:13"
  wire width 16 \DL_s

  attribute \src "../../verilog/counter32_rv2.v:14"
  attribute \unused_bits "0"
  wire \Overflow_s

  attribute \src "../../verilog/counter32_rv2.v:5"
  wire width 32 input 4 \PresetVal_i

  attribute \src "../../verilog/counter32_rv2.v:4"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter32_rv2.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter32.v:5"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:19"
  wire width 16 \ThisCounter.DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:21"
  wire width 16 \ThisCounter.DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:23"
  attribute \unused_bits "0"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:15"
  wire width 16 \ThisCounter.PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:17"
  wire width 16 \ThisCounter.PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:9"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter32.v:3"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../verilog/counter32.v:28"
  wire width 33 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:25"
  wire \ThisCounter.Zero_o

  attribute \src "../../verilog/counter32_rv2.v:6"
  wire output 5 \Zero_o

  attribute \src "../../verilog/counter32.v:55"
  cell $eq $techmap\ThisCounter.$eq$../../verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [31:0]
    connect \B 0
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../verilog/counter32.v:30"
  cell $adff $techmap\ThisCounter.$procdff$39
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 33'000000000000000000000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 33
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $techmap\ThisCounter.$0\Value[32:0]
    connect \Q \ThisCounter.Value
  end

  cell $mux $techmap\ThisCounter.$procmux$25
    parameter \WIDTH 33
    connect \A $techmap\ThisCounter.$procmux$20_Y
    connect \B { 1'0 \PresetVal_i }
    connect \S \Preset_i
    connect \Y $techmap\ThisCounter.$0\Value[32:0]
  end

  attribute \src "../../verilog/counter32.v:47"
  cell $sub $techmap\ThisCounter.$sub$../../verilog/counter32.v:47$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \ThisCounter.Value [31:0] }
    connect \B 1'1
    connect \Y $techmap\ThisCounter.$procmux$20_Y
  end

  connect \Zero_o \ThisCounter.Zero_o
  connect \DH_s \ThisCounter.Value [31:16]
  connect \DL_s \ThisCounter.Value [15:0]
  connect \Overflow_s \ThisCounter.Value [32]
  connect \ThisCounter.Reset_n_i \Reset_n_i
  connect \ThisCounter.DH_o \ThisCounter.Value [31:16]
  connect \ThisCounter.Clk_i \Clk_i
  connect \ThisCounter.Preset_i \Preset_i
  connect \ThisCounter.PresetValH_i \PresetVal_i [31:16]
  connect \ThisCounter.PresetValL_i \PresetVal_i [15:0]
  connect \ThisCounter.DL_o \ThisCounter.Value [15:0]
  connect \ThisCounter.Overflow_o \ThisCounter.Value [32]
end

attribute \src "../../verilog/counter32_rv1.v:1"
module \Counter32_RV1_Main

  attribute \src "../../verilog/counter32.v:30"
  wire width 33 $techmap\ThisCounter.$0\Value[32:0]

  wire width 33 $techmap\ThisCounter.$procmux$20_Y

  wire width 33 $techmap\ThisCounter.$procmux$22_Y

  attribute \src "../../verilog/counter32_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter32_rv1.v:12"
  wire width 16 \DH_s

  attribute \src "../../verilog/counter32_rv1.v:13"
  wire width 16 \DL_s

  attribute \src "../../verilog/counter32_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../verilog/counter32_rv1.v:14"
  wire \Overflow_s

  attribute \src "../../verilog/counter32_rv1.v:6"
  wire width 32 input 5 \PresetVal_i

  attribute \src "../../verilog/counter32_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter32_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter32.v:5"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:19"
  wire width 16 \ThisCounter.DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:21"
  wire width 16 \ThisCounter.DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:11"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:23"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:15"
  wire width 16 \ThisCounter.PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter32.v:17"
  wire width 16 \ThisCounter.PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:9"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter32.v:3"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../verilog/counter32.v:28"
  wire width 33 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter32.v:25"
  wire \ThisCounter.Zero_o

  attribute \src "../../verilog/counter32_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../verilog/counter32.v:55"
  cell $eq $techmap\ThisCounter.$eq$../../verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [31:0]
    connect \B 0
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../verilog/counter32.v:30"
  cell $adff $techmap\ThisCounter.$procdff$39
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 33'000000000000000000000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 33
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $techmap\ThisCounter.$0\Value[32:0]
    connect \Q \ThisCounter.Value
  end

  cell $mux $techmap\ThisCounter.$procmux$22
    parameter \WIDTH 33
    connect \A \ThisCounter.Value
    connect \B $techmap\ThisCounter.$procmux$20_Y
    connect \S \Enable_i
    connect \Y $techmap\ThisCounter.$procmux$22_Y
  end

  cell $mux $techmap\ThisCounter.$procmux$25
    parameter \WIDTH 33
    connect \A $techmap\ThisCounter.$procmux$22_Y
    connect \B { 1'0 \PresetVal_i }
    connect \S \Preset_i
    connect \Y $techmap\ThisCounter.$0\Value[32:0]
  end

  attribute \src "../../verilog/counter32.v:47"
  cell $sub $techmap\ThisCounter.$sub$../../verilog/counter32.v:47$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \ThisCounter.Value [31:0] }
    connect \B 1'1
    connect \Y $techmap\ThisCounter.$procmux$20_Y
  end

  connect \Zero_o \ThisCounter.Zero_o
  connect \DH_s \ThisCounter.Value [31:16]
  connect \DL_s \ThisCounter.Value [15:0]
  connect \Overflow_s \ThisCounter.Value [32]
  connect \ThisCounter.Reset_n_i \Reset_n_i
  connect \ThisCounter.DH_o \ThisCounter.Value [31:16]
  connect \ThisCounter.Clk_i \Clk_i
  connect \ThisCounter.Preset_i \Preset_i
  connect \ThisCounter.Enable_i \Enable_i
  connect \ThisCounter.PresetValH_i \PresetVal_i [31:16]
  connect \ThisCounter.PresetValL_i \PresetVal_i [15:0]
  connect \ThisCounter.DL_o \ThisCounter.Value [15:0]
  connect \ThisCounter.Overflow_o \ThisCounter.Value [32]
end
