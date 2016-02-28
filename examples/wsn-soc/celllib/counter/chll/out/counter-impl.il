
attribute \src "../../verilog/counter.v:1"
module \Counter

  attribute \src "../../verilog/counter.v:28"
  wire width 17 $0\Value[16:0]

  attribute \src "../../verilog/counter.v:43"
  wire width 17 $add$../../verilog/counter.v:43$4_Y

  wire width 17 $procmux$22_Y

  wire width 17 $procmux$24_Y

  wire width 17 $procmux$27_Y

  attribute \src "../../verilog/counter.v:45"
  wire width 17 $sub$../../verilog/counter.v:45$5_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter.v:7"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter.v:19"
  wire width 16 output 8 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:15"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:13"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:21"
  wire output 9 \Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter.v:17"
  wire width 16 input 7 \PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:11"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:9"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter.v:5"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter.v:26"
  wire width 17 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:23"
  wire output 10 \Zero_o

  attribute \src "../../verilog/counter.v:43"
  cell $add $add$../../verilog/counter.v:43$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A { 1'0 \Value [15:0] }
    connect \B 1'1
    connect \Y $add$../../verilog/counter.v:43$4_Y
  end

  attribute \src "../../verilog/counter.v:52"
  cell $eq $eq$../../verilog/counter.v:52$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value [15:0]
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter.v:28"
  cell $adff $procdff$62
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 17'00000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 17
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[16:0]
    connect \Q \Value
  end

  cell $mux $procmux$22
    parameter \WIDTH 17
    connect \A $add$../../verilog/counter.v:43$4_Y
    connect \B $sub$../../verilog/counter.v:45$5_Y
    connect \S \Direction_i
    connect \Y $procmux$22_Y
  end

  cell $mux $procmux$24
    parameter \WIDTH 17
    connect \A \Value
    connect \B $procmux$22_Y
    connect \S \Enable_i
    connect \Y $procmux$24_Y
  end

  cell $mux $procmux$27
    parameter \WIDTH 17
    connect \A $procmux$24_Y
    connect \B { 1'0 \PresetVal_i }
    connect \S \Preset_i
    connect \Y $procmux$27_Y
  end

  cell $mux $procmux$30
    parameter \WIDTH 17
    connect \A $procmux$27_Y
    connect \B 17'00000000000000000
    connect \S \ResetSig_i
    connect \Y $0\Value[16:0]
  end

  attribute \src "../../verilog/counter.v:45"
  cell $sub $sub$../../verilog/counter.v:45$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A { 1'0 \Value [15:0] }
    connect \B 1'1
    connect \Y $sub$../../verilog/counter.v:45$5_Y
  end

  connect \D_o \Value [15:0]
  connect \Overflow_o \Value [16]
end

attribute \src "../../verilog/counter_tv1.v:1"
module \Counter_TV1

  attribute \src "../../verilog/counter_tv1.v:29"
  wire $0\Ovfl[0:0]

  attribute \src "../../verilog/counter_tv1.v:29"
  wire width 16 $0\Value[15:0]

  attribute \src "../../verilog/counter_tv1.v:47"
  wire width 17 $add$../../verilog/counter_tv1.v:47$11_Y

  wire $procmux$40_Y

  wire $procmux$42_Y

  wire $procmux$45_Y

  wire width 16 $procmux$52_Y

  wire width 16 $procmux$54_Y

  wire width 16 $procmux$57_Y

  attribute \src "../../verilog/counter_tv1.v:49"
  wire width 17 $sub$../../verilog/counter_tv1.v:49$12_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter_tv1.v:7"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter_tv1.v:19"
  wire width 16 output 8 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:15"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:13"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:21"
  wire output 9 \Overflow_o

  attribute \src "../../verilog/counter_tv1.v:27"
  wire \Ovfl

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter_tv1.v:17"
  wire width 16 input 7 \PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:11"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:9"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter_tv1.v:5"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter_tv1.v:26"
  wire width 16 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:23"
  wire output 10 \Zero_o

  attribute \src "../../verilog/counter_tv1.v:47"
  cell $add $add$../../verilog/counter_tv1.v:47$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A \Value
    connect \B 1'1
    connect \Y $add$../../verilog/counter_tv1.v:47$11_Y
  end

  attribute \src "../../verilog/counter_tv1.v:56"
  cell $eq $eq$../../verilog/counter_tv1.v:56$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter_tv1.v:29"
  cell $adff $procdff$64
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 1'0
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 1
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Ovfl[0:0]
    connect \Q \Ovfl
  end

  attribute \src "../../verilog/counter_tv1.v:29"
  cell $adff $procdff$65
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[15:0]
    connect \Q \Value
  end

  cell $mux $procmux$40
    parameter \WIDTH 1
    connect \A $add$../../verilog/counter_tv1.v:47$11_Y [16]
    connect \B $sub$../../verilog/counter_tv1.v:49$12_Y [16]
    connect \S \Direction_i
    connect \Y $procmux$40_Y
  end

  cell $mux $procmux$42
    parameter \WIDTH 1
    connect \A \Ovfl
    connect \B $procmux$40_Y
    connect \S \Enable_i
    connect \Y $procmux$42_Y
  end

  cell $mux $procmux$45
    parameter \WIDTH 1
    connect \A $procmux$42_Y
    connect \B 1'0
    connect \S \Preset_i
    connect \Y $procmux$45_Y
  end

  cell $mux $procmux$48
    parameter \WIDTH 1
    connect \A $procmux$45_Y
    connect \B 1'0
    connect \S \ResetSig_i
    connect \Y $0\Ovfl[0:0]
  end

  cell $mux $procmux$52
    parameter \WIDTH 16
    connect \A $add$../../verilog/counter_tv1.v:47$11_Y [15:0]
    connect \B $sub$../../verilog/counter_tv1.v:49$12_Y [15:0]
    connect \S \Direction_i
    connect \Y $procmux$52_Y
  end

  cell $mux $procmux$54
    parameter \WIDTH 16
    connect \A \Value
    connect \B $procmux$52_Y
    connect \S \Enable_i
    connect \Y $procmux$54_Y
  end

  cell $mux $procmux$57
    parameter \WIDTH 16
    connect \A $procmux$54_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $procmux$57_Y
  end

  cell $mux $procmux$60
    parameter \WIDTH 16
    connect \A $procmux$57_Y
    connect \B 16'0000000000000000
    connect \S \ResetSig_i
    connect \Y $0\Value[15:0]
  end

  attribute \src "../../verilog/counter_tv1.v:49"
  cell $sub $sub$../../verilog/counter_tv1.v:49$12
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../verilog/counter_tv1.v:49$12_Y
  end

  connect \D_o \Value
  connect \Overflow_o \Ovfl
end

attribute \src "../../verilog/counter_rv1.v:35"
module \Counter_RV1_Timer

  attribute \src "../../verilog/counter_rv1.v:48"
  wire width 16 $0\Value[15:0]

  wire width 16 $procmux$33_Y

  attribute \src "../../verilog/counter_rv1.v:60"
  wire width 16 $sub$../../verilog/counter_rv1.v:60$17_Y

  attribute \src "../../verilog/counter_rv1.v:37"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter_rv1.v:39"
  wire input 4 \Enable_i

  attribute \src "../../verilog/counter_rv1.v:40"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../verilog/counter_rv1.v:38"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter_rv1.v:36"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/counter_rv1.v:46"
  wire width 16 \Value

  attribute \src "../../verilog/counter_rv1.v:41"
  wire output 6 \Zero_o

  attribute \src "../../verilog/counter_rv1.v:65"
  cell $eq $eq$../../verilog/counter_rv1.v:65$18
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/counter_rv1.v:48"
  cell $adff $procdff$63
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Value[15:0]
    connect \Q \Value
  end

  cell $mux $procmux$33
    parameter \WIDTH 16
    connect \A \Value
    connect \B $sub$../../verilog/counter_rv1.v:60$17_Y
    connect \S \Enable_i
    connect \Y $procmux$33_Y
  end

  cell $mux $procmux$36
    parameter \WIDTH 16
    connect \A $procmux$33_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $0\Value[15:0]
  end

  attribute \src "../../verilog/counter_rv1.v:60"
  cell $sub $sub$../../verilog/counter_rv1.v:60$17
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 16
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../verilog/counter_rv1.v:60$17_Y
  end
end

attribute \src "../../verilog/counter_rv1.v:1"
module \Counter_RV1_Main

  attribute \src "../../verilog/counter.v:28"
  wire width 17 $techmap\ThisCounter.$0\Value[16:0]

  wire width 17 $techmap\ThisCounter.$procmux$22_Y

  wire width 17 $techmap\ThisCounter.$procmux$24_Y

  attribute \src "../../verilog/counter_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter_rv1.v:14"
  wire width 16 \D_s

  attribute \src "../../verilog/counter_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../verilog/counter_rv1.v:15"
  wire \Overflow_s

  attribute \src "../../verilog/counter_rv1.v:6"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../verilog/counter_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter.v:7"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter.v:19"
  wire width 16 \ThisCounter.D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:13"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:21"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter.v:17"
  wire width 16 \ThisCounter.PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:11"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter.v:5"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../verilog/counter.v:26"
  wire width 17 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter.v:23"
  wire \ThisCounter.Zero_o

  attribute \src "../../verilog/counter_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../verilog/counter.v:52"
  cell $eq $techmap\ThisCounter.$eq$../../verilog/counter.v:52$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [15:0]
    connect \B 16'0000000000000000
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../verilog/counter.v:28"
  cell $adff $techmap\ThisCounter.$procdff$62
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 17'00000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 17
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $techmap\ThisCounter.$0\Value[16:0]
    connect \Q \ThisCounter.Value
  end

  cell $mux $techmap\ThisCounter.$procmux$24
    parameter \WIDTH 17
    connect \A \ThisCounter.Value
    connect \B $techmap\ThisCounter.$procmux$22_Y
    connect \S \Enable_i
    connect \Y $techmap\ThisCounter.$procmux$24_Y
  end

  cell $mux $techmap\ThisCounter.$procmux$27
    parameter \WIDTH 17
    connect \A $techmap\ThisCounter.$procmux$24_Y
    connect \B { 1'0 \PresetVal_i }
    connect \S \Preset_i
    connect \Y $techmap\ThisCounter.$0\Value[16:0]
  end

  attribute \src "../../verilog/counter.v:45"
  cell $sub $techmap\ThisCounter.$sub$../../verilog/counter.v:45$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A { 1'0 \ThisCounter.Value [15:0] }
    connect \B 1'1
    connect \Y $techmap\ThisCounter.$procmux$22_Y
  end

  connect \D_s \ThisCounter.Value [15:0]
  connect \Overflow_s \ThisCounter.Value [16]
  connect \ThisCounter.Clk_i \Clk_i
  connect \ThisCounter.D_o \ThisCounter.Value [15:0]
  connect \ThisCounter.Enable_i \Enable_i
  connect \ThisCounter.Overflow_o \ThisCounter.Value [16]
  connect \ThisCounter.PresetVal_i \PresetVal_i
  connect \ThisCounter.Preset_i \Preset_i
  connect \ThisCounter.Reset_n_i \Reset_n_i
  connect \Zero_o \ThisCounter.Zero_o
end

attribute \src "../../verilog/counter_rv1.v:1"
module \Counter_RV1_TV1

  attribute \src "../../verilog/counter_tv1.v:29"
  wire width 16 $techmap\ThisCounter.$0\Value[15:0]

  attribute \unused_bits "0"
  wire $techmap\ThisCounter.$procmux$40_Y

  wire width 16 $techmap\ThisCounter.$procmux$52_Y

  wire width 16 $techmap\ThisCounter.$procmux$54_Y

  attribute \src "../../verilog/counter_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../verilog/counter_rv1.v:14"
  wire width 16 \D_s

  attribute \src "../../verilog/counter_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../verilog/counter_rv1.v:6"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../verilog/counter_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../verilog/counter_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/counter_tv1.v:7"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter_tv1.v:19"
  wire width 16 \ThisCounter.D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:13"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/counter_tv1.v:17"
  wire width 16 \ThisCounter.PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:11"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/counter_tv1.v:5"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../verilog/counter_tv1.v:26"
  wire width 16 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/counter_tv1.v:23"
  wire \ThisCounter.Zero_o

  attribute \src "../../verilog/counter_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../verilog/counter_tv1.v:56"
  cell $eq $techmap\ThisCounter.$eq$../../verilog/counter_tv1.v:56$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value
    connect \B 16'0000000000000000
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../verilog/counter_tv1.v:29"
  cell $adff $techmap\ThisCounter.$procdff$65
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $techmap\ThisCounter.$0\Value[15:0]
    connect \Q \ThisCounter.Value
  end

  cell $mux $techmap\ThisCounter.$procmux$54
    parameter \WIDTH 16
    connect \A \ThisCounter.Value
    connect \B $techmap\ThisCounter.$procmux$52_Y
    connect \S \Enable_i
    connect \Y $techmap\ThisCounter.$procmux$54_Y
  end

  cell $mux $techmap\ThisCounter.$procmux$57
    parameter \WIDTH 16
    connect \A $techmap\ThisCounter.$procmux$54_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $techmap\ThisCounter.$0\Value[15:0]
  end

  attribute \src "../../verilog/counter_tv1.v:49"
  cell $sub $techmap\ThisCounter.$sub$../../verilog/counter_tv1.v:49$12
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A \ThisCounter.Value
    connect \B 1'1
    connect \Y { $techmap\ThisCounter.$procmux$40_Y $techmap\ThisCounter.$procmux$52_Y }
  end

  connect \D_s \ThisCounter.Value
  connect \ThisCounter.Clk_i \Clk_i
  connect \ThisCounter.D_o \ThisCounter.Value
  connect \ThisCounter.Enable_i \Enable_i
  connect \ThisCounter.PresetVal_i \PresetVal_i
  connect \ThisCounter.Preset_i \Preset_i
  connect \ThisCounter.Reset_n_i \Reset_n_i
  connect \Zero_o \ThisCounter.Zero_o
end
