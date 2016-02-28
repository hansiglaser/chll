# Cell Counter with all variants from ../../../../celllib/counter/chll/out/counter-impl.il

attribute \src "../../../../celllib/counter/verilog/counter.v:1"
attribute \extract_order 1
module \Counter

  attribute \src "../../../../celllib/counter/verilog/counter.v:28"
  wire width 17 $0\Value[16:0]

  attribute \src "../../../../celllib/counter/verilog/counter.v:43"
  wire width 17 $add$../../../../celllib/counter/verilog/counter.v:43$4_Y

  wire width 17 $procmux$22_Y

  wire width 17 $procmux$24_Y

  wire width 17 $procmux$27_Y

  attribute \src "../../../../celllib/counter/verilog/counter.v:45"
  wire width 17 $sub$../../../../celllib/counter/verilog/counter.v:45$5_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter/verilog/counter.v:7"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter.v:19"
  wire width 16 output 8 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:15"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:13"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:21"
  wire output 9 \Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter.v:17"
  wire width 16 input 7 \PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:11"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:9"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter/verilog/counter.v:5"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter/verilog/counter.v:26"
  wire width 17 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:23"
  wire output 10 \Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter.v:43"
  cell $add $add$../../../../celllib/counter/verilog/counter.v:43$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A { 1'0 \Value [15:0] }
    connect \B 1'1
    connect \Y $add$../../../../celllib/counter/verilog/counter.v:43$4_Y
  end

  attribute \src "../../../../celllib/counter/verilog/counter.v:52"
  cell $eq $eq$../../../../celllib/counter/verilog/counter.v:52$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value [15:0]
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter/verilog/counter.v:28"
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
    connect \A $add$../../../../celllib/counter/verilog/counter.v:43$4_Y
    connect \B $sub$../../../../celllib/counter/verilog/counter.v:45$5_Y
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

  attribute \src "../../../../celllib/counter/verilog/counter.v:45"
  cell $sub $sub$../../../../celllib/counter/verilog/counter.v:45$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A { 1'0 \Value [15:0] }
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter/verilog/counter.v:45$5_Y
  end

  connect \D_o \Value [15:0]
  connect \Overflow_o \Value [16]
end

attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:1"
attribute \extract_order 2
module \Counter_TV1

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
  wire $0\Ovfl[0:0]

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
  wire width 16 $0\Value[15:0]

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:47"
  wire width 17 $add$../../../../celllib/counter/verilog/counter_tv1.v:47$11_Y

  wire $procmux$40_Y

  wire $procmux$42_Y

  wire $procmux$45_Y

  wire width 16 $procmux$52_Y

  wire width 16 $procmux$54_Y

  wire width 16 $procmux$57_Y

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:49"
  wire width 17 $sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:7"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:19"
  wire width 16 output 8 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:15"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:13"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:21"
  wire output 9 \Overflow_o

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:27"
  wire \Ovfl

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:17"
  wire width 16 input 7 \PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:11"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:9"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:5"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:26"
  wire width 16 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:23"
  wire output 10 \Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:47"
  cell $add $add$../../../../celllib/counter/verilog/counter_tv1.v:47$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A \Value
    connect \B 1'1
    connect \Y $add$../../../../celllib/counter/verilog/counter_tv1.v:47$11_Y
  end

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:56"
  cell $eq $eq$../../../../celllib/counter/verilog/counter_tv1.v:56$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
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

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
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
    connect \A $add$../../../../celllib/counter/verilog/counter_tv1.v:47$11_Y [16]
    connect \B $sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12_Y [16]
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
    connect \A $add$../../../../celllib/counter/verilog/counter_tv1.v:47$11_Y [15:0]
    connect \B $sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12_Y [15:0]
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

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:49"
  cell $sub $sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12_Y
  end

  connect \D_o \Value
  connect \Overflow_o \Ovfl
end

attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:35"
attribute \extract_order 3
module \Counter_RV1_Timer

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:48"
  wire width 16 $0\Value[15:0]

  wire width 16 $procmux$33_Y

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:60"
  wire width 16 $sub$../../../../celllib/counter/verilog/counter_rv1.v:60$17_Y

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:37"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:39"
  wire input 4 \Enable_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:40"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:38"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:36"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:46"
  wire width 16 \Value

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:41"
  wire output 6 \Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:65"
  cell $eq $eq$../../../../celllib/counter/verilog/counter_rv1.v:65$18
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:48"
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
    connect \B $sub$../../../../celllib/counter/verilog/counter_rv1.v:60$17_Y
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

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:60"
  cell $sub $sub$../../../../celllib/counter/verilog/counter_rv1.v:60$17
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 16
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter/verilog/counter_rv1.v:60$17_Y
  end
end

attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:1"
attribute \extract_order 4
module \Counter_RV1_Main

  attribute \src "../../../../celllib/counter/verilog/counter.v:28"
  wire width 17 $techmap\ThisCounter.$0\Value[16:0]

  wire width 17 $techmap\ThisCounter.$procmux$22_Y

  wire width 17 $techmap\ThisCounter.$procmux$24_Y

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:14"
  wire width 16 \D_s

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:15"
  wire \Overflow_s

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:6"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter/verilog/counter.v:7"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter.v:19"
  wire width 16 \ThisCounter.D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:13"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:21"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter.v:17"
  wire width 16 \ThisCounter.PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:11"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter/verilog/counter.v:5"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../../../celllib/counter/verilog/counter.v:26"
  wire width 17 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter.v:23"
  wire \ThisCounter.Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter.v:52"
  cell $eq $techmap\ThisCounter.$eq$../../../../celllib/counter/verilog/counter.v:52$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [15:0]
    connect \B 16'0000000000000000
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../../../celllib/counter/verilog/counter.v:28"
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

  attribute \src "../../../../celllib/counter/verilog/counter.v:45"
  cell $sub $techmap\ThisCounter.$sub$../../../../celllib/counter/verilog/counter.v:45$5
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

attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:1"
attribute \extract_order 5
module \Counter_RV1_TV1

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
  wire width 16 $techmap\ThisCounter.$0\Value[15:0]

  attribute \unused_bits "0"
  wire $techmap\ThisCounter.$procmux$40_Y

  wire width 16 $techmap\ThisCounter.$procmux$52_Y

  wire width 16 $techmap\ThisCounter.$procmux$54_Y

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:14"
  wire width 16 \D_s

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:6"
  wire width 16 input 5 \PresetVal_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:7"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:19"
  wire width 16 \ThisCounter.D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:13"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:17"
  wire width 16 \ThisCounter.PresetVal_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:11"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:5"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:26"
  wire width 16 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:23"
  wire \ThisCounter.Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:56"
  cell $eq $techmap\ThisCounter.$eq$../../../../celllib/counter/verilog/counter_tv1.v:56$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value
    connect \B 16'0000000000000000
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:29"
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

  attribute \src "../../../../celllib/counter/verilog/counter_tv1.v:49"
  cell $sub $techmap\ThisCounter.$sub$../../../../celllib/counter/verilog/counter_tv1.v:49$12
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

# Cell Counter32 with all variants from ../../../../celllib/counter32/chll/out/counter32-impl.il

autoidx 56

attribute \src "../../../../celllib/counter32/verilog/counter32.v:1"
attribute \extract_order 6
module \Counter32

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
  wire width 33 $0\Value[32:0]

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:45"
  wire width 33 $add$../../../../celllib/counter32/verilog/counter32.v:45$4_Y

  wire width 33 $procmux$20_Y

  wire width 33 $procmux$22_Y

  wire width 33 $procmux$25_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:47"
  wire width 33 $sub$../../../../celllib/counter32/verilog/counter32.v:47$5_Y

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:19"
  wire width 16 output 9 \DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:21"
  wire width 16 output 10 \DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:13"
  wire input 6 \Direction_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:23"
  wire output 11 \Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:15"
  wire width 16 input 7 \PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:17"
  wire width 16 input 8 \PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:9"
  wire input 4 \Preset_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:7"
  wire input 3 \ResetSig_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:28"
  wire width 33 \Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:25"
  wire output 12 \Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:45"
  cell $add $add$../../../../celllib/counter32/verilog/counter32.v:45$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \Value [31:0] }
    connect \B 1'1
    connect \Y $add$../../../../celllib/counter32/verilog/counter32.v:45$4_Y
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:55"
  cell $eq $eq$../../../../celllib/counter32/verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value [31:0]
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
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
    connect \A $add$../../../../celllib/counter32/verilog/counter32.v:45$4_Y
    connect \B $sub$../../../../celllib/counter32/verilog/counter32.v:47$5_Y
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

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:47"
  cell $sub $sub$../../../../celllib/counter32/verilog/counter32.v:47$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 33
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 33
    connect \A { 1'0 \Value [31:0] }
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter32/verilog/counter32.v:47$5_Y
  end

  connect \DH_o \Value [31:16]
  connect \DL_o \Value [15:0]
  connect \Overflow_o \Value [32]
end

autoidx 56

attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:36"
attribute \extract_order 7
module \Counter32_RV1_Timer

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:47"
  wire width 32 $0\Value[31:0]

  wire width 32 $procmux$31_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:59"
  wire width 32 $sub$../../../../celllib/counter32/verilog/counter32_rv1.v:59$10_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:38"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:40"
  wire input 4 \Enable_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:41"
  wire width 32 input 5 \PresetVal_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:39"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:37"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:45"
  wire width 32 \Value

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:42"
  wire output 6 \Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:64"
  cell $eq $eq$../../../../celllib/counter32/verilog/counter32_rv1.v:64$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:47"
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
    connect \B $sub$../../../../celllib/counter32/verilog/counter32_rv1.v:59$10_Y
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

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:59"
  cell $sub $sub$../../../../celllib/counter32/verilog/counter32_rv1.v:59$10
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 32
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter32/verilog/counter32_rv1.v:59$10_Y
  end
end

attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:37"
attribute \extract_order 8
module \Counter32_RV2_Timer

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:47"
  wire width 32 $0\Value[31:0]

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:59"
  wire width 32 $sub$../../../../celllib/counter32/verilog/counter32_rv2.v:59$15_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:39"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:41"
  wire width 32 input 4 \PresetVal_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:40"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:38"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:45"
  wire width 32 \Value

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:42"
  wire output 5 \Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:64"
  cell $eq $eq$../../../../celllib/counter32/verilog/counter32_rv2.v:64$16
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \Value
    connect \B 0
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:47"
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
    connect \A $sub$../../../../celllib/counter32/verilog/counter32_rv2.v:59$15_Y
    connect \B \PresetVal_i
    connect \S \Preset_i
    connect \Y $0\Value[31:0]
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:59"
  cell $sub $sub$../../../../celllib/counter32/verilog/counter32_rv2.v:59$15
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 32
    connect \A \Value
    connect \B 1'1
    connect \Y $sub$../../../../celllib/counter32/verilog/counter32_rv2.v:59$15_Y
  end
end

autoidx 56

attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:1"
attribute \extract_order 9
module \Counter32_RV2_Main

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
  wire width 33 $techmap\ThisCounter.$0\Value[32:0]

  wire width 33 $techmap\ThisCounter.$procmux$20_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:3"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:12"
  wire width 16 \DH_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:13"
  wire width 16 \DL_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:14"
  attribute \unused_bits "0"
  wire \Overflow_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:5"
  wire width 32 input 4 \PresetVal_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:4"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:5"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:19"
  wire width 16 \ThisCounter.DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:21"
  wire width 16 \ThisCounter.DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:23"
  attribute \unused_bits "0"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:15"
  wire width 16 \ThisCounter.PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:17"
  wire width 16 \ThisCounter.PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:9"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:3"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:28"
  wire width 33 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:25"
  wire \ThisCounter.Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv2.v:6"
  wire output 5 \Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:55"
  cell $eq $techmap\ThisCounter.$eq$../../../../celllib/counter32/verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [31:0]
    connect \B 0
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
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

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:47"
  cell $sub $techmap\ThisCounter.$sub$../../../../celllib/counter32/verilog/counter32.v:47$5
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

attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:1"
attribute \extract_order 10
module \Counter32_RV1_Main

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
  wire width 33 $techmap\ThisCounter.$0\Value[32:0]

  wire width 33 $techmap\ThisCounter.$procmux$20_Y

  wire width 33 $techmap\ThisCounter.$procmux$22_Y

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:3"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:12"
  wire width 16 \DH_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:13"
  wire width 16 \DL_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:5"
  wire input 4 \Enable_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:14"
  wire \Overflow_s

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:6"
  wire width 32 input 5 \PresetVal_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:4"
  wire input 3 \Preset_i

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:2"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:5"
  wire \ThisCounter.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:19"
  wire width 16 \ThisCounter.DH_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:21"
  wire width 16 \ThisCounter.DL_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:11"
  wire \ThisCounter.Enable_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:23"
  wire \ThisCounter.Overflow_o

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:15"
  wire width 16 \ThisCounter.PresetValH_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:17"
  wire width 16 \ThisCounter.PresetValL_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:9"
  wire \ThisCounter.Preset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:3"
  wire \ThisCounter.Reset_n_i

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:28"
  wire width 33 \ThisCounter.Value

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/counter32/verilog/counter32.v:25"
  wire \ThisCounter.Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32_rv1.v:7"
  wire output 6 \Zero_o

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:55"
  cell $eq $techmap\ThisCounter.$eq$../../../../celllib/counter32/verilog/counter32.v:55$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \ThisCounter.Value [31:0]
    connect \B 0
    connect \Y \ThisCounter.Zero_o
  end

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:30"
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

  attribute \src "../../../../celllib/counter32/verilog/counter32.v:47"
  cell $sub $techmap\ThisCounter.$sub$../../../../celllib/counter32/verilog/counter32.v:47$5
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

# Cell AbsDiff with all variants from ../../../../celllib/absdiff/chll/out/absdiff-impl.il

attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:1"
attribute \extract_order 11
module \AbsDiff

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:3"
  wire width 16 input 1 \A_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:5"
  wire width 16 input 2 \B_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:7"
  wire width 16 output 3 \D_o

  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:10"
  wire width 17 \DiffAB

  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:11"
  wire width 16 \DiffBA

  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:12"
  cell $sub $sub$../../../../celllib/absdiff/verilog/absdiff.v:12$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B { 1'0 \B_i }
    connect \Y \DiffAB
  end

  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:13"
  cell $sub $sub$../../../../celllib/absdiff/verilog/absdiff.v:13$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \B_i
    connect \B \A_i
    connect \Y \DiffBA
  end

  attribute \src "../../../../celllib/absdiff/verilog/absdiff.v:14"
  cell $mux $ternary$../../../../celllib/absdiff/verilog/absdiff.v:14$3
    parameter \WIDTH 16
    connect \A \DiffAB [15:0]
    connect \B \DiffBA
    connect \S \DiffAB [16]
    connect \Y \D_o
  end
end

# Cell WordRegister with all variants from ../../../../celllib/wordregister/chll/out/wordregister-impl.il

attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:1"
attribute \extract_order 12
module \WordRegister

  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:14"
  wire width 16 $0\Q_o[15:0]

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:7"
  wire width 16 input 3 \D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:9"
  wire width 16 output 4 \Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:14"
  cell $adff $procdff$16
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Q_o[15:0]
    connect \Q \Q_o
  end

  cell $mux $procmux$8
    parameter \WIDTH 16
    connect \A \Q_o
    connect \B \D_i
    connect \S \Enable_i
    connect \Y $0\Q_o[15:0]
  end
end

attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:45"
attribute \extract_order 13
module \WordRegister_Mux_Direct

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:55"
  wire width 16 $0\Q_o[15:0]

  wire width 16 $procmux$11_Y

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:47"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:48"
  wire width 16 input 3 \D1_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:49"
  wire width 16 input 4 \D2_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:51"
  wire input 6 \Enable1_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:52"
  wire input 7 \Enable2_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:50"
  wire width 16 output 5 \Q_o

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:46"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:55"
  cell $adff $procdff$17
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Q_o[15:0]
    connect \Q \Q_o
  end

  cell $mux $procmux$11
    parameter \WIDTH 16
    connect \A \Q_o
    connect \B \D2_i
    connect \S \Enable2_i
    connect \Y $procmux$11_Y
  end

  cell $mux $procmux$14
    parameter \WIDTH 16
    connect \A $procmux$11_Y
    connect \B \D1_i
    connect \S \Enable1_i
    connect \Y $0\Q_o[15:0]
  end
end

attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:19"
attribute \extract_order 14
module \WordRegister_Mux_Main

  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:14"
  wire width 16 $techmap\ThisWordRegister.$0\Q_o[15:0]

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:21"
  wire input 2 \Clk_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:22"
  wire width 16 input 3 \D1_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:23"
  wire width 16 input 4 \D2_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:30"
  wire width 16 \D_s

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:25"
  wire input 6 \Enable1_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:26"
  wire input 7 \Enable2_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:29"
  wire \Enable_s

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:24"
  wire width 16 output 5 \Q_o

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:20"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:5"
  wire \ThisWordRegister.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:7"
  wire width 16 \ThisWordRegister.D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:11"
  wire \ThisWordRegister.Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:9"
  wire width 16 \ThisWordRegister.Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:3"
  wire \ThisWordRegister.Reset_n_i

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:32"
  cell $or $or$../../../../celllib/wordregister/verilog/wordregister_mux.v:32$3
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Enable1_i
    connect \B \Enable2_i
    connect \Y \Enable_s
  end

  attribute \src "../../../../celllib/wordregister/verilog/wordregister.v:14"
  cell $adff $techmap\ThisWordRegister.$procdff$16
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 16'0000000000000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 16
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $techmap\ThisWordRegister.$0\Q_o[15:0]
    connect \Q \ThisWordRegister.Q_o
  end

  cell $mux $techmap\ThisWordRegister.$procmux$8
    parameter \WIDTH 16
    connect \A \ThisWordRegister.Q_o
    connect \B \D_s
    connect \S \Enable_s
    connect \Y $techmap\ThisWordRegister.$0\Q_o[15:0]
  end

  attribute \src "../../../../celllib/wordregister/verilog/wordregister_mux.v:33"
  cell $mux $ternary$../../../../celllib/wordregister/verilog/wordregister_mux.v:33$4
    parameter \WIDTH 16
    connect \A \D2_i
    connect \B \D1_i
    connect \S \Enable1_i
    connect \Y \D_s
  end

  connect \Q_o \ThisWordRegister.Q_o
  connect \ThisWordRegister.Clk_i \Clk_i
  connect \ThisWordRegister.D_i \D_s
  connect \ThisWordRegister.Enable_i \Enable_s
  connect \ThisWordRegister.Reset_n_i \Reset_n_i
end

# Cell ByteRegister with all variants from ../../../../celllib/byteregister/chll/out/byteregister-impl.il

attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:1"
attribute \extract_order 15
module \ByteRegister

  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:14"
  wire width 8 $0\Q_o[7:0]

  attribute \intersynth_port "Clk_i"
  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:7"
  wire width 8 input 3 \D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:9"
  wire width 8 output 4 \Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../../../celllib/byteregister/verilog/byteregister.v:14"
  cell $adff $procdff$6
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 8'00000000
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 8
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D $0\Q_o[7:0]
    connect \Q \Q_o
  end

  cell $mux $procmux$4
    parameter \WIDTH 8
    connect \A \Q_o
    connect \B \D_i
    connect \S \Enable_i
    connect \Y $0\Q_o[7:0]
  end
end

# Cell AddSubCmp with all variants from ../../../../celllib/addsubcmp/chll/out/addsubcmp-impl.il

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:1"
attribute \extract_order 16
module \AddSubCmp

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  wire width 17 $add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:42"
  wire $eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$11_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:42"
  wire $ne$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$12_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:30"
  wire width 16 $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:31"
  wire $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:31$5_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:23"
  wire width 17 \A

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:7"
  wire width 16 input 2 \A_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:5"
  wire input 1 \AddOrSub_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:24"
  wire width 17 \B

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:9"
  wire width 16 input 3 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:26"
  wire \Carry

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:13"
  wire input 5 \Carry_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:15"
  wire output 6 \Carry_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:27"
  wire width 16 \D

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:11"
  wire width 16 output 4 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:21"
  wire output 9 \Overflow_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:25"
  wire width 17 \Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:19"
  wire output 8 \Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:17"
  wire output 7 \Zero_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B \B
    connect \Y $add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
    connect \B \Carry
    connect \Y { \Carry_o \D }
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:39"
  cell $eq $eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \D
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:42"
  cell $eq $eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \A_i [15]
    connect \B \B [15]
    connect \Y $eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$11_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:42"
  cell $logic_and $logic_and$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$11_Y
    connect \B $ne$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$12_Y
    connect \Y \Overflow_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:42"
  cell $ne $ne$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$12
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \D [15]
    connect \B \A_i [15]
    connect \Y $ne$../../../../celllib/addsubcmp/verilog/addsubcmp.v:42$12_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:30"
  cell $not $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \B_i
    connect \Y $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:31"
  cell $not $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:31$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Carry_i
    connect \Y $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:31$5_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:30"
  cell $mux $ternary$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$3
    parameter \WIDTH 17
    connect \A { 1'0 \B_i }
    connect \B { 1'0 $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2_Y }
    connect \S \AddOrSub_i
    connect \Y \B
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:31"
  cell $mux $ternary$../../../../celllib/addsubcmp/verilog/addsubcmp.v:31$6
    parameter \WIDTH 1
    connect \A \Carry_i
    connect \B $not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:31$5_Y
    connect \S \AddOrSub_i
    connect \Y \Carry
  end

  connect \A { 1'0 \A_i }
  connect \D_o \D
  connect \Result { \Carry_o \D }
  connect \Sign_o \D [15]
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:26"
attribute \extract_order 17
module \AddSubCmp_Add_Direct

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:27"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:28"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:29"
  wire width 16 output 3 \D_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:32"
  cell $add $add$../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:32$23
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \A_i
    connect \B \B_i
    connect \Y \D_o
  end
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:29"
attribute \extract_order 18
module \AddSubCmp_Greater_Direct

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:30"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:31"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:32"
  wire output 3 \Greater_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:35"
  cell $gt $gt$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:35$17
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \A_i
    connect \B \B_i
    connect \Y \Greater_o
  end
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:29"
attribute \extract_order 19
module \AddSubCmp_Less_Direct

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:30"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:31"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:32"
  wire output 3 \Less_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:35"
  cell $lt $lt$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:35$21
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \A_i
    connect \B \B_i
    connect \Y \Less_o
  end
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:1"
attribute \extract_order 20
module \AddSubCmp_Add_Main

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:7"
  attribute \unused_bits "0"
  wire \Carry_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:4"
  wire width 16 output 3 \D_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_add.v:9"
  wire \Sign_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:15"
  attribute \unused_bits "0"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B { 1'0 \B_i }
    connect \Y $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
    connect \B 1'0
    connect \Y { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  end

  connect \Carry_s \ThisAddSubCmp.Carry_o
  connect \D_o \ThisAddSubCmp.D
  connect \Sign_s \ThisAddSubCmp.D [15]
  connect \ThisAddSubCmp.A { 1'0 \A_i }
  connect \ThisAddSubCmp.A_i \A_i
  connect \ThisAddSubCmp.B { 1'0 \B_i }
  connect \ThisAddSubCmp.B_i \B_i
  connect \ThisAddSubCmp.D_o \ThisAddSubCmp.D
  connect \ThisAddSubCmp.Result { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  connect \ThisAddSubCmp.Sign_o \ThisAddSubCmp.D [15]
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:1"
attribute \extract_order 21
module \AddSubCmp_Greater_Main

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25"
  wire $not$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25$15_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:8"
  wire \Carry_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:7"
  wire width 16 \D_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:4"
  wire output 3 \Greater_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:10"
  wire \Sign_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:15"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:17"
  wire \ThisAddSubCmp.Zero_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:9"
  wire \Zero_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25"
  cell $and $and$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25$16
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Carry_o
    connect \B $not$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25$15_Y
    connect \Y \Greater_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25"
  cell $not $not$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25$15
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Zero_o
    connect \Y $not$../../../../celllib/addsubcmp/verilog/addsubcmp_greater.v:25$15_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B { 1'0 \ThisAddSubCmp.B [15:0] }
    connect \Y $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
    connect \B 1'1
    connect \Y { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:39"
  cell $eq $techmap\ThisAddSubCmp.$eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.D
    connect \B 16'0000000000000000
    connect \Y \ThisAddSubCmp.Zero_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:30"
  cell $not $techmap\ThisAddSubCmp.$not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \B_i
    connect \Y \ThisAddSubCmp.B [15:0]
  end

  connect \Carry_s \ThisAddSubCmp.Carry_o
  connect \D_s \ThisAddSubCmp.D
  connect \Sign_s \ThisAddSubCmp.D [15]
  connect \ThisAddSubCmp.A { 1'0 \A_i }
  connect \ThisAddSubCmp.A_i \A_i
  connect \ThisAddSubCmp.B [16] 1'0
  connect \ThisAddSubCmp.B_i \B_i
  connect \ThisAddSubCmp.D_o \ThisAddSubCmp.D
  connect \ThisAddSubCmp.Result { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  connect \ThisAddSubCmp.Sign_o \ThisAddSubCmp.D [15]
  connect \Zero_s \ThisAddSubCmp.Zero_o
end

attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:1"
attribute \extract_order 22
module \AddSubCmp_Less_Main

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25"
  wire $not$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25$19_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:8"
  wire \Carry_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:7"
  wire width 16 \D_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:4"
  wire output 3 \Less_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:10"
  wire \Sign_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:15"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:17"
  wire \ThisAddSubCmp.Zero_o

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:9"
  wire \Zero_s

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25"
  cell $and $and$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25$20
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Carry_o
    connect \B $not$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25$19_Y
    connect \Y \Less_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25"
  cell $not $not$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25$19
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Zero_o
    connect \Y $not$../../../../celllib/addsubcmp/verilog/addsubcmp_less.v:25$19_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \B_i }
    connect \B { 1'0 \ThisAddSubCmp.B [15:0] }
    connect \Y $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../../../celllib/addsubcmp/verilog/addsubcmp.v:33$7_Y
    connect \B 1'1
    connect \Y { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:39"
  cell $eq $techmap\ThisAddSubCmp.$eq$../../../../celllib/addsubcmp/verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.D
    connect \B 16'0000000000000000
    connect \Y \ThisAddSubCmp.Zero_o
  end

  attribute \src "../../../../celllib/addsubcmp/verilog/addsubcmp.v:30"
  cell $not $techmap\ThisAddSubCmp.$not$../../../../celllib/addsubcmp/verilog/addsubcmp.v:30$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \A_i
    connect \Y \ThisAddSubCmp.B [15:0]
  end

  connect \Carry_s \ThisAddSubCmp.Carry_o
  connect \D_s \ThisAddSubCmp.D
  connect \Sign_s \ThisAddSubCmp.D [15]
  connect \ThisAddSubCmp.A { 1'0 \B_i }
  connect \ThisAddSubCmp.A_i \B_i
  connect \ThisAddSubCmp.B [16] 1'0
  connect \ThisAddSubCmp.B_i \A_i
  connect \ThisAddSubCmp.D_o \ThisAddSubCmp.D
  connect \ThisAddSubCmp.Result { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  connect \ThisAddSubCmp.Sign_o \ThisAddSubCmp.D [15]
  connect \Zero_s \ThisAddSubCmp.Zero_o
end

# Cell ByteMuxOct with all variants from ../../../../celllib/bytemuxoct/chll/out/bytemuxoct-impl.il

attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:1"
attribute \extract_order 23
module \ByteMuxOct

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:36"
  wire width 8 \AB

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:37"
  wire width 8 \ABC

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:38"
  wire width 8 \ABCD

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:39"
  wire width 8 \ABCDE

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:40"
  wire width 8 \ABCDEF

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:41"
  wire width 8 \ABCDEFG

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:3"
  wire width 8 input 1 \A_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:5"
  wire width 8 input 2 \B_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:7"
  wire width 8 input 3 \C_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:9"
  wire width 8 input 4 \D_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:11"
  wire width 8 input 5 \E_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:13"
  wire width 8 input 6 \F_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:15"
  wire width 8 input 7 \G_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:17"
  wire width 8 input 8 \H_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:19"
  wire input 9 \SAB_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:21"
  wire input 10 \SC_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:23"
  wire input 11 \SD_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:25"
  wire input 12 \SE_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:27"
  wire input 13 \SF_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:29"
  wire input 14 \SG_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:31"
  wire input 15 \SH_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:33"
  wire width 8 output 16 \Y_o

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:36"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:36$1
    parameter \WIDTH 8
    connect \A \A_i
    connect \B \B_i
    connect \S \SAB_i
    connect \Y \AB
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:37"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:37$2
    parameter \WIDTH 8
    connect \A \AB
    connect \B \C_i
    connect \S \SC_i
    connect \Y \ABC
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:38"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:38$3
    parameter \WIDTH 8
    connect \A \ABC
    connect \B \D_i
    connect \S \SD_i
    connect \Y \ABCD
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:39"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:39$4
    parameter \WIDTH 8
    connect \A \ABCD
    connect \B \E_i
    connect \S \SE_i
    connect \Y \ABCDE
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:40"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:40$5
    parameter \WIDTH 8
    connect \A \ABCDE
    connect \B \F_i
    connect \S \SF_i
    connect \Y \ABCDEF
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:41"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:41$6
    parameter \WIDTH 8
    connect \A \ABCDEF
    connect \B \G_i
    connect \S \SG_i
    connect \Y \ABCDEFG
  end

  attribute \src "../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:42"
  cell $mux $ternary$../../../../celllib/bytemuxoct/verilog/bytemuxoct.v:42$7
    parameter \WIDTH 8
    connect \A \ABCDEFG
    connect \B \H_i
    connect \S \SH_i
    connect \Y \Y_o
  end
end

# Cell ByteMuxQuad with all variants from ../../../../celllib/bytemuxquad/chll/out/bytemuxquad-impl.il

attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:1"
attribute \extract_order 24
module \ByteMuxQuad

  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:20"
  wire width 8 \AB

  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:21"
  wire width 8 \ABC

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:3"
  wire width 8 input 1 \A_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:5"
  wire width 8 input 2 \B_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:7"
  wire width 8 input 3 \C_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:9"
  wire width 8 input 4 \D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:11"
  wire input 5 \SAB_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:13"
  wire input 6 \SC_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:15"
  wire input 7 \SD_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:17"
  wire width 8 output 8 \Y_o

  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:20"
  cell $mux $ternary$../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:20$1
    parameter \WIDTH 8
    connect \A \A_i
    connect \B \B_i
    connect \S \SAB_i
    connect \Y \AB
  end

  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:21"
  cell $mux $ternary$../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:21$2
    parameter \WIDTH 8
    connect \A \AB
    connect \B \C_i
    connect \S \SC_i
    connect \Y \ABC
  end

  attribute \src "../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:22"
  cell $mux $ternary$../../../../celllib/bytemuxquad/verilog/bytemuxquad.v:22$3
    parameter \WIDTH 8
    connect \A \ABC
    connect \B \D_i
    connect \S \SD_i
    connect \Y \Y_o
  end
end

# Cell ByteMuxDual with all variants from ../../../../celllib/bytemuxdual/chll/out/bytemuxdual-impl.il

attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:1"
attribute \extract_order 25
module \ByteMuxDual

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:3"
  wire width 8 input 1 \A_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:5"
  wire width 8 input 2 \B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:7"
  wire input 3 \S_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:9"
  wire width 8 output 4 \Y_o

  attribute \src "../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:12"
  cell $mux $ternary$../../../../celllib/bytemuxdual/verilog/bytemuxdual.v:12$1
    parameter \WIDTH 8
    connect \A \A_i
    connect \B \B_i
    connect \S \S_i
    connect \Y \Y_o
  end
end

# Cell Byte2Word with all variants from ../../../../celllib/byte2word/chll/out/byte2word-impl.il

attribute \src "../../../../celllib/byte2word/verilog/byte2word.v:1"
attribute \extract_order 26
module \Byte2Word

  wire width 16 $auto$splice.cc:141:get_spliced_signal$2

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/byte2word/verilog/byte2word.v:7"
  wire width 16 $auto$splice.cc:237:run$3

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2word/verilog/byte2word.v:3"
  wire width 8 input 1 \H_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2word/verilog/byte2word.v:5"
  wire width 8 input 2 \L_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/byte2word/verilog/byte2word.v:7"
  wire width 16 output 3 \Y_o

  cell $concat $auto$splice.cc:135:get_spliced_signal$1
    parameter \A_WIDTH 8
    parameter \B_WIDTH 8
    connect \A \L_i
    connect \B \H_i
    connect \Y $auto$splice.cc:141:get_spliced_signal$2
  end

  connect $auto$splice.cc:237:run$3 { \H_i \L_i }
  connect \Y_o $auto$splice.cc:141:get_spliced_signal$2
end

# Cell Byte2WordSel with all variants from ../../../../celllib/byte2wordsel/chll/out/byte2wordsel-impl.il

attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:1"
attribute \extract_order 27
module \Byte2WordSel

  wire width 16 $auto$splice.cc:141:get_spliced_signal$6

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:13"
  wire width 16 $auto$splice.cc:237:run$7

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15"
  wire width 32 $sub$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15$2_Y

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:13"
  wire width 16 \Concat

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:3"
  wire width 8 input 1 \H_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:5"
  wire width 8 input 2 \L_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15"
  wire width 16 \Mask

  attribute \intersynth_config 4
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:11"
  wire width 4 input 5 \Mask_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16"
  wire width 16 \Masked

  attribute \intersynth_config 4
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:9"
  wire width 4 input 4 \Shift_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14"
  wire width 16 \Shifted

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:7"
  wire width 16 output 3 \Y_o

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16"
  cell $and $and$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \Shifted
    connect \B \Mask
    connect \Y \Y_o
  end

  cell $concat $auto$splice.cc:135:get_spliced_signal$5
    parameter \A_WIDTH 8
    parameter \B_WIDTH 8
    connect \A \L_i
    connect \B \H_i
    connect \Y $auto$splice.cc:141:get_spliced_signal$6
  end

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14"
  cell $shr $shr$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 16
    connect \A $auto$splice.cc:141:get_spliced_signal$6
    connect \B \Shift_i
    connect \Y \Shifted
  end

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15"
  cell $shr $shr$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15$3
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 16
    connect \A 16'1111111111111111
    connect \B $sub$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15$2_Y
    connect \Y \Mask
  end

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15"
  cell $sub $sub$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 32
    connect \A 15
    connect \B \Mask_i
    connect \Y $sub$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:15$2_Y
  end

  connect $auto$splice.cc:237:run$7 { \H_i \L_i }
  connect \Masked \Y_o
  connect \Concat $auto$splice.cc:141:get_spliced_signal$6
end

attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:20"
attribute \extract_order 28
module \Byte2WordSel_11MSB_Direct

  wire width 11 $auto$splice.cc:141:get_spliced_signal$11

  wire width 16 $auto$splice.cc:141:get_spliced_signal$13

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:23"
  wire width 16 $auto$splice.cc:237:run$14

  wire width 3 $auto$splice.cc:80:get_sliced_signal$9

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:21"
  wire width 8 input 1 \H_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:22"
  wire width 8 input 2 \L_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:23"
  wire width 16 output 3 \Y_o

  cell $concat $auto$splice.cc:135:get_spliced_signal$10
    parameter \A_WIDTH 3
    parameter \B_WIDTH 8
    connect \A $auto$splice.cc:80:get_sliced_signal$9
    connect \B \H_i
    connect \Y $auto$splice.cc:141:get_spliced_signal$11
  end

  cell $concat $auto$splice.cc:135:get_spliced_signal$12
    parameter \A_WIDTH 11
    parameter \B_WIDTH 5
    connect \A $auto$splice.cc:141:get_spliced_signal$11
    connect \B { 1'0 1'0 1'0 1'0 1'0 }
    connect \Y $auto$splice.cc:141:get_spliced_signal$13
  end

  cell $slice $auto$splice.cc:74:get_sliced_signal$8
    parameter \A_WIDTH 8
    parameter \OFFSET 5
    parameter \Y_WIDTH 3
    connect \A { \L_i [7] \L_i [6] \L_i [5] \L_i [4] \L_i [3] \L_i [2] \L_i [1] \L_i [0] }
    connect \Y $auto$splice.cc:80:get_sliced_signal$9
  end

  connect $auto$splice.cc:237:run$14 { 5'00000 \H_i \L_i [7:5] }
  connect \Y_o $auto$splice.cc:141:get_spliced_signal$13
end

attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:1"
attribute \extract_order 29
module \Byte2WordSel_11MSB_Main

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:13"
  wire width 16 \DUT.Concat

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:3"
  wire width 8 \DUT.H_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:5"
  wire width 8 \DUT.L_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16"
  wire width 16 \DUT.Masked

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14"
  wire width 16 \DUT.Shifted

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:7"
  wire width 16 \DUT.Y_o

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:2"
  wire width 8 input 1 \H_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:3"
  wire width 8 input 2 \L_i

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel_11msb.v:4"
  wire width 16 output 3 \Y_o

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16"
  cell $and $techmap\DUT.$and$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:16$4
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \DUT.Shifted
    connect \B 16'0000111111111111
    connect \Y \DUT.Y_o
  end

  cell $concat $techmap\DUT.$auto$splice.cc:135:get_spliced_signal$5
    parameter \A_WIDTH 8
    parameter \B_WIDTH 8
    connect \A \L_i
    connect \B \H_i
    connect \Y \DUT.Concat
  end

  attribute \src "../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14"
  cell $shr $techmap\DUT.$shr$../../../../celllib/byte2wordsel/verilog/byte2wordsel.v:14$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 16
    connect \A \DUT.Concat
    connect \B 4'0101
    connect \Y \DUT.Shifted
  end

  connect \DUT.H_i \H_i
  connect \DUT.L_i \L_i
  connect \DUT.Masked \DUT.Y_o
  connect \Y_o \DUT.Y_o
end

# Cell WordMuxDual with all variants from ../../../../celllib/wordmuxdual/chll/out/wordmuxdual-impl.il

attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:1"
attribute \extract_order 30
module \WordMuxDual

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:3"
  wire width 16 input 1 \A_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:5"
  wire width 16 input 2 \B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:7"
  wire input 3 \S_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:9"
  wire width 16 output 4 \Y_o

  attribute \src "../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:12"
  cell $mux $ternary$../../../../celllib/wordmuxdual/verilog/wordmuxdual.v:12$1
    parameter \WIDTH 16
    connect \A \A_i
    connect \B \B_i
    connect \S \S_i
    connect \Y \Y_o
  end
end

