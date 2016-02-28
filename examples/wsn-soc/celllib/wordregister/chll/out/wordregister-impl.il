
attribute \src "../../verilog/wordregister.v:1"
module \WordRegister

  attribute \src "../../verilog/wordregister.v:14"
  wire width 16 $0\Q_o[15:0]

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/wordregister.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordregister.v:7"
  wire width 16 input 3 \D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/wordregister.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordregister.v:9"
  wire width 16 output 4 \Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/wordregister.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/wordregister.v:14"
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

attribute \src "../../verilog/wordregister_mux.v:45"
module \WordRegister_Mux_Direct

  attribute \src "../../verilog/wordregister_mux.v:55"
  wire width 16 $0\Q_o[15:0]

  wire width 16 $procmux$11_Y

  attribute \src "../../verilog/wordregister_mux.v:47"
  wire input 2 \Clk_i

  attribute \src "../../verilog/wordregister_mux.v:48"
  wire width 16 input 3 \D1_i

  attribute \src "../../verilog/wordregister_mux.v:49"
  wire width 16 input 4 \D2_i

  attribute \src "../../verilog/wordregister_mux.v:51"
  wire input 6 \Enable1_i

  attribute \src "../../verilog/wordregister_mux.v:52"
  wire input 7 \Enable2_i

  attribute \src "../../verilog/wordregister_mux.v:50"
  wire width 16 output 5 \Q_o

  attribute \src "../../verilog/wordregister_mux.v:46"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/wordregister_mux.v:55"
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

attribute \src "../../verilog/wordregister_mux.v:19"
module \WordRegister_Mux_Main

  attribute \src "../../verilog/wordregister.v:14"
  wire width 16 $techmap\ThisWordRegister.$0\Q_o[15:0]

  attribute \src "../../verilog/wordregister_mux.v:21"
  wire input 2 \Clk_i

  attribute \src "../../verilog/wordregister_mux.v:22"
  wire width 16 input 3 \D1_i

  attribute \src "../../verilog/wordregister_mux.v:23"
  wire width 16 input 4 \D2_i

  attribute \src "../../verilog/wordregister_mux.v:30"
  wire width 16 \D_s

  attribute \src "../../verilog/wordregister_mux.v:25"
  wire input 6 \Enable1_i

  attribute \src "../../verilog/wordregister_mux.v:26"
  wire input 7 \Enable2_i

  attribute \src "../../verilog/wordregister_mux.v:29"
  wire \Enable_s

  attribute \src "../../verilog/wordregister_mux.v:24"
  wire width 16 output 5 \Q_o

  attribute \src "../../verilog/wordregister_mux.v:20"
  wire input 1 \Reset_n_i

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/wordregister.v:5"
  wire \ThisWordRegister.Clk_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordregister.v:7"
  wire width 16 \ThisWordRegister.D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/wordregister.v:11"
  wire \ThisWordRegister.Enable_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordregister.v:9"
  wire width 16 \ThisWordRegister.Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/wordregister.v:3"
  wire \ThisWordRegister.Reset_n_i

  attribute \src "../../verilog/wordregister_mux.v:32"
  cell $or $or$../../verilog/wordregister_mux.v:32$3
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Enable1_i
    connect \B \Enable2_i
    connect \Y \Enable_s
  end

  attribute \src "../../verilog/wordregister.v:14"
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

  attribute \src "../../verilog/wordregister_mux.v:33"
  cell $mux $ternary$../../verilog/wordregister_mux.v:33$4
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
