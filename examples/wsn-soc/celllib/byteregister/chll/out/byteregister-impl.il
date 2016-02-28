
attribute \src "../../verilog/byteregister.v:1"
module \ByteRegister

  attribute \src "../../verilog/byteregister.v:14"
  wire width 8 $0\Q_o[7:0]

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/byteregister.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byteregister.v:7"
  wire width 8 input 3 \D_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/byteregister.v:11"
  wire input 5 \Enable_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byteregister.v:9"
  wire width 8 output 4 \Q_o

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/byteregister.v:3"
  wire input 1 \Reset_n_i

  attribute \src "../../verilog/byteregister.v:14"
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
