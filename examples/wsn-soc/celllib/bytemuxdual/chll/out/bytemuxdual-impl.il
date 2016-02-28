
attribute \src "../../verilog/bytemuxdual.v:1"
module \ByteMuxDual

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/bytemuxdual.v:3"
  wire width 8 input 1 \A_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/bytemuxdual.v:5"
  wire width 8 input 2 \B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/bytemuxdual.v:7"
  wire input 3 \S_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/bytemuxdual.v:9"
  wire width 8 output 4 \Y_o

  attribute \src "../../verilog/bytemuxdual.v:12"
  cell $mux $ternary$../../verilog/bytemuxdual.v:12$1
    parameter \WIDTH 8
    connect \A \A_i
    connect \B \B_i
    connect \S \S_i
    connect \Y \Y_o
  end
end
