
attribute \src "../../verilog/wordmuxdual.v:1"
module \WordMuxDual

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordmuxdual.v:3"
  wire width 16 input 1 \A_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordmuxdual.v:5"
  wire width 16 input 2 \B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/wordmuxdual.v:7"
  wire input 3 \S_i

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/wordmuxdual.v:9"
  wire width 16 output 4 \Y_o

  attribute \src "../../verilog/wordmuxdual.v:12"
  cell $mux $ternary$../../verilog/wordmuxdual.v:12$1
    parameter \WIDTH 16
    connect \A \A_i
    connect \B \B_i
    connect \S \S_i
    connect \Y \Y_o
  end
end
