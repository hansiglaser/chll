attribute \src "map.v:1"
attribute \aint 3
attribute \avec 4'0101
attribute \aescape "ABC\nABC\tABC\141\142\143"
module \mydff
  attribute \src "map.v:9"
  wire width 4 $0\q[3:0]
  wire width 4 $procmux$4_Y
  attribute \src "map.v:5"
  wire input 1 \clk
  attribute \src "map.v:6"
  wire width 4 input 4 \d
  attribute \src "map.v:5"
  wire input 3 \en
  attribute \src "map.v:7"
  wire width 4 output 5 \q
  attribute \src "map.v:5"
  wire input 2 \rst
  cell $dff $procdff$9
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 4
    connect \CLK \clk
    connect \D $0\q[3:0]
    connect \Q \q
  end
  cell $mux $procmux$4
    parameter \WIDTH 4
    connect \A \q
    connect \B \d
    connect \S \en
    connect \Y $procmux$4_Y
  end
  cell $mux $procmux$7
    parameter \WIDTH 4
    connect \A $procmux$4_Y
    connect \B 4'0000
    connect \S \rst
    connect \Y $0\q[3:0]
  end
end

attribute \src "map.v:18"
module \myinc
  attribute \src "map.v:21"
  wire width 4 input 1 \a
  attribute \src "map.v:22"
  wire width 4 output 2 \y
  attribute \src "map.v:24"
  cell $add $add$map.v:24$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 4
    parameter \B_SIGNED 1
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 4
    connect \A \a
    connect \B 1
    connect \Y \y
  end
end
