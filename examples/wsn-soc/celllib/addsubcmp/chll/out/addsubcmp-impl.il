
attribute \src "../../verilog/addsubcmp.v:1"
module \AddSubCmp

  attribute \src "../../verilog/addsubcmp.v:33"
  wire width 17 $add$../../verilog/addsubcmp.v:33$7_Y

  attribute \src "../../verilog/addsubcmp.v:42"
  wire $eq$../../verilog/addsubcmp.v:42$11_Y

  attribute \src "../../verilog/addsubcmp.v:42"
  wire $ne$../../verilog/addsubcmp.v:42$12_Y

  attribute \src "../../verilog/addsubcmp.v:30"
  wire width 16 $not$../../verilog/addsubcmp.v:30$2_Y

  attribute \src "../../verilog/addsubcmp.v:31"
  wire $not$../../verilog/addsubcmp.v:31$5_Y

  attribute \src "../../verilog/addsubcmp.v:23"
  wire width 17 \A

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:7"
  wire width 16 input 2 \A_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:5"
  wire input 1 \AddOrSub_i

  attribute \src "../../verilog/addsubcmp.v:24"
  wire width 17 \B

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:9"
  wire width 16 input 3 \B_i

  attribute \src "../../verilog/addsubcmp.v:26"
  wire \Carry

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:13"
  wire input 5 \Carry_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:15"
  wire output 6 \Carry_o

  attribute \src "../../verilog/addsubcmp.v:27"
  wire width 16 \D

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:11"
  wire width 16 output 4 \D_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:21"
  wire output 9 \Overflow_o

  attribute \src "../../verilog/addsubcmp.v:25"
  wire width 17 \Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:19"
  wire output 8 \Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:17"
  wire output 7 \Zero_o

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $add$../../verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B \B
    connect \Y $add$../../verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $add$../../verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $add$../../verilog/addsubcmp.v:33$7_Y
    connect \B \Carry
    connect \Y { \Carry_o \D }
  end

  attribute \src "../../verilog/addsubcmp.v:39"
  cell $eq $eq$../../verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \D
    connect \B 16'0000000000000000
    connect \Y \Zero_o
  end

  attribute \src "../../verilog/addsubcmp.v:42"
  cell $eq $eq$../../verilog/addsubcmp.v:42$11
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \A_i [15]
    connect \B \B [15]
    connect \Y $eq$../../verilog/addsubcmp.v:42$11_Y
  end

  attribute \src "../../verilog/addsubcmp.v:42"
  cell $logic_and $logic_and$../../verilog/addsubcmp.v:42$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $eq$../../verilog/addsubcmp.v:42$11_Y
    connect \B $ne$../../verilog/addsubcmp.v:42$12_Y
    connect \Y \Overflow_o
  end

  attribute \src "../../verilog/addsubcmp.v:42"
  cell $ne $ne$../../verilog/addsubcmp.v:42$12
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \D [15]
    connect \B \A_i [15]
    connect \Y $ne$../../verilog/addsubcmp.v:42$12_Y
  end

  attribute \src "../../verilog/addsubcmp.v:30"
  cell $not $not$../../verilog/addsubcmp.v:30$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \Y_WIDTH 16
    connect \A \B_i
    connect \Y $not$../../verilog/addsubcmp.v:30$2_Y
  end

  attribute \src "../../verilog/addsubcmp.v:31"
  cell $not $not$../../verilog/addsubcmp.v:31$5
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Carry_i
    connect \Y $not$../../verilog/addsubcmp.v:31$5_Y
  end

  attribute \src "../../verilog/addsubcmp.v:30"
  cell $mux $ternary$../../verilog/addsubcmp.v:30$3
    parameter \WIDTH 17
    connect \A { 1'0 \B_i }
    connect \B { 1'0 $not$../../verilog/addsubcmp.v:30$2_Y }
    connect \S \AddOrSub_i
    connect \Y \B
  end

  attribute \src "../../verilog/addsubcmp.v:31"
  cell $mux $ternary$../../verilog/addsubcmp.v:31$6
    parameter \WIDTH 1
    connect \A \Carry_i
    connect \B $not$../../verilog/addsubcmp.v:31$5_Y
    connect \S \AddOrSub_i
    connect \Y \Carry
  end

  connect \A { 1'0 \A_i }
  connect \D_o \D
  connect \Result { \Carry_o \D }
  connect \Sign_o \D [15]
end

attribute \src "../../verilog/addsubcmp_add.v:26"
module \AddSubCmp_Add_Direct

  attribute \src "../../verilog/addsubcmp_add.v:27"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_add.v:28"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_add.v:29"
  wire width 16 output 3 \D_o

  attribute \src "../../verilog/addsubcmp_add.v:32"
  cell $add $add$../../verilog/addsubcmp_add.v:32$23
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

attribute \src "../../verilog/addsubcmp_greater.v:29"
module \AddSubCmp_Greater_Direct

  attribute \src "../../verilog/addsubcmp_greater.v:30"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_greater.v:31"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_greater.v:32"
  wire output 3 \Greater_o

  attribute \src "../../verilog/addsubcmp_greater.v:35"
  cell $gt $gt$../../verilog/addsubcmp_greater.v:35$17
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

attribute \src "../../verilog/addsubcmp_less.v:29"
module \AddSubCmp_Less_Direct

  attribute \src "../../verilog/addsubcmp_less.v:30"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_less.v:31"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_less.v:32"
  wire output 3 \Less_o

  attribute \src "../../verilog/addsubcmp_less.v:35"
  cell $lt $lt$../../verilog/addsubcmp_less.v:35$21
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

attribute \src "../../verilog/addsubcmp_add.v:1"
module \AddSubCmp_Add_Main

  attribute \src "../../verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y

  attribute \src "../../verilog/addsubcmp_add.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_add.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_add.v:7"
  attribute \unused_bits "0"
  wire \Carry_s

  attribute \src "../../verilog/addsubcmp_add.v:4"
  wire width 16 output 3 \D_o

  attribute \src "../../verilog/addsubcmp_add.v:9"
  wire \Sign_s

  attribute \src "../../verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:15"
  attribute \unused_bits "0"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B { 1'0 \B_i }
    connect \Y $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
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

attribute \src "../../verilog/addsubcmp_greater.v:1"
module \AddSubCmp_Greater_Main

  attribute \src "../../verilog/addsubcmp_greater.v:25"
  wire $not$../../verilog/addsubcmp_greater.v:25$15_Y

  attribute \src "../../verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y

  attribute \src "../../verilog/addsubcmp_greater.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_greater.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_greater.v:8"
  wire \Carry_s

  attribute \src "../../verilog/addsubcmp_greater.v:7"
  wire width 16 \D_s

  attribute \src "../../verilog/addsubcmp_greater.v:4"
  wire output 3 \Greater_o

  attribute \src "../../verilog/addsubcmp_greater.v:10"
  wire \Sign_s

  attribute \src "../../verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:15"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:17"
  wire \ThisAddSubCmp.Zero_o

  attribute \src "../../verilog/addsubcmp_greater.v:9"
  wire \Zero_s

  attribute \src "../../verilog/addsubcmp_greater.v:25"
  cell $and $and$../../verilog/addsubcmp_greater.v:25$16
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Carry_o
    connect \B $not$../../verilog/addsubcmp_greater.v:25$15_Y
    connect \Y \Greater_o
  end

  attribute \src "../../verilog/addsubcmp_greater.v:25"
  cell $not $not$../../verilog/addsubcmp_greater.v:25$15
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Zero_o
    connect \Y $not$../../verilog/addsubcmp_greater.v:25$15_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \A_i }
    connect \B { 1'0 \ThisAddSubCmp.B [15:0] }
    connect \Y $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
    connect \B 1'1
    connect \Y { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  end

  attribute \src "../../verilog/addsubcmp.v:39"
  cell $eq $techmap\ThisAddSubCmp.$eq$../../verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.D
    connect \B 16'0000000000000000
    connect \Y \ThisAddSubCmp.Zero_o
  end

  attribute \src "../../verilog/addsubcmp.v:30"
  cell $not $techmap\ThisAddSubCmp.$not$../../verilog/addsubcmp.v:30$2
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

attribute \src "../../verilog/addsubcmp_less.v:1"
module \AddSubCmp_Less_Main

  attribute \src "../../verilog/addsubcmp_less.v:25"
  wire $not$../../verilog/addsubcmp_less.v:25$19_Y

  attribute \src "../../verilog/addsubcmp.v:33"
  wire width 17 $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y

  attribute \src "../../verilog/addsubcmp_less.v:2"
  wire width 16 input 1 \A_i

  attribute \src "../../verilog/addsubcmp_less.v:3"
  wire width 16 input 2 \B_i

  attribute \src "../../verilog/addsubcmp_less.v:8"
  wire \Carry_s

  attribute \src "../../verilog/addsubcmp_less.v:7"
  wire width 16 \D_s

  attribute \src "../../verilog/addsubcmp_less.v:4"
  wire output 3 \Less_o

  attribute \src "../../verilog/addsubcmp_less.v:10"
  wire \Sign_s

  attribute \src "../../verilog/addsubcmp.v:23"
  wire width 17 \ThisAddSubCmp.A

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:7"
  wire width 16 \ThisAddSubCmp.A_i

  attribute \src "../../verilog/addsubcmp.v:24"
  wire width 17 \ThisAddSubCmp.B

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:9"
  wire width 16 \ThisAddSubCmp.B_i

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:15"
  wire \ThisAddSubCmp.Carry_o

  attribute \src "../../verilog/addsubcmp.v:27"
  wire width 16 \ThisAddSubCmp.D

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/addsubcmp.v:11"
  wire width 16 \ThisAddSubCmp.D_o

  attribute \src "../../verilog/addsubcmp.v:25"
  wire width 17 \ThisAddSubCmp.Result

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:19"
  wire \ThisAddSubCmp.Sign_o

  attribute \intersynth_conntype "Bit"
  attribute \src "../../verilog/addsubcmp.v:17"
  wire \ThisAddSubCmp.Zero_o

  attribute \src "../../verilog/addsubcmp_less.v:9"
  wire \Zero_s

  attribute \src "../../verilog/addsubcmp_less.v:25"
  cell $and $and$../../verilog/addsubcmp_less.v:25$20
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Carry_o
    connect \B $not$../../verilog/addsubcmp_less.v:25$19_Y
    connect \Y \Less_o
  end

  attribute \src "../../verilog/addsubcmp_less.v:25"
  cell $not $not$../../verilog/addsubcmp_less.v:25$19
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.Zero_o
    connect \Y $not$../../verilog/addsubcmp_less.v:25$19_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 17
    parameter \Y_WIDTH 17
    connect \A { 1'0 \B_i }
    connect \B { 1'0 \ThisAddSubCmp.B [15:0] }
    connect \Y $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
  end

  attribute \src "../../verilog/addsubcmp.v:33"
  cell $add $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$8
    parameter \A_SIGNED 0
    parameter \A_WIDTH 17
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 17
    connect \A $techmap\ThisAddSubCmp.$add$../../verilog/addsubcmp.v:33$7_Y
    connect \B 1'1
    connect \Y { \ThisAddSubCmp.Carry_o \ThisAddSubCmp.D }
  end

  attribute \src "../../verilog/addsubcmp.v:39"
  cell $eq $techmap\ThisAddSubCmp.$eq$../../verilog/addsubcmp.v:39$9
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 16
    parameter \Y_WIDTH 1
    connect \A \ThisAddSubCmp.D
    connect \B 16'0000000000000000
    connect \Y \ThisAddSubCmp.Zero_o
  end

  attribute \src "../../verilog/addsubcmp.v:30"
  cell $not $techmap\ThisAddSubCmp.$not$../../verilog/addsubcmp.v:30$2
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
