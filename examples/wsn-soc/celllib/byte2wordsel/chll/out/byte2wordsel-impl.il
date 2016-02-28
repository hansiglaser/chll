
attribute \src "../../verilog/byte2wordsel.v:1"
module \Byte2WordSel

  wire width 16 $auto$splice.cc:141:get_spliced_signal$6

  attribute \src "../../verilog/byte2wordsel.v:13"
  wire width 16 $auto$splice.cc:237:run$7

  attribute \src "../../verilog/byte2wordsel.v:15"
  wire width 32 $sub$../../verilog/byte2wordsel.v:15$2_Y

  attribute \src "../../verilog/byte2wordsel.v:13"
  wire width 16 \Concat

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byte2wordsel.v:3"
  wire width 8 input 1 \H_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byte2wordsel.v:5"
  wire width 8 input 2 \L_i

  attribute \src "../../verilog/byte2wordsel.v:15"
  wire width 16 \Mask

  attribute \intersynth_config 4
  attribute \src "../../verilog/byte2wordsel.v:11"
  wire width 4 input 5 \Mask_i

  attribute \src "../../verilog/byte2wordsel.v:16"
  wire width 16 \Masked

  attribute \intersynth_config 4
  attribute \src "../../verilog/byte2wordsel.v:9"
  wire width 4 input 4 \Shift_i

  attribute \src "../../verilog/byte2wordsel.v:14"
  wire width 16 \Shifted

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/byte2wordsel.v:7"
  wire width 16 output 3 \Y_o

  attribute \src "../../verilog/byte2wordsel.v:16"
  cell $and $and$../../verilog/byte2wordsel.v:16$4
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

  attribute \src "../../verilog/byte2wordsel.v:14"
  cell $shr $shr$../../verilog/byte2wordsel.v:14$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 16
    connect \A $auto$splice.cc:141:get_spliced_signal$6
    connect \B \Shift_i
    connect \Y \Shifted
  end

  attribute \src "../../verilog/byte2wordsel.v:15"
  cell $shr $shr$../../verilog/byte2wordsel.v:15$3
    parameter \A_SIGNED 0
    parameter \A_WIDTH 16
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 16
    connect \A 16'1111111111111111
    connect \B $sub$../../verilog/byte2wordsel.v:15$2_Y
    connect \Y \Mask
  end

  attribute \src "../../verilog/byte2wordsel.v:15"
  cell $sub $sub$../../verilog/byte2wordsel.v:15$2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 32
    connect \A 15
    connect \B \Mask_i
    connect \Y $sub$../../verilog/byte2wordsel.v:15$2_Y
  end

  connect $auto$splice.cc:237:run$7 { \H_i \L_i }
  connect \Masked \Y_o
  connect \Concat $auto$splice.cc:141:get_spliced_signal$6
end

attribute \src "../../verilog/byte2wordsel_11msb.v:20"
module \Byte2WordSel_11MSB_Direct

  wire width 11 $auto$splice.cc:141:get_spliced_signal$11

  wire width 16 $auto$splice.cc:141:get_spliced_signal$13

  attribute \src "../../verilog/byte2wordsel_11msb.v:23"
  wire width 16 $auto$splice.cc:237:run$14

  wire width 3 $auto$splice.cc:80:get_sliced_signal$9

  attribute \src "../../verilog/byte2wordsel_11msb.v:21"
  wire width 8 input 1 \H_i

  attribute \src "../../verilog/byte2wordsel_11msb.v:22"
  wire width 8 input 2 \L_i

  attribute \src "../../verilog/byte2wordsel_11msb.v:23"
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

attribute \src "../../verilog/byte2wordsel_11msb.v:1"
module \Byte2WordSel_11MSB_Main

  attribute \src "../../verilog/byte2wordsel.v:13"
  wire width 16 \DUT.Concat

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byte2wordsel.v:3"
  wire width 8 \DUT.H_i

  attribute \intersynth_conntype "Byte"
  attribute \src "../../verilog/byte2wordsel.v:5"
  wire width 8 \DUT.L_i

  attribute \src "../../verilog/byte2wordsel.v:16"
  wire width 16 \DUT.Masked

  attribute \src "../../verilog/byte2wordsel.v:14"
  wire width 16 \DUT.Shifted

  attribute \intersynth_conntype "Word"
  attribute \src "../../verilog/byte2wordsel.v:7"
  wire width 16 \DUT.Y_o

  attribute \src "../../verilog/byte2wordsel_11msb.v:2"
  wire width 8 input 1 \H_i

  attribute \src "../../verilog/byte2wordsel_11msb.v:3"
  wire width 8 input 2 \L_i

  attribute \src "../../verilog/byte2wordsel_11msb.v:4"
  wire width 16 output 3 \Y_o

  attribute \src "../../verilog/byte2wordsel.v:16"
  cell $and $techmap\DUT.$and$../../verilog/byte2wordsel.v:16$4
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

  attribute \src "../../verilog/byte2wordsel.v:14"
  cell $shr $techmap\DUT.$shr$../../verilog/byte2wordsel.v:14$1
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
