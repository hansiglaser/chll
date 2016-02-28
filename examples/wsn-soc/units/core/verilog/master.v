
// cat $FILENAME
// grep -i integer $FILENAME | sed -r 's/^.*[^A-Za-z0-9_]([A-Za-z0-9_]+).*:.*:= *([A-F0-9#]*).*$/  parameter \1 = \2;/' | sed -r "s/16#(.*)#/'h\1/"
// grep -i integer $FILENAME | sed -r 's/^.*[^A-Za-z0-9_]([A-Za-z0-9_]+).*:.*:= *([A-F0-9#]*).*$/    .\1(\2),/' | sed -r "s/16#(.*)#/'h\1/"

`ifndef SkipSPIMaster
module SPI_Master ( Reset_n, Clk, CPOL_i, CPHA_i, LSBFE_i, SPPR_i, 
        SPR_i, SCK_o, MOSI_o, MISO_i, Transmission_o, Write_i, ReadNext_i, 
        Data_i, Data_o, FIFOFull_o, FIFOEmpty_o, ScanEnable_i, ScanClk_i, 
        ScanDataIn_i, ScanDataOut_o );
  parameter DataWidth = 8;
  parameter SPPRWidth = 3;
  parameter SPRWidth = 3;
  parameter FIFOReadWidth = 4;
  parameter FIFOWriteWidth = 4;
  input [(SPPRWidth-1):0] SPPR_i;
  input [(SPRWidth-1):0] SPR_i;
  input [(DataWidth-1):0] Data_i;
  output [7:0] Data_o;
  input Reset_n, Clk, CPOL_i, CPHA_i, LSBFE_i, MISO_i, Write_i, ReadNext_i,
         ScanEnable_i, ScanClk_i, ScanDataIn_i;
  output SCK_o, MOSI_o, Transmission_o, FIFOFull_o, FIFOEmpty_o, ScanDataOut_o;

  parameter X = 3;
endmodule
`endif

`ifndef SkipI2CMaster
module i2c_master ( Reset_i, Clk_i, F100_400_n_i, Divider800_i, 
        StartProcess_i, ReceiveSend_n_i, Busy_o, ReadCount_i, FIFOReadNext_i, 
        FIFOWrite_i, FIFOEmpty_o, FIFOFull_o, Data_i, Data_o, ErrAck_i, 
        ErrBusColl_o, ErrFIFOFull_o, ErrGotNAck_o, ErrCoreBusy_o, 
        ErrFIFOEmpty_o, ErrCoreStopped_o, ErrDevNotPresent_o, 
        ErrReadCountZero_o, SDA_i, SDA_o, SCL_o, ScanEnable_i, ScanClk_i, 
        ScanDataIn_i, ScanDataOut_o );
  parameter ReadCountWidth_g = 4;
  parameter FIFOAddressWidth_g = 4;
  parameter DividerWidth_g = 16;
  input [(DividerWidth_g-1):0] Divider800_i;
  input [(ReadCountWidth_g-1):0] ReadCount_i;
  input [7:0] Data_i;
  output [7:0] Data_o;
  input Reset_i, Clk_i, F100_400_n_i, StartProcess_i, ReceiveSend_n_i,
         FIFOReadNext_i, FIFOWrite_i, ErrAck_i, SDA_i, ScanEnable_i, ScanClk_i,
         ScanDataIn_i;
  output Busy_o, FIFOEmpty_o, FIFOFull_o, ErrBusColl_o, ErrFIFOFull_o,
         ErrGotNAck_o, ErrCoreBusy_o, ErrFIFOEmpty_o, ErrCoreStopped_o,
         ErrDevNotPresent_o, ErrReadCountZero_o, SDA_o, SCL_o, ScanDataOut_o;
endmodule
`endif

module onewire_master ( Clk, Reset, OWIn_i, OWOut_o, 
        OWReset_i, DeactivateOverdriveMode_i, SearchROM_i, ReadROM_i, 
        MatchROM_i, SkipROM_i, CondSearchROM_i, OverdriveSkipROM_i, 
        OverdriveMatchROM_i, CondReadROM_i, ResumeROM_i, WriteByte_i, 
        ReadByte_i, GetROMID_i, Data_i, Data_o, ROMIDsInArray_o, Noslaves_o, 
        ROMIDArrayToSmall_o, PDROut_o, Ready_o, ResetLowTime, ResetTime, 
        ResetWaitForDetectionDuration, ResetPrecenceIntervalDuration, 
        WRSlotHighDataTime, RDSlotSampleTime, SlotLowDataTime, SlotDuration, 
        RDSlotInitTime, ODResetLowTime, ODResetTime, 
        ODResetWaitForDetectionDuration, ODResetPrecenceIntervalDuration, 
        ODWRSlotHighDataTime, ODRDSlotSampleTime, ODSlotLowDataTime, 
        ODSlotDuration, ODRDSlotInitTime, ScanEnable_i, ScanClk_i, 
        ScanDataIn_i, ScanDataOut_o );
  parameter ROMIDArraySize = 4;
  parameter ROMIDIndexSize = 2;
  parameter ROMIDByteIndexSize = 3;
  parameter SearchCommand = 'hF0;
  parameter CondSearchCommand = 'hEC;
  parameter MatchCommand = 'h55;
  parameter ReadCommand = 'h33;
  parameter SkipCommand = 'hCC;
  parameter OverdriveSkipCommand = 'h3C;
  parameter OverdriveMatchCommand = 'h69;
  parameter ConditionalReadCommand = 'h0F;
  parameter ResumeCommand = 'hA5;
  parameter TimerWidth = 16;
  input [7:0] Data_i;
  output [7:0] Data_o;
  output [(ROMIDIndexSize-1):0] ROMIDsInArray_o;
  input [(TimerWidth-1):0] ResetLowTime;
  input [(TimerWidth-1):0] ResetTime;
  input [(TimerWidth-1):0] ResetWaitForDetectionDuration;
  input [(TimerWidth-1):0] ResetPrecenceIntervalDuration;
  input [(TimerWidth-1):0] WRSlotHighDataTime;
  input [(TimerWidth-1):0] RDSlotSampleTime;
  input [(TimerWidth-1):0] SlotLowDataTime;
  input [(TimerWidth-1):0] SlotDuration;
  input [(TimerWidth-1):0] RDSlotInitTime;
  input [(TimerWidth-1):0] ODResetLowTime;
  input [(TimerWidth-1):0] ODResetTime;
  input [(TimerWidth-1):0] ODResetWaitForDetectionDuration;
  input [(TimerWidth-1):0] ODResetPrecenceIntervalDuration;
  input [(TimerWidth-1):0] ODWRSlotHighDataTime;
  input [(TimerWidth-1):0] ODRDSlotSampleTime;
  input [(TimerWidth-1):0] ODSlotLowDataTime;
  input [(TimerWidth-1):0] ODSlotDuration;
  input [(TimerWidth-1):0] ODRDSlotInitTime;
  input Clk, Reset, OWIn_i, OWReset_i, DeactivateOverdriveMode_i, SearchROM_i,
         ReadROM_i, MatchROM_i, SkipROM_i, CondSearchROM_i, OverdriveSkipROM_i,
         OverdriveMatchROM_i, CondReadROM_i, ResumeROM_i, WriteByte_i,
         ReadByte_i, GetROMID_i, ScanEnable_i, ScanClk_i, ScanDataIn_i;
  output OWOut_o, Noslaves_o, ROMIDArrayToSmall_o, PDROut_o, Ready_o,
         ScanDataOut_o;
endmodule

module pwm_master ( Clk, Reset, Polarity_i, Input_i, Value_o, NewValue_o, 
        ScanEnable_i, ScanClk_i, ScanDataIn_i, ScanDataOut_o );
  parameter Resolution_g = 12;
  parameter CounterWidth_g = 20;
  output [(Resolution_g-1):0] Value_o;
  input Clk, Reset, Polarity_i, Input_i, ScanEnable_i, ScanClk_i, ScanDataIn_i;
  output NewValue_o, ScanDataOut_o;

endmodule

module sent_master ( Clk, Reset, Chipselect_i, NumDatNibble_i, Input_i, 
        MinSync_i, Out_o, NewData_o, CrcOk_o, ScanEnable_i, ScanClk_i, 
        ScanDataIn_i, ScanDataOut_o );
  parameter MaxDatNibble_g = 6;
  parameter CountWidth_g = 16;
  input [2:0] NumDatNibble_i;  // width is RoundUp(Ld(MaxDatNibble_g))
  input [(CountWidth_g-1):0] MinSync_i;
  output [(MaxDatNibble_g*4+3):0] Out_o;
  input Clk, Reset, Chipselect_i, Input_i, ScanEnable_i, ScanClk_i,
         ScanDataIn_i;
  output NewData_o, CrcOk_o, ScanDataOut_o;
endmodule

module spc_master ( Clk, Reset, Input_i, Start_i, NumDatNibble_i, 
        LengthTrigger_i, LengthTimeout_i, MinSync_i, Out_o, DataOut_o, 
        NewData_o, CrcOk_o, SPCReady_o, ScanEnable_i, ScanClk_i, ScanDataIn_i, 
        ScanDataOut_o );
  parameter MaxDatNibble_g = 6;
  parameter CountWidth_g = 16;
  parameter TimeoutWidth_g = 16;
  parameter UseTimeout_g = 1;
  input [2:0] NumDatNibble_i;  // width is RoundUp(Ld(MaxDatNibble_g))
  input [(CountWidth_g-1):0] LengthTrigger_i;
  input [(CountWidth_g-1):0] MinSync_i;
  output [(MaxDatNibble_g*4+3):0] DataOut_o;
  input [(TimeoutWidth_g-1):0] LengthTimeout_i;
  input Clk, Reset, Input_i, Start_i, ScanEnable_i, ScanClk_i, ScanDataIn_i;
  output Out_o, NewData_o, CrcOk_o, SPCReady_o, ScanDataOut_o;
endmodule
