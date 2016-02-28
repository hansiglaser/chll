-- Automatically generated: write_netlist -wraprm_vhdl2008 -vhdl -module slowadt7410-wrapreconfmodule-vhdl2008.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SlowADT7410 is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    Enable_i : in std_logic;
    CpuIntr_o : out std_logic;
    I2C_ReceiveSend_n_o : out std_logic;
    I2C_ReadCount_o : out std_logic_vector(7 downto 0);
    I2C_StartProcess_o : out std_logic;
    I2C_Busy_i : in std_logic;
    I2C_FIFOReadNext_o : out std_logic;
    I2C_FIFOWrite_o : out std_logic;
    I2C_Data_o : out std_logic_vector(7 downto 0);
    I2C_Data_i : in std_logic_vector(7 downto 0);
    I2C_Error_i : in std_logic;
    PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
    PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
    SensorValue_o : out std_logic_vector(15 downto 0);
    Threshold_i : in std_logic_vector(15 downto 0);
    WaitCounterPresetH_i : in std_logic_vector(15 downto 0);
    WaitCounterPresetL_i : in std_logic_vector(15 downto 0)
  );
  attribute intersynth_port : string;
  attribute intersynth_conntype : string;
  attribute intersynth_param : string;
  attribute intersynth_port of Reset_n_i : signal is "Reset_n_i";
  attribute intersynth_port of Clk_i : signal is "Clk_i";
  attribute intersynth_port of Enable_i : signal is "ReconfModuleIn_s";
  attribute intersynth_conntype of Enable_i : signal is "Bit";
  attribute intersynth_port of CpuIntr_o : signal is "ReconfModuleIRQs_s";
  attribute intersynth_conntype of CpuIntr_o : signal is "Bit";
  attribute intersynth_port of I2C_ReceiveSend_n_o : signal is "I2C_ReceiveSend_n";
  attribute intersynth_conntype of I2C_ReceiveSend_n_o : signal is "Bit";
  attribute intersynth_port of I2C_ReadCount_o : signal is "I2C_ReadCount";
  attribute intersynth_conntype of I2C_ReadCount_o : signal is "Byte";
  attribute intersynth_port of I2C_StartProcess_o : signal is "I2C_StartProcess";
  attribute intersynth_conntype of I2C_StartProcess_o : signal is "Bit";
  attribute intersynth_port of I2C_Busy_i : signal is "I2C_Busy";
  attribute intersynth_conntype of I2C_Busy_i : signal is "Bit";
  attribute intersynth_port of I2C_FIFOReadNext_o : signal is "I2C_FIFOReadNext";
  attribute intersynth_conntype of I2C_FIFOReadNext_o : signal is "Bit";
  attribute intersynth_port of I2C_FIFOWrite_o : signal is "I2C_FIFOWrite";
  attribute intersynth_conntype of I2C_FIFOWrite_o : signal is "Bit";
  attribute intersynth_port of I2C_Data_o : signal is "I2C_DataIn";
  attribute intersynth_conntype of I2C_Data_o : signal is "Byte";
  attribute intersynth_port of I2C_Data_i : signal is "I2C_DataOut";
  attribute intersynth_conntype of I2C_Data_i : signal is "Byte";
  attribute intersynth_port of I2C_Error_i : signal is "I2C_Error";
  attribute intersynth_conntype of I2C_Error_i : signal is "Bit";
  attribute intersynth_param of PeriodCounterPresetH_i : signal is "PeriodCounterPresetH_i";
  attribute intersynth_conntype of PeriodCounterPresetH_i : signal is "Word";
  attribute intersynth_param of PeriodCounterPresetL_i : signal is "PeriodCounterPresetL_i";
  attribute intersynth_conntype of PeriodCounterPresetL_i : signal is "Word";
  attribute intersynth_param of SensorValue_o : signal is "SensorValue_o";
  attribute intersynth_conntype of SensorValue_o : signal is "Word";
  attribute intersynth_param of Threshold_i : signal is "Threshold_i";
  attribute intersynth_conntype of Threshold_i : signal is "Word";
  attribute intersynth_param of WaitCounterPresetH_i : signal is "WaitCounterPresetH_i";
  attribute intersynth_conntype of WaitCounterPresetH_i : signal is "Word";
  attribute intersynth_param of WaitCounterPresetL_i : signal is "WaitCounterPresetL_i";
  attribute intersynth_conntype of WaitCounterPresetL_i : signal is "Word";
end SlowADT7410;

architecture WrapReconfModule of SlowADT7410 is

  component MyReconfigLogic
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      AdcConvComplete_i : in std_logic;
      AdcDoConvert_o : out std_logic;
      AdcValue_i : in std_logic_vector(9 downto 0);
      I2C_Busy_i : in std_logic;
      I2C_DataIn_o : out std_logic_vector(7 downto 0);
      I2C_DataOut_i : in std_logic_vector(7 downto 0);
      I2C_Divider800_o : out std_logic_vector(15 downto 0);
      I2C_ErrAckParam_o : out std_logic;
      I2C_Error_i : in std_logic;
      I2C_F100_400_n_o : out std_logic;
      I2C_FIFOEmpty_i : in std_logic;
      I2C_FIFOFull_i : in std_logic;
      I2C_FIFOReadNext_o : out std_logic;
      I2C_FIFOWrite_o : out std_logic;
      I2C_ReadCount_o : out std_logic_vector(3 downto 0);
      I2C_ReceiveSend_n_o : out std_logic;
      I2C_StartProcess_o : out std_logic;
      Inputs_i : in std_logic_vector(7 downto 0);
      Outputs_o : out std_logic_vector(7 downto 0);
      ReconfModuleIRQs_o : out std_logic_vector(4 downto 0);
      SPI_CPHA_o : out std_logic;
      SPI_CPOL_o : out std_logic;
      SPI_DataIn_o : out std_logic_vector(7 downto 0);
      SPI_DataOut_i : in std_logic_vector(7 downto 0);
      SPI_FIFOEmpty_i : in std_logic;
      SPI_FIFOFull_i : in std_logic;
      SPI_LSBFE_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_SPPR_SPR_o : out std_logic_vector(7 downto 0);
      SPI_Transmission_i : in std_logic;
      SPI_Write_o : out std_logic;
      ReconfModuleIn_i : in std_logic_vector(7 downto 0);
      ReconfModuleOut_o : out std_logic_vector(7 downto 0);
      I2C_Errors_i : in std_logic_vector(7 downto 0);
      PerAddr_i : in std_logic_vector(13 downto 0);
      PerDIn_i : in std_logic_vector(15 downto 0);
      PerWr_i : in std_logic_vector(1 downto 0);
      PerEn_i : in std_logic;
      CfgIntfDOut_o : out std_logic_vector(15 downto 0);
      ParamIntfDOut_o : out std_logic_vector(15 downto 0)
    );
  end component;

  signal ReconfModuleIn_s : std_logic_vector(7 downto 0);
  signal ReconfModuleIRQs_s : std_logic_vector(4 downto 0);
  signal I2C_ReadCount_s : std_logic_vector(3 downto 0);
  signal AdcDoConvert_s : std_logic;
  signal CfgIntfDOut_s : std_logic_vector(15 downto 0);
  signal I2C_Divider800_s : std_logic_vector(15 downto 0);
  signal I2C_ErrAckParam_s : std_logic;
  signal I2C_F100_400_n_s : std_logic;
  signal Outputs_s : std_logic_vector(7 downto 0);
  signal ParamIntfDOut_s : std_logic_vector(15 downto 0);
  signal ReconfModuleOut_s : std_logic_vector(7 downto 0);
  signal SPI_CPHA_s : std_logic;
  signal SPI_CPOL_s : std_logic;
  signal SPI_DataIn_s : std_logic_vector(7 downto 0);
  signal SPI_LSBFE_s : std_logic;
  signal SPI_ReadNext_s : std_logic;
  signal SPI_SPPR_SPR_s : std_logic_vector(7 downto 0);
  signal SPI_Write_s : std_logic;

begin

  MyReconfigLogic_0: MyReconfigLogic
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      ReconfModuleIn_i => ReconfModuleIn_s,
      ReconfModuleIRQs_o => ReconfModuleIRQs_s,
      I2C_ReceiveSend_n_o => I2C_ReceiveSend_n_o,
      I2C_ReadCount_o => I2C_ReadCount_s,
      I2C_StartProcess_o => I2C_StartProcess_o,
      I2C_Busy_i => I2C_Busy_i,
      I2C_FIFOReadNext_o => I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => I2C_FIFOWrite_o,
      I2C_DataIn_o => I2C_Data_o,
      I2C_DataOut_i => I2C_Data_i,
      I2C_Error_i => I2C_Error_i,
      AdcConvComplete_i => '0',
      AdcDoConvert_o => AdcDoConvert_s,
      AdcValue_i => "0000000000",
      CfgIntfDOut_o => CfgIntfDOut_s,
      I2C_Divider800_o => I2C_Divider800_s,
      I2C_ErrAckParam_o => I2C_ErrAckParam_s,
      I2C_Errors_i => "00000000",
      I2C_F100_400_n_o => I2C_F100_400_n_s,
      I2C_FIFOEmpty_i => '0',
      I2C_FIFOFull_i => '0',
      Inputs_i => "00000000",
      Outputs_o => Outputs_s,
      ParamIntfDOut_o => ParamIntfDOut_s,
      PerAddr_i => "00000000000000",
      PerDIn_i => "0000000000000000",
      PerEn_i => '0',
      PerWr_i => "00",
      ReconfModuleOut_o => ReconfModuleOut_s,
      SPI_CPHA_o => SPI_CPHA_s,
      SPI_CPOL_o => SPI_CPOL_s,
      SPI_DataIn_o => SPI_DataIn_s,
      SPI_DataOut_i => "00000000",
      SPI_FIFOEmpty_i => '0',
      SPI_FIFOFull_i => '0',
      SPI_LSBFE_o => SPI_LSBFE_s,
      SPI_ReadNext_o => SPI_ReadNext_s,
      SPI_SPPR_SPR_o => SPI_SPPR_SPR_s,
      SPI_Transmission_i => '0',
      SPI_Write_o => SPI_Write_s
    );

  CpuIntr_o <= ReconfModuleIRQs_s(0);
  I2C_ReadCount_o <= "0000" & I2C_ReadCount_s;
  << signal MyReconfigLogic_0.ParamIn_Word_4_s : std_logic_vector(15 downto 0) >> <= PeriodCounterPresetH_i;
  << signal MyReconfigLogic_0.ParamIn_Word_1_s : std_logic_vector(15 downto 0) >> <= PeriodCounterPresetL_i;
  SensorValue_o <= << signal MyReconfigLogic_0.ParamOut_Word_0_s : std_logic_vector(15 downto 0) >>;
  << signal MyReconfigLogic_0.ParamIn_Word_2_s : std_logic_vector(15 downto 0) >> <= Threshold_i;
  << signal MyReconfigLogic_0.ParamIn_Word_3_s : std_logic_vector(15 downto 0) >> <= WaitCounterPresetH_i;
  << signal MyReconfigLogic_0.ParamIn_Word_0_s : std_logic_vector(15 downto 0) >> <= WaitCounterPresetL_i;
  << signal MyReconfigLogic_0.I2C_Divider800_o : std_logic_vector(15 downto 0) >> <= "0000000001111100";
  << signal MyReconfigLogic_0.I2C_ErrAckParam_o : std_logic >> <= '0';
  ReconfModuleIn_s <= '0' & '0' & '0' & '0' & '0' & '0' & '0' & Enable_i;

end WrapReconfModule;

