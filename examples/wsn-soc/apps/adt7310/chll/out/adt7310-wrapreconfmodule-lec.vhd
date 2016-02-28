-- Automatically generated: write_netlist -wraprm_lec -vhdl -module adt7310-wrapreconfmodule-lec.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ADT7310 is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    Enable_i : in std_logic;
    CpuIntr_o : out std_logic;
    ADT7310CS_n_o : out std_logic;
    SPI_Data_i : in std_logic_vector(7 downto 0);
    SPI_Write_o : out std_logic;
    SPI_ReadNext_o : out std_logic;
    SPI_Data_o : out std_logic_vector(7 downto 0);
    SPI_FIFOFull_i : in std_logic;
    SPI_FIFOEmpty_i : in std_logic;
    SPI_Transmission_i : in std_logic;
    PeriodCounterPreset_i : in std_logic_vector(15 downto 0);
    SPICounterPresetH_i : in std_logic_vector(15 downto 0);
    SPICounterPresetL_i : in std_logic_vector(15 downto 0);
    SensorValue_o : out std_logic_vector(15 downto 0);
    Threshold_i : in std_logic_vector(15 downto 0);
    SPI_CPOL_o : out std_logic;
    SPI_CPHA_o : out std_logic;
    SPI_LSBFE_o : out std_logic
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
  attribute intersynth_port of ADT7310CS_n_o : signal is "Outputs_o";
  attribute intersynth_conntype of ADT7310CS_n_o : signal is "Bit";
  attribute intersynth_port of SPI_Data_i : signal is "SPI_DataOut";
  attribute intersynth_conntype of SPI_Data_i : signal is "Byte";
  attribute intersynth_port of SPI_Write_o : signal is "SPI_Write";
  attribute intersynth_conntype of SPI_Write_o : signal is "Bit";
  attribute intersynth_port of SPI_ReadNext_o : signal is "SPI_ReadNext";
  attribute intersynth_conntype of SPI_ReadNext_o : signal is "Bit";
  attribute intersynth_port of SPI_Data_o : signal is "SPI_DataIn";
  attribute intersynth_conntype of SPI_Data_o : signal is "Byte";
  attribute intersynth_port of SPI_FIFOFull_i : signal is "SPI_FIFOFull";
  attribute intersynth_conntype of SPI_FIFOFull_i : signal is "Bit";
  attribute intersynth_port of SPI_FIFOEmpty_i : signal is "SPI_FIFOEmpty";
  attribute intersynth_conntype of SPI_FIFOEmpty_i : signal is "Bit";
  attribute intersynth_port of SPI_Transmission_i : signal is "SPI_Transmission";
  attribute intersynth_conntype of SPI_Transmission_i : signal is "Bit";
  attribute intersynth_param of PeriodCounterPreset_i : signal is "PeriodCounterPreset_i";
  attribute intersynth_conntype of PeriodCounterPreset_i : signal is "Word";
  attribute intersynth_param of SPICounterPresetH_i : signal is "SPICounterPresetH_i";
  attribute intersynth_conntype of SPICounterPresetH_i : signal is "Word";
  attribute intersynth_param of SPICounterPresetL_i : signal is "SPICounterPresetL_i";
  attribute intersynth_conntype of SPICounterPresetL_i : signal is "Word";
  attribute intersynth_param of SensorValue_o : signal is "SensorValue_o";
  attribute intersynth_conntype of SensorValue_o : signal is "Word";
  attribute intersynth_param of Threshold_i : signal is "Threshold_i";
  attribute intersynth_conntype of Threshold_i : signal is "Word";
  attribute intersynth_port of SPI_CPOL_o : signal is "SPI_CPOL";
  attribute intersynth_conntype of SPI_CPOL_o : signal is "Bit";
  attribute intersynth_port of SPI_CPHA_o : signal is "SPI_CPHA";
  attribute intersynth_conntype of SPI_CPHA_o : signal is "Bit";
  attribute intersynth_port of SPI_LSBFE_o : signal is "SPI_LSBFE";
  attribute intersynth_conntype of SPI_LSBFE_o : signal is "Bit";
end ADT7310;

architecture WrapReconfModule of ADT7310 is

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
  signal Outputs_s : std_logic_vector(7 downto 0);
  signal AdcDoConvert_s : std_logic;
  signal CfgIntfDOut_s : std_logic_vector(15 downto 0);
  signal I2C_DataIn_s : std_logic_vector(7 downto 0);
  signal I2C_Divider800_s : std_logic_vector(15 downto 0);
  signal I2C_ErrAckParam_s : std_logic;
  signal I2C_F100_400_n_s : std_logic;
  signal I2C_FIFOReadNext_s : std_logic;
  signal I2C_FIFOWrite_s : std_logic;
  signal I2C_ReadCount_s : std_logic_vector(3 downto 0);
  signal I2C_ReceiveSend_n_s : std_logic;
  signal I2C_StartProcess_s : std_logic;
  signal ParamIntfDOut_s : std_logic_vector(15 downto 0);
  signal ReconfModuleOut_s : std_logic_vector(7 downto 0);
  signal SPI_SPPR_SPR_s : std_logic_vector(7 downto 0);

begin

  MyReconfigLogic_0: MyReconfigLogic
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      ReconfModuleIn_i => ReconfModuleIn_s,
      ReconfModuleIRQs_o => ReconfModuleIRQs_s,
      Outputs_o => Outputs_s,
      SPI_DataOut_i => SPI_Data_i,
      SPI_Write_o => SPI_Write_o,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_DataIn_o => SPI_Data_o,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_Transmission_i => SPI_Transmission_i,
      SPI_CPOL_o => SPI_CPOL_o,
      SPI_CPHA_o => SPI_CPHA_o,
      SPI_LSBFE_o => SPI_LSBFE_o,
      AdcConvComplete_i => '0',
      AdcDoConvert_o => AdcDoConvert_s,
      AdcValue_i => "0000000000",
      CfgIntfDOut_o => CfgIntfDOut_s,
      I2C_Busy_i => '0',
      I2C_DataIn_o => I2C_DataIn_s,
      I2C_DataOut_i => "00000000",
      I2C_Divider800_o => I2C_Divider800_s,
      I2C_ErrAckParam_o => I2C_ErrAckParam_s,
      I2C_Error_i => '0',
      I2C_Errors_i => "00000000",
      I2C_F100_400_n_o => I2C_F100_400_n_s,
      I2C_FIFOEmpty_i => '0',
      I2C_FIFOFull_i => '0',
      I2C_FIFOReadNext_o => I2C_FIFOReadNext_s,
      I2C_FIFOWrite_o => I2C_FIFOWrite_s,
      I2C_ReadCount_o => I2C_ReadCount_s,
      I2C_ReceiveSend_n_o => I2C_ReceiveSend_n_s,
      I2C_StartProcess_o => I2C_StartProcess_s,
      Inputs_i => "00000000",
      ParamIntfDOut_o => ParamIntfDOut_s,
      PerAddr_i => "00000000000000",
      PerDIn_i => "0000000000000000",
      PerEn_i => '0',
      PerWr_i => "00",
      ReconfModuleOut_o => ReconfModuleOut_s,
      SPI_SPPR_SPR_o => SPI_SPPR_SPR_s
    );

  CpuIntr_o <= ReconfModuleIRQs_s(0);
  ADT7310CS_n_o <= Outputs_s(0);
  ReconfModuleIn_s <= '0' & '0' & '0' & '0' & '0' & '0' & '0' & Enable_i;

end WrapReconfModule;

