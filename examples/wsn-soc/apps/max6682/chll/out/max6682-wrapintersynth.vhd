-- Automatically generated: write_netlist -wrapis -vhdl -module max6682-wrapintersynth.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MAX6682 is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    Enable_i : in std_logic;
    CpuIntr_o : out std_logic;
    MAX6682CS_n_o : out std_logic;
    SPI_Data_i : in std_logic_vector(7 downto 0);
    SPI_Write_o : out std_logic;
    SPI_ReadNext_o : out std_logic;
    SPI_Data_o : out std_logic_vector(7 downto 0);
    SPI_FIFOFull_i : in std_logic;
    SPI_FIFOEmpty_i : in std_logic;
    SPI_Transmission_i : in std_logic;
    PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
    PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
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
  attribute intersynth_port of MAX6682CS_n_o : signal is "Outputs_o";
  attribute intersynth_conntype of MAX6682CS_n_o : signal is "Bit";
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
  attribute intersynth_param of PeriodCounterPresetH_i : signal is "PeriodCounterPresetH_i";
  attribute intersynth_conntype of PeriodCounterPresetH_i : signal is "Word";
  attribute intersynth_param of PeriodCounterPresetL_i : signal is "PeriodCounterPresetL_i";
  attribute intersynth_conntype of PeriodCounterPresetL_i : signal is "Word";
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
end MAX6682;

architecture WrapInterSynth of MAX6682 is

  component MyInterSynthModule
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      bitdata : in std_logic_vector(1281 downto 0);
      AdcConvComplete_i : in std_logic;
      AdcDoConvert_o : out std_logic;
      AdcValue_i : in std_logic_vector(15 downto 0);
      I2C_Busy_i : in std_logic;
      I2C_DataIn_o : out std_logic_vector(7 downto 0);
      I2C_DataOut_i : in std_logic_vector(7 downto 0);
      I2C_Error_i : in std_logic;
      I2C_FIFOEmpty_i : in std_logic;
      I2C_FIFOFull_i : in std_logic;
      I2C_FIFOReadNext_o : out std_logic;
      I2C_FIFOWrite_o : out std_logic;
      I2C_ReadCount_o : out std_logic_vector(7 downto 0);
      I2C_ReceiveSend_n_o : out std_logic;
      I2C_StartProcess_o : out std_logic;
      Inputs_i_0 : in std_logic;
      Inputs_i_1 : in std_logic;
      Inputs_i_2 : in std_logic;
      Inputs_i_3 : in std_logic;
      Inputs_i_4 : in std_logic;
      Inputs_i_5 : in std_logic;
      Inputs_i_6 : in std_logic;
      Inputs_i_7 : in std_logic;
      Outputs_o_0 : out std_logic;
      Outputs_o_1 : out std_logic;
      Outputs_o_2 : out std_logic;
      Outputs_o_3 : out std_logic;
      Outputs_o_4 : out std_logic;
      Outputs_o_5 : out std_logic;
      Outputs_o_6 : out std_logic;
      Outputs_o_7 : out std_logic;
      ReconfModuleIRQs_o_0 : out std_logic;
      ReconfModuleIRQs_o_1 : out std_logic;
      ReconfModuleIRQs_o_2 : out std_logic;
      ReconfModuleIRQs_o_3 : out std_logic;
      ReconfModuleIRQs_o_4 : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_CPOL_o : out std_logic;
      SPI_DataIn_o : out std_logic_vector(7 downto 0);
      SPI_DataOut_i : in std_logic_vector(7 downto 0);
      SPI_FIFOEmpty_i : in std_logic;
      SPI_FIFOFull_i : in std_logic;
      SPI_LSBFE_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_Transmission_i : in std_logic;
      SPI_Write_o : out std_logic;
      ReconfModuleIn_i_0 : in std_logic;
      ReconfModuleIn_i_1 : in std_logic;
      ReconfModuleIn_i_2 : in std_logic;
      ReconfModuleIn_i_3 : in std_logic;
      ReconfModuleIn_i_4 : in std_logic;
      ReconfModuleIn_i_5 : in std_logic;
      ReconfModuleIn_i_6 : in std_logic;
      ReconfModuleIn_i_7 : in std_logic;
      ReconfModuleOut_o_0 : out std_logic;
      ReconfModuleOut_o_1 : out std_logic;
      ReconfModuleOut_o_2 : out std_logic;
      ReconfModuleOut_o_3 : out std_logic;
      ReconfModuleOut_o_4 : out std_logic;
      ReconfModuleOut_o_5 : out std_logic;
      ReconfModuleOut_o_6 : out std_logic;
      ReconfModuleOut_o_7 : out std_logic;
      ParamIn_Word_i : in std_logic_vector(79 downto 0);
      ParamOut_Word_o : out std_logic_vector(31 downto 0);
      CfgMode_i : in std_logic;
      CfgClk_TRFSM0_i : in std_logic;
      CfgClk_TRFSM1_i : in std_logic;
      CfgShift_TRFSM0_i : in std_logic;
      CfgShift_TRFSM1_i : in std_logic;
      CfgDataIn_i : in std_logic;
      CfgDataOut_TRFSM0_o : out std_logic;
      CfgDataOut_TRFSM1_o : out std_logic
    );
  end component;

  signal ParamIn_Word_s : std_logic_vector(79 downto 0);
  signal ParamOut_Word_s : std_logic_vector(31 downto 0);
  signal BitData_s : std_logic_vector(1281 downto 0);
  signal CfgDataOut_TRFSM0_s : std_logic;
  signal CfgDataOut_TRFSM1_s : std_logic;
  signal AdcDoConvert_o_s : std_logic;
  signal I2C_DataIn_o_s : std_logic_vector(7 downto 0);
  signal I2C_FIFOReadNext_o_s : std_logic;
  signal I2C_FIFOWrite_o_s : std_logic;
  signal I2C_ReadCount_o_s : std_logic_vector(7 downto 0);
  signal I2C_ReceiveSend_n_o_s : std_logic;
  signal I2C_StartProcess_o_s : std_logic;
  signal Outputs_o_1_s : std_logic;
  signal Outputs_o_2_s : std_logic;
  signal Outputs_o_3_s : std_logic;
  signal Outputs_o_4_s : std_logic;
  signal Outputs_o_5_s : std_logic;
  signal Outputs_o_6_s : std_logic;
  signal Outputs_o_7_s : std_logic;
  signal ReconfModuleIRQs_o_1_s : std_logic;
  signal ReconfModuleIRQs_o_2_s : std_logic;
  signal ReconfModuleIRQs_o_3_s : std_logic;
  signal ReconfModuleIRQs_o_4_s : std_logic;
  signal ReconfModuleOut_o_0_s : std_logic;
  signal ReconfModuleOut_o_1_s : std_logic;
  signal ReconfModuleOut_o_2_s : std_logic;
  signal ReconfModuleOut_o_3_s : std_logic;
  signal ReconfModuleOut_o_4_s : std_logic;
  signal ReconfModuleOut_o_5_s : std_logic;
  signal ReconfModuleOut_o_6_s : std_logic;
  signal ReconfModuleOut_o_7_s : std_logic;

begin

  MyInterSynthModule_0: MyInterSynthModule
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      ReconfModuleIn_i_0 => Enable_i,
      ReconfModuleIRQs_o_0 => CpuIntr_o,
      Outputs_o_0 => MAX6682CS_n_o,
      SPI_DataOut_i => SPI_Data_i,
      SPI_Write_o => SPI_Write_o,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_DataIn_o => SPI_Data_o,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_Transmission_i => SPI_Transmission_i,
      ParamIn_Word_i => ParamIn_Word_s,
      ParamOut_Word_o => ParamOut_Word_s,
      SPI_CPOL_o => SPI_CPOL_o,
      SPI_CPHA_o => SPI_CPHA_o,
      SPI_LSBFE_o => SPI_LSBFE_o,
      bitdata => BitData_s,
      CfgMode_i => '0',
      CfgClk_TRFSM0_i => '0',
      CfgClk_TRFSM1_i => '0',
      CfgShift_TRFSM0_i => '0',
      CfgShift_TRFSM1_i => '0',
      CfgDataIn_i => '0',
      CfgDataOut_TRFSM0_o => CfgDataOut_TRFSM0_s,
      CfgDataOut_TRFSM1_o => CfgDataOut_TRFSM1_s,
      AdcConvComplete_i => '0',
      AdcDoConvert_o => AdcDoConvert_o_s,
      AdcValue_i => "0000000000000000",
      I2C_Busy_i => '0',
      I2C_DataIn_o => I2C_DataIn_o_s,
      I2C_DataOut_i => "00000000",
      I2C_Error_i => '0',
      I2C_FIFOEmpty_i => '0',
      I2C_FIFOFull_i => '0',
      I2C_FIFOReadNext_o => I2C_FIFOReadNext_o_s,
      I2C_FIFOWrite_o => I2C_FIFOWrite_o_s,
      I2C_ReadCount_o => I2C_ReadCount_o_s,
      I2C_ReceiveSend_n_o => I2C_ReceiveSend_n_o_s,
      I2C_StartProcess_o => I2C_StartProcess_o_s,
      Inputs_i_0 => '0',
      Inputs_i_1 => '0',
      Inputs_i_2 => '0',
      Inputs_i_3 => '0',
      Inputs_i_4 => '0',
      Inputs_i_5 => '0',
      Inputs_i_6 => '0',
      Inputs_i_7 => '0',
      Outputs_o_1 => Outputs_o_1_s,
      Outputs_o_2 => Outputs_o_2_s,
      Outputs_o_3 => Outputs_o_3_s,
      Outputs_o_4 => Outputs_o_4_s,
      Outputs_o_5 => Outputs_o_5_s,
      Outputs_o_6 => Outputs_o_6_s,
      Outputs_o_7 => Outputs_o_7_s,
      ReconfModuleIRQs_o_1 => ReconfModuleIRQs_o_1_s,
      ReconfModuleIRQs_o_2 => ReconfModuleIRQs_o_2_s,
      ReconfModuleIRQs_o_3 => ReconfModuleIRQs_o_3_s,
      ReconfModuleIRQs_o_4 => ReconfModuleIRQs_o_4_s,
      ReconfModuleIn_i_1 => '0',
      ReconfModuleIn_i_2 => '0',
      ReconfModuleIn_i_3 => '0',
      ReconfModuleIn_i_4 => '0',
      ReconfModuleIn_i_5 => '0',
      ReconfModuleIn_i_6 => '0',
      ReconfModuleIn_i_7 => '0',
      ReconfModuleOut_o_0 => ReconfModuleOut_o_0_s,
      ReconfModuleOut_o_1 => ReconfModuleOut_o_1_s,
      ReconfModuleOut_o_2 => ReconfModuleOut_o_2_s,
      ReconfModuleOut_o_3 => ReconfModuleOut_o_3_s,
      ReconfModuleOut_o_4 => ReconfModuleOut_o_4_s,
      ReconfModuleOut_o_5 => ReconfModuleOut_o_5_s,
      ReconfModuleOut_o_6 => ReconfModuleOut_o_6_s,
      ReconfModuleOut_o_7 => ReconfModuleOut_o_7_s
    );

  ParamIn_Word_s(63 downto 48) <= PeriodCounterPresetH_i;
  ParamIn_Word_s(15 downto 0) <= PeriodCounterPresetL_i;
  SensorValue_o <= ParamOut_Word_s(15 downto 0);
  ParamIn_Word_s(47 downto 32) <= Threshold_i;
  ParamIn_Word_s(31 downto 16) <= "0000000000000000";
  ParamIn_Word_s(79 downto 64) <= "0000000000000000";
  BitData_s <= "0000000000000000000000000000000000000000000000000000000000000000101011010100000000000000100000100000000000000001100000011000000000000011000000000000000000000000000000000010001000100000000000000000000000000000000000000000000011000000000000000000000000000100100000000000000000000000000000000000000000000000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010110110100000000000000000000000000000000000000110101011101001010100010000000000000000000000000000110001100000000000000000000000000000000000000000100011001000110001100011000110001100011000110001101101010000100010000000000010010100000000110100100000000000000000000000000000011011000010001111001110000000000000000000000000000000001000000000000000000000000000000011000100110100000000000000000000001000000010000000000000000000000010100000000000000000000000000000000110100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000001011000000000011100000000000000000000000000000001101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";

end WrapInterSynth;

