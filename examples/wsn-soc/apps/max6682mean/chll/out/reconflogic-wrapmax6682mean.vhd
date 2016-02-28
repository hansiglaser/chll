-- Automatically generated: write_netlist -wrapapp -vhdl -module reconflogic-wrapmax6682mean.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MyReconfigLogic is
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
end MyReconfigLogic;

architecture WrapMAX6682Mean of MyReconfigLogic is

  component CfgIntf
    generic (
      -- Number of configuration chains
      NumCfgs : integer := 3;
      BaseAddr : integer := 16#0180#
    );
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      -- OpenMSP430 Interface
      PerAddr_i : in std_logic_vector(13 downto 0);
      PerDIn_i : in std_logic_vector(15 downto 0);
      PerDOut_o : out std_logic_vector(15 downto 0);
      PerWr_i : in std_logic_vector(1 downto 0);
      PerEn_i : in std_logic;
      CfgClk_o : out std_logic_vector(NumCfgs-1 downto 0);
      CfgMode_o : out std_logic;
      CfgShift_o : out std_logic_vector(NumCfgs-1 downto 0);
      CfgDataOut_o : out std_logic;
      CfgDataIn_i : in std_logic_vector(NumCfgs-1 downto 0)
    );
  end component;

  component MAX6682Mean
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
      PauseCounterPreset_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ParamIntf
    generic (
      WrAddrWidth : integer range 1 to 15 := 4;
      RdAddrWidth : integer range 1 to 15 := 4;
      BaseAddr : integer := 16#0180#
    );
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      -- OpenMSP430 Interface
      PerAddr_i : in std_logic_vector(13 downto 0);
      PerDIn_i : in std_logic_vector(15 downto 0);
      PerDOut_o : out std_logic_vector(15 downto 0);
      PerWr_i : in std_logic_vector(1 downto 0);
      PerEn_i : in std_logic;
      -- Param Out
      ParamWrAddr_o : out std_logic_vector(WrAddrWidth-1 downto 0);
      ParamWrData_o : out std_logic_vector(15 downto 0);
      ParamWr_o : out std_logic;
      -- Param In
      ParamRdAddr_o : out std_logic_vector(RdAddrWidth-1 downto 0);
      ParamRdData_i : in std_logic_vector(15 downto 0)
    );
  end component;

  component ParamOutReg
    generic (
      Width : integer := 16
    );
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      ParamWrData_i : in std_logic_vector(Width-1 downto 0);
      Param_o : out std_logic_vector(Width-1 downto 0)
    );
  end component;

  signal PauseCounterPreset_s : std_logic_vector(15 downto 0);
  signal PeriodCounterPresetH_s : std_logic_vector(15 downto 0);
  signal PeriodCounterPresetL_s : std_logic_vector(15 downto 0);
  signal SensorValue_s : std_logic_vector(15 downto 0);
  signal Threshold_s : std_logic_vector(15 downto 0);
  signal CfgClk_s : std_logic_vector(0 downto 0);
  signal CfgMode_s : std_logic;
  signal CfgShift_s : std_logic_vector(0 downto 0);
  signal CfgDataOut_s : std_logic;
  signal CfgDataIn_s : std_logic_vector(0 downto 0);
  signal ParamWrAddr_s : std_logic_vector(2 downto 0);
  signal ParamWrData_s : std_logic_vector(15 downto 0);
  signal ParamWr_s : std_logic;
  signal ParamRdAddr_s : std_logic_vector(0 downto 0);
  signal ParamRdData_s : std_logic_vector(15 downto 0);


  type Params_t is array(0 to 1) of std_logic_vector(15 downto 0);

  signal Params_s : Params_t;
  signal I2C_ErrAckParam_s : std_logic_vector(0 downto 0);
  signal ParamI2C_Divider800Enable_s : std_logic;
  signal ParamI2C_ErrAckParamEnable_s : std_logic;
  signal ParamPauseCounterPresetEnable_s : std_logic;
  signal ParamPeriodCounterPresetHEnable_s : std_logic;
  signal ParamPeriodCounterPresetLEnable_s : std_logic;
  signal ParamThresholdEnable_s : std_logic;

begin

  -- Configuration Interface
  CfgIntf_0: CfgIntf
    generic map (
      BaseAddr => 16#0180#,
      NumCfgs => 1
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      PerAddr_i => PerAddr_i,
      PerDIn_i => PerDIn_i,
      PerDOut_o => CfgIntfDOut_o,
      PerWr_i => PerWr_i,
      PerEn_i => PerEn_i,
      CfgClk_o => CfgClk_s,
      CfgMode_o => CfgMode_s,
      CfgShift_o => CfgShift_s,
      CfgDataOut_o => CfgDataOut_s,
      CfgDataIn_i => CfgDataIn_s
    );


  -- Parameterization Interface: 6 write addresses, 2 read addresses
  ParamIntf_0: ParamIntf
    generic map (
      BaseAddr => 16#0188#,
      WrAddrWidth => 3,
      RdAddrWidth => 1
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      PerAddr_i => PerAddr_i,
      PerDIn_i => PerDIn_i,
      PerDOut_o => ParamIntfDOut_o,
      PerWr_i => PerWr_i,
      PerEn_i => PerEn_i,
      ParamWrAddr_o => ParamWrAddr_s,
      ParamWrData_o => ParamWrData_s,
      ParamWr_o => ParamWr_s,
      ParamRdAddr_o => ParamRdAddr_s,
      ParamRdData_i => ParamRdData_s
    );


  MAX6682Mean_0: MAX6682Mean
    port map (
      MAX6682CS_n_o => Outputs_o(0),
      CpuIntr_o => ReconfModuleIRQs_o(0),
      SPI_Data_o => SPI_DataIn_o,
      SPI_Data_i => SPI_DataOut_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_Transmission_i => SPI_Transmission_i,
      SPI_Write_o => SPI_Write_o,
      Enable_i => ReconfModuleIn_i(0),
      Clk_i => Clk_i,
      Reset_n_i => Reset_n_i,
      PauseCounterPreset_i => PauseCounterPreset_s,
      PeriodCounterPresetH_i => PeriodCounterPresetH_s,
      PeriodCounterPresetL_i => PeriodCounterPresetL_s,
      SensorValue_o => SensorValue_s,
      Threshold_i => Threshold_s
    );

  AdcDoConvert_o <= '0';
  I2C_DataIn_o <= "00000000";
  I2C_F100_400_n_o <= '0';
  I2C_FIFOReadNext_o <= '0';
  I2C_FIFOWrite_o <= '0';
  I2C_ReadCount_o <= "0000";
  I2C_ReceiveSend_n_o <= '0';
  I2C_StartProcess_o <= '0';
  Outputs_o(1) <= '0';
  Outputs_o(2) <= '0';
  Outputs_o(3) <= '0';
  Outputs_o(4) <= '0';
  Outputs_o(5) <= '0';
  Outputs_o(6) <= '0';
  Outputs_o(7) <= '0';
  ReconfModuleIRQs_o(1) <= '0';
  ReconfModuleIRQs_o(2) <= '0';
  ReconfModuleIRQs_o(3) <= '0';
  ReconfModuleIRQs_o(4) <= '0';
  SPI_CPHA_o <= '0';
  SPI_CPOL_o <= '0';
  SPI_LSBFE_o <= '0';
  SPI_SPPR_SPR_o <= "00000000";
  ReconfModuleOut_o(0) <= '0';
  ReconfModuleOut_o(1) <= '0';
  ReconfModuleOut_o(2) <= '0';
  ReconfModuleOut_o(3) <= '0';
  ReconfModuleOut_o(4) <= '0';
  ReconfModuleOut_o(5) <= '0';
  ReconfModuleOut_o(6) <= '0';
  ReconfModuleOut_o(7) <= '0';
  -- just a fixed value for the config interface
  CfgDataIn_s <= "0";
  -- Param read address decoder
  -- Synthesis: Accept undefined behavior if ParamRdAddr_s >= NumParams and
  --   hope that the synthesis optimizes the MUX
  -- Simulation: ModelSim complains "Fatal: (vsim-3421) Value x is out of range
  --   0 to n.", even during param write cycles, because ParamRdAddr has the
  --   source as ParamWrAddr. Use the parameter "-noindexcheck" during
  --   compilation ("vcom"). Simulation works fine then, but ModelSim generates
  --   numerous "INTERNAL ERROR"s to stdout, which seem harmless.
  ParamRdData_s <= Params_s(to_integer(unsigned(ParamRdAddr_s)));


  ParamOutReg_I2C_Divider800: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => I2C_Divider800_o,
      Enable_i => ParamI2C_Divider800Enable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_I2C_ErrAckParam: ParamOutReg
    generic map (
      Width => 1
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => I2C_ErrAckParam_s,
      Enable_i => ParamI2C_ErrAckParamEnable_s,
      ParamWrData_i => ParamWrData_s(0 downto 0)
    );


  ParamOutReg_PauseCounterPreset: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => PauseCounterPreset_s,
      Enable_i => ParamPauseCounterPresetEnable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_PeriodCounterPresetH: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => PeriodCounterPresetH_s,
      Enable_i => ParamPeriodCounterPresetHEnable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_PeriodCounterPresetL: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => PeriodCounterPresetL_s,
      Enable_i => ParamPeriodCounterPresetLEnable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_Threshold: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => Threshold_s,
      Enable_i => ParamThresholdEnable_s,
      ParamWrData_i => ParamWrData_s
    );

  I2C_ErrAckParam_o <= I2C_ErrAckParam_s(0);
  -- Address $00
  Params_s(0) <= "00000000" & I2C_Errors_i;
  -- Address $01
  Params_s(1) <= SensorValue_s;
  -- Address $00
  ParamI2C_Divider800Enable_s <= ParamWr_s when ParamWrAddr_s = "000" else 
                                 '0';
  -- Address $01
  ParamI2C_ErrAckParamEnable_s <= ParamWr_s when ParamWrAddr_s = "001" else 
                                  '0';
  -- Address $02
  ParamPauseCounterPresetEnable_s <= ParamWr_s when ParamWrAddr_s = "010" else 
                                     '0';
  -- Address $03
  ParamPeriodCounterPresetHEnable_s <= ParamWr_s when ParamWrAddr_s = "011" else 
                                       '0';
  -- Address $04
  ParamPeriodCounterPresetLEnable_s <= ParamWr_s when ParamWrAddr_s = "100" else 
                                       '0';
  -- Address $05
  ParamThresholdEnable_s <= ParamWr_s when ParamWrAddr_s = "101" else 
                            '0';

end WrapMAX6682Mean;

