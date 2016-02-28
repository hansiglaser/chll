architecture TestParamIntf of MyReconfigLogic is

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
      ParamWrData_i : in std_logic_vector(15 downto 0);
      Param_o : out std_logic_vector(15 downto 0)
    );
  end component;

  signal ParamWrAddr_s : std_logic_vector(1 downto 0);
  signal ParamWrData_s : std_logic_vector(15 downto 0);
  signal ParamWr_s : std_logic;
  signal ParamRdAddr_s : std_logic_vector(1 downto 0);
  signal ParamRdData_s : std_logic_vector(15 downto 0);

  type Params_t is array(0 to 3) of std_logic_vector(15 downto 0);
  signal Params_s : Params_t;
  signal Param0Enable_s : std_logic;
  signal Param1Enable_s : std_logic;
  signal Param2Enable_s : std_logic;
  signal Param3Enable_s : std_logic;

begin

  -- Parameterization Interface
  ParamIntf_0: ParamIntf
    generic map (
      BaseAddr => 16#0188#,
      WrAddrWidth => 2,
      RdAddrWidth => 2
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

  AdcDoConvert_o <= '0';
  I2C_DataIn_o <= "00000000";
  I2C_ErrAckParam_o <= '0';
  I2C_F100_400_n_o <= '0';
  I2C_FIFOReadNext_o <= '0';
  I2C_FIFOWrite_o <= '0';
  I2C_ReceiveSend_n_o <= '0';
  I2C_StartProcess_o <= '0';
  I2C_ReadCount_o <= (others => '0');
--  OneWire_CondReadROM_o <= '0';
--  OneWire_CondSearchROM_o <= '0';
--  OneWire_DataIn_o <= "00000000";
--  OneWire_DeactivateOverdriveMode_o <= '0';
--  OneWire_GetROMID_o <= '0';
--  OneWire_MatchROM_o <= '0';
--  OneWire_OWReset_o <= '0';
--  OneWire_OverdriveMatchROM_o <= '0';
--  OneWire_OverdriveSkipROM_o <= '0';
--  OneWire_ReadByte_o <= '0';
--  OneWire_ReadROM_o <= '0';
--  OneWire_ResumeROM_o <= '0';
--  OneWire_SearchROM_o <= '0';
--  OneWire_SkipROM_o <= '0';
--  OneWire_WriteByte_o <= '0';
  Outputs_o <= (others => '0');
--  PWM_Polarity_o <= '0';
--  SENT_Chipselect_o <= '0';
--  SENT_OutMUX_o(0) <= '0';
--  SENT_OutMUX_o(1) <= '0';
--  SENT_NumDatNibble_o <= (others => '0');
--  SPC_OutMUX_o(0) <= '0';
--  SPC_OutMUX_o(1) <= '0';
--  SPC_Start_o <= '0';
--  SPC_NumDatNibble_o <= (others => '0');
  SPI_SPPR_SPR_o <= "00000000";
  ReconfModuleIRQs_o <= (others => '0');
  ReconfModuleOut_o <= (others => '0');
  CfgIntfDOut_o <= (others => '0');
  I2C_Divider800_o <= (others => '0');
  SPI_CPHA_o <= '0';
  SPI_CPOL_o <= '0';
  SPI_DataIn_o <= (others => '0');
  SPI_LSBFE_o <= '0';
  SPI_ReadNext_o <= '0';
  SPI_Write_o <= '0';
--  OneWire_ODRDSlotInitTime_o <= (others => '0');
--  OneWire_ODRDSlotSampleTime_o <= (others => '0');
--  OneWire_ODResetLowTime_o <= (others => '0');
--  OneWire_ODResetPrecenceIntervalDuration_o <= (others => '0');
--  OneWire_ODResetTime_o <= (others => '0');
--  OneWire_ODResetWaitForDetectionDuration_o <= (others => '0');
--  OneWire_ODSlotDuration_o <= (others => '0');
--  OneWire_ODSlotLowDataTime_o <= (others => '0');
--  OneWire_ODWRSlotHighDataTime_o <= (others => '0');
--  OneWire_RDSlotInitTime_o <= (others => '0');
--  OneWire_RDSlotSampleTime_o <= (others => '0');
--  OneWire_ResetLowTime_o <= (others => '0');
--  OneWire_ResetPrecenceIntervalDuration_o <= (others => '0');
--  OneWire_ResetTime_o <= (others => '0');
--  OneWire_ResetWaitForDetectionDuration_o <= (others => '0');
--  OneWire_SlotDuration_o <= (others => '0');
--  OneWire_SlotLowDataTime_o <= (others => '0');
--  OneWire_WRSlotHighDataTime_o <= (others => '0');
--  SENT_MinSync_o <= (others => '0');
--  SPC_LengthTimeout_o <= (others => '0');
--  SPC_LengthTrigger_o <= (others => '0');
--  SPC_MinSync_o <= (others => '0');

  -- Param read address decoder
  ParamRdData_s <= Params_s(to_integer(unsigned(ParamRdAddr_s)));

  -- Address $00
  Param0Enable_s <= ParamWr_s when ParamWrAddr_s = "00" else 
                                 '0';

  ParamOutReg_Param0: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Param0Enable_s,
      ParamWrData_i => ParamWrData_s,
      Param_o => Params_s(0)
    );

  -- Address $01
  Param1Enable_s <= ParamWr_s when ParamWrAddr_s = "01" else 
                                 '0';

  ParamOutReg_Param1: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Param1Enable_s,
      ParamWrData_i => ParamWrData_s,
      Param_o => Params_s(1)
    );

  -- Address $02
  Param2Enable_s <= ParamWr_s when ParamWrAddr_s = "10" else 
                                 '0';

  ParamOutReg_Param2: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Param2Enable_s,
      ParamWrData_i => ParamWrData_s,
      Param_o => Params_s(2)
    );

  -- Address $03
  Param3Enable_s <= ParamWr_s when ParamWrAddr_s = "11" else 
                                 '0';

  ParamOutReg_Param3: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Param3Enable_s,
      ParamWrData_i => ParamWrData_s,
      Param_o => Params_s(3)
    );

end TestParamIntf;
