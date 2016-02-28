architecture TestCfgIntf of MyReconfigLogic is

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
      CfgMode_o : out std_logic;
      CfgShift_o : out std_logic_vector(NumCfgs-1 downto 0);
      CfgDataOut_o : out std_logic;
      CfgDataIn_i : in std_logic_vector(NumCfgs-1 downto 0)
    );
  end component;

  constant NumCfgs : integer := 3;

  signal CfgMode_s    : std_logic;
  signal CfgShift_s   : std_logic_vector(NumCfgs-1 downto 0);
  signal CfgDataOut_s : std_logic;
  signal CfgDataIn_s  : std_logic_vector(NumCfgs-1 downto 0);

begin

  -- Configuration Interface
  CfgIntf_0: CfgIntf
    generic map (
      BaseAddr => 16#0180#,
      NumCfgs => NumCfgs
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      PerAddr_i => PerAddr_i,
      PerDIn_i => PerDIn_i,
      PerDOut_o => CfgIntfDOut_o,
      PerWr_i => PerWr_i,
      PerEn_i => PerEn_i,
      CfgMode_o => CfgMode_s,
      CfgShift_o => CfgShift_s,
      CfgDataOut_o => CfgDataOut_s,
      CfgDataIn_i => CfgDataIn_s
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
  ParamIntfDOut_o <= (others => '0');
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

  -- just a fixed value for the config interface
  CfgDataIn_s <= (others => '0');

end TestCfgIntf;
