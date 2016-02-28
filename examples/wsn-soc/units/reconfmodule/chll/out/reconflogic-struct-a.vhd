-- Automatically generated: write_netlist -reconfmodule -vhdl -architecture reconflogic-struct-a.vhd

architecture struct of MyReconfigLogic is

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

  component ConfigRegister
    generic (
      Width : integer := 1
    );
    port (
      Reset_n_i : in std_logic;
      Output_o : out std_logic_vector(Width-1 downto 0);
      CfgMode_i : in std_logic;
      CfgClk_i : in std_logic;
      CfgShift_i : in std_logic;
      CfgDataIn_i : in std_logic;
      CfgDataOut_o : out std_logic
    );
  end component;

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

  signal BitData_s : std_logic_vector(1281 downto 0);
  signal AdcValue_s : std_logic_vector(15 downto 0);
  signal I2C_ReadCount_s : std_logic_vector(7 downto 0);
  signal ParamIn_Word_s : std_logic_vector(79 downto 0);
  signal ParamIn_Word_0_s : std_logic_vector(15 downto 0);
  signal ParamIn_Word_1_s : std_logic_vector(15 downto 0);
  signal ParamIn_Word_2_s : std_logic_vector(15 downto 0);
  signal ParamIn_Word_3_s : std_logic_vector(15 downto 0);
  signal ParamIn_Word_4_s : std_logic_vector(15 downto 0);
  signal ParamOut_Word_s : std_logic_vector(31 downto 0);
  signal ParamOut_Word_0_s : std_logic_vector(15 downto 0);
  signal ParamOut_Word_1_s : std_logic_vector(15 downto 0);
  signal CfgClk_s : std_logic_vector(3 downto 0);
  signal CfgMode_s : std_logic;
  signal CfgShift_s : std_logic_vector(3 downto 0);
  signal CfgDataOut_s : std_logic;
  signal CfgDataIn_s : std_logic_vector(3 downto 0);
  signal CfgReconfSignals_s : std_logic_vector(8 downto 0);
  signal ParamWrAddr_s : std_logic_vector(2 downto 0);
  signal ParamWrData_s : std_logic_vector(15 downto 0);
  signal ParamWr_s : std_logic;
  signal ParamRdAddr_s : std_logic_vector(1 downto 0);
  signal ParamRdData_s : std_logic_vector(15 downto 0);


  type Params_t is array(0 to 2) of std_logic_vector(15 downto 0);

  signal Params_s : Params_t;
  signal I2C_ErrAckParam_s : std_logic_vector(0 downto 0);
  signal ParamI2C_Divider800Enable_s : std_logic;
  signal ParamI2C_ErrAckParamEnable_s : std_logic;
  signal ParamParamIn_Word_0Enable_s : std_logic;
  signal ParamParamIn_Word_1Enable_s : std_logic;
  signal ParamParamIn_Word_2Enable_s : std_logic;
  signal ParamParamIn_Word_3Enable_s : std_logic;
  signal ParamParamIn_Word_4Enable_s : std_logic;

begin

  -- Configuration Interface
  CfgIntf_0: CfgIntf
    generic map (
      BaseAddr => 16#0180#,
      NumCfgs => 4
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


  -- Parameterization Interface: 7 write addresses, 3 read addresses
  ParamIntf_0: ParamIntf
    generic map (
      BaseAddr => 16#0188#,
      WrAddrWidth => 3,
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


  MyInterSynthModule_0: MyInterSynthModule
    port map (
      bitdata => BitData_s,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o => AdcDoConvert_o,
      AdcValue_i => AdcValue_s,
      I2C_Busy_i => I2C_Busy_i,
      I2C_DataIn_o => I2C_DataIn_o,
      I2C_DataOut_i => I2C_DataOut_i,
      I2C_Error_i => I2C_Error_i,
      I2C_FIFOEmpty_i => I2C_FIFOEmpty_i,
      I2C_FIFOFull_i => I2C_FIFOFull_i,
      I2C_FIFOReadNext_o => I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => I2C_FIFOWrite_o,
      I2C_ReadCount_o => I2C_ReadCount_s,
      I2C_ReceiveSend_n_o => I2C_ReceiveSend_n_o,
      I2C_StartProcess_o => I2C_StartProcess_o,
      Inputs_i_0 => Inputs_i(0),
      Inputs_i_1 => Inputs_i(1),
      Inputs_i_2 => Inputs_i(2),
      Inputs_i_3 => Inputs_i(3),
      Inputs_i_4 => Inputs_i(4),
      Inputs_i_5 => Inputs_i(5),
      Inputs_i_6 => Inputs_i(6),
      Inputs_i_7 => Inputs_i(7),
      Outputs_o_0 => Outputs_o(0),
      Outputs_o_1 => Outputs_o(1),
      Outputs_o_2 => Outputs_o(2),
      Outputs_o_3 => Outputs_o(3),
      Outputs_o_4 => Outputs_o(4),
      Outputs_o_5 => Outputs_o(5),
      Outputs_o_6 => Outputs_o(6),
      Outputs_o_7 => Outputs_o(7),
      ReconfModuleIRQs_o_0 => ReconfModuleIRQs_o(0),
      ReconfModuleIRQs_o_1 => ReconfModuleIRQs_o(1),
      ReconfModuleIRQs_o_2 => ReconfModuleIRQs_o(2),
      ReconfModuleIRQs_o_3 => ReconfModuleIRQs_o(3),
      ReconfModuleIRQs_o_4 => ReconfModuleIRQs_o(4),
      SPI_CPHA_o => SPI_CPHA_o,
      SPI_CPOL_o => SPI_CPOL_o,
      SPI_DataIn_o => SPI_DataIn_o,
      SPI_DataOut_i => SPI_DataOut_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_LSBFE_o => SPI_LSBFE_o,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_Transmission_i => SPI_Transmission_i,
      SPI_Write_o => SPI_Write_o,
      ReconfModuleIn_i_0 => ReconfModuleIn_i(0),
      ReconfModuleIn_i_1 => ReconfModuleIn_i(1),
      ReconfModuleIn_i_2 => ReconfModuleIn_i(2),
      ReconfModuleIn_i_3 => ReconfModuleIn_i(3),
      ReconfModuleIn_i_4 => ReconfModuleIn_i(4),
      ReconfModuleIn_i_5 => ReconfModuleIn_i(5),
      ReconfModuleIn_i_6 => ReconfModuleIn_i(6),
      ReconfModuleIn_i_7 => ReconfModuleIn_i(7),
      ReconfModuleOut_o_0 => ReconfModuleOut_o(0),
      ReconfModuleOut_o_1 => ReconfModuleOut_o(1),
      ReconfModuleOut_o_2 => ReconfModuleOut_o(2),
      ReconfModuleOut_o_3 => ReconfModuleOut_o(3),
      ReconfModuleOut_o_4 => ReconfModuleOut_o(4),
      ReconfModuleOut_o_5 => ReconfModuleOut_o(5),
      ReconfModuleOut_o_6 => ReconfModuleOut_o(6),
      ReconfModuleOut_o_7 => ReconfModuleOut_o(7),
      Clk_i => Clk_i,
      Reset_n_i => Reset_n_i,
      ParamIn_Word_i => ParamIn_Word_s,
      ParamOut_Word_o => ParamOut_Word_s,
      CfgMode_i => CfgMode_s,
      CfgDataIn_i => CfgDataOut_s,
      CfgClk_TRFSM0_i => CfgClk_s(2),
      CfgShift_TRFSM0_i => CfgShift_s(2),
      CfgDataOut_TRFSM0_o => CfgDataIn_s(2),
      CfgClk_TRFSM1_i => CfgClk_s(3),
      CfgShift_TRFSM1_i => CfgShift_s(3),
      CfgDataOut_TRFSM1_o => CfgDataIn_s(3)
    );

  AdcValue_s <= "000000" & AdcValue_i;
  I2C_ReadCount_o <= I2C_ReadCount_s(3 downto 0);
  ParamIn_Word_s <= ParamIn_Word_4_s & ParamIn_Word_3_s & ParamIn_Word_2_s & ParamIn_Word_1_s & ParamIn_Word_0_s;
  ParamOut_Word_0_s <= ParamOut_Word_s(15 downto 0);
  ParamOut_Word_1_s <= ParamOut_Word_s(31 downto 16);


  CfgRegReconfSignals: ConfigRegister
    generic map (
      Width => 9
    )
    port map (
      Reset_n_i => Reset_n_i,
      Output_o => CfgReconfSignals_s,
      CfgMode_i => CfgMode_s,
      CfgClk_i => CfgClk_s(0),
      CfgShift_i => CfgShift_s(0),
      CfgDataIn_i => CfgDataOut_s,
      CfgDataOut_o => CfgDataIn_s(0)
    );


  CfgRegbitdata: ConfigRegister
    generic map (
      Width => 1282
    )
    port map (
      Reset_n_i => Reset_n_i,
      Output_o => BitData_s,
      CfgMode_i => CfgMode_s,
      CfgClk_i => CfgClk_s(1),
      CfgShift_i => CfgShift_s(1),
      CfgDataIn_i => CfgDataOut_s,
      CfgDataOut_o => CfgDataIn_s(1)
    );

  I2C_F100_400_n_o <= CfgReconfSignals_s(0);
  SPI_SPPR_SPR_o <= CfgReconfSignals_s(8 downto 1);
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


  ParamOutReg_ParamIn_Word_0: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => ParamIn_Word_0_s,
      Enable_i => ParamParamIn_Word_0Enable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_ParamIn_Word_1: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => ParamIn_Word_1_s,
      Enable_i => ParamParamIn_Word_1Enable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_ParamIn_Word_2: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => ParamIn_Word_2_s,
      Enable_i => ParamParamIn_Word_2Enable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_ParamIn_Word_3: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => ParamIn_Word_3_s,
      Enable_i => ParamParamIn_Word_3Enable_s,
      ParamWrData_i => ParamWrData_s
    );


  ParamOutReg_ParamIn_Word_4: ParamOutReg
    generic map (
      Width => 16
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Param_o => ParamIn_Word_4_s,
      Enable_i => ParamParamIn_Word_4Enable_s,
      ParamWrData_i => ParamWrData_s
    );

  I2C_ErrAckParam_o <= I2C_ErrAckParam_s(0);
  -- Address $00
  Params_s(0) <= "00000000" & I2C_Errors_i;
  -- Address $01
  Params_s(1) <= ParamOut_Word_0_s;
  -- Address $02
  Params_s(2) <= ParamOut_Word_1_s;
  -- Address $00
  ParamI2C_Divider800Enable_s <= ParamWr_s when ParamWrAddr_s = "000" else 
                                 '0';
  -- Address $01
  ParamI2C_ErrAckParamEnable_s <= ParamWr_s when ParamWrAddr_s = "001" else 
                                  '0';
  -- Address $02
  ParamParamIn_Word_0Enable_s <= ParamWr_s when ParamWrAddr_s = "010" else 
                                 '0';
  -- Address $03
  ParamParamIn_Word_1Enable_s <= ParamWr_s when ParamWrAddr_s = "011" else 
                                 '0';
  -- Address $04
  ParamParamIn_Word_2Enable_s <= ParamWr_s when ParamWrAddr_s = "100" else 
                                 '0';
  -- Address $05
  ParamParamIn_Word_3Enable_s <= ParamWr_s when ParamWrAddr_s = "101" else 
                                 '0';
  -- Address $06
  ParamParamIn_Word_4Enable_s <= ParamWr_s when ParamWrAddr_s = "110" else 
                                 '0';

end struct;

