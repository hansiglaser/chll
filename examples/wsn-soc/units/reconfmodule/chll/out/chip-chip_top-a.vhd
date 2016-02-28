-- Automatically generated: write_netlist -chip -vhdl -architecture chip-chip_top-a.vhd

architecture chip_top of chip is

  component ICP
    port (
      PAD : in std_logic;
      Y : out std_logic
    );
  end component;

  component BU16P
    port (
      A : in std_logic;
      PAD : out std_logic
    );
  end component;

  component ICCK8P
    port (
      PAD : in std_logic;
      Y : out std_logic
    );
  end component;

  component BBC16P
    port (
      A : in std_logic;
      EN : in std_logic;
      PAD : inout std_logic;
      Y : out std_logic
    );
  end component;

  component BUDD16P
    port (
      A : in std_logic;
      PAD : out std_logic
    );
  end component;

  component ISUP
    port (
      PAD : in std_logic;
      Y : out std_logic
    );
  end component;

  component Core
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Cpu_En_i : in std_logic;
      LFXT_Clk_i : in std_logic;
      Dbg_En_i : in std_logic;
      Dbg_SCL_i : in std_logic;
      Dbg_SDA_Out_o : out std_logic;
      Dbg_SDA_In_i : in std_logic;
      P1_DOut_o : out std_logic_vector(7 downto 0);
      P1_En_o : out std_logic_vector(7 downto 0);
      P1_DIn_i : in std_logic_vector(7 downto 0);
      P2_DOut_o : out std_logic_vector(7 downto 0);
      P2_En_o : out std_logic_vector(7 downto 0);
      P2_DIn_i : in std_logic_vector(7 downto 0);
      UartRxD_i : in std_logic;
      UartTxD_o : out std_logic;
      SCK_o : out std_logic;
      MOSI_o : out std_logic;
      MISO_i : in std_logic;
      Inputs_i : in std_logic_vector(7 downto 0);
      Outputs_o : out std_logic_vector(7 downto 0);
      SPIMISO_i : in std_logic;
      SPIMOSI_o : out std_logic;
      SPISCK_o : out std_logic;
      I2CSCL_o : out std_logic;
      I2CSDA_i : in std_logic;
      I2CSDA_o : out std_logic;
      AdcConvComplete_i : in std_logic;
      AdcDoConvert_o : out std_logic;
      AdcValue_i : in std_logic_vector(9 downto 0)
    );
  end component;

  signal Reset_n_i_s : std_logic;
  signal Clk_i_s : std_logic;
  signal Cpu_En_i_s : std_logic;
  signal Dbg_En_i_s : std_logic;
  signal Dbg_SCL_i_s : std_logic;
  signal Dbg_SDA_In_i_s : std_logic;
  signal Dbg_SDA_Out_o_s : std_logic;
  signal P1_DIn_i_s : std_logic_vector(7 downto 0);
  signal P1_DOut_o_s : std_logic_vector(7 downto 0);
  signal P1_En_o_s : std_logic_vector(7 downto 0);
  signal P1_En_o_s_n : std_logic_vector(7 downto 0);
  signal P2_DIn_i_s : std_logic_vector(7 downto 0);
  signal P2_DOut_o_s : std_logic_vector(7 downto 0);
  signal P2_En_o_s : std_logic_vector(7 downto 0);
  signal P2_En_o_s_n : std_logic_vector(7 downto 0);
  signal UartRxD_i_s : std_logic;
  signal UartTxD_o_s : std_logic;
  signal MISO_i_s : std_logic;
  signal MOSI_o_s : std_logic;
  signal SCK_o_s : std_logic;
  signal Inputs_i_s : std_logic_vector(7 downto 0);
  signal Outputs_o_s : std_logic_vector(7 downto 0);
  signal SPIMISO_i_s : std_logic;
  signal SPIMOSI_o_s : std_logic;
  signal SPISCK_o_s : std_logic;
  signal I2CSCL_o_s : std_logic;
  signal I2CSDA_i_s : std_logic;
  signal I2CSDA_o_s : std_logic;
  signal AdcConvComplete_i_s : std_logic;
  signal AdcDoConvert_o_s : std_logic;
  signal AdcValue_i_s : std_logic_vector(9 downto 0);

begin

  core_1: Core
    port map (
      Reset_n_i => Reset_n_i_s,
      Clk_i => Clk_i_s,
      Cpu_En_i => Cpu_En_i_s,
      LFXT_Clk_i => '0',
      Dbg_En_i => Dbg_En_i_s,
      Dbg_SCL_i => Dbg_SCL_i_s,
      Dbg_SDA_In_i => Dbg_SDA_In_i_s,
      Dbg_SDA_Out_o => Dbg_SDA_Out_o_s,
      P1_DIn_i => P1_DIn_i_s,
      P1_DOut_o => P1_DOut_o_s,
      P1_En_o => P1_En_o_s,
      P2_DIn_i => P2_DIn_i_s,
      P2_DOut_o => P2_DOut_o_s,
      P2_En_o => P2_En_o_s,
      UartRxD_i => UartRxD_i_s,
      UartTxD_o => UartTxD_o_s,
      MISO_i => MISO_i_s,
      MOSI_o => MOSI_o_s,
      SCK_o => SCK_o_s,
      Inputs_i => Inputs_i_s,
      Outputs_o => Outputs_o_s,
      SPIMISO_i => SPIMISO_i_s,
      SPIMOSI_o => SPIMOSI_o_s,
      SPISCK_o => SPISCK_o_s,
      I2CSCL_o => I2CSCL_o_s,
      I2CSDA_i => I2CSDA_i_s,
      I2CSDA_o => I2CSDA_o_s,
      AdcConvComplete_i => AdcConvComplete_i_s,
      AdcDoConvert_o => AdcDoConvert_o_s,
      AdcValue_i => AdcValue_i_s
    );


  PadReset_n_i: ISUP
    port map (
      PAD => Reset_n_i,
      Y => Reset_n_i_s
    );


  PadClk_i: ICCK8P
    port map (
      PAD => Clk_i,
      Y => Clk_i_s
    );


  PadCpu_En_i: ICP
    port map (
      PAD => Cpu_En_i,
      Y => Cpu_En_i_s
    );


  PadDbg_En_i: ICP
    port map (
      PAD => Dbg_En_i,
      Y => Dbg_En_i_s
    );


  PadDbg_SCL_i: ICP
    port map (
      PAD => Dbg_SCL_i,
      Y => Dbg_SCL_i_s
    );


  PadDbg_SDA_b: BBC16P
    port map (
      PAD => Dbg_SDA_b,
      Y => Dbg_SDA_In_i_s,
      A => '0',
      EN => Dbg_SDA_Out_o_s
    );

  P1_En_o_s_n <= not P1_En_o_s;


  PadP1_b_0: BBC16P
    port map (
      PAD => P1_b(0),
      Y => P1_DIn_i_s(0),
      A => P1_DOut_o_s(0),
      EN => P1_En_o_s_n(0)
    );


  PadP1_b_1: BBC16P
    port map (
      PAD => P1_b(1),
      Y => P1_DIn_i_s(1),
      A => P1_DOut_o_s(1),
      EN => P1_En_o_s_n(1)
    );


  PadP1_b_2: BBC16P
    port map (
      PAD => P1_b(2),
      Y => P1_DIn_i_s(2),
      A => P1_DOut_o_s(2),
      EN => P1_En_o_s_n(2)
    );


  PadP1_b_3: BBC16P
    port map (
      PAD => P1_b(3),
      Y => P1_DIn_i_s(3),
      A => P1_DOut_o_s(3),
      EN => P1_En_o_s_n(3)
    );


  PadP1_b_4: BBC16P
    port map (
      PAD => P1_b(4),
      Y => P1_DIn_i_s(4),
      A => P1_DOut_o_s(4),
      EN => P1_En_o_s_n(4)
    );


  PadP1_b_5: BBC16P
    port map (
      PAD => P1_b(5),
      Y => P1_DIn_i_s(5),
      A => P1_DOut_o_s(5),
      EN => P1_En_o_s_n(5)
    );


  PadP1_b_6: BBC16P
    port map (
      PAD => P1_b(6),
      Y => P1_DIn_i_s(6),
      A => P1_DOut_o_s(6),
      EN => P1_En_o_s_n(6)
    );


  PadP1_b_7: BBC16P
    port map (
      PAD => P1_b(7),
      Y => P1_DIn_i_s(7),
      A => P1_DOut_o_s(7),
      EN => P1_En_o_s_n(7)
    );

  P2_En_o_s_n <= not P2_En_o_s;


  PadP2_b_0: BBC16P
    port map (
      PAD => P2_b(0),
      Y => P2_DIn_i_s(0),
      A => P2_DOut_o_s(0),
      EN => P2_En_o_s_n(0)
    );


  PadP2_b_1: BBC16P
    port map (
      PAD => P2_b(1),
      Y => P2_DIn_i_s(1),
      A => P2_DOut_o_s(1),
      EN => P2_En_o_s_n(1)
    );


  PadP2_b_2: BBC16P
    port map (
      PAD => P2_b(2),
      Y => P2_DIn_i_s(2),
      A => P2_DOut_o_s(2),
      EN => P2_En_o_s_n(2)
    );


  PadP2_b_3: BBC16P
    port map (
      PAD => P2_b(3),
      Y => P2_DIn_i_s(3),
      A => P2_DOut_o_s(3),
      EN => P2_En_o_s_n(3)
    );


  PadP2_b_4: BBC16P
    port map (
      PAD => P2_b(4),
      Y => P2_DIn_i_s(4),
      A => P2_DOut_o_s(4),
      EN => P2_En_o_s_n(4)
    );


  PadP2_b_5: BBC16P
    port map (
      PAD => P2_b(5),
      Y => P2_DIn_i_s(5),
      A => P2_DOut_o_s(5),
      EN => P2_En_o_s_n(5)
    );


  PadP2_b_6: BBC16P
    port map (
      PAD => P2_b(6),
      Y => P2_DIn_i_s(6),
      A => P2_DOut_o_s(6),
      EN => P2_En_o_s_n(6)
    );


  PadP2_b_7: BBC16P
    port map (
      PAD => P2_b(7),
      Y => P2_DIn_i_s(7),
      A => P2_DOut_o_s(7),
      EN => P2_En_o_s_n(7)
    );


  PadUartRxD_i: ICP
    port map (
      PAD => UartRxD_i,
      Y => UartRxD_i_s
    );


  PadUartTxD_o: BU16P
    port map (
      PAD => UartTxD_o,
      A => UartTxD_o_s
    );


  PadMISO_i: ICP
    port map (
      PAD => MISO_i,
      Y => MISO_i_s
    );


  PadMOSI_o: BU16P
    port map (
      PAD => MOSI_o,
      A => MOSI_o_s
    );


  PadSCK_o: BU16P
    port map (
      PAD => SCK_o,
      A => SCK_o_s
    );


  PadInputs_i_0: ICP
    port map (
      PAD => Inputs_i(0),
      Y => Inputs_i_s(0)
    );


  PadInputs_i_1: ICP
    port map (
      PAD => Inputs_i(1),
      Y => Inputs_i_s(1)
    );


  PadInputs_i_2: ICP
    port map (
      PAD => Inputs_i(2),
      Y => Inputs_i_s(2)
    );


  PadInputs_i_3: ICP
    port map (
      PAD => Inputs_i(3),
      Y => Inputs_i_s(3)
    );


  PadInputs_i_4: ICP
    port map (
      PAD => Inputs_i(4),
      Y => Inputs_i_s(4)
    );


  PadInputs_i_5: ICP
    port map (
      PAD => Inputs_i(5),
      Y => Inputs_i_s(5)
    );


  PadInputs_i_6: ICP
    port map (
      PAD => Inputs_i(6),
      Y => Inputs_i_s(6)
    );


  PadInputs_i_7: ICP
    port map (
      PAD => Inputs_i(7),
      Y => Inputs_i_s(7)
    );


  PadOutputs_o_0: BU16P
    port map (
      PAD => Outputs_o(0),
      A => Outputs_o_s(0)
    );


  PadOutputs_o_1: BU16P
    port map (
      PAD => Outputs_o(1),
      A => Outputs_o_s(1)
    );


  PadOutputs_o_2: BU16P
    port map (
      PAD => Outputs_o(2),
      A => Outputs_o_s(2)
    );


  PadOutputs_o_3: BU16P
    port map (
      PAD => Outputs_o(3),
      A => Outputs_o_s(3)
    );


  PadOutputs_o_4: BU16P
    port map (
      PAD => Outputs_o(4),
      A => Outputs_o_s(4)
    );


  PadOutputs_o_5: BU16P
    port map (
      PAD => Outputs_o(5),
      A => Outputs_o_s(5)
    );


  PadOutputs_o_6: BU16P
    port map (
      PAD => Outputs_o(6),
      A => Outputs_o_s(6)
    );


  PadOutputs_o_7: BU16P
    port map (
      PAD => Outputs_o(7),
      A => Outputs_o_s(7)
    );


  PadSPIMISO_i: ICP
    port map (
      PAD => SPIMISO_i,
      Y => SPIMISO_i_s
    );


  PadSPIMOSI_o: BU16P
    port map (
      PAD => SPIMOSI_o,
      A => SPIMOSI_o_s
    );


  PadSPISCK_o: BU16P
    port map (
      PAD => SPISCK_o,
      A => SPISCK_o_s
    );


  PadI2CSCL_b: BUDD16P
    port map (
      PAD => I2CSCL_b,
      A => I2CSCL_o_s
    );


  PadI2CSDA_b: BBC16P
    port map (
      PAD => I2CSDA_b,
      Y => I2CSDA_i_s,
      A => '0',
      EN => I2CSDA_o_s
    );


  PadAdcConvComplete_i: ICP
    port map (
      PAD => AdcConvComplete_i,
      Y => AdcConvComplete_i_s
    );


  PadAdcDoConvert_o: BU16P
    port map (
      PAD => AdcDoConvert_o,
      A => AdcDoConvert_o_s
    );


  PadAdcValue_i_0: ICP
    port map (
      PAD => AdcValue_i(0),
      Y => AdcValue_i_s(0)
    );


  PadAdcValue_i_1: ICP
    port map (
      PAD => AdcValue_i(1),
      Y => AdcValue_i_s(1)
    );


  PadAdcValue_i_2: ICP
    port map (
      PAD => AdcValue_i(2),
      Y => AdcValue_i_s(2)
    );


  PadAdcValue_i_3: ICP
    port map (
      PAD => AdcValue_i(3),
      Y => AdcValue_i_s(3)
    );


  PadAdcValue_i_4: ICP
    port map (
      PAD => AdcValue_i(4),
      Y => AdcValue_i_s(4)
    );


  PadAdcValue_i_5: ICP
    port map (
      PAD => AdcValue_i(5),
      Y => AdcValue_i_s(5)
    );


  PadAdcValue_i_6: ICP
    port map (
      PAD => AdcValue_i(6),
      Y => AdcValue_i_s(6)
    );


  PadAdcValue_i_7: ICP
    port map (
      PAD => AdcValue_i(7),
      Y => AdcValue_i_s(7)
    );


  PadAdcValue_i_8: ICP
    port map (
      PAD => AdcValue_i(8),
      Y => AdcValue_i_s(8)
    );


  PadAdcValue_i_9: ICP
    port map (
      PAD => AdcValue_i(9),
      Y => AdcValue_i_s(9)
    );


end chip_top;

