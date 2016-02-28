library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Core_tb is
end Core_tb;

architecture behavior of Core_tb is

  component Core
    port (
      Reset_n_i         : in  std_logic;
      Clk_i             : in  std_logic;
      LFXT_Clk_i        : in  std_logic;
      Cpu_En_i          : in  std_logic;
      Dbg_En_i          : in  std_logic;
--      Dbg_UART_RxD_i    : in  std_logic;
--      Dbg_UART_TxD_o    : out std_logic;
      Dbg_SCL_i         : in  std_logic;
      Dbg_SDA_Out_o     : out std_logic;
      Dbg_SDA_In_i      : in  std_logic;
      P1_DOut_o         : out std_logic_vector(7 downto 0);
      P1_En_o           : out std_logic_vector(7 downto 0);
      P1_DIn_i          : in  std_logic_vector(7 downto 0);
      P2_DOut_o         : out std_logic_vector(7 downto 0);
      P2_En_o           : out std_logic_vector(7 downto 0);
      P2_DIn_i          : in  std_logic_vector(7 downto 0);
      UartRxD_i         : in  std_logic;
      UartTxD_o         : out std_logic;
      SCK_o             : out std_logic;
      MOSI_o            : out std_logic;
      MISO_i            : in  std_logic;
      Inputs_i          : in  std_logic_vector(7 downto 0);
      Outputs_o         : out std_logic_vector(7 downto 0);
      SPIMISO_i         : in  std_logic;
      SPIMOSI_o         : out std_logic;
      SPISCK_o          : out std_logic;
      I2CSCL_o          : out std_logic;
      I2CSDA_i          : in  std_logic;
      I2CSDA_o          : out std_logic;
--      OneWire_i         : in  std_logic;
--      OneWire_o         : out std_logic;
--      PWMInput_i        : in  std_logic;
--      SENTInput_i       : in  std_logic;
--      SPCInput_i        : in  std_logic;
--      SPCTrigger_o      : out std_logic;
      AdcConvComplete_i : in  std_logic;
      AdcDoConvert_o    : out std_logic;
      AdcValue_i        : in  std_logic_vector(9 downto 0));
  end component;

  component ExtNames
    port (
      CpuIntr     : out std_logic;
      SensorValue : out std_logic_vector(15 downto 0);
      Enable      : out std_logic
    );
  end component;

  -- Reset
  signal Reset_n_i         : std_logic := '0';
  -- Clock
  signal Clk_i             : std_logic := '1';
  signal LFXT_Clk_i        : std_logic;
  signal Cpu_En_i          : std_logic := '1';
  signal Dbg_En_i          : std_logic;
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic;
  signal Dbg_SDA_Out_o     : std_logic;
  signal Dbg_SDA_In_i      : std_logic;
  signal P1_DOut_o         : std_logic_vector(7 downto 0);
  signal P1_En_o           : std_logic_vector(7 downto 0);
  signal P1_DIn_i          : std_logic_vector(7 downto 0);
  signal P2_DOut_o         : std_logic_vector(7 downto 0);
  signal P2_En_o           : std_logic_vector(7 downto 0);
  signal P2_DIn_i          : std_logic_vector(7 downto 0);
  signal UartRxD_i         : std_logic;
  signal UartTxD_o         : std_logic;
  signal SCK_o             : std_logic;
  signal MOSI_o            : std_logic;
  signal MISO_i            : std_logic := '0';
  signal Inputs_i          : std_logic_vector(7 downto 0);
  signal Outputs_o         : std_logic_vector(7 downto 0);
  signal SPIMISO_i         : std_logic;
  signal SPIMOSI_o         : std_logic;
  signal SPISCK_o          : std_logic;
  signal I2CSCL_o          : std_logic;
  signal I2CSDA_i          : std_logic;
  signal I2CSDA_o          : std_logic;
--  signal OneWire_i         : std_logic;
--  signal OneWire_o         : std_logic;
--  signal PWMInput_i        : std_logic;
--  signal SENTInput_i       : std_logic;
--  signal SPCInput_i        : std_logic;
--  signal SPCTrigger_o      : std_logic;
  signal AdcConvComplete_i : std_logic;
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

  -- look into the ExtAdc app
  signal CpuIntr_e     : std_logic;  -- directly from inside SPI_FSM
  signal SensorValue_e : std_logic_vector(15 downto 0);
  signal Enable_e      : std_logic;  -- directly from inside
  signal CpuIntr_o     : std_logic;  -- sightly delayed
  signal SensorValue_o : std_logic_vector(15 downto 0);  -- sightly delayed
  signal Enable_i      : std_logic;  -- directly from inside

  -- External Sensor ports
  constant AdcValueWidth : integer := 10;
  signal SensorPower_o : std_logic;
  signal SensorStart_o : std_logic;
  signal SensorReady_i : std_logic;
  alias  AdcStart_o    : std_logic is AdcDoConvert_o;
  alias  AdcDone_i     : std_logic is AdcConvComplete_i;

  constant ClkPeriode : time := 10 ns;

begin

  DUT: Core
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      LFXT_Clk_i        => LFXT_Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => Dbg_SCL_i,
      Dbg_SDA_Out_o     => Dbg_SDA_Out_o,
      Dbg_SDA_In_i      => Dbg_SDA_In_i,
      P1_DOut_o         => P1_DOut_o,
      P1_En_o           => P1_En_o,
      P1_DIn_i          => P1_DIn_i,
      P2_DOut_o         => P2_DOut_o,
      P2_En_o           => P2_En_o,
      P2_DIn_i          => P2_DIn_i,
      UartRxD_i         => UartRxD_i,
      UartTxD_o         => UartTxD_o,
      SCK_o             => SCK_o,
      MOSI_o            => MOSI_o,
      MISO_i            => MISO_i,
      Inputs_i          => Inputs_i,
      Outputs_o         => Outputs_o,
      SPIMISO_i         => SPIMISO_i,
      SPIMOSI_o         => SPIMOSI_o,
      SPISCK_o          => SPISCK_o,
      I2CSCL_o          => I2CSCL_o,
      I2CSDA_i          => I2CSDA_i,
      I2CSDA_o          => I2CSDA_o,
--      OneWire_i         => OneWire_i,
--      OneWire_o         => OneWire_o,
--      PWMInput_i        => PWMInput_i,
--      SENTInput_i       => SENTInput_i,
--      SPCInput_i        => SPCInput_i,
--      SPCTrigger_o      => SPCTrigger_o,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

  SensorPower_o <= Outputs_o(0);
  SensorStart_o <= Outputs_o(1);
  Inputs_i(0) <= SensorReady_i;
  Inputs_i(Inputs_i'high downto 1) <= (others => '0');

  P1_DIn_i <= (others => '0');
  P2_DIn_i <= (others => '0');

  ExtNames_1: ExtNames
    port map (
      CpuIntr     => CpuIntr_e,
      SensorValue => SensorValue_e,
      Enable      => Enable_e
    );
  CpuIntr_o     <= CpuIntr_e     after 1.0 ns;
  SensorValue_o <= SensorValue_e after 1.0 ns;
  Enable_i      <= Enable_e      after 1.0 ns;

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    AdcValue_i <= (others => '0');

    wait for 2.2*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    -- wait until SensorFSM is enabled
    wait until Enable_i = '1';

    -- Check constant values of dynamic signals coming out of the application modules
    wait for 0.1*ClkPeriode;
    -- none to check

    wait for 9*ClkPeriode;                     -- 9 cycles
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should be '1'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should be '1'" severity error;
    wait for 35*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should still be '0'" severity error;
    SensorReady_i <= '1';
    wait for 0.1*ClkPeriode;
    assert AdcStart_o    = '1' report "AdcStart_o should be '1'" severity error;
    wait for 0.9*ClkPeriode;
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    wait for 35*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    AdcDone_i <= '1';
    wait for 0.1*ClkPeriode;
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    wait for 0.9*ClkPeriode;
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(0,16)) report "SensorValue_o should be 0" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;

    -- new sensor value: 38 -> large difference -> notify required
    wait for 3*ClkPeriode;                     -- 3 cycle
    AdcValue_i <= std_logic_vector(to_unsigned(38,AdcValueWidth));
    wait for 6*ClkPeriode;                     -- 9 cycles
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should be '1'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should be '1'" severity error;
    wait for 3*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should still be '0'" severity error;
    SensorReady_i <= '1';
    wait for 3*ClkPeriode;                     -- 3 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    AdcDone_i <= '1';
    wait for 1*ClkPeriode;                     -- 1 cycle
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;

    -- new sensor value: 30 -> small difference -> no notification
    wait for 3*ClkPeriode;                     -- 3 cycle
    AdcValue_i <= std_logic_vector(to_unsigned(30,AdcValueWidth));
    wait for 6*ClkPeriode;                     -- 9 cycles
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should be '1'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should be '1'" severity error;
    wait for 3*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should still be '0'" severity error;
    SensorReady_i <= '1';
    wait for 3*ClkPeriode;                     -- 3 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    AdcDone_i <= '1';
    wait for 1*ClkPeriode;                     -- 1 cycle
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '1'" severity error;

    -- new sensor value: 28 -> small difference -> no notification
    wait for 3*ClkPeriode;                     -- 3 cycle
    AdcValue_i <= std_logic_vector(to_unsigned(28,AdcValueWidth));
    wait for 6*ClkPeriode;                     -- 9 cycles
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should be '1'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should be '1'" severity error;
    wait for 3*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should still be '0'" severity error;
    SensorReady_i <= '1';
    wait for 3*ClkPeriode;                     -- 3 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    AdcDone_i <= '1';
    wait for 1*ClkPeriode;                     -- 1 cycle
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '1'" severity error;

    -- new sensor value: 27 -> large difference -> notify required
    wait for 3*ClkPeriode;                     -- 3 cycle
    AdcValue_i <= std_logic_vector(to_unsigned(27,AdcValueWidth));
    wait for 6*ClkPeriode;                     -- 9 cycles
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should be '1'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should be '1'" severity error;
    wait for 3*ClkPeriode;                    -- 35 cycles
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should still be '0'" severity error;
    SensorReady_i <= '1';
    wait for 3*ClkPeriode;                     -- 3 cycle
    assert SensorPower_o = '1' report "SensorPower_o should still be '1'" severity error;
    assert SensorStart_o = '1' report "SensorStart_o should still be '1'" severity error;
    assert AdcStart_o    = '1' report "AdcStart_o should still be '1'" severity error;
    AdcDone_i <= '1';
    wait for 1*ClkPeriode;                     -- 1 cycle
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    assert SensorPower_o = '0' report "SensorPower_o should be '0'" severity error;
    assert SensorStart_o = '0' report "SensorStart_o should be '0'" severity error;
    assert AdcStart_o    = '0' report "AdcStart_o should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(27,16)) report "SensorValue_o should be 27" severity error;
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;


    wait for 10*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
