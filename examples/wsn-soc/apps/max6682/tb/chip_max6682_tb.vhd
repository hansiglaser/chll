library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Chip_tb is
end Chip_tb;

architecture behavior of Chip_tb is

  component Chip
    port (
      Reset_n_i         : in    std_logic;
      Clk_i             : in    std_logic;
      Cpu_En_i          : in    std_logic;
      Dbg_En_i          : in    std_logic;
--      Dbg_UART_RxD_i    : in    std_logic;
--      Dbg_UART_TxD_o    : out   std_logic;
      Dbg_SCL_i         : in    std_logic;
      Dbg_SDA_b         : inout std_logic;
      P1_b              : inout std_logic_vector(7 downto 0);
      P2_b              : inout std_logic_vector(7 downto 0);
      UartRxD_i         : in    std_logic;
      UartTxD_o         : out   std_logic;
      SCK_o             : out std_logic;
      MOSI_o            : out std_logic;
      MISO_i            : in  std_logic;
      Inputs_i          : in    std_logic_vector(7 downto 0);
      Outputs_o         : out   std_logic_vector(7 downto 0);
      SPIMISO_i         : in    std_logic;
      SPIMOSI_o         : out   std_logic;
      SPISCK_o          : out   std_logic;
      I2CSCL_b          : out   std_logic;
      I2CSDA_b          : inout std_logic;
--      OneWire_b         : inout std_logic;
--      PWM_i             : in    std_logic;
--      SENT_i            : in    std_logic;
--      SPC_b             : inout std_logic;
      AdcConvComplete_i : in    std_logic;
      AdcDoConvert_o    : out   std_logic;
      AdcValue_i        : in    std_logic_vector(9 downto 0));
  end component;

  component MAX6682_Model
    port (
      ChipSelect_n_i : in  std_logic;
      SCLK_i         : in  std_logic;
      SO_o           : out std_logic;
      Value_i        : in  std_logic_vector(10 downto 0)
    );
  end component;

  component ExtNames
    port (
      SPIFSM_Done : out std_logic;
      CpuIntr     : out std_logic;
      SensorValue : out std_logic_vector(15 downto 0);
      Enable      : out std_logic
    );
  end component;

  -- Reset
  signal Reset_n_i         : std_logic := '0';
  -- Clock
  signal Clk_i             : std_logic := '1';
  signal Cpu_En_i          : std_logic := '1';
  signal Dbg_En_i          : std_logic;
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic;
  signal Dbg_SDA_b         : std_logic;
  signal P1_b              : std_logic_vector(7 downto 0);
  signal P2_b              : std_logic_vector(7 downto 0);
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
  signal I2CSCL_b          : std_logic;
  signal I2CSDA_b          : std_logic;
--  signal OneWire_b         : std_logic;
--  signal PWM_i             : std_logic;
--  signal SENT_i            : std_logic;
--  signal SPC_b             : std_logic;
  signal AdcConvComplete_i : std_logic;
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

  -- look into the ADT7310 app
  -- alias SPIFSM_Done_i is << signal .adt7310_tb.DUT.SPIFSM_Done_s : std_logic >>;
  -- ModelSim complains here, that the references signal is not a VHDL object.
  -- True, this is a Verilog object. As a workaround the module ExtNames is created
  -- which uses Verilog hierarchical names to reference the wire and assigns it to
  -- an output. This module is instantiated (and it seems ModelSim only adds
  -- Verilog<->VHDL signal converters on instance boundaries) and this output is
  -- connected with the SPIFSM_Done_i signal.
  signal SPIFSM_Done_e : std_logic;  -- directly from inside SPI_FSM
  signal CpuIntr_e     : std_logic;  -- directly from inside SPI_FSM
  signal SensorValue_e : std_logic_vector(15 downto 0);
  signal Enable_e      : std_logic;  -- directly from inside
  -- Using the extracted Yosys FSM we get delta cycles and a glitch on
  -- SPIFSM_Done_i. Therefore we generate a slightly delayed version and wait
  -- on the ANDed value.
  signal SPIFSM_Done_d : std_logic;  -- sightly delayed
  signal CpuIntr_o     : std_logic;  -- sightly delayed
  signal SensorValue_o : std_logic_vector(15 downto 0);  -- sightly delayed
  signal Enable_i      : std_logic;  -- directly from inside

  -- MAX6682 component ports
  signal MAX6682CS_n_o : std_logic;

  constant ClkPeriode : time := 10 ns;

  -- MAX6682 simulation
  signal MAX6682Value       : unsigned(10 downto 0);

begin

  DUT: Chip
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => Dbg_SCL_i,
      Dbg_SDA_b         => Dbg_SDA_b,
      P1_b              => P1_b,
      P2_b              => P2_b,
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
      I2CSCL_b          => I2CSCL_b,
      I2CSDA_b          => I2CSDA_b,
--      OneWire_b         => OneWire_b,
--      PWM_i             => PWM_i,
--      SENT_i            => SENT_i,
--      SPC_b             => SPC_b,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

  MAX6682CS_n_o <= Outputs_o(0);
  Inputs_i <= (others => '0');

  Cpu_En_i  <= '1';
  Dbg_En_i  <= '0';
--  Dbg_UART_RxD_i <= '1';
  Dbg_SCL_i <= 'H';
  Dbg_SDA_b <= 'H';
  P1_b      <= (others => 'H');
  P2_b      <= (others => 'H');
  UartRxD_i <= '1';
  MISO_i    <= '0';
  I2CSCL_b  <= 'H';
  I2CSDA_b  <= 'H';
--  OneWire_b <= 'H';
--  PWM_i     <= 'H';
--  SENT_i    <= 'H';
--  SPC_b     <= 'H';
  AdcConvComplete_i <= '0';
  AdcValue_i <= (others => '0');

  ExtNames_1: ExtNames
    port map (
      SPIFSM_Done => SPIFSM_Done_e,
      CpuIntr     => CpuIntr_e,
      SensorValue => SensorValue_e,
      Enable      => Enable_e
    );
  SPIFSM_Done_d <= SPIFSM_Done_e after 1.0 ns;
  CpuIntr_o     <= CpuIntr_e     after 1.0 ns;
  SensorValue_o <= SensorValue_e after 1.0 ns;
  Enable_i      <= Enable_e      after 1.0 ns;

  SPIMISO_i <= 'H';

  MAX6682_1: MAX6682_Model
    port map (
      ChipSelect_n_i => MAX6682CS_n_o,
      SCLK_i         => SPISCK_o,
      SO_o           => SPIMISO_i,
      Value_i        => std_logic_vector(MAX6682Value));

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    MAX6682Value <= (others => '0');
    wait for 2.2*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    -- enable SensorFSM
    wait until Enable_i = '1';

    wait for 9*ClkPeriode;                     -- 9 cycles
    assert MAX6682CS_n_o = '1' report "CS_n should be '1'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 10 cycles" severity error;
    wait for 35*ClkPeriode;                    -- 35 cycles
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(0,16)) report "SensorValue_o should be 0" severity error;

    -- new sensor value: 38 -> large difference -> notify required
    wait for 3*ClkPeriode;                     -- 3 cycle
    MAX6682Value <= to_unsigned(38,11);
    wait for 43*ClkPeriode;                     -- 43 cycle
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;
    wait for 1*ClkPeriode;                     -- 1 more cycle if notification happened

    -- new sensor value: 30 -> small difference -> no notification
    wait for 3*ClkPeriode;                     -- 3 cycle
    MAX6682Value <= to_unsigned(30,11);
    wait for 43*ClkPeriode;                     -- 43 cycle
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;

    -- new sensor value: 28 -> small difference -> no notification
    wait for 3*ClkPeriode;                     -- 3 cycle
    MAX6682Value <= to_unsigned(28,11);
    wait for 43*ClkPeriode;                     -- 43 cycle
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38,16)) report "SensorValue_o should be 38" severity error;

    -- new sensor value: 27 -> large difference -> notify required
    wait for 3*ClkPeriode;                     -- 3 cycle
    MAX6682Value <= to_unsigned(27,11);
    wait for 43*ClkPeriode;                     -- 43 cycle
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(27,16)) report "SensorValue_o should be 27" severity error;
    wait for 1*ClkPeriode;                     -- 1 more cycle if notification happened
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;


    wait for 10*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
