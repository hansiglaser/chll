library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

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

  component adt7410_model
    port (
      scl_i      : in    std_logic;
      sda_io     : inout std_logic;
      i2c_addr_i : in    std_logic_vector(1 downto 0);
      int_o      : out   std_logic;
      ct_o       : out   std_logic;
      temp_i     : in    std_logic_vector(15 downto 0));
  end component;

  component ExtNames
    port (
      I2CFSM_Done : out std_logic;
      CpuIntr     : out std_logic;
      SensorValue : out std_logic_vector(15 downto 0)
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
--  signal OneWire_b         : std_logic;
--  signal PWM_i             : std_logic;
--  signal SENT_i            : std_logic;
--  signal SPC_b             : std_logic;
  signal AdcConvComplete_i : std_logic;
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

  -- look into the ADT7310 app
  -- alias I2CFSM_Done_i is << signal .adt7310_tb.DUT.I2CFSM_Done_s : std_logic >>;
  -- ModelSim complains here, that the references signal is not a VHDL object.
  -- True, this is a Verilog object. As a workaround the module ExtNames is created
  -- which uses Verilog hierarchical names to reference the wire and assigns it to
  -- an output. This module is instantiated (and it seems ModelSim only adds
  -- Verilog<->VHDL signal converters on instance boundaries) and this output is
  -- connected with the I2CFSM_Done_i signal.
  signal I2CFSM_Done_e : std_logic;  -- directly from inside I2C_FSM
  signal CpuIntr_e     : std_logic;  -- directly from inside I2C_FSM
  signal SensorValue_e : std_logic_vector(15 downto 0);
  -- Using the extracted Yosys FSM we get delta cycles and a glitch on
  -- I2CFSM_Done_i. Therefore we generate a slightly delayed version and wait
  -- on the ANDed value.
  signal I2CFSM_Done_d : std_logic;  -- sightly delayed
  signal CpuIntr_o     : std_logic;  -- sightly delayed
  signal SensorValue_o : std_logic_vector(15 downto 0);  -- sightly delayed
  signal SensorValue_real : real;

  -- SlowADT7410 component ports
  signal I2C_SDA_b     : std_logic;
  signal I2C_SCL_b     : std_logic;
  signal CT_n_s        : std_logic;
  signal INT_n_s       : std_logic;
  signal Temp_s        : real := 23.7;
  signal TempBin_s     : std_logic_vector(15 downto 0);

  -- The timer has to wait for 240ms. With a 16 bit resolution, the maximumn
  -- counting periode is 3.66us. Here we set the clock signal to 10us = 100kHz.
  -- The timer is preset to 24000.
  constant ClkPeriode : time := 10 us;

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
      I2CSCL_b          => I2C_SCL_b,
      I2CSDA_b          => I2C_SDA_b,
--      OneWire_b         => OneWire_b,
--      PWM_i             => PWM_i,
--      SENT_i            => SENT_i,
--      SPC_b             => SPC_b,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

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
--  OneWire_b <= 'H';
--  PWM_i     <= 'H';
--  SENT_i    <= 'H';
--  SPC_b     <= 'H';
  AdcConvComplete_i <= '0';
  AdcValue_i <= (others => '0');

  TempBin_s <= std_logic_vector(to_unsigned(integer(Temp_s*128.0),16));

  ExtNames_1: ExtNames
    port map (
      I2CFSM_Done => I2CFSM_Done_e,
      CpuIntr     => CpuIntr_e,
      SensorValue => SensorValue_e
    );
  I2CFSM_Done_d <= I2CFSM_Done_e after 1.0 ns;
  CpuIntr_o     <= CpuIntr_e     after 1.0 ns;
  SensorValue_o <= SensorValue_e after 1.0 ns;

  SensorValue_real <= real(to_integer(unsigned(SensorValue_o)))/128.0;

  I2C_SCL_b <= 'H';      -- weak 1 -> simulate pull-up
  I2C_SDA_b <= 'H';      -- weak 1 -> simulate pull-up

  adt7410_1: adt7410_model
    port map (
      scl_i      => I2C_SCL_b,
      sda_io     => I2C_SDA_b,
      i2c_addr_i => "00",
      INT_o      => INT_n_s,
      CT_o       => CT_n_s,
      temp_i     => TempBin_s);

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    wait for 2.3*ClkPeriode;

    -- deassert Reset
    Reset_n_i <= '1';
    wait for 1.3*ClkPeriode;                     -- wait until spi_master's SCK_o goes '1' to conform to CPOL_i = '1'

    Temp_s <= 23.7;                     -- degree C

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- The digital value is 128*Temp_s (plus/minus rounding to nearest
    -- modulo 8). The threshold for too large changes is 30 (see
    -- sensorfsm.vhd).
    -- 23.7°C --> 3032
    -- 25.7°C --> 3288  (delta: | 256| >  30)
    -- 25.6°C --> 3280  (delta: |  -8| <  30)
    -- 25.5°C --> 3264  (delta: | -24| <  30)
    -- 25.4°C --> 3248  (delta: | -40| >= 30)

    -- new sensor value with large difference -> notify required
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.7;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.6;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after I2CFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.5;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after I2CFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with large difference -> notify required
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.4;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    wait for 100 ms;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
