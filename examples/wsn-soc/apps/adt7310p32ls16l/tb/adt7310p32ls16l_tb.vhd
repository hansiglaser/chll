library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.BusMasters.all;

entity ADT7310P32LS16L_tb is
end ADT7310P32LS16L_tb;

architecture behavior of ADT7310P32LS16L_tb is

  component ADT7310P32LS16L
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
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component adt7310_model
    port (
      SCLK_i  : in  std_logic;
      DOUT_o  : out std_logic;
      DIN_i   : in  std_logic;
      CS_n_i  : in  std_logic;
      CT_n_o  : out std_logic;
      INT_n_o : out std_logic;
      Temp_i  : in  real);
  end component;

  component ExtNames
    port (
      SPIFSM_Done : out std_logic
    );
  end component;

  -- component generics
  constant DataWidth : integer := 8;

  -- Reset
  signal Reset_n_i : std_logic := '0';
  -- Clock
  signal Clk_i : std_logic := '1';
  signal Enable_i : std_logic;
  signal CpuIntr_o : std_logic;
  signal ADT7310CS_n_o : std_logic;
  signal SPI_Data_i : std_logic_vector(7 downto 0);
  signal SPI_Write_o : std_logic;
  signal SPI_ReadNext_o : std_logic;
  signal SPI_Data_o : std_logic_vector(7 downto 0);
  signal SPI_FIFOFull_i : std_logic;
  signal SPI_FIFOEmpty_i : std_logic;
  signal SPI_Transmission_i : std_logic;
  signal SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal Threshold_i : std_logic_vector(15 downto 0);
  signal PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal SensorValue_o : std_logic_vector(15 downto 0);
  signal SensorValue_real : real;
  signal SPI_CPOL_o : std_logic;
  signal SPI_CPHA_o : std_logic;
  signal SPI_LSBFE_o : std_logic;
  signal SPI_SPPR_SPR_o : std_logic_vector(7 downto 0);

  -- look into the ADT7310P32LS16L app
  -- alias SPIFSM_Done_i is << signal .adt7310_tb.DUT.SPIFSM_Done_s : std_logic >>;
  -- ModelSim complains here, that the references signal is not a VHDL object.
  -- True, this is a Verilog object. As a workaround the module ExtNames is created
  -- which uses Verilog hierarchical names to reference the wire and assigns it to
  -- an output. This module is instantiated (and it seems ModelSim only adds
  -- Verilog<->VHDL signal converters on instance boundaries) and this output is
  -- connected with the SPIFSM_Done_i signal.
  signal SPIFSM_Done_i : std_logic;  -- directly from inside SPI_FSM
  -- Using the extracted Yosys FSM we get delta cycles and a glitch on
  -- SPIFSM_Done_i. Therefore we generate a slightly delayed version and wait
  -- on the ANDed value.
  signal SPIFSM_Done_d : std_logic;  -- sightly delayed
  signal SPIFSM_Done_a : std_logic;  -- SPIFSM_Done_i and SPIFSM_Done_d

  -- ADT7310 component ports
  signal SCLK_s        : std_logic := '1';
  signal DOUT_s        : std_logic;
  signal DIN_s         : std_logic := '0';
  signal CT_n_s        : std_logic;
  signal INT_n_s       : std_logic;
  signal Temp_s        : real := 23.7;

  -- SPI Master generics
  constant SPPRWidth         : integer := 4;
  constant SPRWidth          : integer := 4;
  constant SPIFIFOReadWidth  : integer := 4;
  constant SPIFIFOWriteWidth : integer := 4;
  -- SPI Master component ports
  signal SPI_ScanEnable_s   : std_logic := '0';
  signal SPI_ScanClk_s      : std_logic := '0';
  signal SPI_ScanDataIn_s   : std_logic := '0';
  signal SPI_ScanDataOut_s  : std_logic := '0';

  -- The timer has to wait for 240ms. With a 16 bit resolution, the maximumn
  -- counting periode is 3.66us. Here we set the clock signal to 10us = 100kHz.
  -- The timer is preset to 24000.
  constant ClkPeriode : time := 10 us;

begin

  DUT: ADT7310P32LS16L
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Enable_i,
      CpuIntr_o => CpuIntr_o,
      ADT7310CS_n_o => ADT7310CS_n_o,
      SPI_Data_i => SPI_Data_i,
      SPI_Write_o => SPI_Write_o,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_Data_o => SPI_Data_o,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_Transmission_i => SPI_Transmission_i,
      SPICounterPreset_i => SPICounterPreset_i,
      Threshold_i => Threshold_i,
      PeriodCounterPresetH_i => PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => PeriodCounterPresetL_i,
      SensorValue_o => SensorValue_o,
      SPI_CPOL_o => SPI_CPOL_o,
      SPI_CPHA_o => SPI_CPHA_o,
      SPI_LSBFE_o => SPI_LSBFE_o
    );

  SensorValue_real <= real(to_integer(unsigned(SensorValue_o)))/128.0;

  ExtNames_1: ExtNames
    port map (
      SPIFSM_Done => SPIFSM_Done_i
    );
  SPIFSM_Done_d <= SPIFSM_Done_i after 1.0 ns;
  SPIFSM_Done_a <= SPIFSM_Done_i and SPIFSM_Done_d;

  spi_master_1: spi_master
    generic map (
      DataWidth      => DataWidth,
      SPPRWidth      => SPPRWidth,
      SPRWidth       => SPRWidth,
      FIFOReadWidth  => SPIFIFOReadWidth,
      FIFOWriteWidth => SPIFIFOWriteWidth
    )
    port map (
      Reset_n        => Reset_n_i,
      Clk            => Clk_i,
      -- IO
      SCK_o          => SCLK_s,
      MOSI_o         => DIN_s,
      MISO_i         => DOUT_s,
      -- control signals
      CPOL_i         => SPI_CPOL_o,
      CPHA_i         => SPI_CPHA_o,
      LSBFE_i        => SPI_LSBFE_o,
      SPPR_i         => SPI_SPPR_SPR_o(7 downto 4),
      SPR_i          => SPI_SPPR_SPR_o(3 downto 0),
      Transmission_o => SPI_Transmission_i,
      Write_i        => SPI_Write_o,
      ReadNext_i     => SPI_ReadNext_o,
      Data_i         => SPI_Data_o,
      Data_o         => SPI_Data_i,
      FIFOFull_o     => SPI_FIFOFull_i,
      FIFOEmpty_o    => SPI_FIFOEmpty_i,
      ScanEnable_i   => SPI_ScanEnable_s,
      ScanClk_i      => SPI_ScanClk_s,
      ScanDataIn_i   => SPI_ScanDataIn_s,
      ScanDataOut_o  => SPI_ScanDataOut_s
    );

  adt7310_1: adt7310_model
    port map (
      SCLK_i  => SCLK_s,
      DOUT_o  => DOUT_s,
      DIN_i   => DIN_s,
      CS_n_i  => ADT7310CS_n_o,
      CT_n_o  => CT_n_s,
      INT_n_o => INT_n_s,
      Temp_i  => Temp_s);

  -- constant value for reconfig signal
  SPI_SPPR_SPR_o <= "00000000";

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    Enable_i <= '0';
    SPICounterPreset_i     <= "0101110111000000";
    Threshold_i            <= "0000000000011110";
    PeriodCounterPresetH_i <= "0000000000000000";
    PeriodCounterPresetL_i <= "0000000000001010";

    wait for 2.3*ClkPeriode;

    assert SPI_CPOL_o = '1'
      report "Dynamic signal SPI_CPOL_o should have constant value '1'"  severity failure;
    assert SPI_CPHA_o = '1'
      report "Dynamic signal SPI_CPHA_o should have constant value '1'"  severity failure;
    assert SPI_LSBFE_o = '0'
      report "Dynamic signal SPI_LSBFE_o should have constant value '0'"  severity failure;

    -- deassert Reset
    Reset_n_i <= '1';
    wait for 1.3*ClkPeriode;                     -- wait until spi_master's SCK_o goes '1' to conform to CPOL_i = '1'

    Temp_s <= 23.7;                     -- degree C

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    -- enable SensorFSM
    Enable_i <= '1';
    wait until SPIFSM_Done_d = '1';
    assert ADT7310CS_n_o = '1' report "CS_n should be '1' when SPIFSM is done" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after SPIFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after SPIFSM is done" severity error;
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
    wait until SPIFSM_Done_d = '1';
    assert ADT7310CS_n_o = '1' report "CS_n should be '1' when SPIFSM is done" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after SPIFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after SPIFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.6;
    wait until SPIFSM_Done_d = '1';
    assert ADT7310CS_n_o = '1' report "CS_n should be '1' when SPIFSM is done" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after SPIFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after SPIFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.5;
    wait until SPIFSM_Done_d = '1';
    assert ADT7310CS_n_o = '1' report "CS_n should be '1' when SPIFSM is done" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after SPIFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after SPIFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with large difference -> notify required
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.4;
    wait until SPIFSM_Done_d = '1';
    assert ADT7310CS_n_o = '1' report "CS_n should be '1' when SPIFSM is done" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after SPIFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after SPIFSM is done" severity error;
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
