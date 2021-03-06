library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ExtADC_tb is
end ExtADC_tb;

architecture behavior of ExtADC_tb is

  component ExtADC
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      SensorPower_o : out std_logic;
      SensorStart_o : out std_logic;
      SensorReady_i : in std_logic;
      AdcStart_o : out std_logic;
      AdcDone_i : in std_logic;
      AdcValue_i : in std_logic_vector(15 downto 0);
      PeriodCounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0)
    );
  end component;

  -- Reset
  signal Reset_n_i : std_logic := '0';
  -- Clock
  signal Clk_i : std_logic := '1';
  signal Enable_i : std_logic;
  signal CpuIntr_o : std_logic;
  signal SensorPower_o : std_logic;
  signal SensorStart_o : std_logic;
  signal SensorReady_i : std_logic;
  signal AdcStart_o : std_logic;
  signal AdcDone_i : std_logic;
  constant AdcValueWidth : integer := 16;
  signal AdcValue_i : std_logic_vector(AdcValueWidth-1 downto 0);
  signal PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal SensorValue_o : std_logic_vector(15 downto 0);
  signal Threshold_i : std_logic_vector(15 downto 0);

  constant ClkPeriode : time := 10 ns;

begin

  DUT: ExtADC
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Enable_i,
      CpuIntr_o => CpuIntr_o,
      SensorPower_o => SensorPower_o,
      SensorStart_o => SensorStart_o,
      SensorReady_i => SensorReady_i,
      AdcStart_o => AdcStart_o,
      AdcDone_i => AdcDone_i,
      AdcValue_i => AdcValue_i,
      PeriodCounterPreset_i => PeriodCounterPreset_i,
      SensorValue_o => SensorValue_o,
      Threshold_i => Threshold_i
    );

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process 
  begin
    Enable_i <= '0';
    SensorReady_i <= '0';
    AdcDone_i <= '0';
    AdcValue_i <= (others => '0');
    PeriodCounterPreset_i <= "0000000000001010";
    Threshold_i <= "0000000000001010";

    -- Check constant values of dynamic signals coming out of the application modules
    wait for 0.1*ClkPeriode;
    -- none to check

    wait for 2.2*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    -- enable SensorFSM
    Enable_i <= '1';
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
