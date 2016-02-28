library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blinki_tb is
end blinki_tb;

architecture behavior of blinki_tb is

  component blinki
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      LED_o : out std_logic;
      PeriodH_i : in std_logic_vector(15 downto 0);
      PeriodL_i : in std_logic_vector(15 downto 0)
    );
  end component;

  -- Reset
  signal Reset_n_i : std_logic := '0';
  -- Clock
  signal Clk_i : std_logic := '1';
  signal LED_o : std_logic;
  signal PeriodH_i : std_logic_vector(15 downto 0);
  signal PeriodL_i : std_logic_vector(15 downto 0);

  constant ClkPeriode : time := 10 ns;

begin

  DUT: blinki
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      LED_o => LED_o,
      PeriodH_i => PeriodH_i,
      PeriodL_i => PeriodL_i
    );

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    PeriodH_i <= "0000000000000000";
    PeriodL_i <= "0000000000001010";

    -- Check constant values of dynamic signals coming out of the application modules
    wait for 0.1*ClkPeriode;

    wait for 2.2*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    wait for 1000*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
