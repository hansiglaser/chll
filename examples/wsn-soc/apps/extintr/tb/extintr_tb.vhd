library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ExtIntr_tb is
end ExtIntr_tb;

architecture behavior of ExtIntr_tb is

  component ExtIntr
    port (
      ExtIntrOut_o : out std_logic;
      ExtIntrIn_i : in std_logic
    );
  end component;

  signal ExtIntrOut_o : std_logic;
  signal ExtIntrIn_i : std_logic;

begin

  DUT: ExtIntr
    port map (
      ExtIntrOut_o => ExtIntrOut_o,
      ExtIntrIn_i => ExtIntrIn_i
    );

  StimulusProc: process
  begin
    ExtIntrIn_i <= '0';

    -- Check constant values of dynamic signals coming out of the application modules
    wait for 1 ns;

    assert ExtIntrOut_o = ExtIntrIn_i report "Wrong output value" severity failure;
    wait for 1 us;
    ExtIntrIn_i <= '1';
    wait for 1 us;
    assert ExtIntrOut_o = ExtIntrIn_i report "Wrong output value" severity failure;
    wait for 1 us;
    ExtIntrIn_i <= '0';
    wait for 1 us;
    assert ExtIntrOut_o = ExtIntrIn_i report "Wrong output value" severity failure;
    wait for 1 us;


    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
