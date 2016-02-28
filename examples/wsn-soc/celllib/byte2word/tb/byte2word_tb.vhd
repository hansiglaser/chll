library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity Byte2Word_tb is
end Byte2Word_tb;

architecture behavior of Byte2Word_tb is

  component Byte2Word
    port (
      H_i : in std_logic_vector(7 downto 0);
      L_i : in std_logic_vector(7 downto 0);
      Y_o : out std_logic_vector(15 downto 0)
    );
  end component;

  constant TestCases : natural := 100;
  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  signal H_i : std_logic_vector(7 downto 0);
  signal L_i : std_logic_vector(7 downto 0);
  signal Y_o : std_logic_vector(15 downto 0);

begin

  DUT: Byte2Word
    port map (
      H_i => H_i,
      L_i => L_i,
      Y_o => Y_o
    );

  StimulusProc: process 
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;

  begin
    H_i <= "00000000";
    L_i <= "00000000";

    wait for SetupNextInputDelay;
    
    for i in 1 to TestCases loop
      Uniform(S1,S2,R);
      H_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      L_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));

      wait for CheckOutputDelay;
      assert Y_o = H_i & L_i
        report "Wrong Result Y_o = " & Vector2String(Y_o) &
          " for H_i = " & Vector2String(H_i) &
             ", L_i = " & Vector2String(L_i) &
          ", should be " & Vector2String(H_i & L_i) severity error;

      wait for SetupNextInputDelay;
    end loop;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
