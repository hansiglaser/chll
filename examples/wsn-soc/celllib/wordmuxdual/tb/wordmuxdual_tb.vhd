library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity WordMuxDual_tb is
end WordMuxDual_tb;

architecture behavior of WordMuxDual_tb is

  component WordMuxDual
    port (
      A_i : in std_logic_vector(15 downto 0);
      B_i : in std_logic_vector(15 downto 0);
      S_i : in std_logic;
      Y_o : out std_logic_vector(15 downto 0)
    );
  end component;

  constant TestCases : natural := 100;
  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  signal A_i : std_logic_vector(15 downto 0);
  signal B_i : std_logic_vector(15 downto 0);
  signal S_i : std_logic;
  signal Y_o : std_logic_vector(15 downto 0);

begin

  DUT: WordMuxDual
    port map (
      A_i => A_i,
      B_i => B_i,
      S_i => S_i,
      Y_o => Y_o
    );

  StimulusProc: process 
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;

    procedure Check (
      constant S       : in std_logic;
      constant Correct : in std_logic_vector(15 downto 0)) is
    begin  -- Check
      S_i <= S;
      wait for CheckOutputDelay;
      assert Y_o = Correct
        report "Wrong Result Y_o = " & Vector2String(Y_o) &
          " for A_i = " & Vector2String(A_i) &
             ", B_i = " & Vector2String(B_i) &
             ", S_i = " & std_logic'image(S_i) &
          ", should be " & Vector2String(Correct) severity error;
    end Check;

  begin
    A_i <= "0000000000000000";
    B_i <= "0000000000000000";
    S_i <= '0';

    wait for SetupNextInputDelay;
    
    for i in 1 to TestCases loop
      Uniform(S1,S2,R);
      A_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(65535))),16));
      Uniform(S1,S2,R);
      B_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(65535))),16));

      Check('0',A_i);
      Check('1',B_i);

      wait for SetupNextInputDelay;
    end loop;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
