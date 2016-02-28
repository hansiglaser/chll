library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity ByteMuxQuad_tb is
end ByteMuxQuad_tb;

architecture behavior of ByteMuxQuad_tb is

  component ByteMuxQuad
    port (
      A_i : in std_logic_vector(7 downto 0);
      B_i : in std_logic_vector(7 downto 0);
      C_i : in std_logic_vector(7 downto 0);
      D_i : in std_logic_vector(7 downto 0);
      SAB_i : in std_logic;
      SC_i : in std_logic;
      SD_i : in std_logic;
      Y_o : out std_logic_vector(7 downto 0)
    );
  end component;

  constant TestCases : natural := 100;
  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  signal A_i : std_logic_vector(7 downto 0);
  signal B_i : std_logic_vector(7 downto 0);
  signal C_i : std_logic_vector(7 downto 0);
  signal D_i : std_logic_vector(7 downto 0);
  signal SAB_i : std_logic;
  signal SC_i : std_logic;
  signal SD_i : std_logic;
  signal Y_o : std_logic_vector(7 downto 0);

begin

  DUT: ByteMuxQuad
    port map (
      A_i => A_i,
      B_i => B_i,
      C_i => C_i,
      D_i => D_i,
      SAB_i => SAB_i,
      SC_i => SC_i,
      SD_i => SD_i,
      Y_o => Y_o
    );

  StimulusProc: process 
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;

    procedure Check (
      constant SAB   : in std_logic;
      constant SC    : in std_logic;
      constant SD    : in std_logic;
      constant Correct : in std_logic_vector(7 downto 0)) is
    begin  -- Check
      SAB_i <= SAB; SC_i <= SC; SD_i <= SD;
      wait for CheckOutputDelay;
      assert Y_o = Correct
        report "Wrong Result Y_o = " & Vector2String(Y_o) &
          " for A_i = " & Vector2String(A_i) &
             ", B_i = " & Vector2String(B_i) &
             ", C_i = " & Vector2String(C_i) &
             ", D_i = " & Vector2String(D_i) &
             ", SAB_i = " & std_logic'image(SAB_i) &
             ", SC_i = " & std_logic'image(SC_i) &
             ", SD_i = " & std_logic'image(SD_i) &
          ", should be " & Vector2String(A_i) severity error;
    end Check;

  begin
    A_i <= "00000000";
    B_i <= "00000000";
    C_i <= "00000000";
    D_i <= "00000000";
    SAB_i <= '0';
    SC_i <= '0';
    SD_i <= '0';

    wait for SetupNextInputDelay;
    
    for i in 1 to TestCases loop
      Uniform(S1,S2,R);
      A_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      B_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      C_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      D_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));

      Check('0','0','0',A_i);
      Check('1','0','0',B_i);
      Check('0','1','0',C_i);
      Check('1','1','0',C_i);
      Check('0','0','1',D_i);
      Check('1','0','1',D_i);
      Check('0','1','1',D_i);
      Check('1','1','1',D_i);

      wait for SetupNextInputDelay;
    end loop;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
