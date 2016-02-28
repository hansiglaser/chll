library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity ByteMuxOct_tb is
end ByteMuxOct_tb;

architecture behavior of ByteMuxOct_tb is

  component ByteMuxOct
    port (
      A_i : in std_logic_vector(7 downto 0);
      B_i : in std_logic_vector(7 downto 0);
      C_i : in std_logic_vector(7 downto 0);
      D_i : in std_logic_vector(7 downto 0);
      E_i : in std_logic_vector(7 downto 0);
      F_i : in std_logic_vector(7 downto 0);
      G_i : in std_logic_vector(7 downto 0);
      H_i : in std_logic_vector(7 downto 0);
      SAB_i : in std_logic;
      SC_i : in std_logic;
      SD_i : in std_logic;
      SE_i : in std_logic;
      SF_i : in std_logic;
      SG_i : in std_logic;
      SH_i : in std_logic;
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
  signal E_i : std_logic_vector(7 downto 0);
  signal F_i : std_logic_vector(7 downto 0);
  signal G_i : std_logic_vector(7 downto 0);
  signal H_i : std_logic_vector(7 downto 0);
  signal SAB_i : std_logic;
  signal SC_i : std_logic;
  signal SD_i : std_logic;
  signal SE_i : std_logic;
  signal SF_i : std_logic;
  signal SG_i : std_logic;
  signal SH_i : std_logic;
  signal Y_o : std_logic_vector(7 downto 0);

begin

  DUT: ByteMuxOct
    port map (
      A_i => A_i,
      B_i => B_i,
      C_i => C_i,
      D_i => D_i,
      E_i => E_i,
      F_i => F_i,
      G_i => G_i,
      H_i => H_i,
      SAB_i => SAB_i,
      SC_i => SC_i,
      SD_i => SD_i,
      SE_i => SE_i,
      SF_i => SF_i,
      SG_i => SG_i,
      SH_i => SH_i,
      Y_o => Y_o
    );

  StimulusProc: process 
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;
    variable S  : std_logic_vector(6 downto 0);   -- SAB..SH, SAB at 0, SH at 6

    procedure Check (
      constant SAB   : in std_logic;
      constant SC    : in std_logic;
      constant SD    : in std_logic;
      constant SE    : in std_logic;
      constant SF    : in std_logic;
      constant SG    : in std_logic;
      constant SH    : in std_logic
    ) is
      variable C  : std_logic_vector(7 downto 0);
    begin  -- Check
      SAB_i <= SAB; SC_i <= SC; SD_i <= SD; SE_i <= SE; SF_i <= SF; SG_i <= SG; SH_i <= SH;
      wait for CheckOutputDelay;
      if    SH  = '1' then C := H_i;
      elsif SG  = '1' then C := G_i;
      elsif SF  = '1' then C := F_i;
      elsif SE  = '1' then C := E_i;
      elsif SD  = '1' then C := D_i;
      elsif SC  = '1' then C := C_i;
      elsif SAB = '1' then C := B_i;
      else                 C := A_i;
      end if;
      assert Y_o = C
        report "Wrong Result Y_o = " & Vector2String(Y_o) &
          " for A_i = " & Vector2String(A_i) &
             ", B_i = " & Vector2String(B_i) &
             ", C_i = " & Vector2String(C_i) &
             ", D_i = " & Vector2String(D_i) &
             ", E_i = " & Vector2String(E_i) &
             ", F_i = " & Vector2String(F_i) &
             ", G_i = " & Vector2String(G_i) &
             ", H_i = " & Vector2String(H_i) &
             ", SAB_i = " & std_logic'image(SAB_i) &
             ", SC_i = " & std_logic'image(SC_i) &
             ", SD_i = " & std_logic'image(SD_i) &
             ", SE_i = " & std_logic'image(SE_i) &
             ", SF_i = " & std_logic'image(SF_i) &
             ", SG_i = " & std_logic'image(SG_i) &
             ", SH_i = " & std_logic'image(SH_i) &
          ", should be " & Vector2String(C) severity error;
    end Check;

  begin
    A_i <= "00000000";
    B_i <= "00000000";
    C_i <= "00000000";
    D_i <= "00000000";
    E_i <= "00000000";
    F_i <= "00000000";
    G_i <= "00000000";
    H_i <= "00000000";
    SAB_i <= '0';
    SC_i <= '0';
    SD_i <= '0';
    SE_i <= '0';
    SF_i <= '0';
    SG_i <= '0';
    SH_i <= '0';

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
      Uniform(S1,S2,R);
      E_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      F_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      G_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));
      Uniform(S1,S2,R);
      H_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(255))),8));

      for j in 0 to (2**7-1) loop
        S := std_logic_vector(to_unsigned(j,7));
        Check(S(0),S(1),S(2),S(3),S(4),S(5),S(6));
        wait for SetupNextInputDelay;
      end loop;

    end loop;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
