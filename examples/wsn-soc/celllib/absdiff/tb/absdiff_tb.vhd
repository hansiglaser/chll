library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity AbsDiff_tb is
end AbsDiff_tb;

architecture behavior of AbsDiff_tb is

  component AbsDiff
    port (
      A_i : in std_logic_vector(15 downto 0);
      B_i : in std_logic_vector(15 downto 0);
      D_o : out std_logic_vector(15 downto 0)
    );
  end component;

  constant TestCases : natural := 10000;   -- 0: try all (2**16 * 2**16) cases; >0: use random numbers
  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  -- component generics
  constant Width : integer := 16;

  -- component ports
  signal A_i : std_logic_vector(Width-1 downto 0);
  signal B_i : std_logic_vector(Width-1 downto 0);
  signal D_o : std_logic_vector(Width-1 downto 0);

  procedure CheckAbsDiff (
    constant A   : in  integer;
    constant B   : in  integer;
    signal   A_i : out std_logic_vector(Width-1 downto 0);
    signal   B_i : out std_logic_vector(Width-1 downto 0);
    signal   D_o : in  std_logic_vector(Width-1 downto 0)
  ) is 
    variable D   : integer;
    variable D_s : std_logic_vector(Width-1 downto 0);
  begin  -- CheckAbsDiff
    D := abs(A - B);
    D_s := std_logic_vector(to_unsigned(D,Width));
    -- set inputs
    A_i <= std_logic_vector(to_unsigned(A,Width));
    B_i <= std_logic_vector(to_unsigned(B,Width));
    wait for CheckOutputDelay;
    -- check output
    assert D_o = D_s
      report "Wrong Result " & Vector2String(D_o) & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & Vector2String(D_s) severity error;
    wait for SetupNextInputDelay;
  end CheckAbsDiff;
  
begin  -- behavior

  -- component instantiation
  DUT: AbsDiff
    port map (
      A_i => A_i,
      B_i => B_i,
      D_o => D_o
    );

  StimulusProc: process
    variable A  : integer;
    variable B  : integer;
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;
  begin
    if TestCases = 0 then
      for A in 0 to 2**(Width-1) loop
        for B in 0 to 2**(Width-1) loop
          CheckAbsDiff(A,B,A_i,B_i,D_o);
        end loop;  -- B
      end loop;  -- A
    else
      for i in 1 to TestCases loop
        Uniform(S1,S2,R);
        A := integer(trunc(R * real(2**Width-1)));
        Uniform(S1,S2,R);
        B := integer(trunc(R * real(2**Width-1)));
        CheckAbsDiff(A,B,A_i,B_i,D_o);
      end loop;
    end if;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;

end behavior;
