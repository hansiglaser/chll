library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.tbfuncs.all;

entity ByteRegister_tb is
end ByteRegister_tb;

architecture behavior of ByteRegister_tb is

  component ByteRegister
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      D_i : in std_logic_vector(7 downto 0);
      Q_o : out std_logic_vector(7 downto 0);
      Enable_i : in std_logic
    );
  end component;

  constant CfgClkHalfPeriode   : time := 100 ns;

  -- component generics
  constant Width : integer := 8;

  -- component ports
  -- Reset
  signal Reset_n_i     : std_logic := '0';
  -- Clock
  signal Clk_i         : std_logic := '1';
  signal D_i           : std_logic_vector(Width-1 downto 0);
  signal Q_o           : std_logic_vector(Width-1 downto 0);
  signal Enable_i      : std_logic;

  constant ClkPeriode : time := 10 ns;

  -- purpose: Check the output of the ByteRegister
  procedure CheckByteRegister (
    constant Q   : in std_logic_vector(Width-1 downto 0);
    signal   Q_o : in std_logic_vector(Width-1 downto 0)) is
  begin  -- CheckByteRegister
    assert Q_o = Q report "Wrong output value " & Vector2String(Q_o)  & ", should be " & Vector2String(Q) severity error;
  end CheckByteRegister;

begin  -- behavior

  -- component instantiation
  DUT: ByteRegister
    port map (
      Reset_n_i     => Reset_n_i,
      Clk_i         => Clk_i,
      D_i           => D_i,
      Q_o           => Q_o,
      Enable_i      => Enable_i
    );

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process 
  begin
    D_i          <= (others => '0');
    Enable_i     <= '0';

    wait for 2.3*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    ---------------------------------------------------------------------------
    -- Action
    ---------------------------------------------------------------------------
    -- 0 after reset
    CheckByteRegister((others => '0'),Q_o);
    -- test a view values while Enable_i = 0
    for i in 0 to 2**Width-1 loop
      D_i <= std_logic_vector(to_unsigned(i,Width));
      wait for ClkPeriode;
      CheckByteRegister((others => '0'),Q_o);
    end loop;  -- i
    -- test a view values while Enable_i = 1
    Enable_i <= '1';
    for i in 0 to 2**Width-1 loop
      D_i <= std_logic_vector(to_unsigned(i,Width));
      wait for ClkPeriode;
      CheckByteRegister(D_i,Q_o);
    end loop;  -- i
    -- test a view values while Enable_i = 0
    Enable_i <= '0';
    for i in 0 to 2**Width-1 loop
      D_i <= std_logic_vector(to_unsigned(i,Width));
      wait for ClkPeriode;
      CheckByteRegister((others => '1'),Q_o);  -- last value
    end loop;  -- i
    -- 0 after reset
    Reset_n_i <= '0';
    wait for ClkPeriode;
    Reset_n_i <= '1';
    wait for ClkPeriode;
    CheckByteRegister((others => '0'),Q_o);
    
    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;

end behavior;
