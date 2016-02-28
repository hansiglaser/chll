-------------------------------------------------------------------------------
-- Title      : Testbench for design "Counter"
-- Project    : 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use work.tbfuncs.all;

-------------------------------------------------------------------------------

entity Counter_tb is

end Counter_tb;

-------------------------------------------------------------------------------

architecture behavior of Counter_tb is

  constant CfgClkHalfPeriode   : time := 100 ns;
  constant CheckOutputDelay    : time :=  20 ns;
  constant SetupNextInputDelay : time :=  20 ns;

  component Counter
    generic (
      Width : integer);
    port (
      Reset_n_i     : in  std_logic;
      Clk_i         : in  std_logic;
      ResetSig_i    : in  std_logic;
      Preset_i      : in  std_logic;
      Enable_i      : in  std_logic;
      Direction_i   : in  std_logic;
      PresetVal_i   : in  std_logic_vector(Width-1 downto 0);
      D_o           : out std_logic_vector(Width-1 downto 0);
      Overflow_o    : out std_logic;
      Zero_o        : out std_logic);
  end component;

  -- component generics
  constant Width : integer := 10;

  -- component ports
  signal Reset_n_i     : std_logic;
  signal Clk_i         : std_logic;
  signal ResetSig_i    : std_logic;
  signal Preset_i      : std_logic;
  signal Enable_i      : std_logic;
  signal Direction_i   : std_logic;
  signal PresetVal_i   : std_logic_vector(Width-1 downto 0);
  signal D_o           : std_logic_vector(Width-1 downto 0);
  signal Overflow_o    : std_logic;
  signal Zero_o        : std_logic;

  -- purpose: Check the counter outputs
  procedure CheckCounter (
    constant Value      : in std_logic_vector(Width-1 downto 0);
    constant Overflow   : in std_logic;
    signal   D_o        : in std_logic_vector(Width-1 downto 0);
    signal   Overflow_o : in std_logic;
    signal   Zero_o     : in std_logic
  ) is
    variable Zero : std_logic;
  begin  -- CheckCounter
    Zero := '0';
    if Value = 0 then
      Zero := '1';
    end if;
    assert D_o        = Value    report "Wrong output value " & Vector2String(D_o)          & ", should be " & Vector2String(Value)      severity error;
    assert OverFlow   = 'X' or
           Overflow_o = Overflow report "Wrong Overflow_o = " & std_logic'image(Overflow_o) & ", should be " & std_logic'image(Overflow) severity error;
    assert Zero_o     = Zero     report "Wrong Zero_o = "     & std_logic'image(Zero_o)     & ", should be " & std_logic'image(Zero)     severity error;
  end CheckCounter;

  -- purpose: Assert Clk_i for NumCycles clock cycles
  procedure ClkCycles (
    constant NumCycles : in  positive;
    signal   Clk_i     : out std_logic) is
  begin  -- ClkCycles
    for i in 1 to NumCycles loop
      ClkCycle(CfgClkHalfPeriode,Clk_i);
    end loop;  -- i
  end ClkCycles;

begin  -- behavior

  -- component instantiation
  DUT: Counter
    generic map (
      Width => Width)
    port map (
      Reset_n_i     => Reset_n_i,
      Clk_i         => Clk_i,
      ResetSig_i    => ResetSig_i,
      Preset_i      => Preset_i,
      Enable_i      => Enable_i,
      Direction_i   => Direction_i,
      PresetVal_i   => PresetVal_i,
      D_o           => D_o,
      Overflow_o    => Overflow_o,
      Zero_o        => Zero_o);

  -- waveform generation
  TestProc: process
  begin
    Clk_i        <= '0';
    ResetSig_i   <= '0';
    Preset_i     <= '0';
    Enable_i     <= '0';
    Direction_i  <= '0';
    PresetVal_i  <= (others => '0');
    
    ---------------------------------------------------------------------------
    -- Reset
    ---------------------------------------------------------------------------
    Reset_n_i <= '0';
    wait for 1 us;
    Reset_n_i <= '1';
    wait for 1 ns;

    ---------------------------------------------------------------------------
    -- Action
    ---------------------------------------------------------------------------
    -- 0 after Reset, Zero_o = '1'
    CheckCounter((others => '0'),'0',D_o,Overflow_o,Zero_o);
    -- 0 after counting a view cycles with Enable_i = '0'
    ClkCycles(17,Clk_i);
    CheckCounter((others => '0'),'0',D_o,Overflow_o,Zero_o);
    -- X after counting a view cycles with Enable_i = '1'
    Enable_i <= '1';
    ClkCycles(11,Clk_i);
    CheckCounter(conv_std_logic_vector(11,Width),'0',D_o,Overflow_o,Zero_o);
    -- still X after futher counting a view cycles with Enable_i = '0'
    Enable_i <= '0';
    ClkCycles(17,Clk_i);
    CheckCounter(conv_std_logic_vector(11,Width),'0',D_o,Overflow_o,Zero_o);
    -- 0 after ResetSig_i
    ResetSig_i <= '1';
    ClkCycles(1,Clk_i);
    CheckCounter((others => '0'),'0',D_o,Overflow_o,Zero_o);
    ResetSig_i <= '0';
    CheckCounter((others => '0'),'0',D_o,Overflow_o,Zero_o);
    -- X after counting a view cycles with Enable_i = '1'
    Enable_i <= '1';
    ClkCycles(15,Clk_i);
    CheckCounter(conv_std_logic_vector(15,Width),'0',D_o,Overflow_o,Zero_o);
    -- Y after counting downwards with Enable_i = '1'
    Direction_i <= '1';
    ClkCycles(11,Clk_i);
    CheckCounter(conv_std_logic_vector(4,Width),'0',D_o,Overflow_o,Zero_o);
    -- still Y after further counting in any direction with Enable_i = '0'
    Enable_i <= '0';
    ClkCycles(3,Clk_i);
    CheckCounter(conv_std_logic_vector(4,Width),'0',D_o,Overflow_o,Zero_o);
    -- PresetValue after Preset_i with Enable_i = '0'
    Direction_i <= '0';
    Enable_i <= '0';
    PresetVal_i <= conv_std_logic_vector(29,Width);
    Preset_i <= '1';
    ClkCycles(1,Clk_i);
    CheckCounter(conv_std_logic_vector(29,Width),'X',D_o,Overflow_o,Zero_o);
    Preset_i <= '0';
    CheckCounter(conv_std_logic_vector(29,Width),'X',D_o,Overflow_o,Zero_o);
    -- another PresetValue after Preset_i with Enable_i = '1'
    Enable_i <= '1';
    PresetVal_i <= conv_std_logic_vector(23,Width);
    Preset_i <= '1';
    ClkCycles(1,Clk_i);
    CheckCounter(conv_std_logic_vector(23,Width),'X',D_o,Overflow_o,Zero_o);
    Preset_i <= '0';
    CheckCounter(conv_std_logic_vector(23,Width),'X',D_o,Overflow_o,Zero_o);
    -- Preset to 0, Zero_o?
    Enable_i <= '0';
    PresetVal_i <= (others => '0');
    Preset_i <= '1';
    ClkCycles(1,Clk_i);
    CheckCounter((others => '0'),'X',D_o,Overflow_o,Zero_o);
    Preset_i <= '0';
    CheckCounter((others => '0'),'X',D_o,Overflow_o,Zero_o);
    -- Preset to nearly overflow, count upwards, overflow, zero
    Enable_i <= '1';
    for i in 1 to 2**Width-1 loop
      ClkCycles(1,Clk_i);
      CheckCounter(conv_std_logic_vector(i,Width),'0',D_o,Overflow_o,Zero_o);
    end loop;  -- i
    ClkCycles(1,Clk_i);
    CheckCounter((others => '0'),'1',D_o,Overflow_o,Zero_o);
    for i in 1 to 4 loop
      ClkCycles(1,Clk_i);
      CheckCounter(conv_std_logic_vector(i,Width),'0',D_o,Overflow_o,Zero_o);
    end loop;  -- i
    -- count downwards, zero, overflow
    Direction_i <= '1';
    for i in 3 downto 1 loop
      ClkCycles(1,Clk_i);
      CheckCounter(conv_std_logic_vector(i,Width),'0',D_o,Overflow_o,Zero_o);
    end loop;  -- i
    ClkCycles(1,Clk_i);
    CheckCounter((others => '0'),'0',D_o,Overflow_o,Zero_o);
    ClkCycles(1,Clk_i);
    CheckCounter((others => '1'),'1',D_o,Overflow_o,Zero_o);
    for i in 2**Width-2 downto 0 loop
      ClkCycles(1,Clk_i);
      CheckCounter(conv_std_logic_vector(i,Width),'0',D_o,Overflow_o,Zero_o);
    end loop;  -- i
    ClkCycles(1,Clk_i);
    CheckCounter((others => '1'),'1',D_o,Overflow_o,Zero_o);
    for i in 2**Width-2 downto 2**Width-5 loop
      ClkCycles(1,Clk_i);
      CheckCounter(conv_std_logic_vector(i,Width),'0',D_o,Overflow_o,Zero_o);
    end loop;  -- i

    ---------------------------------------------------------------------------
    -- Simulation is finished
    ---------------------------------------------------------------------------
    assert 0 = 1
      report "### simulation is finished ###"
      severity failure ;

  end process TestProc;

end behavior;

configuration Counter_tb_verilog_cfg of Counter_tb is
  for behavior
    for DUT : Counter
      -- doesn't work???
      -- use entity work.Counter(verilog);
      use configuration work.CounterVerilog;
    end for;
  end for;
end Counter_tb_verilog_cfg;

-- use Verilog implementation of the Counter cell as topological variant 1
configuration Counter_tb_verilogTV1_cfg of Counter_tb is
  for behavior
    for DUT : Counter
      use configuration work.CounterTV1Verilog;
    end for;
  end for;
end Counter_tb_verilogTV1_cfg;

