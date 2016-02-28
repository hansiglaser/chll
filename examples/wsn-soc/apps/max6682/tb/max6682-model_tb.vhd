-------------------------------------------------------------------------------
-- Title      : Testbench for design "MAX6682_Model"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : max6682_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2011-04-14
-- Last update: 2011-04-14
-- Platform   : 
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2011 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2011-04-14  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

entity MAX6682_Model_tb is

end MAX6682_Model_tb;

-------------------------------------------------------------------------------

architecture behavior of MAX6682_Model_tb is

  component MAX6682_Model
    port (
      ChipSelect_n_i : in  std_logic;
      SCLK_i         : in  std_logic;
      SO_o           : out std_logic;
      Value_i        : in  std_logic_vector(10 downto 0));
  end component;

  -- component ports
  signal ChipSelect_n_i : std_logic;
  signal SCLK_i         : std_logic;
  signal SO_o           : std_logic;
  signal Value_i        : std_logic_vector(10 downto 0);

  -- clock
  signal Clk : std_logic := '0';

begin  -- behavior

  -- component instantiation
  DUT: MAX6682_Model
    port map (
      ChipSelect_n_i => ChipSelect_n_i,
      SCLK_i         => SCLK_i,
      SO_o           => SO_o,
      Value_i        => Value_i);

  -- waveform generation
  WaveGen_Proc: process
  begin
    ChipSelect_n_i <= '1';
    SCLK_i         <= '0';
    Value_i        <= "10110011101";
    -- 1st round
    wait for 1 us;
    ChipSelect_n_i <= '0';
    wait for 200 ns;
    for i in 0 to 14 loop
      SCLK_i <= '1';
      wait for 100 ns;
      SCLK_i <= '0';
      wait for 100 ns;
    end loop;  -- i
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;
    ChipSelect_n_i <= '1';
    wait for 100 ns;
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;

    -- 2nd round
    Value_i        <= "11010011010";
    wait for 1 us;
    ChipSelect_n_i <= '0';
    wait for 200 ns;
    for i in 0 to 14 loop
      SCLK_i <= '1';
      wait for 100 ns;
      if i = 5 then
        Value_i <= (others => '0');
      end if;
      SCLK_i <= '0';
      wait for 100 ns;
    end loop;  -- i
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;
    ChipSelect_n_i <= '1';
    wait for 200 ns;
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;
    wait for 200 ns;

    -- 3rd round
    Value_i        <= "00011010011";
    wait for 1 us;
    ChipSelect_n_i <= '0';
    wait for 200 ns;
    for i in 0 to 8 loop
      SCLK_i <= '1';
      wait for 100 ns;
      SCLK_i <= '0';
      wait for 100 ns;
    end loop;  -- i
    ChipSelect_n_i <= '1';
    wait for 200 ns;
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;
    wait for 200 ns;

    -- 4th round
    Value_i        <= "11100101000";
    wait for 1 us;
    ChipSelect_n_i <= '0';
    wait for 200 ns;
    for i in 0 to 10 loop
      SCLK_i <= '1';
      wait for 100 ns;
      SCLK_i <= '0';
      wait for 100 ns;
    end loop;  -- i
    ChipSelect_n_i <= '1';
    wait for 200 ns;
    assert SO_o = 'Z' report "SO_o should be 'Z'" severity error;
    wait for 200 ns;

    assert false report "*** Simulation Finished ***" severity failure;
    
  end process WaveGen_Proc;
  

end behavior;

-------------------------------------------------------------------------------

configuration MAX6682_Model_tb_behavior_cfg of MAX6682_Model_tb is
  for behavior
  end for;
end MAX6682_Model_tb_behavior_cfg;

-------------------------------------------------------------------------------
