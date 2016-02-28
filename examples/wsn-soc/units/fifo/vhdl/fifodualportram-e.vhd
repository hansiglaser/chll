----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
-- 
-- Create Date: 14:11:00 11/22/2009 
-- Design Name: FIFO
-- Module Name: FIFODualPortRam - rtl 
-- Description: Dual port RAM for FIFO
--
-- Revision: 
-- Revision 0.01 - File Created
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
--use IEEE.RTL_ATTRIBUTES.ALL; 
-- from IEEE Std 1076.6-2004 (not supported by every tool)
--   please refer to chapter 6.5.2 Random-access memory (RAM) and chapter 7.1.5.3 Logic block
-- alternative disable "RAM Extraction" in Design Goals & Strategies

entity FIFODualPortRam is
  Generic (
    DataWidth      : integer range 2 to 64 := 8;
    AdressWidth    : integer range 2 to 10 := 4);
  Port ( 
    Reset_n_i      : in  STD_LOGIC;
    ClkA           : in  STD_LOGIC;
    DataA_i        : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    AdressA_i      : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    WriteEnableA_i : in  STD_LOGIC;
    DataB_o        : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    AdressB_i      : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0));
end FIFODualPortRam;

