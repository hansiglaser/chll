----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
-- 
-- Create Date: 17:41:00 12/02/2009 
-- Design Name: FIFO
-- Module Name: FIFOSyncCmp - rtl 
-- Description: Synchronous compare for FIFO
--
-- Revision: 
-- Revision 0.01 - File Created
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FIFOSyncCmp is
  Generic (
    AdressWidth : integer range 2 to 10 := 4);
  Port ( 
    PointerA_i    : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    PointerB_i    : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    MSBPointerA_i : in  STD_LOGIC;
    MSBPointerB_i : in  STD_LOGIC;
    FIFOFull_o    : out STD_LOGIC;
    FIFOEmpty_o   : out STD_LOGIC);
end FIFOSyncCmp;

