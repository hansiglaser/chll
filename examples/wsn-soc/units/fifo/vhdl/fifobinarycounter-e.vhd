----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
-- 
-- Create Date: 17:41:00 12/02/2009 
-- Design Name: FIFO
-- Module Name: FIFOBinaryCounter - rtl 
-- Description: Binary counter for FIFO
--
-- Revision: 
-- Revision 0.01 - File Created
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FIFOBinaryCounter is
  Generic (
    AdressWidth : integer range 2 to 10 := 4);
  Port ( 
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    ClkEnable_i : in  STD_LOGIC;
    Binary_o    : out STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    BinaryMSB_o : out STD_LOGIC);
end FIFOBinaryCounter;

