----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
--
-- Create Date: 19:47:48 01/31/2010
-- Design Name: SPI
-- Module Name: SPIFrqDiv - RTL
-- Description: Frequency Divider for SPI
--
-- Revision:
-- Revision 0.01 - File Created
--
-- Associated Testbench:
--   tb_SPIFrqDiv.vhd
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity SPIFrqDiv is
  Generic (
    SPPRWidth      : integer range 1 to 8 := 3;
    SPRWidth       : integer range 1 to 8 := 3);
  Port (
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    SPPR_i         : in  STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
    SPR_i          : in  STD_LOGIC_VECTOR(SPRWidth-1 downto 0);
    EnFrqDivider_i : in  STD_LOGIC;
    NextStep_o     : out STD_LOGIC);
end SPIFrqDiv;

