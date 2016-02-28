----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
--
-- Create Date: 19:43:36 01/31/2010
-- Design Name: SPI
-- Module Name: SPIShifter - RTL
-- Description: Shifter for SPI
--
-- Revision:
-- Revision 0.01 - File Created
--
-- Associated Testbench:
--   tb_SPIShifter.vhd
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity SPIShifter is
  Generic (
    DataWidth   : integer range 2 to 64 := 8);
  Port (
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    -- SPI config param
    LSBFE_i     : in  STD_LOGIC;
    -- SPI input/output
    MOSI_o      : out STD_LOGIC;
    MISO_i      : in  STD_LOGIC;
    -- control signals
    LdShifter_i : in  STD_LOGIC;
    EnShift_i   : in  STD_LOGIC;
    EnSample_i  : in  STD_LOGIC;
    -- data signals
    Data_i      : in  STD_LOGIC_VECTOR(DataWidth-1 downto 0);
    Data_o      : out STD_LOGIC_VECTOR(DataWidth-1 downto 0));
end SPIShifter;

