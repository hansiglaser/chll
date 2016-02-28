----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
--
-- Create Date: 21:57:29 01/28/2010
-- Design Name: SPI
-- Module Name: SPIControl - RTL
-- Description: Control module for SPI
--
-- Revision:
-- Revision 0.01 - File Created
--
-- Associated Testbench:
--   tb_SPIControl.vhd
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.Utils.all;

entity SPIControl is
  Generic (
    DataWidth      : integer range 2 to 64 := 8);
  Port (
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    -- SPI config param
    CPOL_i         : in  STD_LOGIC;
    CPHA_i         : in  STD_LOGIC;
    -- SPI clock output
    SCK_o          : out STD_LOGIC;
    -- SPI control signals
    Transmission_o : out STD_LOGIC;
    EnFrqDivider_o : out STD_LOGIC;
    NextStep_i     : in  STD_LOGIC;
    LdShifter_o    : out STD_LOGIC;
    EnShift_o      : out STD_LOGIC;
    EnSample_o     : out STD_LOGIC;
    WrFIFOEmpty_i  : in  STD_LOGIC;
    RdWriteFIFO_o  : out STD_LOGIC;
    RdFIFOFull_i   : in  STD_LOGIC;
    LdReadFIFO_o   : out STD_LOGIC);
end SPIControl;

