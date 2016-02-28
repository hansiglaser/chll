----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
--
-- Create Date: 17:14:00 01/18/2010
-- Design Name: SPI
-- Module Name: SPI - structure
-- Description: Top module of SPI
--
-- Revision:
-- Revision 0.01 - File Created
--
-- Required Files:
--   SPI_Master.vhd
--   |-SPIControl.vhd
--   |-SPIFrqDiv.vhd
--   |-SPIShifter.vhd
--   |-FIFOSyncTop.vhd
--     |-FIFODualportRam.vhd
--     |-FIFOBinaryCounter.vhd
--     |-FIFOSyncCmp.vhd
--
-- Associated Testbench:
--   tb_SPI.vhd
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity SPI_Master is
  Generic (
    DataWidth      : integer range 2 to 64 := 8;
    SPPRWidth      : integer range 1 to  8 := 3;
    SPRWidth       : integer range 1 to  8 := 3;
    FIFOReadWidth  : integer range 2 to 10 := 4;
    FIFOWriteWidth : integer range 2 to 10 := 4);
  Port (
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    -- configuration
    CPOL_i         : in  STD_LOGIC;
    CPHA_i         : in  STD_LOGIC;
    LSBFE_i        : in  STD_LOGIC;
    SPPR_i         : in  STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
    SPR_i          : in  STD_LOGIC_VECTOR(SPRWidth-1 downto 0);
    -- external signals
    SCK_o          : out STD_LOGIC;
    MOSI_o         : out STD_LOGIC;
    MISO_i         : in  STD_LOGIC;
    -- control signals
    Transmission_o : out STD_LOGIC;
    Write_i        : in  STD_LOGIC;
    ReadNext_i     : in  STD_LOGIC;
    Data_i         : in  STD_LOGIC_VECTOR(DataWidth-1 downto 0);
    Data_o         : out STD_LOGIC_VECTOR(DataWidth-1 downto 0);
    FIFOFull_o     : out STD_LOGIC;
    FIFOEmpty_o    : out STD_LOGIC;
    --------------------------------------------------------------------------
    -- Scan Chain
    ScanEnable_i      : in  std_logic;
    ScanClk_i         : in  std_logic;
    ScanDataIn_i      : in  std_logic;
    ScanDataOut_o     : out std_logic
    );
end SPI_Master;

