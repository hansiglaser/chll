----------------------------------------------------------------------------------
-- Company:     TU Vienna
-- Engineer:    Georg Blemenschitz
-- 
-- Create Date: 17:41:00 12/02/2009 
-- Design Name: FIFO
-- Module Name: FIFOSyncTop - structure 
-- Description: Top module of synchronous FIFO buffer
--
-- Revision: 
-- Revision 0.01 - File Created
--
-- Required Files:
--   FIFOSyncTop.vhd
--   |-FIFODualportRam.vhd
--   |-FIFOBinaryCounter.vhd
--   |-FIFOSyncCmp.vhd
--
-- Associated Testbench:
--   tb_FIFOSync.vhd
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

----------------------------------------------------------------------------------
--I/O Description:
--  DataWidth:   bit-width of the data input and output
--  AdressWidth: number of bits used for internal adressing
--                 buffer size = 2^AdressWidth
--  Reset_n:     active low reset
--  Clk:         clock input
--  DataA_i:     data input
--  WriteA_i:    if '1' then store DataA_i in the buffer 
--                 on next rising clock edge
--  DataB_o:     data output (data is ready after FIFOEmpty_o goes '0')
--  ReadNextB_i: if '1' then increment read pointer on next rising clock edge
--  FIFOFull_o:  indicates that buffer is full
--  FIFOEmpty_o: indicates that buffer is empty
----------------------------------------------------------------------------------

entity FIFOSyncTop is
  Generic ( 
    DataWidth   : integer range 2 to 64 := 8;
    AdressWidth : integer range 2 to 10 := 4);
  Port ( 
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    WriteA_i    : in  STD_LOGIC;
    DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    ReadNextB_i : in  STD_LOGIC;
    FIFOFull_o  : out STD_LOGIC;
    FIFOEmpty_o : out STD_LOGIC);
end FIFOSyncTop;

