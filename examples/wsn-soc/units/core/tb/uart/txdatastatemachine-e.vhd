----------------------------------------------------------------------------------
-- Company:  TU Vienna
-- Engineer: Armin FALTINGER
-- 
-- Create Date:  16:15:33 11/28/2009
-- Module Name:  TxDataStateMachine - RTL
-- Project Name: Uart
-- Description:  module to transmit serial data, enable the baud generator,
--                 data to transmit is taken from TxFifo
--
-- Dependencies: pure RTL, no reference to other modules
--               packages: UartPkg
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use work.BusMasters.all;

entity TxDataStateMachine is
  generic ( MaxDataWidth    : integer range 2 to 64:= 9);  -- for synthesis
  port    ( Reset_i_n       : in  STD_LOGIC;
            -- Clock inputs
            Clk_i           : in  STD_LOGIC;
            -- BaudClock
            BaudClk_i       : in  STD_LOGIC;
            -- Parallel data input from fifo
            ParallelData_i  : in  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
            -- Status of the fifo
            FifoEmpty_i     : in  STD_LOGIC;
            -- Configuration bits
            BitSelect_i     : in  BitSelectionType;
            ParityOn_i      : in  STD_LOGIC;
            ParityEvenOdd_i : in  ParityType;
            -- controll outputs for fifo and Baudgenerator
              -- loadData from fifo
            LoadData_o      : out STD_LOGIC;
              -- enable for baudgenerator
            BGEnable_o      : out STD_LOGIC;
            -- Serial data output
            TxD_o           : out STD_LOGIC);
end TxDataStateMachine;

