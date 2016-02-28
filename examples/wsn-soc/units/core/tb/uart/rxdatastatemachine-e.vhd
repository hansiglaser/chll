----------------------------------------------------------------------------------
-- Company:   TU Vienna
-- Engineer:  Armin FALTINGER
-- 
-- Create Date:    13:13:02 12/01/2009
-- Module Name:    RxDataStateMachine - RTL
-- Project Name:   Uart
-- Description:    module to receive serial data, enable the baud generator,
--                   store the data into the RxFifo and manage the error bits
--
-- Dependencies:   pure RTL
--                 package: UartPkg
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
-- Package with the global constants
use work.UartPkg.all;

-- sampling behavior
--   sample 3 bits @ the 4th samplingenable decide what bit was received
--   go to next state / sample next received bit

entity RxDataStateMachine is
  generic ( MaxDataWidth     : integer range 2 to 64;
            -- (2^Oversampling)-1 = number of samples for every bit
            Oversampling     : integer range 2 to 2  -- limited to 2, because majority decision would be too complicated
          );
  Port ( Reset_i_n           : in  STD_LOGIC;
         -- clock input
         Clk_i               : in  STD_LOGIC;
         -- baud clock enable signals
         SamplingBaudClk_i   : in  STD_LOGIC;
         BaudClk_i           : in  STD_LOGIC;
         -- Status of Rx Fifo
         FifoFull_i          : in  STD_LOGIC;
         -- Configuration bits
         BitSelect_i         : in  BitSelectionType;
         ParityOn_i          : in  STD_LOGIC;
         ParityEvenOdd_i     : in  ParityType;
         -- Serial Data Input
         RxD_i               : in  STD_LOGIC;
         -- Enable outputs
         --   Enable of the BaudrateGenerator
         BGEnable_o          : out STD_LOGIC;
         --   Parallel Data Output to RX FIFO
         ParallelData_o      : out STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
         --   Enable Fifo Write
         WriteParallelData_o : out STD_LOGIC;
         -- Error Bits
         ParityError_o       : out STD_LOGIC;
         StopBitError_o      : out STD_LOGIC;
         RxBufferFullError_o : out STD_LOGIC);
end RxDataStateMachine;

