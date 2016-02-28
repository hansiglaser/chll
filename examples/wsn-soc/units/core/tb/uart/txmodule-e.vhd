----------------------------------------------------------------------------------
-- Company:  TU Vienna
-- Engineer: Armin Faltinger
-- 
-- Create Date:    10:30:32 02/11/2010
-- Module Name:    TxModule - structure
-- Project Name:   Uart
-- Description:    TxModule binds all modules for transmission
--                   the generic Oversampling may be set to 0 in order to have
--                   the baudrate determined by ClockRate/SpeedDivider_i
--
-- Dependencies:   pure structure
--                 TxModule
--                  |- BaudGenerator: TXBAUD
--                  |- TxDataStateMachine: TXSM
--                  |- FIFOSyncTop: TXFIFO, 
--                      |- FIFODualPortRam: DualPortRam
--                      |- FIFOBinaryCounter: WriteCounter, ReadCounter
--                      |- FIFOSyncCmp: SyncCmp
--                 package: UartPkg
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.UartPkg.all;

entity TxModule is
  generic ( MaxDataWidth                 : integer range 2 to 64 := 9;  -- 9
            MaxSpeedDividerWidth         : integer               := 16; -- 16 bits
            TxFifoAdressWidth            : integer range 2 to 10 := 4;
            Oversampling                 : integer range 0 to 3  := 2);

  Port    ( -- Parallel data inputs; CPU sided
            TxData_i                     : in   STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
            TxWr_i                       : in   STD_LOGIC;
            TxEmpty_o                    : out  STD_LOGIC;
            TxFull_o                     : out  STD_LOGIC;
            -- Configuration bits
            BitsSelect_i                 : in   BitSelectionType;
            ParityOn_i                   : in   STD_LOGIC;
            ParityEvenOdd_i              : in   ParityType;
            SpeedDivider_i               : in   STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
            -- Global Signals
            Clk_i                        : in   STD_LOGIC;
            Reset_i_n                    : in   STD_LOGIC;
            -- Seriell in/output ports
            TxD_o                        : out  STD_LOGIC);
end TxModule;

