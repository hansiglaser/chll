----------------------------------------------------------------------------------
-- Company:  TU Vienna
-- Engineer: Armin Faltinger
-- 
-- Create Date:    09:35:01 11/19/2009
-- Module Name:    RxModule - structure 
-- Project Name:   Uart
-- Description:    RxModule binds all receive modules
--
-- Dependencies:   pure structure
--                 RxModule
--                  |- BaudGenerator: RXBAUD
--                  |- RxDataStateMachine: RXSM
--                  |- ErrorIndicator: RXERRORIND
--                      |- ErrorBit: PARITYERR, STOPERR, RXBUFFERR
--                  |- FIFOSyncTop: RXFIFO
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

entity RxModule is
  generic ( MaxDataWidth                 : integer range 2 to 64 := 9;
            MaxSpeedDividerWidth         : integer               := 16;
            RxFifoAdressWidth            : integer range 2 to 10 := 4;  -- 16 entries
            Oversampling                 : integer range 2 to 2  := 2);  -- only 2 allowed due to majority decision logic

  Port    ( -- Parallel data inputs; CPU sided
            RxData_o                     : out  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
            RxRd_i                       : in   STD_LOGIC;
            RxFull_o                     : out  STD_LOGIC;
            RxEmpty_o                    : out  STD_LOGIC;
            -- Configuration bits
            BitsSelect_i                 : in   BitSelectionType;
            ParityOn_i                   : in   STD_LOGIC;
            ParityEvenOdd_i              : in   ParityType;
            SpeedDivider_i               : in   STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
            -- Global Signals
            Clk_i                        : in   STD_LOGIC;
            Reset_i_n                    : in   STD_LOGIC;
            ErrorReset_i                 : in   STD_LOGIC;
            -- Error Signals
            RxParityErrorIndicator_o     : out  STD_LOGIC;
            RxStopBitErrorIndicator_o    : out  STD_LOGIC;
            RxBufferFullErrorIndicator_o : out  STD_LOGIC;
            -- Seriell input port
            RxD_i                        : in   STD_LOGIC);
end RxModule;

