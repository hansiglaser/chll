----------------------------------------------------------------------------------
-- Company:  TU Vienna
-- Engineer: Armin Faltinger
-- 
-- Create Date:    09:35:01 11/19/2009
-- Module Name:    Uart - structure
-- Project Name:   Uart
-- Description:    Uart binds all modules
--
-- Dependencies:   pure structure
--                 TxModule
--                  |- BaudGenerator: TXBAUD
--                  |- TxDataStateMachine: TXSM
--                  |- FIFOSyncTop: TXFIFO, 
--                      |- FIFODualPortRam: DualPortRam
--                      |- FIFOBinaryCounter: WriteCounter, ReadCounter
--                      |- FIFOSyncCmp: SyncCmp
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

-- Uart is the implementation of the UART core
-- MaxDataWith is in the range of 5-9 bits
-- The Fifo Size of Rx and Tx - module is independent
-- The 16550 Standard is determined by a 16 entries sized FIFO
-- (2^Oversampling)-1 = number of samplings for every bit; used in RXSM
-- coding of PartiyOn: parity is on at '1', off at '0'

entity Uart is
  generic ( MaxDataWidth                 : integer range 5 to 9  := 9;  -- 9
            MaxSpeedDividerWidth         : integer range 2 to 32 := 16; -- 16 bits
            TxFifoAdressWidth            : integer range 2 to 10 := 4;  -- 16 entries
            RxFifoAdressWidth            : integer range 2 to 10 := 4;  -- 16 entries
            Oversampling                 : integer range 2 to 2  := 2); -- only 2 allowed due to majority decision logic
  Port    ( -- Parallel data inputs; CPU sided
            TxData_i                     : in   STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
            TxWr_i                       : in   STD_LOGIC;
            TxEmpty_o                    : out  STD_LOGIC;
            TxFull_o                     : out  STD_LOGIC;
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
            -- Seriell in/output ports
            TxD_o                        : out  STD_LOGIC;
            RxD_i                        : in   STD_LOGIC;
    --------------------------------------------------------------------------
    -- Scan Chain
    ScanEnable_i      : in  std_logic;
    ScanClk_i         : in  std_logic;
    ScanDataIn_i      : in  std_logic;
    ScanDataOut_o     : out std_logic
            );
end Uart;

