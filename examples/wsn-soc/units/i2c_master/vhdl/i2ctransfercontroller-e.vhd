--------------------------------------------------------------------------------
-- Company:        vienna university of technology
-- Engineer:       mario faschang
-- Create Date:    14:24:06 11/30/2009
-- Module Name:    I2CTransferController - rtl
-- Project Name:   i2c master controller
-- Description:    This module controls both the i2c-core and the fifo in the
--                 i2c-bus-master.
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity i2ctransfercontroller is
    Generic (ReadCountWidth_g :        INTEGER  := 4); -- 4 Bit ReadCount_i Vector

    Port ( Reset_i            : in     STD_LOGIC;
           Clk_i              : in     STD_LOGIC;
           ReadCount_i        : in     STD_LOGIC_VECTOR (ReadCountWidth_g-1 downto 0);
           StartProcess_i     : in     STD_LOGIC;
           ReceiveSend_n_i    : in     STD_LOGIC;
           Busy_o             : out    STD_LOGIC;
           FiFoReadNext_o     : out    STD_LOGIC;
           FiFoWrite_o        : out    STD_LOGIC;
           FiFoEmpty_i        : in     STD_LOGIC;
           FiFoFull_i         : in     STD_LOGIC;

           CoreDoTransfer_o   : out    STD_LOGIC;
           CoreReadWrite_n_o  : out    STD_LOGIC;
           CoreAckTx_o        : out    STD_LOGIC;
           CoreAckRx_i        : in     STD_LOGIC;
           CoreAckValid_i     : in     STD_LOGIC;
           CoreBusy_i         : in     STD_LOGIC;
           CoreByteReady_i    : in     STD_LOGIC;
           CoreBusErr_i       : in     STD_LOGIC;

           ErrAck_i           : in     STD_LOGIC;
           ErrBusColl_o       : out    STD_LOGIC;
           ErrFiFoFull_o      : out    STD_LOGIC;
           ErrGotNAck_o       : out    STD_LOGIC;
           ErrCoreBusy_o      : out    STD_LOGIC;
           ErrFiFoEmpty_o     : out    STD_LOGIC;
           ErrCoreStopped_o   : out    STD_LOGIC;
           ErrDevNotPresent_o : out    STD_LOGIC;
           ErrReadCountZero_o : out    STD_LOGIC);

end i2ctransfercontroller;

