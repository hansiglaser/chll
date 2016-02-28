--------------------------------------------------------------------------------
-- Company:        vienna university of technology
-- Engineer:       mario faschang
-- Create Date:    14:24:06 11/30/2009
-- Module Name:    i2ccore - rtl
-- Project Name:   i2c master controller
-- Description:  * the core controller (which basically is a mealy-state-
--                 machine) is able to communicate with 7-bit-i2c-slaves as a
--                 single master on the i2c bus
--               * as long as DoTransfer_i is '1' the core-controller sends
--                 bytes (from Data_i) to a slave on the bus or reads bytes
--                 from the slave (to Data_o) depending on the value of
--                 ReadWrite_n_i
--               * every transfer starts with an addressing-procedure and then
--                 continues with reading or writing bytes. the slave's
--                 address has to be available at data_i from the beginning
--                 of the procedure till first rising edge of ByteReady_o
--               * the i2c-core does not read SCL-Line so clock-
--                 synchronization, arbitration, multi-master and clock-
--                 stretching is not supported.
--               * if the data at SDA does not match the i2c-core's SDA_o,
--                 communication will stop and BusErr_o will be set to '1'
--                 till DoTransfer_i is '0' again.
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity i2ccore is
    Generic ( DividerWidth_g : integer range 4 to 32);

    Port ( Reset_i        : in      STD_LOGIC;
           Clk_i          : in      STD_LOGIC;
           Data_i         : in      STD_LOGIC_VECTOR (7 downto 0);
           Data_o         : out     STD_LOGIC_VECTOR (7 downto 0);
           DoTransfer_i   : in      STD_LOGIC;
           ReadWrite_n_i  : in      STD_LOGIC;
           AckTx_i        : in      STD_LOGIC;
           AckRx_o        : out     STD_LOGIC;
           AckValid_o     : out     STD_LOGIC;
           Busy_o         : out     STD_LOGIC;
           ByteReady_o    : out     STD_LOGIC;
           BusErr_o       : out     STD_LOGIC;
           SDA_o          : out     STD_LOGIC;
           SDA_i          : in      STD_LOGIC;
           SCL_o          : out     STD_LOGIC;
           F100_400_n_i   : in      STD_LOGIC;
           Divider800_i   : in      std_logic_vector(DividerWidth_g-1 downto 0));
end i2ccore;

