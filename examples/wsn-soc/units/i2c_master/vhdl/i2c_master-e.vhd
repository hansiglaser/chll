--------------------------------------------------------------------------------
-- Company:        vienna university of technology
-- Engineer:       mario faschang
-- Create Date:    11:12:56 19/01/2010
-- Module Name:    I2C Bus Master (TOP MODULE)
-- Project Name:   i2c master controller
-- Description:  * Top Module for the communication with slaves over the i2c-bus
--                 containing the I2CCore, I2CTransferController and the FIFO.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY i2c_master IS
  Generic ( ReadCountWidth_g    : INTEGER := 4; -- 4 Bit ReadCount_i Vector
            FIFOAddressWidth_g  : INTEGER := 4; -- 2^4 Memory Cells
            DividerWidth_g      : integer range 4 to 32 := 16);

  Port (  Reset_i               : in  STD_LOGIC;
          Clk_i                 : in  STD_LOGIC;
          F100_400_n_i          : in  STD_LOGIC;
          Divider800_i          : in  STD_LOGIC_VECTOR(DividerWidth_g-1 downto 0);

          StartProcess_i        : in  STD_LOGIC;
          ReceiveSend_n_i       : in  STD_LOGIC;
          Busy_o                : out STD_LOGIC;
          ReadCount_i           : in  STD_LOGIC_VECTOR (ReadCountWidth_g-1 downto 0);

          FIFOReadNext_i        : in  STD_LOGIC;
          FIFOWrite_i           : in  STD_LOGIC;
          FIFOEmpty_o           : out STD_LOGIC;
          FIFOFull_o            : out STD_LOGIC;

          Data_i                : in  STD_LOGIC_VECTOR(7 downto 0);
          Data_o                : out STD_LOGIC_VECTOR(7 downto 0);

          ErrAck_i              : in  STD_LOGIC;
          ErrBusColl_o          : out STD_LOGIC;
          ErrFIFOFull_o         : out STD_LOGIC;
          ErrGotNAck_o          : out STD_LOGIC;
          ErrCoreBusy_o         : out STD_LOGIC;
          ErrFIFOEmpty_o        : out STD_LOGIC;
          ErrCoreStopped_o      : out STD_LOGIC;
          ErrDevNotPresent_o    : out STD_LOGIC;
          ErrReadCountZero_o    : out STD_LOGIC;

          SDA_i                 : in  STD_LOGIC;
          SDA_o                 : out STD_LOGIC;
          SCL_o                 : out STD_LOGIC;
    --------------------------------------------------------------------------
    -- Scan Chain
    ScanEnable_i      : in  std_logic;
    ScanClk_i         : in  std_logic;
    ScanDataIn_i      : in  std_logic;
    ScanDataOut_o     : out std_logic
          );
END i2c_master;
