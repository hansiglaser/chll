library IEEE;
use IEEE.std_logic_1164.all;

package BusMasters is

  ------------------------------------------------------------------------------
  -- FIFO ----------------------------------------------------------------------
  ------------------------------------------------------------------------------

  component FIFOSyncTop
    Generic (
      DataWidth   : integer range 2 to 64 := 8;
      AdressWidth : integer range 2 to 10 := 4);
    Port (
      Reset_n     : in  STD_LOGIC;
      Clk         : in  STD_LOGIC;
      DataA_i     : in  STD_LOGIC_VECTOR(DataWidth-1 downto 0);
      WriteA_i    : in  STD_LOGIC;
      DataB_o     : out STD_LOGIC_VECTOR(DataWidth-1 downto 0);
      ReadNextB_i : in  STD_LOGIC;
      FIFOFull_o  : out STD_LOGIC;
      FIFOEmpty_o : out STD_LOGIC);
  end component;

  component FIFOSemiSyncTop
    Generic (
      DataWidth   : integer range 2 to 64 := 8;
      AdressWidth : integer range 2 to 10 := 4);
    Port (
      Reset_n     : in  STD_LOGIC;
      ClkA        : in  STD_LOGIC;
      DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
      WriteA_i    : in  STD_LOGIC;
      ClkB        : in  STD_LOGIC;
      DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
      ReadNextB_i : in  STD_LOGIC;
      FIFOFull_o  : out STD_LOGIC;
      FIFOEmpty_o : out STD_LOGIC);
  end component;

  component FIFOAsyncTop
    Generic (
      DataWidth   : integer range 2 to 64 := 8;
      AdressWidth : integer range 2 to 10 := 4);
    Port (
      Reset_n     : in  STD_LOGIC;
      ClkA        : in  STD_LOGIC;
      DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
      WriteA_i    : in  STD_LOGIC;
      ClkB        : in  STD_LOGIC;
      DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
      ReadB_i     : in  STD_LOGIC;
      FIFOFull_o  : out STD_LOGIC;
      FIFOEmpty_o : out STD_LOGIC);
  end component;

  ------------------------------------------------------------------------------
  -- UART ----------------------------------------------------------------------
  ------------------------------------------------------------------------------

  -- actual DataWidth
  constant BitSelCounterSize : integer := 4;

  type BitSelectionType is (Sel5Bits, Sel6Bits, Sel7Bits, Sel8Bits, Sel9Bits);
  type ParityType is (Even, Odd);
 
  -- 5 -> Sel5Bits, ...
  function IntegerToBitSelectionType(Sel:integer) return BitSelectionType;
  -- 0 -> Even, 1 -> Odd
  function IntegerToParityType(Parity:integer) return ParityType;
  -- "000" -> Sel5Bits, ...
  function StdLogicToBitSelectionType(Sel:std_logic_vector(2 downto 0)) return BitSelectionType;
  function BitSelectionTypeToStdLogic(Sel:BitSelectionType) return std_logic_vector;
  -- '0' -> Even, '1' -> Odd
  function StdLogicToParityType(Parity:std_logic) return ParityType;
  function ParityTypeToStdLogic(Parity:ParityType) return std_logic;

  component Uart
    generic (
      MaxDataWidth                 : integer range 2 to 64;
      MaxSpeedDividerWidth         : integer;
      TxFifoAdressWidth            : integer range 2 to 10;
      RxFifoAdressWidth            : integer range 2 to 10;
      Oversampling                 : integer range 2 to 2
    );
    port (
      -- Parallel data inputs; CPU sided
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
  end component;

  ------------------------------------------------------------------------------
  -- I2C -----------------------------------------------------------------------
  ------------------------------------------------------------------------------

  component i2c_master
    generic (
      ReadCountWidth_g    : INTEGER;
      FIFOAddressWidth_g  : INTEGER;
      DividerWidth_g      : INTEGER
    );
    port (
      Reset_i             : in  STD_LOGIC;
      Clk_i               : in  STD_LOGIC;
      Divider800_i        : in  std_logic_vector(DividerWidth_g-1 downto 0);
      F100_400_n_i        : in  STD_LOGIC;

      StartProcess_i      : in  STD_LOGIC;
      ReceiveSend_n_i     : in  STD_LOGIC;
      Busy_o              : out STD_LOGIC;
      ReadCount_i         : in  STD_LOGIC_VECTOR (ReadCountWidth_g-1 downto 0);

      FIFOReadNext_i      : in  STD_LOGIC;
      FIFOWrite_i         : in  STD_LOGIC;
      FIFOEmpty_o         : out STD_LOGIC;
      FIFOFull_o          : out STD_LOGIC;

      Data_i              : in  STD_LOGIC_VECTOR(7 downto 0);
      Data_o              : out STD_LOGIC_VECTOR(7 downto 0);

      ErrAck_i            : in  STD_LOGIC;
      ErrBusColl_o        : out STD_LOGIC;
      ErrFIFOFull_o       : out STD_LOGIC;
      ErrGotNAck_o        : out STD_LOGIC;
      ErrCoreBusy_o       : out STD_LOGIC;
      ErrFIFOEmpty_o      : out STD_LOGIC;
      ErrCoreStopped_o    : out STD_LOGIC;
      ErrDevNotPresent_o  : out STD_LOGIC;
      ErrReadCountZero_o  : out STD_LOGIC;

      SDA_i               : in  STD_LOGIC;
      SDA_o               : out STD_LOGIC;
      SCL_o               : out STD_LOGIC;
      --------------------------------------------------------------------------
      -- Scan Chain
      ScanEnable_i      : in  std_logic;
      ScanClk_i         : in  std_logic;
      ScanDataIn_i      : in  std_logic;
      ScanDataOut_o     : out std_logic
    );
  end component;

  ------------------------------------------------------------------------------
  -- SPI -----------------------------------------------------------------------
  ------------------------------------------------------------------------------

  component spi_master
    generic (
      DataWidth      : integer range 2 to 64 := 8;
      SPPRWidth      : integer range 1 to  8 := 3;
      SPRWidth       : integer range 1 to  8 := 3;
      FIFOReadWidth  : integer range 2 to 10 := 4;
      FIFOWriteWidth : integer range 2 to 10 := 4);
    port (
      Reset_n        : in  STD_LOGIC;
      Clk            : in  STD_LOGIC;
      CPOL_i         : in  STD_LOGIC;
      CPHA_i         : in  STD_LOGIC;
      LSBFE_i        : in  STD_LOGIC;
      SPPR_i         : in  STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
      SPR_i          : in  STD_LOGIC_VECTOR(SPRWidth-1 downto 0);
      SCK_o          : out STD_LOGIC;
      MOSI_o         : out STD_LOGIC;
      MISO_i         : in  STD_LOGIC;
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
  end component;

end;

package body BusMasters is

  function IntegerToBitSelectionType(Sel:integer) return BitSelectionType is
  begin
       if Sel = 5 then return Sel5Bits;
    elsif Sel = 6 then return Sel6Bits;
    elsif Sel = 7 then return Sel7Bits;
    elsif Sel = 8 then return Sel8Bits;
    elsif Sel = 9 then return Sel9Bits;
    else               return Sel8Bits;
    end if;
  end IntegerToBitSelectionType;

  function IntegerToParityType(Parity:integer) return ParityType is
  begin
    if Parity = 0 then return Even;
    else               return Odd;
    end if;
  end IntegerToParityType;

  function StdLogicToBitSelectionType(Sel:std_logic_vector(2 downto 0)) return BitSelectionType is
  begin
       if Sel = "000" then return Sel5Bits;
    elsif Sel = "001" then return Sel6Bits;
    elsif Sel = "010" then return Sel7Bits;
    elsif Sel = "011" then return Sel8Bits;
    elsif Sel = "100" then return Sel9Bits;
    else                   return Sel8Bits;
    end if;
  end StdLogicToBitSelectionType;

  function BitSelectionTypeToStdLogic(Sel:BitSelectionType) return std_logic_vector is
  begin
       if Sel = Sel5Bits then return "000";
    elsif Sel = Sel6Bits then return "001";
    elsif Sel = Sel7Bits then return "010";
    elsif Sel = Sel8Bits then return "011";
    else                      return "100";
    end if;
  end BitSelectionTypeToStdLogic;

  function StdLogicToParityType(Parity:std_logic) return ParityType is
  begin
    if Parity = '0' then return Even;
    else                 return Odd;
    end if;
  end StdLogicToParityType;

  function ParityTypeToStdLogic (Parity : ParityType) return std_logic is
  begin  -- ParityTypeToStdLogic
    if Parity = Even then return '0';
    else                  return '1';
    end if;
  end ParityTypeToStdLogic;

end BusMasters;
