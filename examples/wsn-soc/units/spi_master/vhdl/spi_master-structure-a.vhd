

architecture structure of SPI_Master is

-- component declarations
component SPIControl
  Generic (
    DataWidth      : integer range 2 to 64 := 8);
  Port (
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    -- SPI config param
    CPOL_i         : in  STD_LOGIC;
    CPHA_i         : in  STD_LOGIC;
    -- SPI clock output
    SCK_o          : out STD_LOGIC;
    -- SPI control signals
    Transmission_o : out STD_LOGIC;
    EnFrqDivider_o : out STD_LOGIC;
    NextStep_i     : in  STD_LOGIC;
    LdShifter_o    : out STD_LOGIC;
    EnShift_o      : out STD_LOGIC;
    EnSample_o     : out STD_LOGIC;
    WrFIFOEmpty_i  : in  STD_LOGIC;
    RdWriteFIFO_o  : out STD_LOGIC;
    RdFIFOFull_i   : in  STD_LOGIC;
    LdReadFIFO_o   : out STD_LOGIC);
end component;

component SPIFrqDiv
  Generic (
    SPPRWidth      : integer range 1 to 8 := 3;
    SPRWidth       : integer range 1 to 8 := 3);
  Port (
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    SPPR_i         : in  STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
    SPR_i          : in  STD_LOGIC_VECTOR(SPRWidth-1 downto 0);
    EnFrqDivider_i : in  STD_LOGIC;
    NextStep_o     : out STD_LOGIC);
end component;

component SPIShifter
  Generic (
    DataWidth   : integer range 2 to 64 := 8);
  Port (
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    -- SPI config param
    LSBFE_i     : in  STD_LOGIC;
    -- SPI input/output
    MOSI_o      : out STD_LOGIC;
    MISO_i      : in  STD_LOGIC;
    -- control signals
    LdShifter_i : in  STD_LOGIC;
    EnShift_i   : in  STD_LOGIC;
    EnSample_i  : in  STD_LOGIC;
    -- data signals
    Data_i      : in  STD_LOGIC_VECTOR(DataWidth-1 downto 0);
    Data_o      : out STD_LOGIC_VECTOR(DataWidth-1 downto 0));
end component;

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

-- signal definitions
signal EnFrqDivider   : STD_LOGIC;
signal NextStep       : STD_LOGIC;
signal LoadShifter    : STD_LOGIC;
signal EnShift        : STD_LOGIC;
signal EnSample       : STD_LOGIC;
signal WriteFIFOEmpty : STD_LOGIC;
signal ReadFIFOFull   : STD_LOGIC;
signal WriteFIFORead  : STD_LOGIC;
signal ReadFIFOLoad   : STD_LOGIC;
signal WriteFIFOData  : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal ReadFIFOData   : STD_LOGIC_VECTOR(DataWidth-1 downto 0);

begin

  SPIControlUnit: SPIControl
    Generic Map (
      DataWidth      => DataWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      CPOL_i         => CPOL_i,
      CPHA_i         => CPHA_i,
      SCK_o          => SCK_o,
      Transmission_o => Transmission_o,
      EnFrqDivider_o => EnFrqDivider,
      NextStep_i     => NextStep,
      LdShifter_o    => LoadShifter,
      EnShift_o      => EnShift,
      EnSample_o     => EnSample,
      WrFIFOEmpty_i  => WriteFIFOEmpty,
      RdWriteFIFO_o  => WriteFIFORead,
      RdFIFOFull_i   => ReadFIFOFull,
      LdReadFIFO_o   => ReadFIFOLoad);

  FrqDivider: SPIFrqDiv
    Generic Map (
      SPPRWidth      => SPPRWidth,
      SPRWidth       => SPRWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      SPPR_i         => SPPR_i,
      SPR_i          => SPR_i,
      EnFrqDivider_i => EnFrqDivider,
      NextStep_o     => NextStep);

  Shifter: SPIShifter
    Generic Map (
      DataWidth      => DataWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      LSBFE_i        => LSBFE_i,
      MOSI_o         => MOSI_o,
      MISO_i         => MISO_i,
      LdShifter_i    => LoadShifter,
      EnShift_i      => EnShift,
      EnSample_i     => EnSample,
      Data_i         => WriteFIFOData,
      Data_o         => ReadFIFOData);

  WriteFIFO: FIFOSyncTop
    Generic Map (
      DataWidth   => DataWidth,
      AdressWidth => FIFOWriteWidth)
    Port Map (
      Reset_n     => Reset_n,
      Clk         => Clk,
      DataA_i     => Data_i,
      WriteA_i    => Write_i,
      DataB_o     => WriteFIFOData,
      ReadNextB_i => WriteFIFORead,
      FIFOFull_o  => FIFOFull_o,
      FIFOEmpty_o => WriteFIFOEmpty);

  ReadFIFO: FIFOSyncTop
    Generic Map (
      DataWidth   => DataWidth,
      AdressWidth => FIFOReadWidth)
    Port Map (
      Reset_n     => Reset_n,
      Clk         => Clk,
      DataA_i     => ReadFIFOData,
      WriteA_i    => ReadFIFOLoad,
      DataB_o     => Data_o,
      ReadNextB_i => ReadNext_i,
      FIFOFull_o  => ReadFIFOFull,
      FIFOEmpty_o => FIFOEmpty_o);

end structure;

