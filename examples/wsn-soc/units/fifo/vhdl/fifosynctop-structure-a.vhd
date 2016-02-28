

architecture structure of FIFOSyncTop is

component FIFODualPortRam
  Generic (
    DataWidth      : integer range 2 to 64 := 8;
    AdressWidth    : integer range 2 to 10 := 4);
  Port ( 
    Reset_n_i      : in  STD_LOGIC;
    ClkA           : in  STD_LOGIC;
    DataA_i        : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    AdressA_i      : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    WriteEnableA_i : in  STD_LOGIC;
    DataB_o        : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    AdressB_i      : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0));
end component;

component FIFOBinaryCounter
  Generic (
    AdressWidth : integer range 2 to 10 := 4);
  Port ( 
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    ClkEnable_i : in  STD_LOGIC;
    Binary_o    : out STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    BinaryMSB_o : out STD_LOGIC);
end component;

component FIFOSyncCmp
  Generic (
    AdressWidth : integer range 2 to 10 := 4);
  Port ( 
    PointerA_i    : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    PointerB_i    : in  STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
    MSBPointerA_i : in  STD_LOGIC;
    MSBPointerB_i : in  STD_LOGIC;
    FIFOFull_o    : out STD_LOGIC;
    FIFOEmpty_o   : out STD_LOGIC);
end component;

signal AdressA    : STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
signal AdressB    : STD_LOGIC_VECTOR (AdressWidth - 1 downto 0);
signal MSBAdressA : STD_LOGIC;
signal MSBAdressB : STD_LOGIC;

begin
  
  DualPortRam: FIFODualPortRam
    Generic Map (
      DataWidth      => DataWidth,
      AdressWidth    => AdressWidth)
    Port Map ( 
      Reset_n_i      => Reset_n,
      ClkA           => Clk,
      DataA_i        => DataA_i,
      AdressA_i      => AdressA,
      WriteEnableA_i => WriteA_i,
      DataB_o        => DataB_o,
      AdressB_i      => AdressB);

  WriteCounter: FIFOBinaryCounter
    Generic Map (
      AdressWidth    => AdressWidth)
    Port Map ( 
      Reset_n        => Reset_n,
      Clk            => Clk,
      ClkEnable_i    => WriteA_i,
      Binary_o       => AdressA,
      BinaryMSB_o    => MSBAdressA);
  
  ReadCounter: FIFOBinaryCounter
    Generic Map (
      AdressWidth    => AdressWidth)
    Port Map ( 
      Reset_n        => Reset_n,
      Clk            => Clk,
      ClkEnable_i    => ReadNextB_i,
      Binary_o       => AdressB,
      BinaryMSB_o    => MSBAdressB);

  SyncCmp: FIFOSyncCmp
    Generic Map (
      AdressWidth    => AdressWidth)
    Port Map ( 
      PointerA_i     => AdressA,
      PointerB_i     => AdressB,
      MSBPointerA_i  => MSBAdressA,
      MSBPointerB_i  => MSBAdressB,
      FIFOFull_o     => FIFOFull_o,
      FIFOEmpty_o    => FIFOEmpty_o);

end structure;

