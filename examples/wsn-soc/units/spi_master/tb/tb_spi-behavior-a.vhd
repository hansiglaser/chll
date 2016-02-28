
 
architecture behavior of tb_SPI is

constant DataWidth      : integer range 2 to 64 := 8;
constant SPPRWidth      : integer range 1 to  8 := 3;
constant SPRWidth       : integer range 1 to  8 := 3;
constant FIFOReadWidth  : integer range 2 to 10 := 2;
constant FIFOWriteWidth : integer range 2 to 10 := 2;

-- Component Declaration for the Unit Under Test (UUT)
component SPI_Master is
  Generic ( 
    DataWidth      : integer range 2 to 64 := 8;
    SPPRWidth      : integer range 1 to  8 := 3;
    SPRWidth       : integer range 1 to  8 := 3;
    FIFOReadWidth  : integer range 2 to 10 := 4;
    FIFOWriteWidth : integer range 2 to 10 := 4);
  Port ( 
    Reset_n        : in  STD_LOGIC;
    Clk            : in  STD_LOGIC;
    CPOL_i         : in  STD_LOGIC;
    CPHA_i         : in  STD_LOGIC;
    LSBFE_i        : in  STD_LOGIC;
    SPPR_i         : in  STD_LOGIC_VECTOR (SPPRWidth - 1 downto 0);
    SPR_i          : in  STD_LOGIC_VECTOR (SPRWidth - 1 downto 0);
    SCK_o          : out STD_LOGIC;
    MOSI_o         : out STD_LOGIC;
    MISO_i         : in  STD_LOGIC;
    Transmission_o : out STD_LOGIC;
    Write_i        : in  STD_LOGIC;
    ReadNext_i     : in  STD_LOGIC;
    Data_i         : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    Data_o         : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    FIFOFull_o     : out STD_LOGIC;
    FIFOEmpty_o    : out STD_LOGIC);
end component;

-- Inputs
signal Reset_n     : STD_LOGIC := '0';
signal Clk         : STD_LOGIC := '0';
signal CPOL        : STD_LOGIC := '0';
signal CPHA        : STD_LOGIC := '0';
signal LSBFE       : STD_LOGIC := '0';
signal SPPR        : STD_LOGIC_VECTOR (SPPRWidth - 1 downto 0) := "010"; --:= (others => '0'); --:= "010";
signal SPR         : STD_LOGIC_VECTOR (SPRWidth - 1 downto 0) := (others => '0');
signal MISO        : STD_LOGIC := '0';
signal WriteData   : STD_LOGIC := '0';
signal ReadNext    : STD_LOGIC := '0';
signal DataIn      : STD_LOGIC_VECTOR (DataWidth - 1 downto 0) := (others => '0');

-- Outputs
signal SCK          : STD_LOGIC;
signal MOSI         : STD_LOGIC;
signal Transmission : STD_LOGIC;
signal DataOut      : STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
signal FIFOFull     : STD_LOGIC;
signal FIFOEmpty    : STD_LOGIC;

-- Clock period definitions
constant Clk_period : time := 10 us;
constant Clk_delay  : time := Clk_period/10;

begin
  -- Instantiate the Unit Under Test (UUT)
  uut: SPI_Master
    Generic Map (
      DataWidth      => DataWidth,
      SPPRWidth      => SPPRWidth,
      SPRWidth       => SPRWidth,
      FIFOReadWidth  => FIFOReadWidth,
      FIFOWriteWidth => FIFOWriteWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      CPOL_i         => CPOL,
      CPHA_i         => CPHA,
      LSBFE_i        => LSBFE,
      SPPR_i         => SPPR,
      SPR_i          => SPR,
      SCK_o          => SCK,
      MOSI_o         => MOSI,
      MISO_i         => MISO,
      Transmission_o => Transmission,
      Write_i        => WriteData,
      ReadNext_i     => ReadNext,
      Data_i         => DataIn,
      Data_o         => DataOut,
      FIFOFull_o     => FIFOFull,
      FIFOEmpty_o    => FIFOEmpty);

  -- Clock process definitions
  Clk_process :process
  begin
    Clk <= '0';
    wait for Clk_period/2;
    Clk <= '1';
    wait for Clk_period/2;
  end process Clk_process;

  -- loopback inverted signal in "slave"
  MISO <= not MOSI;

  -- Stimulus process
  stim_proc: process
  begin		
    -- hold reset state for Clk_period*5.
    wait for Clk_period*5;	
    Reset_n <= '1';
    wait for Clk_period*5;
    
    -- testcase 1: test for initial state
    -- end testcase 1;
    
    wait until Clk'event and Clk = '1';
    DataIn <= "11001010";
    WriteData <= '1';
    wait until Clk'event and Clk = '1';
    wait until Clk'event and Clk = '1';
    WriteData <= '0';
    
    -- insert some space time
    wait for Clk_period*100;
    
    -- end simulation
    report "NONE. Simulation finished" severity failure; -- used to stop simulation
  end process;

end behavior;

