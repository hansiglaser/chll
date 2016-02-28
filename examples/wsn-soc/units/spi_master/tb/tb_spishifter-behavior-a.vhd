

architecture behavior of tb_SPIShifter is

constant DataWidth : integer range 2 to 64 := 8;

-- Component Declaration for the Unit Under Test (UUT)
component SPIShifter is
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

-- Inputs
signal Reset_n      : STD_LOGIC := '0';
signal Clk          : STD_LOGIC := '0';
signal LSBFE        : STD_LOGIC := '0';
signal MISO         : STD_LOGIC := '0';
signal LdShifter    : STD_LOGIC := '0';
signal EnShift      : STD_LOGIC := '0';
signal EnSample     : STD_LOGIC := '0';
signal DataIn       : STD_LOGIC_VECTOR(DataWidth-1 downto 0) := (others => '0');

-- Outputs
signal MOSI         : STD_LOGIC;
signal DataOut      : STD_LOGIC_VECTOR(DataWidth-1 downto 0);

-- Clock period definitions
constant Clk_period : time := 10 us;
constant Clk_delay  : time := Clk_period/10;

-- constant test data
type TestData_t is array(2 to 3) of STD_LOGIC_VECTOR(DataWidth-1 downto 0);
constant TestData : TestData_t := ("11001010",
                                   "11100010");

-- test procedure
procedure Testcase (       Testcase   : in  integer;
                           TestData   : in  TestData_t;
                           LSBFEnable : in  STD_LOGIC;
                    signal DataIn     : out STD_LOGIC_VECTOR(DataWidth-1 downto 0);
                    signal DataOut    : in  STD_LOGIC_VECTOR(DataWidth-1 downto 0);
                    signal MOSI       : in  STD_LOGIC;
                    signal LdShifter  : out STD_LOGIC;
                    signal EnShift    : out STD_LOGIC;
                    signal EnSample   : out STD_LOGIC;
                    signal LSBFE      : out STD_LOGIC) is
begin
  wait until Clk'event and Clk = '1';
  report "testcase " & integer'image(Testcase) severity note;
  wait for Clk_delay;
  DataIn <= TestData(Testcase);
  LSBFE  <= LSBFEnable;
  LdShifter <= '1';
  wait until Clk'event and Clk = '1';
  wait for Clk_delay;
  LdShifter <= '0';
  for BitCount in DataWidth-1 downto 0 loop
    EnShift  <= '0';
    EnSample <= '1';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    if BitCount = 0 then
      EnShift <= '0';
      assert DataOut = not Testdata(Testcase)
        report "Data_o incorrect"
        severity error;
    else
      EnShift <= '1';
    end if;
    EnSample <= '0';
    if LSBFEnable = '0' then
      assert MOSI = Testdata(Testcase)(BitCount)
        report "MOSI incorrect"
        severity error;
    else
      assert MOSI = Testdata(Testcase)(DataWidth-1 - BitCount)
        report "MOSI incorrect"
        severity error;
    end if;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
  end loop;
  wait until Clk'event and Clk = '1';
  wait until Clk'event and Clk = '1';
end procedure Testcase;

begin
  -- Instantiate the Unit Under Test (UUT)
  uut: SPIShifter
    Generic Map (
      DataWidth   => DataWidth)
    Port Map (
      Reset_n     => Reset_n,
      Clk         => Clk,
      LSBFE_i     => LSBFE,
      MOSI_o      => MOSI,
      MISO_i      => MISO,
      LdShifter_i => LdShifter,
      EnShift_i   => EnShift,
      EnSample_i  => EnSample,
      Data_i      => DataIn,
      Data_o      => DataOut);

  -- Clock process definitions
  Clk_process: process
  begin
    Clk <= '0';
    wait for Clk_period/2;
    Clk <= '1';
    wait for Clk_period/2;
  end process Clk_process;

  -- MOSI loopback
  MISO <= not MOSI;

  -- Stimulus process
  stim_proc: process

  begin
    -- hold reset state for Clk_period*5.
    wait for Clk_period*5;
    Reset_n <= '1';
    wait for Clk_period*5;

    -- testcase 1: test for initial state
    report "testcase 1" severity note;
    assert MOSI = '1'
      report "MOSI_o should be '1' after reset"
      severity error;
    assert DataOut = 2**DataWidth-1
      report "Data_o should be '1...1' after reset"
      severity error;
    -- end testcase 1;

    -- testcase 2: LSBFE = '0'
    Testcase(2, Testdata, '0', DataIn, DataOut, MOSI, LdShifter, EnShift, EnSample, LSBFE);
    -- end testcase 2;

    -- testcase 3: LSBFE = '1'
    Testcase(3, Testdata, '1', DataIn, DataOut, MOSI, LdShifter, EnShift, EnSample, LSBFE);
    -- end testcase 3;

    -- insert some space time
    wait for Clk_period*5;

    -- end simulation
    report "NONE. Simulation finished" severity failure; -- used to stop simulation
  end process;

end behavior;

