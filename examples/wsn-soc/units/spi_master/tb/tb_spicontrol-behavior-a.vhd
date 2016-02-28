

architecture behavior of tb_SPIControl is

constant DataWidth : integer range 2 to 64 := 8;

-- Component Declaration for the Unit Under Test (UUT)
component SPIControl is
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

-- Inputs
signal Reset_n      : STD_LOGIC := '0';
signal Clk          : STD_LOGIC := '0';
signal CPOL         : STD_LOGIC := '0';
signal CPHA         : STD_LOGIC := '0';
signal NextStep     : STD_LOGIC := '1';
signal WrFIFOEmpty  : STD_LOGIC := '1';
signal RdFIFOFull   : STD_LOGIC := '0';

-- Outputs
signal SCK          : STD_LOGIC;
signal Transmission : STD_LOGIC;
signal EnFrqDivider : STD_LOGIC;
signal LdShifter    : STD_LOGIC;
signal EnShift      : STD_LOGIC;
signal EnSample     : STD_LOGIC;
signal RdWriteFIFO  : STD_LOGIC;
signal LdReadFIFO   : STD_LOGIC;

-- TB control signals
signal ClkDivby3    : STD_LOGIC := '0';

-- Clock period definitions
constant Clk_period : time := 10 us;
constant Clk_delay  : time := Clk_period/10;

begin
  -- Instantiate the Unit Under Test (UUT)
  uut: SPIControl
    Generic Map (
      DataWidth      => DataWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      CPOL_i         => CPOL,
      CPHA_i         => CPHA,
      SCK_o          => SCK,
      Transmission_o => Transmission,
      EnFrqDivider_o => EnFrqDivider,
      NextStep_i     => NextStep,
      LdShifter_o    => LdShifter,
      EnShift_o      => EnShift,
      EnSample_o     => EnSample,
      WrFIFOEmpty_i  => WrFIFOEmpty,
      RdWriteFIFO_o  => RdWriteFIFO,
      RdFIFOFull_i   => RdFIFOFull,
      LdReadFIFO_o   => LdReadFIFO);

  -- Clock process definitions
  Clk_process: process
  begin
    Clk <= '0';
    wait for Clk_period/2;
    Clk <= '1';
    wait for Clk_period/2;
  end process Clk_process;

  -- Clock Divider by 3
  ClockkDividerby3: process
  begin
    if ClkDivby3 = '0' then
      NextStep <= '1';
      wait until Clk'event and Clk = '1';
    else
      NextStep <= '0';
      wait until Clk'event and Clk = '1';
      NextStep <= '0';
      wait until Clk'event and Clk = '1';
      NextStep <= '1';
      wait until Clk'event and Clk = '1';
    end if;
  end process ClockkDividerby3;

  -- Stimulus process
  stim_proc: process

  begin
    -- hold reset state for Clk_period*5.
    wait for Clk_period*5;
    Reset_n <= '1';
    wait for Clk_period*5;

    -- testcase 1: test for initial state
    report "testcase 1" severity note;
    assert SCK = '0'
      report "SCK_o should be '0' after reset"
      severity error;
    assert Transmission = '0'
      report "Transmission_o should be '0' after reset"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0' after reset"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0' after reset"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' after reset"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' after reset"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' after reset"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' after reset"
      severity error;    
    -- end testcase 1;

    -- testcase 2: idle - transmission - transmission - idle
    --             baudrate = clk / 6
    -- testcase 2a: CPOL = 0, CPHA = 0
    report "testcase 2a" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '0';
    CPHA        <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '1';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '0';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at start of testcase 2a"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '0'; -- continue transmission
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at restart of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at restart of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at restart of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at restart of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at restart of transmission"
      severity error;      
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at end of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at end of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at end of transmission"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' at end of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at end of transmission"
      severity error;
    ClkDivby3   <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 2a;

    -- testcase 2b: CPOL = 1, CPHA = 0
    report "testcase 2b" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '1';
    CPHA        <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '1';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '0';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at start of testcase 2b"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '0'; -- continue transmission
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at restart of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at restart of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at restart of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at restart of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at restart of transmission"
      severity error;      
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at end of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at end of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at end of transmission"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' at end of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at end of transmission"
      severity error;
    ClkDivby3   <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 2b;

    -- testcase 2c: CPOL = 0, CPHA = 1
    report "testcase 2c" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '0';
    CPHA        <= '1';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '1';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '0';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at start of testcase 2c"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '0'; -- continue transmission
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at restart of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at restart of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at restart of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at restart of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at restart of transmission"
      severity error;      
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at end of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at end of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at end of transmission"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' at end of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at end of transmission"
      severity error;
    ClkDivby3   <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;    
    -- end testcase 2c;
    
    -- testcase 2d: CPOL = 1, CPHA = 1
    report "testcase 2d" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '1';
    CPHA        <= '1';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '1';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '0';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at start of testcase 2d"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '0'; -- continue transmission
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at restart of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at restart of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at restart of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at restart of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at restart of transmission"
      severity error;      
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1' at end of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at end of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at end of transmission"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' at end of transmission"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at end of transmission"
      severity error;
    ClkDivby3   <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 2d;
    -- end testcase 2;

    -- testcase 3: idle - transmission - suspend - suspend -
    --             transmission - suspend - idle
    --             baudrate = clk / 2
    -- testcase 3a: CPOL = 0, CPHA = 0
    report "testcase 3a" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '0';
    CPHA        <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '0';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '1'; -- force suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at start of testcase 3a"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' before entering suspend mode"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0' before entering suspend mode"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0' before entering suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' before entering suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' before entering suspend mode"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' before entering suspend mode"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' before entering suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    RdFIFOFull  <= '0'; -- release from suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at recovering from suspend mode"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' at recovering from suspend mode"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' at recovering from suspend mode"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at recovering from suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at recovering from suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at recovering from suspend mode"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at recovering from suspend mode"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at recovering from suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    RdFIFOFull  <= '1'; -- force suspend after transmission
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' before entering suspend mode"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0' before entering suspend mode"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0' before entering suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' before entering suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' before entering suspend mode"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' before entering suspend mode"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' before entering suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    RdFIFOFull  <= '0'; -- release from suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '0'
      report "Transmission_o should be '0'"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 3a;

    -- testcase 3b: CPOL = 0, CPHA = 1
    report "testcase 3b" severity note;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    CPOL        <= '0';
    CPHA        <= '1';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ClkDivby3   <= '0';
    WrFIFOEmpty <= '0'; -- start transmission
    RdFIFOFull  <= '1'; -- force suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at start of testcase 3b"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' after transmission has started"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' after transmission has started"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at start of transmission"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at start of transmission"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at start of transmission"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at start of transmission"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' at start of transmission"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' before entering suspend mode"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0' before entering suspend mode"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0' before entering suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' before entering suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' before entering suspend mode"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' before entering suspend mode"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' before entering suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    RdFIFOFull  <= '0'; -- release from suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0' at recovering from suspend mode"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' at recovering from suspend mode"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1' at recovering from suspend mode"
      severity error;
    assert LdShifter = '1'
      report "LdShifter_o should be '1' at recovering from suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' at recovering from suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' at recovering from suspend mode"
      severity error;
    assert RdWriteFIFO = '1'
      report "RdWriteFIFO_o should be '1' at recovering from suspend mode"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1' at recovering from suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WrFIFOEmpty <= '1';
    RdFIFOFull  <= '1'; -- force suspend after transmission
    for BitCounter in DataWidth - 1 downto 1 loop
      assert SCK = '1'
        report "SCK_o should be '1'"
        severity error;
      assert EnShift = '0'
        report "EnShift_o should be '0'"
        severity error;
      assert EnSample = '1'
        report "EnSample_o should be '1'"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
      assert SCK = '0'
        report "SCK_o should be '0'"
        severity error;
      assert EnShift = '1'
        report "EnShift_o should be '1'"
        severity error;
      assert EnSample = '0'
        report "EnSample_o should be '0'"
        severity error;      
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    assert SCK = '1'
      report "SCK_o should be '1'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '1'
      report "EnFrqDivider_o should be '1'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '1'
      report "EnSample_o should be '1'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1' before entering suspend mode"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0' before entering suspend mode"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0' before entering suspend mode"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0' before entering suspend mode"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0' before entering suspend mode"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0' before entering suspend mode"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0' before entering suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert Transmission = '1'
      report "Transmission_o should be '1' in suspend mode"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    RdFIFOFull  <= '0'; -- release from suspend
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '1'
      report "Transmission_o should be '1'"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '1'
      report "LdReadFIFO_o should be '1'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert SCK = '0'
      report "SCK_o should be '0'"
      severity error;
    assert Transmission = '0'
      report "Transmission_o should be '0'"
      severity error;
    assert EnFrqDivider = '0'
      report "EnFrqDivider_o should be '0'"
      severity error;
    assert LdShifter = '0'
      report "LdShifter_o should be '0'"
      severity error;
    assert EnShift = '0'
      report "EnShift_o should be '0'"
      severity error;
    assert EnSample = '0'
      report "EnSample_o should be '0'"
      severity error;
    assert RdWriteFIFO = '0'
      report "RdWriteFIFO_o should be '0'"
      severity error;
    assert LdReadFIFO = '0'
      report "LdReadFIFO_o should be '0'"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 3b;
    -- end testcase 3;

    -- insert some space time
    wait for Clk_period*5;

    -- end simulation
    report "NONE. Simulation finished" severity failure; -- used to stop simulation
  end process;

end behavior;

