
 
architecture behavior of tb_FIFOSync is

constant DataWidth   : integer := 16;
constant AdressWidth : integer := 4;

-- Component Declaration for the Unit Under Test (UUT)
component FIFOSyncTop
  Generic ( 
    DataWidth   : integer range 2 to 64 := 8;
    AdressWidth : integer range 2 to 10 := 4);
  Port (
    Reset_n     : in  STD_LOGIC;
    Clk         : in  STD_LOGIC;
    DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    WriteA_i    : in  STD_LOGIC;
    DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
    ReadNextB_i : in  STD_LOGIC;
    FIFOFull_o  : out STD_LOGIC;
    FIFOEmpty_o : out STD_LOGIC);
end component;

-- Inputs
signal Reset_n     : STD_LOGIC := '0';
signal Clk         : STD_LOGIC := '0';
signal DataA       : STD_LOGIC_VECTOR (DataWidth - 1 downto 0) := (others => '0');
signal WriteA      : STD_LOGIC := '0';
signal ReadB       : STD_LOGIC := '0';

-- Outputs
signal DataB       : STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
signal FIFOFull    : STD_LOGIC;
signal FIFOEmpty   : STD_LOGIC;

-- Clock period definitions
constant Clk_period : time := 10 us;
constant Clk_delay  : time := Clk_period/10;

-- increment write counter 
-- convert write counter to std_logic_vector for FIFO input
procedure GenerateWriteData (variable WriteCounter : inout integer;
                             signal   WriteData    : out   STD_LOGIC_VECTOR) is
begin
  WriteCounter := WriteCounter + 1;
  WriteData <= conv_std_logic_vector(WriteCounter, DataWidth);
end procedure GenerateWriteData;

-- increment read counter
-- compare read counter with read data and assert
procedure CheckReadData (variable ReadCounter : inout integer;
                         signal   ReadData    : in    STD_LOGIC_VECTOR) is
begin
  ReadCounter := ReadCounter + 1;
  assert ReadData = conv_std_logic_vector(ReadCounter, DataWidth)
    report "FIFO Read Data #" & integer'image(ReadCounter) & " not correct"
    severity error;
end procedure CheckReadData;

begin
  -- Instantiate the Unit Under Test (UUT)
  uut: FIFOSyncTop
    Generic Map (
      DataWidth   => DataWidth,
      AdressWidth => AdressWidth)
    Port Map (
      Reset_n     => Reset_n,
      Clk         => Clk,
      DataA_i     => DataA,
      WriteA_i    => WriteA,
      DataB_o     => DataB,
      ReadNextB_i => ReadB,
      FIFOFull_o  => FIFOFull,
      FIFOEmpty_o => FIFOEmpty);

  -- Clock process definitions
  Clk_process :process
  begin
    Clk <= '0';
    wait for Clk_period/2;
    Clk <= '1';
    wait for Clk_period/2;
  end process Clk_process;

  -- Stimulus process
  stim_proc: process
  
  -- WriteCounter holds the value of the last written data word
  -- ReadCounter holds the value of next expected read data word
  -- are used to create and compare unique data 
  variable WriteCounter : integer range 0 to (2**AdressWidth * (2**AdressWidth + 4)) := 0;
  variable ReadCounter  : integer range 0 to (2**AdressWidth * (2**AdressWidth + 4)) := 0;
  
  begin		
    -- hold reset state for Clk_period*5.
    wait for Clk_period*5;	
    Reset_n <= '1';
    wait for Clk_period*5;
    
    -- testcase 1: test for initial state
    assert FIFOEmpty = '1'
      report "FIFOEmpty should be '1' after reset"
      severity error;
    assert FIFOFull = '0'
      report "FIFOFull should be '0' after reset"
      severity error;
    -- end testcase 1;
    
    -- testcase 2: write 1 data word and then read 1 data word
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    GenerateWriteData (WriteCounter, DataA);
    WriteA <= '1';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    WriteA <= '0';
    assert FIFOEmpty = '0'
      report "FIFOEmpty should be '0' one cycle after the first word has been written"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ReadB <= '1';
    CheckReadData (ReadCounter, DataB);
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ReadB <= '0';
    assert FIFOEmpty = '1'
      report "FIFOEmpty should be '1' one cycle after the last word has been read"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 2;

    -- testcase 3: write until FIFO is full and then read until FIFO is empty again
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- writing first data word
    WriteA <= '1';
    GenerateWriteData (WriteCounter, DataA);
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- writing second to last data word
    for writeindex in 2 to (2**AdressWidth) loop
      GenerateWriteData (WriteCounter, DataA);
      assert FIFOEmpty = '0'
        report "FIFOEmpty should be '0' one cycle after the first word has been written"
        severity error;
      assert FIFOFull = '0'
        report "FIFOFull should be '0' until one cycle after FIFO is full"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    -- writing finished, FIFO full
    WriteA <= '0';
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert FIFOFull = '1'
        report "FIFOFull should be '1' one cycle after last word has been written"
        severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- start reading
    ReadB <= '1';
    -- reading first data word
    CheckReadData (ReadCounter, DataB);    
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- reading second to the next to last data word
    for readindex in 2 to ((2**AdressWidth) - 1) loop
      CheckReadData (ReadCounter, DataB);
      assert FIFOFull = '0'
        report "FIFOFull should be '0' one cycle after the first data word of the full FIFO is read"
        severity error;
      assert FIFOEmpty = '0'
        report "FIFOEmpty should be '0' until the FIFO is empty"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    -- reading last data word
    CheckReadData (ReadCounter, DataB);
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    ReadB <= '0';
    assert FIFOEmpty = '1'
      report "FIFOEmpty should be '1' one cycle after last word has been read"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert FIFOEmpty = '1' and FIFOFull = '0'
      report "FIFO should be empty again"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 3;
    
    -- testcase 4: continous write and read for (buffersize * (buffersize + 2)) times
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    assert FIFOEmpty = '1' and FIFOFull = '0'
      report "FIFO should be empty at start of continous test"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- start writing
    WriteA <= '1';
    GenerateWriteData (WriteCounter, DataA);
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- writing buffer 50% full
    for writeindex in 2 to (2**(AdressWidth - 1) - 1) loop
      GenerateWriteData (WriteCounter, DataA);
      assert FIFOEmpty = '0' and FIFOFull = '0'
        report "FIFO should never run full neither get empty in countinous test"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    -- start simultanous writing and reading
    GenerateWriteData (WriteCounter, DataA);
    ReadB <= '1';
    CheckReadData (ReadCounter, DataB);
    assert FIFOEmpty = '0' and FIFOFull = '0'
      report "FIFO should never run full neither get empty in countinous test"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    for combinedindex in 1 to (2**AdressWidth * (2**AdressWidth + 2)) loop
      GenerateWriteData (WriteCounter, DataA);
      CheckReadData (ReadCounter, DataB);
      assert FIFOEmpty = '0' and FIFOFull = '0'
        report "FIFO should never run full neither get empty in countinous test"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    -- stop writing, continue reading
    WriteA <= '0';
    for readindex in 1 to (2**(AdressWidth - 1) - 1) loop
      CheckReadData (ReadCounter, DataB);
      assert FIFOEmpty = '0' and FIFOFull = '0'
        report "FIFO should never run full neither get empty in countinous test"
        severity error;
      wait until Clk'event and Clk = '1';
      wait for Clk_delay;
    end loop;
    -- reading last data word
    ReadB <= '0';
    assert FIFOEmpty = '1'
      report "FIFOEmpty should be '1' one cycle after last word has been read"
      severity error;
    wait until Clk'event and Clk = '1';
    wait for Clk_delay;
    -- end testcase 4;
    
    -- insert some space time
    wait for Clk_period*5;
    
    -- end simulation
    report "NONE. Simulation finished" severity failure; -- used to stop simulation
  end process;

end behavior;

