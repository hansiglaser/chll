

architecture behavior of tb_SPIFrqDiv is

constant SPPRWidth : integer range 1 to 8 := 3;
constant SPRWidth  : integer range 1 to 8 := 3;

-- Component Declaration for the Unit Under Test (UUT)
component SPIFrqDiv is
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

-- Inputs
signal Reset_n      : STD_LOGIC := '0';
signal Clk          : STD_LOGIC := '0';
signal SPPR         : STD_LOGIC_VECTOR(SPPRWidth-1 downto 0) := (others => '0');
signal SPR          : STD_LOGIC_VECTOR(SPRWidth-1 downto 0)  := (others => '0');
signal EnFrqDivider : STD_LOGIC := '0';

-- Outputs
signal NextStep     : STD_LOGIC;

-- Clock period definitions
constant Clk_period : time := 10 us;
constant Clk_delay  : time := Clk_period/10;

-- test procedure
procedure Testcase (       Testcase     : in  integer;
                           SPPR_in      : in  integer;
                           SPR_in       : in  integer;
                    signal SPPR         : out STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
                    signal SPR          : out STD_LOGIC_VECTOR(SPRWidth-1 downto 0);
                    signal EnFrqDivider : out STD_LOGIC;
                    signal NextStep     : in  STD_LOGIC) is
begin
  wait until Clk'event and Clk = '1';
  report "testcase " & integer'image(Testcase) severity note;
  wait for Clk_delay;
  SPPR <= std_logic_vector(to_unsigned(SPPR_in, SPPRWidth));
  SPR  <= std_logic_vector(to_unsigned(SPR_in,  SPRWidth));
  EnFrqDivider <= '1';
  for repetition in 2 downto 0 loop
    for SPPRCycle in SPPR_in downto 0 loop
      for SPRCycle in 2**SPR_in-1 downto 0 loop
        wait until Clk'event and Clk = '1';
        wait for Clk_delay;
        -- assert
        if SPPRCycle = 0 and SPRCycle = 0 then
          assert NextStep = '1'
            report "NextStep should be '1'"
            severity error;
        else
          assert NextStep = '0'
            report "NextStep should be '0'"
            severity error;
        end if;
        -- stop frequency divider
        if repetition = 0 and SPPRcycle = 0 and SPRCycle = 0 then
          EnFrqDivider <= '0';
        end if;
      end loop;
    end loop;
  end loop;
  wait until Clk'event and Clk = '1';
  wait until Clk'event and Clk = '1';
end procedure Testcase;

begin
  -- Instantiate the Unit Under Test (UUT)
  uut: SPIFrqDiv
    Generic Map (
      SPPRWidth      => SPPRWidth,
      SPRWidth       => SPRWidth)
    Port Map (
      Reset_n        => Reset_n,
      Clk            => Clk,
      SPPR_i         => SPPR,
      SPR_i          => SPR,
      EnFrqDivider_i => EnFrqDivider,
      NextStep_o     => NextStep);

  -- Clock process definitions
  Clk_process: process
  begin
    Clk <= '0';
    wait for Clk_period/2;
    Clk <= '1';
    wait for Clk_period/2;
  end process Clk_process;

  -- Stimulus process
  stim_proc: process

  begin
    -- hold reset state for Clk_period*5.
    wait for Clk_period*5;
    Reset_n <= '1';
    wait for Clk_period*5;

    -- testcase 1: test for initial state
    report "testcase 1" severity note;
    assert NextStep = '1'
      report "NextStep should be '1' after reset"
      severity error;
    -- end testcase 1;

    -- testcase 2:
    Testcase(2, 0, 0, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 2;

    -- testcase 3:
    Testcase(3, 1, 0, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 3;

    -- testcase 4:
    Testcase(4, 2, 0, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 4;

    -- testcase 5:
    Testcase(5, 7, 0, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 5;

    -- testcase 6:
    Testcase(6, 0, 1, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 6;

    -- testcase 7:
    Testcase(7, 0, 2, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 7;

    -- testcase 8:
    Testcase(8, 0, 7, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 8;

    -- testcase 9:
    Testcase(9, 1, 1, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 9;

    -- testcase 10:
    Testcase(10, 2, 2, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 10;

    -- testcase 11:
    Testcase(11, 7, 7, SPPR, SPR, EnFrqDivider, NextStep);
    -- end testcase 11;

    -- insert some space time
    wait for Clk_period*5;

    -- end simulation
    report "NONE. Simulation finished" severity failure; -- used to stop simulation
  end process;

end behavior;

