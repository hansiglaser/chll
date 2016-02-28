

ARCHITECTURE behavior OF tb_ClkDiv IS 
   
  CONSTANT HalfClkPer : time      := 62.5 ns; -- 62.5ns => 8 MHz
  
  -- Component Declaration
  COMPONENT ClkDiv IS
    Generic (DividerWidth_g : integer range 4 to 32 := 16);
            
    Port (  F100_400_n_i : in      STD_LOGIC;
            Divider800_i : in      std_logic_vector(DividerWidth_g-1 downto 0);
            Clk_i        : in      STD_LOGIC;
            Reset_i      : in      STD_LOGIC;
            Clk_o        : out     STD_LOGIC);
  END COMPONENT;
 
  -- Signal Declaration
   
   -- General Signals
  SIGNAL Reset_i           : STD_LOGIC;
  SIGNAL Clk_i             : STD_LOGIC;
  SIGNAL Clk_o             : STD_LOGIC;
  signal Divider800_i      : std_logic_vector(15 downto 0);
  SIGNAL F100_400_n_i      : STD_LOGIC; -- Selects either 100 kHz or 400 kHz as
                                        -- resulting SCL-I2C-Bus frequency.
                                        -- (The resulting Clk_o frequency will
                                        -- be 200 kHz or 800 kHz)
 
  BEGIN
    -- Component Instantiation
    myClkDiv : ClkDiv
      GENERIC MAP(DividerWidth_g => 16)
      PORT MAP(   Reset_i      => Reset_i,
                  Clk_i        => Clk_i,
                  Clk_o        => Clk_o,
                  Divider800_i => Divider800_i,
                  F100_400_n_i => F100_400_n_i);
    -- END OF COMPONENTS
      
    -- PROCESS to generate 100-MHz Input-Clock-Signal
    clock: PROCESS
      BEGIN
        Clk_i <= '0';
        wait for HalfClkPer;
        Clk_i <= '1';
        wait for HalfClkPer;   
    END PROCESS clock;
    
    -- Process to generate ControllCommands
    CTRL : PROCESS
      BEGIN
        -- Reset
        F100_400_n_i  <= '0';
        Divider800_i  <= conv_std_logic_vector(8*1000*1000/(800*1000)-1,16);
        Reset_i       <= '1';
        wait for HalfClkPer;
        Reset_i       <= '0';
        wait for 52 us;
        F100_400_n_i  <= '1';

        wait for 50 us;

        F100_400_n_i  <= '0';

        wait for 50 us;

        assert false report "Simulation finished" severity failure;
        
    END PROCESS;

END;
