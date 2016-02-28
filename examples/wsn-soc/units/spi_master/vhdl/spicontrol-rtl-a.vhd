

architecture RTL of SPIControl is

constant CounterWidth : integer := WidthFromMax(DataWidth-1);

type StateType is (Idle, Transmission, Suspend);
signal State        : StateType;
signal NextState    : StateType;
signal BitCount     : STD_LOGIC_VECTOR(CounterWidth-1 downto 0);
signal NextBitCount : STD_LOGIC_VECTOR(CounterWidth-1 downto 0);
signal CountEn      : STD_LOGIC;
signal SCK          : STD_LOGIC;
signal NextSCK      : STD_LOGIC;
signal EnSCK        : STD_LOGIC;
signal LdSCK        : STD_LOGIC;
signal TgSCK        : STD_LOGIC;
signal ShifterEn    : STD_LOGIC;

begin

  FSMState: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      State <= Idle;
    elsif Clk'event and Clk = '1' then
      State <= NextState;
    end if;
  end process FSMState;

  FSMComb: process (State, WrFIFOEmpty_i, CPHA_i, CountEn, BitCount, RdFIFOFull_i)
  begin
    case State is
      when Idle =>
        if WrFIFOEmpty_i = '0' then
          -- start transmitting
          NextState      <= Transmission;
          Transmission_o <= '1';
          EnFrqDivider_o <= '1';
          TgSCK          <= CPHA_i;
          LdSCK          <= '0';
          ShifterEn      <= '0';
          LdShifter_o    <= '1';
          RdWriteFIFO_o  <= '1';
          LdReadFIFO_o   <= '0';
        else
          -- stay idle
          NextState      <= Idle;
          Transmission_o <= '0';
          EnFrqDivider_o <= '0';
          TgSCK          <= '0';
          LdSCK          <= '1';
          ShifterEn      <= '0';
          LdShifter_o    <= '0';
          RdWriteFIFO_o  <= '0';
          LdReadFIFO_o   <= '0';
        end if;
      when Transmission =>
        if CountEn = '1' and BitCount = 0 then
          if RdFIFOFull_i = '1' then
            -- read FIFO full
            NextState      <= Suspend;
            Transmission_o <= '1';
            EnFrqDivider_o <= '0';
            TgSCK          <= not CPHA_i;
            LdSCK          <= '0';
            ShifterEn      <= '0';
            LdShifter_o    <= '0';
            RdWriteFIFO_o  <= '0';
            LdReadFIFO_o   <= '0';
          elsif WrFIFOEmpty_i = '0' then
            -- restart transmitting
            NextState      <= Transmission;
            Transmission_o <= '1';
            EnFrqDivider_o <= '1';
            TgSCK          <= '1';
            LdSCK          <= '0';
            ShifterEn      <= '0';
            LdShifter_o    <= '1';
            RdWriteFIFO_o  <= '1';
            LdReadFIFO_o   <= '1';
          else
            -- transmition complete
            NextState      <= Idle;
            Transmission_o <= '1';
            EnFrqDivider_o <= '0';
            TgSCK          <= not CPHA_i;
            LdSCK          <= '0';
            ShifterEn      <= '0';
            LdShifter_o    <= '0';
            RdWriteFIFO_o  <= '0';
            LdReadFIFO_o   <= '1';
          end if;
        else
          -- transmitting
          NextState      <= Transmission;
          Transmission_o <= '1';
          EnFrqDivider_o <= '1';
          TgSCK          <= '1';
          LdSCK          <= '0';
          ShifterEn      <= '1';
          LdShifter_o    <= '0';
          RdWriteFIFO_o  <= '0';
          LdReadFIFO_o   <= '0';
        end if;
      when Suspend =>
        if RdFIFOFull_i = '1' then
          -- read FIFO full, remain suspended
          NextState      <= Suspend;
          Transmission_o <= '1';
          EnFrqDivider_o <= '0';
          TgSCK          <= '0';
          LdSCK          <= '0';
          ShifterEn      <= '0';
          LdShifter_o    <= '0';
          RdWriteFIFO_o  <= '0';
          LdReadFIFO_o   <= '0';
        elsif WrFIFOEmpty_i = '0' then
          -- restart transmitting
          NextState      <= Transmission;
          Transmission_o <= '1';
          EnFrqDivider_o <= '1';
          TgSCK          <= CPHA_i;
          LdSCK          <= '0';
          ShifterEn      <= '0';
          LdShifter_o    <= '1';
          RdWriteFIFO_o  <= '1';
          LdReadFIFO_o   <= '1';
        else
          -- transmition complete
          NextState      <= Idle;
          Transmission_o <= '1';
          EnFrqDivider_o <= '0';
          TgSCK          <= '0';
          LdSCK          <= '0';
          ShifterEn      <= '0';
          LdShifter_o    <= '0';
          RdWriteFIFO_o  <= '0';
          LdReadFIFO_o   <= '1';
        end if;
      when others =>
        -- for complete case statement
        NextState      <= Idle;
        Transmission_o <= '0';
        EnFrqDivider_o <= '0';
        TgSCK          <= '0';
        LdSCK          <= '1';
        ShifterEn      <= '0';
        LdShifter_o    <= '0';
        RdWriteFIFO_o  <= '0';
        LdReadFIFO_o   <= '0';
    end case;
  end process FSMComb;

  -- only count when transmitting
  CountEn <= NextStep_i and (SCK xor CPOL_i xor CPHA_i) when State = Transmission else
             '0';
  -- counting down the bits left to transmit
  NextBitCount <= std_logic_vector(to_unsigned(DataWidth-1, CounterWidth)) when BitCount = 0 else
                  BitCount-1;
  BitCounter: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      BitCount <= std_logic_vector(to_unsigned(DataWidth-1, CounterWidth));
    elsif Clk'event and Clk = '1' then
      if CountEn = '1' then
        BitCount <= NextBitCount;
      end if;
    end if;
  end process BitCounter;

  -- toggle SCK when transmitting, load CPOL when idle
  EnSCK   <= LdSCK or (TgSCK and NextStep_i);
  NextSCK <= CPOL_i when LdSCK = '1' else
             not SCK;
  SCKGeneration: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      SCK <= '0';
    elsif Clk'event and Clk = '1' then
      if EnSCK = '1' then
        SCK <= NextSCK;
      end if;
    end if;
  end process SCKGeneration;

  -- combinational outputs
  SCK_o      <= SCK;
  EnShift_o  <= ShifterEn and NextStep_i and (CPOL_i xor CPHA_i xor SCK);
  EnSample_o <= ShifterEn and NextStep_i and (CPOL_i xor CPHA_i xor not SCK);

end RTL;

