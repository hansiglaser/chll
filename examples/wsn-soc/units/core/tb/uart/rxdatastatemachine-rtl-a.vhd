

architecture RTL of RxDataStateMachine is

  -- States of the TxDataStateMachine
  type StatesRX is (stIdle, stReadStart, stReadData, stReadParity, stReadStop);
  -- WriteParallel maybe not neccesaray

  -- Signal for the States
  signal CurrentState : StatesRX;
  signal NextState    : StatesRX;

  -- Signal for the DataBits
  signal DataBitsCounterMax     : STD_LOGIC_VECTOR((BitSelCounterSize-1) downto 0);
  signal DataBitsCounter        : STD_LOGIC_VECTOR((BitSelCounterSize-1) downto 0);
  signal DataBitsCounterIncEn   : STD_LOGIC;
  signal DataBitsCounterResetEn : STD_LOGIC;

  signal ParallelWriteData      : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);

  -- Enable Sampling of Input
  signal SampleEnable : STD_LOGIC;
  -- Enable Deciding and Taking databit into register
  -- decide what bit was sent
  signal DataEnable   : STD_LOGIC;
  signal IdleEnable   : STD_LOGIC;

  -- Enable ParityEnable (to check if received bit is received ParityBit = DetectedParityBit)
  signal ParityEnable  : STD_LOGIC;
  -- Enable StopBit Write ( indicate state stReadStop)
  signal StopBitEnable : STD_LOGIC;

  -- indicate the bit detected
  signal BitDetected  : STD_LOGIC;

  -- 3 samples of each bit
  signal SampledBits  : STD_LOGIC_VECTOR((2**Oversampling - 2) downto 0);

  -- RxD_i is taken, and in SamplingProcess taken
  signal ParityBit           : STD_LOGIC;
  signal ParityReset         : STD_LOGIC;
  signal ParityBitErrorReset : STD_LOGIC;
  -- error signals 
  signal ParityBitError    : STD_LOGIC;
  -- set when ParityError, StopBitError or RxBufferFullError occured
  signal ErrorOccured      : STD_LOGIC;

begin

  StateReg: process(Clk_i, Reset_i_n)
  begin
    if Reset_i_n = '0' then
      CurrentState <= stIdle;
    elsif rising_edge(Clk_i) then
      CurrentState <= NextState;
    end if;
  end process StateReg;

  StateMachine: process(CurrentState, RxD_i, BaudClk_i, BitDetected, ParityOn_i, DataBitsCounter)
  begin
    case CurrentState is
      when stIdle =>
        -- counters enable signals
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '0';
        -- enable bits
        SampleEnable        <= '0';
        DataEnable          <= '0';
        ParityEnable        <= '0';
        StopBitEnable       <= '0';
        IdleEnable          <= '1';
        -- ParityReset      
        ParityReset         <= '1';
        ParityBitErrorReset <= '1';
        -- '0' is detected -> go to next state ReadStart and make
        --   a majority decission to decide if a real start bit is send
        if RxD_i = '0' then
          NextState  <= stReadStart;
        else
          NextState  <= CurrentState;
        end if;

      when stReadStart =>
        -- counters enable, reset just in this single state
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '1';
        -- enable bits
        SampleEnable  <= '1';
        DataEnable    <= '0';
        ParityEnable  <= '0';
        StopBitEnable <= '0';
        IdleEnable    <= '0';
        -- ParityReset
        ParityReset         <= '0';
        ParityBitErrorReset <= '1';
        -- BaudClk_i indicates the end of one bit
        --  go to next state
        if BaudClk_i = '1'then
          if BitDetected = '0' then
            NextState <= stReadData;
          else
            NextState <= stIdle;
          end if;
        else
          NextState   <= CurrentState;
        end if;

      when stReadData =>
        -- counters enable
        DataBitsCounterResetEn <= '0';
        DataBitsCounterIncEn   <= '1';
        -- enable bits
        DataEnable          <= '1';
        SampleEnable        <= '1';
        ParityEnable        <= '0';
        StopBitEnable       <= '0';
        IdleEnable          <= '0';
        -- ParityReset
        ParityReset         <= '0';
        ParityBitErrorReset <= '0';

        if BaudClk_i = '1' then
          if (to_integer(unsigned(DataBitsCounter)) = 0) then
            if (ParityOn_i = '1') then
              NextState <= stReadParity;
            else
              NextState <= stReadStop;
            end if;
          else
            NextState   <= CurrentState;
          end if;
        else
          NextState     <= CurrentState;
        end if;
 
      when stReadParity =>
        -- counters enable
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '0';
        -- enable bits
        SampleEnable   <= '1';
        DataEnable     <= '0';
        ParityEnable   <= '1';
        StopBitEnable  <= '0';
        IdleEnable     <= '0';
        -- ParityReset
        ParityReset         <= '0';
        ParityBitErrorReset <= '0';

        if BaudClk_i = '1' then
          NextState <= stReadStop;
        else
          NextState <= CurrentState;
        end if;

      when stReadStop =>
        -- counters enable
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '0';
        -- enable bits
        SampleEnable   <= '1';
        DataEnable     <= '0';
        ParityEnable   <= '0';
        StopBitEnable  <= '1';
        IdleEnable     <= '0';
        -- parityreset
        ParityReset         <= '1';
        ParityBitErrorReset <= '0';

        if BaudClk_i = '1' then
          NextState <= stIdle;
        else
          NextState <= CurrentState;
        end if;
    end case;
  end process StateMachine;

  -- sample 3 bits
  --   shift received bits into a register
  SamplingPr: process (Clk_i, Reset_i_n)
  begin
    if Reset_i_n = '0' then
      SampledBits <= (others => '0');
    elsif rising_edge(Clk_i) then
      if SampleEnable = '1' and SamplingBaudClk_i = '1' and BaudClk_i = '0' then
        -- shift bits
        SampledBits <= SampledBits(SampledBits'length - 2 downto 0) & RxD_i;
      else
        SampledBits <= SampledBits;
      end if;
    end if;
  end process SamplingPr;

  -- de-serializer
  DataPr: process (Clk_i, Reset_i_n)
  begin
    if Reset_i_n = '0' then
      ParallelWriteData <= (others => '0');
    elsif rising_edge(Clk_i) then
      if DataEnable = '1' and BaudClk_i = '1' then
        -- shift bits right and then override the bit 
        --   at Position DataBitsCounterMax with BitDetected
        ParallelWriteData(MaxDataWidth-2 downto 0) <= ParallelWriteData(MaxDataWidth-1 downto 1);
        ParallelWriteData(to_integer(unsigned(DataBitsCounterMax))) <= BitDetected;
      end if;
    end if;
  end process DataPr;

  -- count the DataBits
  DataCountPr: process(Clk_i, Reset_i_n)
  begin
    if Reset_i_n ='0' then
      -- reset value to 0, reset on correct value happens before stRead with DataBitsCounerResetEn
      DataBitsCounter    <= (others => '0');
    elsif rising_edge(Clk_i) then
      if DataBitsCounterResetEn = '1' then
        DataBitsCounter <= DataBitsCounterMax;
      elsif BaudClk_i = '1' and DataBitsCounterIncEn = '1' then
        DataBitsCounter <= std_logic_vector(unsigned(DataBitsCounter) - 1);
      else
        DataBitsCounter <= DataBitsCounter;
      end if;
    end if; 
  end process DataCountPr;

  -- better solution: use state information to enable parity
  --   instead of DataEnable = '1' use CurrentState = stReadData
  --   or for sampling Current State = not stIdle
  --   therefore DataEnable and SamplingEnable do not have to be set in the Statemachine
  --   but hardware afford would be higher
  -- to optimize current implementation use one hot encoding

  ParityBitPr: process (Clk_i, Reset_i_n, ParityEvenOdd_i)
  begin
    if Reset_i_n = '0' then
      ParityBit <= '0';
    elsif rising_edge(Clk_i) then
      if ParityReset = '1' then
        if (ParityEvenOdd_i = Even) then
          ParityBit <= '0';
        else
          ParityBit <= '1';
        end if;
      elsif DataEnable = '1' and BaudClk_i = '1' then
        ParityBit <= ParityBit xor BitDetected;
      else
        ParityBit <= ParityBit;
      end if;
    end if;
  end process ParityBitPr;

  -- manage ParityError
  ParityBitErrPr: process(Clk_i, Reset_i_n)
  begin
    if Reset_i_n = '0' then
      ParityBitError <= '0';
    elsif rising_edge(Clk_i) then
      if ParityBitErrorReset = '1' then
        ParityBitError <= '0';
      elsif BaudClk_i ='1' and ParityEnable = '1' then
        if BitDetected = ParityBit then
          ParityBitError <= '0';
        else
          ParityBitError <= '1';
        end if;
      else
        ParityBitError <= ParityBitError;
      end if;
    end if;
  end process ParityBitErrPr;

  -- combinatory part

  -- result of DataPr written on output
  ParallelData_o <= ParallelWriteData;

  -- error bits
  ParityError_o       <= '1' when (BaudClk_i = '1' and StopBitEnable = '1' and ParityBitError = '1') else
                         '0';

  StopBitError_o      <= '1' when (BaudClk_i = '1' and StopBitEnable = '1' and BitDetected = '0') else
                         '0';

  RxBufferFullError_o <= '1' when (BaudClk_i = '1' and StopBitEnable = '1' and FifoFull_i = '1') else
                         '0';

  WriteParallelData_o <= '1' when (BaudClk_i = '1' and StopBitEnable = '1' and ErrorOccured = '0') else
                         '0';

  -- BGEnable is 1 if receiving 0 and IdleState, 0 if in idle state
  BGEnable_o <= '1' when ((Rxd_i = '0' and IdleEnable = '1') or IdleEnable = '0') else
                '0';

  -- Error detected
  -- indicates if an error occured during receiving data
  --   (BitDetected indicates if a StopBit occured in stReadStop)
  -- Error Occured is just sampled at the 4th sample bit of stReadStop by the
  --   process WriteDataPr
  -- only ParityBitError maybe set in this state
  ErrorOccured <= '1' when (BitDetected = '0' or FifoFull_i = '1' or ParityBitError = '1') else
                  '0';

  -- majority decision which bit was detected
  BitDetected <= '0' when (SampledBits = "000") else
                 '0' when (SampledBits = "001") else
                 '0' when (SampledBits = "010") else
                 '0' when (SampledBits = "100") else
                 '1';

  -- convert the enum to a std_logic
  DataBitsCounterMax <= "0100" when (BitSelect_i = Sel5Bits) else -- 5
                        "0101" when (BitSelect_i = Sel6Bits) else -- 6
                        "0110" when (BitSelect_i = Sel7Bits) else -- 7
                        "0111" when (BitSelect_i = Sel8Bits) else -- 8
                        "1000" when (BitSelect_i = Sel9Bits) else -- 9
                        (others => '-');

end RTL;

