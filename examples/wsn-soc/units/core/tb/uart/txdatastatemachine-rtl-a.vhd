

architecture RTL of TxDataStateMachine is

  -- States of the TxDataStateMachine
  type StatesTX is(stIdle, stLoad, stShift, stParityWrite, stStopWrite);

-- Signal Declartation
  -- Signal for the States
  signal CurrentState : StatesTX;
  signal NextState    : StatesTX;
  -- Signal for the DataBits
  signal DataBitsCounterMax : STD_LOGIC_VECTOR((BitSelCounterSize-1) downto 0);
  signal DataBitsCounter    : STD_LOGIC_VECTOR((BitSelCounterSize-1) downto 0);
  signal ParallelLoadedData : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
  --Signals for TxD output
  signal TxD_pre                : STD_LOGIC;
  signal TxDEnable              : STD_LOGIC;
  signal DataBitsCounterResetEn : STD_LOGIC;
  signal DataBitsCounterIncEn   : STD_LOGIC;
  signal LoadData               : STD_LOGIC;
  --Signals to generate/manage Parity Bit
  signal ParityBit              : STD_LOGIC;
  signal ParityBitCalcEnable    : STD_LOGIC;
  signal ParityReset            : STD_LOGIC;

begin

  StateReg: process(Clk_i, Reset_i_n)
  begin
    if Reset_i_n ='0' then
      CurrentState <= stIdle;
      DataBitsCounter    <= (others => '0');
      ParallelLoadedData <= (others => '0');
    elsif rising_edge(Clk_i) then
      CurrentState         <= NextState;
      if DataBitsCounterResetEn = '1' then
        DataBitsCounter    <= (others => '0');
      elsif DataBitsCounterIncEn = '1' then
        DataBitsCounter    <= std_logic_vector(unsigned(DataBitsCounter) + 1);
      end if;
      if LoadData = '1' then
        ParallelLoadedData <= ParallelData_i;
      end if;
    end if;
  end process StateReg;

  CombLogic: process(CurrentState, FifoEmpty_i, ParallelData_i, ParityBit ,ParallelLoadedData ,ParityEvenOdd_i, BaudClk_i, DataBitsCounter, DataBitsCounterMax, ParityOn_i)
    variable outputBit : std_logic;
  begin
    case CurrentState is
      when stIdle =>
        -- reset counter for next state
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '1';
        -- Set outputs
        LoadData   <= '0';
        BGEnable_o <= '0';
        TxDEnable  <= '0';
        -- Parity
        ParityBitCalcEnable <= '0';
        ParityReset         <= '0';
        --Data available?
        if FifoEmpty_i = '0' then
          --start bit
          TxD_pre    <= '0';
          NextState  <= stLoad;
        else          
          TxD_pre    <= '1';
          NextState  <= CurrentState;
        end if;

      when stLoad => 
        -- reset counter for next state
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '1';
        -- outputs
        LoadData <= '1';
        -- Start BaudClk
        BGEnable_o <= '1';
        -- Send StartBit
        TxD_pre    <= '0';
        TxDEnable  <= '1';
        -- Parity
        ParityBitCalcEnable <= '0';
        ParityReset         <= '1';
        NextState           <= stShift;
        -- Start bit is set, baudgen is enabled
        -- now reacting on the rising edge of Baudgen
        -- First rising edge at Start bit is not seen in next state
        --   because of a delay of one clock cycle (Clk_i)

      when stShift =>
        -- counter
        DataBitsCounterResetEn <= '0';
        -- outputs
        LoadData   <= '0';
        BGEnable_o <= '1';
        TxDEnable  <= BaudClk_i;
        -- Parity
        ParityReset      <= '0';
        -- prevent from indexing out of bound
        if (DataBitsCounter < DatabitsCounterMax) then
          outputBit := ParallelLoadedData(to_integer(unsigned(DataBitsCounter)));
        else
          outputBit := '1';
        end if;
        --output to write
        if BaudClk_i = '1' then
          TxD_pre   <= outputBit;
          -- everything is send
          if DataBitsCounter = DataBitsCounterMax then
            -- reset counter in state before
            DataBitsCounterIncEn <= '0';
            ParityBitCalcEnable  <= '0';
            if ParityOn_i = '1' then
              -- parity
              NextState <= stParityWrite;
              TxD_pre   <= ParityBit;
            else
              -- stop bit
              NextState <= stStopWrite;
              TxD_pre   <= '1';
            end if;
          else
            TxD_pre              <= outputBit;
            DataBitsCounterIncEn <= '1';
            ParityBitCalcEnable  <= '1';
            NextState            <= CurrentState;
          end if;
        else
          TxD_pre              <= outputBit;
          DataBitsCounterIncEn <= '0';
          ParityBitCalcEnable  <= '0';
          NextState            <= CurrentState;
        end if;

      when stParityWrite =>
                -- reset counter for next state
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '1';
        -- set outputs
        LoadData   <= '0';
        BGEnable_o <= '1';
        TxDEnable  <= BaudClk_i;
        -- parity
        ParityReset         <= '0';
        ParityBitCalcEnable <= '0';
        -- end of parity bit reached?
        if BaudClk_i = '1' then
          TxD_pre   <= '1';  
          NextState <= stStopWrite;
        else
          TxD_pre   <= ParityBit;
          NextState <= CurrentState;
        end if;

      when stStopWrite =>
        -- reset counter for next state
        DataBitsCounterIncEn   <= '0';
        DataBitsCounterResetEn <= '1';
        -- set outputs
        LoadData               <= '0';
        TxDEnable              <= BaudClk_i;
        -- parity
        ParityReset            <= '0';
        ParityBitCalcEnable    <= '0';

        if BaudClk_i = '1' then
          -- more data to send available
          if FifoEmpty_i = '0' then
            -- send start bit
            TxD_pre    <= '0';
            BGEnable_o <= '1';
            NextState  <= stLoad;
          else
            TxD_pre    <= '1';
            BGEnable_o <= '0';
            NextState  <= stIdle;
          end if;
        else
          -- send stop bit 
          TxD_pre      <= '1';
          BGEnable_o   <= '1';
          NextState    <= CurrentState;
        end if;  
    end case;
  end process CombLogic;

  TxDProcess: process (Clk_i, Reset_i_n)
  begin
    if Reset_i_n = '0' then
      TxD_o   <= '1';
    elsif rising_edge(Clk_i) then
      if TxDEnable = '1' then
        TxD_o <= TxD_pre;
      end if;
    end if;
  end process TxDProcess;

  ParityBitPr: process (Clk_i, Reset_i_n, ParityEvenOdd_i)
  begin
    if Reset_i_n = '0' then
      ParityBit <= '0';
    elsif rising_edge(Clk_i) then
      if ParityReset = '1' then
        if ParityEvenOdd_i = Even then
          ParityBit <= '0';
        else
          ParityBit <= '1';
        end if;
      elsif ParityBitCalcEnable = '1' then
        ParityBit <= ParityBit xor TxD_pre;
      else
        ParityBit <= ParityBit;
      end if;
    end if;
  end process ParityBitPr;

  --combinatory part
  LoadData_o <= LoadData;

  DataBitsCounterMax <= "0101" when (BitSelect_i = Sel5Bits) else -- 5
                        "0110" when (BitSelect_i = Sel6Bits) else -- 6
                        "0111" when (BitSelect_i = Sel7Bits) else -- 7
                        "1000" when (BitSelect_i = Sel8Bits) else -- 8
                        "1001" when (BitSelect_i = Sel9Bits) else -- 9
                        (others => '-');
end RTL;

