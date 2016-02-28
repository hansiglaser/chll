

architecture RTL of BaudGenerator is

  constant SamplingRateWidth  : integer := Oversampling;
  constant SamplingRate       : std_logic_vector((SamplingRateWidth-1) downto 0) := (others => '1');

  -- signals for the fast BaudRate x 4
  signal SamplingBaudEnable   : std_logic;
  signal BaudEnable           : std_logic;

-- Divider_i = Divider
-- speed divider 1 not allowed

begin
  -- 4x baud rate for Rx oversampling
  SamplingDivider : process (Clk_i, Reset_i_n)
    variable Counter : std_logic_vector((MaxSpeedDividerWidth-1) downto 0);
  begin
      --Counter := Divider;
    if Reset_i_n ='0' then
      SamplingBaudEnable   <= '0';
      -- sample input on reset
      Counter := (others => '0');
      -- set oversampling
    elsif rising_edge(Clk_i)then
      if BGenable_i = '1' then
        Counter   := std_logic_vector(unsigned(Counter) - 1);
        if ( to_integer(unsigned(Counter)) = 0) then
          SamplingBaudEnable <= '1';
          Counter := std_logic_vector(unsigned(SpeedDivider_i));
        else
          SamplingBaudEnable <= '0';
        end if;
      else
        -- enable is like a reset
        SamplingBaudEnable <= '0';
        Counter := std_logic_vector(unsigned(SpeedDivider_i));
      end if;
    else
      null;
    end if;
  end process SamplingDivider;

  NoOversampling: if Oversampling = 0 generate
    BaudEnable <= SamplingBaudEnable;
  end generate NoOversampling;

  UseOversampling: if Oversampling > 0 generate
    -- real baud rate
    DivideOversampling : process (Clk_i, Reset_i_n)
      variable SubCounter : std_logic_vector((SamplingRateWidth-1) downto 0);
    begin
      if Reset_i_n ='0' then
        SubCounter := SamplingRate;
      elsif rising_edge(Clk_i) then
        if BGenable_i = '1' then
          if SamplingBaudEnable = '1' then
            SubCounter := std_logic_vector(unsigned(SubCounter) -1 );
            if (to_integer(unsigned(SubCounter)) = 0) then
              BaudEnable <= '1';
            else
              BaudEnable <= '0';
            end if;
          end if;
        else
          SubCounter := SamplingRate;
          BaudEnable <= '0';
        end if;
      end if;
    end process DivideOversampling;
  end generate UseOversampling;

  -- Blank out first half of the BaudSamplingClk, so that BaudClk is just high for the second half
  BaudClk_o         <= '1' when((BaudEnable ='1') and (SamplingBaudEnable = '1')) else
                       '0';
  BaudSamplingClk_o <= SamplingBaudEnable;

end RTL;

