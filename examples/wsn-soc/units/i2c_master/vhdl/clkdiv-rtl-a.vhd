

architecture rtl of ClkDiv is

  signal Clk800Counter : std_logic_vector(DividerWidth_g-1 downto 0);
  signal Clk200Counter : std_logic_vector(1 downto 0);
  signal Clk800        : std_logic;
  signal Clk200        : std_logic;
  
begin

  ClkDiv800: process(Clk_i,Reset_i)
  begin
    -- Asynchronous Reset
    if Reset_i='1' then
      Clk800Counter <= (others => '0');
      Clk200Counter <= (others => '0');
      -- after reset we start with all 0, so in the first clock cycle
      -- we set the value to their maximum
    -- Synchronous part
    elsif rising_edge(Clk_i) then
      if Clk800Counter = 0 then
        Clk800Counter <= Divider800_i;
        Clk200Counter <= Clk200Counter-1;
      else
        Clk800Counter <= Clk800Counter-1;
      end if;
    end if;
  end process ClkDiv800;

  Clk800 <= '1' when Clk800Counter = 0 else '0';

  Clk200 <= '1' when (Clk800Counter = 0) and (Clk200Counter = 0) else '0';

  Clk_o <= Clk800 when F100_400_n_i = '0' else Clk200;
   
end rtl;
