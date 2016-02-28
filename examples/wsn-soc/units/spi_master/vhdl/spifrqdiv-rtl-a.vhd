

architecture RTL of SPIFrqDiv is

signal SPPR     : STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
signal NextSPPR : STD_LOGIC_VECTOR(SPPRWidth-1 downto 0);
signal SPR      : STD_LOGIC_VECTOR(2**SPRWidth-2 downto 0);
signal NextSPR  : STD_LOGIC_VECTOR(2**SPRWidth-2 downto 0);

type PreloadTable_t is array(2**SPRWidth-1 downto 0) of STD_LOGIC_VECTOR(2**SPRWidth-2 downto 0);
signal PreLoadTable : PreloadTable_t;

begin

  -- generate preload value for baudrate selection
  PreLoadTable(0) <= (others => '0');
  GenPreload: for i in 1 to 2**SPRWidth-1 generate
    PreLoadTable(i) <= std_logic_vector(to_unsigned(integer(2**i-1),2**SPRWidth-1));
  end generate GenPreload;

  -- baudrate preselector
  NextSPPR <= (others => '0') when EnFrqDivider_i = '0' else
              SPPR_i when SPPR = 0 else
              SPPR-1;

  BaudratePreSelector: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      SPPR <= (others => '0');
    elsif Clk'event and Clk = '1' then
      SPPR <= NextSPPR;
    end if;
  end process BaudratePreSelector;

  -- baudrate selector
  NextSPR <= (others => '0') when EnFrqDivider_i = '0' else
             PreLoadTable(to_integer(unsigned(SPR_i))) when SPR = 0 and SPPR = 0 else
             SPR-1 when SPPR = 0 else
             SPR;

  BaudrateSelector: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      SPR <= (others => '0');
    elsif Clk'event and Clk = '1' then
      SPR <= NextSPR;
    end if;
  end process BaudrateSelector;

  -- generate output
  NextStep_o <= '1' when SPR = 0 and SPPR = 0 else
                '0';

end RTL;

