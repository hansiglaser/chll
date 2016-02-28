

architecture RTL of ErrorBit is

  signal ErrorBitSet : STD_LOGIC;

begin
  ErrorInd: process (Clk_i, Reset_i_n, ErrorReset_i)
  begin
    if (Reset_i_n ='0') then
      ErrorBitSet  <= '0';
    elsif(rising_edge(Clk_i)) then
     if (ErrorReset_i = '1') then
       ErrorBitSet  <= '0';
     else
       -- hold status
        if(ErrorBitSet ='1') then
          ErrorBitSet <= ErrorBitSet;
          -- only set error if getting error from input
        elsif ((ErrorBitSet ='0')) then
          if (ErrorBit_i ='1') then
            ErrorBitSet  <= '1';
          else
            ErrorBitSet  <= '0';
          end if;
        end if;
      end if;
    end if;
  end process ErrorInd;

  ErrorIndicatorBit_o <= ErrorBitSet;

end RTL;

