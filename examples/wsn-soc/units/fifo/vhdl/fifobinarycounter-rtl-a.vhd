

architecture rtl of FIFOBinaryCounter is

signal BinaryCount : STD_LOGIC_VECTOR (AdressWidth downto 0);
signal BinaryNext  : STD_LOGIC_VECTOR (AdressWidth downto 0);

begin

  -- calculate next binary count value
  BinaryNext <= BinaryCount + 1;

  -- binary counter
  BinaryCounter: process (Clk, Reset_n)
  begin
    if Reset_n = '0' then
      BinaryCount <= (others => '0');
    elsif Clk'event and Clk = '1' then
      if ClkEnable_i = '1' then
        BinaryCount <= BinaryNext;
      end if;
    end if;
  end process BinaryCounter;
  
  -- set output
  Binary_o    <= BinaryCount(AdressWidth - 1 downto 0); -- adress pointer
  BinaryMSB_o <= BinaryCount(AdressWidth); -- needed for full/empty detection
  
end rtl;

