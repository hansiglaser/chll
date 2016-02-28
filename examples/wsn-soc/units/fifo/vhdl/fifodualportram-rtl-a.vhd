

architecture rtl of FIFODualPortRam is

-- memory declaration
type DataBuffer_t is array ((2**AdressWidth - 1) downto 0) of STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
signal DataBuffer : DataBuffer_t;
-- attribute logic_block of DataBuffer : signal is TRUE; -- from IEEE Std 1076.6-2004 (not supported by every tool)

begin
  -- write data
  WriteData: process (Reset_n_i,ClkA)
  begin
    if Reset_n_i = '0' then
      DataBuffer <= (others => (others => '0'));
    elsif ClkA'event and ClkA = '1' then
      if WriteEnableA_i = '1' then
        DataBuffer(to_integer(unsigned(AdressA_i))) <= DataA_i; -- write data into buffer
      end if;
    end if;
  end process WriteData;

  -- assign data output
  DataB_o <= DataBuffer(to_integer(unsigned(AdressB_i))); -- write data to output
  
end rtl;

