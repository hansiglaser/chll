

architecture RTL of SPIShifter is

signal EnableShiftReg : STD_LOGIC;
signal ShiftRegister  : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal NextShiftReg   : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal LoadShiftReg   : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal DataInRev      : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal DataOut        : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal DataOutRev     : STD_LOGIC_VECTOR(DataWidth-1 downto 0);
signal SampleRegister : STD_LOGIC;

begin

  -- calculate DataInRev
  DataInputReversion: process (Data_i)
  begin
    for BitNumber in DataWidth-1 downto 0 loop
      DataInRev(BitNumber) <= Data_i((DataWidth-1) - BitNumber);
    end loop;
  end process DataInputReversion;

  -- combinational inputs
  EnableShiftReg <= LdShifter_i or EnShift_i;
  LoadShiftReg   <= Data_i when LSBFE_i = '0' else
                    DataInRev;
  NextShiftReg   <= ShiftRegister(DataWidth-2 downto 0) & SampleRegister when LdShifter_i = '0' else
                    LoadShiftReg;

  -- sequential statements
  SPIShiftRegister: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      ShiftRegister <= (others => '0');
    elsif Clk'event and Clk = '1' then
      if EnableShiftReg = '1' then
        ShiftRegister <= NextShiftReg;
      end if;
    end if;
  end process SPIShiftRegister;

  SPISampleRegister: process (Reset_n, Clk)
  begin
    if Reset_n = '0' then
      SampleRegister <= '0';
    elsif Clk'event and Clk = '1' then
      if EnSample_i = '1' then
        SampleRegister <= To_X01(MISO_i);
      end if;
    end if;
  end process SPISampleRegister;

  -- calculate DataOut
  DataOut <= ShiftRegister(DataWidth-2 downto 0) & SampleRegister;

  -- calculate DataOutRev
  DataOutputReversion: process (DataOut)
  begin
    for BitNumber in DataWidth-1 downto 0 loop
      DataOutRev(BitNumber) <= DataOut((DataWidth-1) - BitNumber);
    end loop;
  end process DataOutputReversion;

  -- combinational outputs
  MOSI_o <= ShiftRegister(DataWidth-1);
  Data_o <= DataOut when LSBFE_i = '0' else
            DataOutRev;

end RTL;

