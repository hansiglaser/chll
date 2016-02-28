

architecture rtl of i2ccore is

  -- defining states
  TYPE StateType IS (Idle,Start1,Start2,WriteByte,ReadNAck,ReadByte,WriteNAck,Stop1,BusErr);

  -- Component Declaration
  COMPONENT ClkDiv
    Generic ( DividerWidth_g : integer range 4 to 32);

    Port (  F100_400_n_i : in      STD_LOGIC;
            Divider800_i : in      std_logic_vector(DividerWidth_g-1 downto 0);
            Clk_i        : in      STD_LOGIC;
            Reset_i      : in      STD_LOGIC;
            Clk_o        : out     STD_LOGIC);
  END COMPONENT;

  SIGNAL   BytePos_s      : Integer range 0 to 7;
  SIGNAL   NextBytePos_s  : Integer range 0 to 7;
  SIGNAL   I2CClk_s       : STD_LOGIC;
  SIGNAL   SCL_s          : STD_LOGIC;
  SIGNAL   SDA_i_s        : STD_LOGIC;
  SIGNAL   SDA_o_s        : STD_LOGIC;
  SIGNAL   NextSDA_o_s    : STD_LOGIC;
  SIGNAL   NextSCL_s      : STD_LOGIC;
  SIGNAL   State          : StateType;
  SIGNAL   NextState      : StateType;
  SIGNAL   Data_s         : STD_LOGIC_VECTOR (7 downto 0);
  SIGNAL   NextData_s     : STD_LOGIC_VECTOR (7 downto 0);

begin
  -- Component Instantiation
  myClkDiv : ClkDiv
    GENERIC MAP(DividerWidth_g => DividerWidth_g)
    PORT MAP(   Reset_i      => Reset_i,
                Divider800_i => Divider800_i,
                Clk_i        => Clk_i,
                Clk_o        => I2CClk_s,
                F100_400_n_i => F100_400_n_i);
  -- END OF COMPONENTS

  -- sequential statements
  sequential: process(Clk_i,Reset_i)
  begin

    -- Asynchronous Reset
    if Reset_i='1' then
      State       <= Idle;
      Data_s      <= (others => '0');
      SCL_s       <= '1';
      BytePos_s   <= 0;
      SDA_i_s     <= '1';
      SDA_o_s     <= '1';

    -- Clk_i TICK
    elsif rising_edge(Clk_i) then
      State       <= NextState;
      Data_s      <= NextData_s;
      SCL_s       <= NextSCL_s;
      BytePos_s   <= NextBytePos_s;
      SDA_i_s     <= SDA_i;
      SDA_o_s     <= NextSDA_o_s;
    end if;
  end process;

  -- combinational statements
  combinational: process(State,Data_s,Data_i,SCL_s,SDA_i_s,SDA_o_s,DoTransfer_i,ReadWrite_n_i,AckTx_i,BytePos_s,I2CClk_s)
  begin

    --Setting Defaults
    NextSCL_s     <= '0';
    NextSDA_o_s   <= '1';
    AckRx_o       <= '0';
    AckValid_o    <= '0';
    ByteReady_o   <= '0';
    Busy_o        <= '1';
    BusErr_o      <= '0';
    NextBytePos_s <= BytePos_s;
    NextState     <= Idle;
    NextData_s    <= Data_s;

    case State is
      when Idle      =>  Busy_o           <= '0';
                         NextBytePos_s    <= 7;
                         NextSCL_s        <= '1';
                         if DoTransfer_i = '1' then
                           NextState      <= Start1;
                         end if;

      -- Start Condition is split into two states because there must be
      -- a minimum time of 4us (at 100kHz SCL) / 0,6us (at 400kHz SCL) between the
      -- two falling edges of SDA & SCL. (See I2C Spec. Rev. 03 - Table 6 / Fig. 27)
      when Start1    =>     NextSCL_s        <= '1';
                            if I2CClk_s = '1' then
                              NextSDA_o_s    <= '0';
                              NextState      <= Start2;
                            else
                              NextState      <= Start1;
                            end if;

      when Start2    =>     NextSDA_o_s    <= '0';
                            NextSCL_s      <= '1';
                            if I2CClk_s = '1' then
                              NextSDA_o_s  <= '0';   -- delay change of SDA_o by 1 clock cycle, i.e. 1 cycle after SCL changes
                              NextSCL_s    <= '0';
                              NextState    <= WriteByte;
                            else
                              NextState    <= Start2;
                            end if;

      -- BytePos_s indicates the current Bit of the Byte. Counts from 7 down
      -- to 0 and then it starts againt at 7 due to the overflow of the three
      -- Bit Integer.
      when WriteByte =>  NextSDA_o_s  <= Data_i(BytePos_s);
                         NextState    <= WriteByte;
                         NextSCL_s    <= SCL_s;

                         if I2CClk_s = '1' then
                            if SCL_s = '1' AND BytePos_s > 0 then
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= Data_i(BytePos_s);  
                               -- BytePos_s still points to the old bit, which is
                               -- therefore prolonged for 1 Clk_i cycle, until the
                               -- new value is set as first assignment in this
                               -- WriteByte state!
                               NextBytePos_s    <= BytePos_s-1;
                            elsif SCL_s = '1' AND BytePos_s = 0 then
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= Data_i(BytePos_s);  -- delay change of SDA_o by 1 clock cycle, i.e. 1 cycle after SCL changes
                               ByteReady_o      <= '1';
                               NextState        <= ReadNAck;
                               NextBytePos_s    <= 7;
                            else -- SCL_s = 0
                               NextSCL_s        <= '1';
                               if SDA_i_s /= SDA_o_s then
                                 NextSCL_s      <= '1';
                                 NextSDA_o_s    <= '1';
                                 BusErr_o       <= '1';
                                 NextState      <= BusErr;
                               end if;
                            end if;
                         end if;

      when ReadNAck  =>  NextSCL_s <= SCL_s;
                         NextState <= ReadNAck;
                         AckRx_o   <= NOT SDA_i_s;
                         AckValid_o <= I2CClk_s and SCL_s;    -- TransferController should only evaluate NAck on rising edge of SCL

                         if I2CClk_s = '1' then
                            if SCL_s = '0' then
                               NextSCL_s        <= '1';
                               NextState        <= ReadNAck;
                            elsif SCL_s = '1' AND SDA_i_s = '1' then --Got NAck
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= '0';
                               NextState        <= Stop1;
                            elsif SCL_s = '1' AND SDA_i_s = '0' AND DoTransfer_i = '0' then  --Got Ack but DoTransfer=0
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= '0';
                               NextState        <= Stop1;
                            elsif SCL_s = '1' AND SDA_i_s = '0' AND DoTransfer_i = '1' AND ReadWrite_n_i = '0' then  --Ack & DoTransfer & Write
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= Data_i(BytePos_s);
                               NextState        <= WriteByte;
                            elsif SCL_s = '1' AND SDA_i_s = '0' AND DoTransfer_i = '1' AND ReadWrite_n_i = '1' then  --Ack & DoTransfer & Read
                               NextSCL_s        <= '0';
                               NextState        <= ReadByte;
                            else
                               --stay in this state
                            end if;
                         end if;

      when ReadByte  =>  NextState   <= ReadByte;
                         NextSCL_s   <= SCL_s;
                         NextSDA_o_s <= '1';

                         if I2CClk_s = '1' then
                            if SCL_s = '0' then
                               NextSCL_s             <= '1';
                               NextState             <= ReadByte;
                            elsif SCL_s = '1' AND BytePos_s > 0 then --Read Bit 7 to 1
                               NextData_s(BytePos_s) <= SDA_i_s;
                               NextSCL_s             <= '0';
                               NextBytePos_s         <= BytePos_s-1;
                               NextState             <= ReadByte;
                            elsif SCL_s = '1' AND BytePos_s = 0 then  --Read Bit 0 Set ByteReady and (N)Ack
                               NextData_s(BytePos_s) <= SDA_i_s; -- Bit 0
                               NextSCL_s             <= '0';
                               NextBytePos_s         <= 7;
                               ByteReady_o           <= '1';
                               NextState             <= WriteNAck;
                            else
                               -- stay in this state
                            end if;
                         end if;

      when WriteNAck =>  NextState   <= WriteNAck;
                         NextSCL_s   <= SCL_s;
                         NextSDA_o_s <= NOT AckTx_i; -- (N)Ack

                         if I2CClk_s = '1' then
                            if SCL_s = '0' then
                               NextSCL_s        <= '1';
                               NextState        <= WriteNAck;
                            elsif SCL_s = '1' AND DoTransfer_i = '0' then --All Read > Stop
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= NOT AckTx_i; -- keep (N)Ack until next state so that SDA_o changes one Clk_i cycle after SCL_o
                               NextState        <= Stop1;
                            elsif SCL_s = '1' AND DoTransfer_i = '1' then  --Read next Byte
                               NextSCL_s        <= '0';
                               NextSDA_o_s      <= NOT AckTx_i; -- keep (N)Ack until next state so that SDA_o changes one Clk_i cycle after SCL_o
                               NextState        <= ReadByte;
                            end if;
                         end if;

      when Stop1     =>  NextState   <= Stop1;
                         NextSCL_s   <= SCL_s;
                         NextSDA_o_s <= '0';

                         if I2CClk_s = '1' then
                            if SCL_s = '0' then
                               NextSCL_s        <= '1';
                               NextState        <= Stop1;
                            else
                               NextSCL_s        <= '1';
                               NextSDA_o_s      <= '1';
                               NextState        <= Idle;
                            end if;
                         end if;

      when BusErr    =>  BusErr_o         <= '1';
                          -- default: Busy_o        <= '1';
                         if DoTransfer_i = '0' then
                            NextSCL_s     <= '1';
                            NextState     <= Idle;
                         else
                            NextSCL_s     <= '1';
                            NextState     <= BusErr;
                         end if;

      when others    =>     NextSCL_s     <= '1';
                            Busy_o        <= '0';
                            NextState     <= Idle;
    end case;
  end process;

  -- Setting SCL-, SDA- & Data-Output Ports
  SCL_o  <= SCL_s;
  SDA_o  <= SDA_o_s;
  Data_o <= NextData_s;

end rtl;

