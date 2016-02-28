

ARCHITECTURE behavior OF tb_I2CCore IS

  -- DEFINE FREQUENCY HERE!
  CONSTANT InputFrequency : integer := 100 * 1000 * 1000; -- 100 MHz

  -- SELECT TESTCASE HERE!

  -- | TestCase | Description                                     | T_sim  |
  -- |---------------------------------------------------------------------|
  -- | Case 1   | Address + Nack + Stop                           | 150 us |
  -- | Case 2   | Address + Ack + DoTransfer_i=0                  | 150 us |
  -- | Case 3   | Address + Ack + 2 Bytes Writing (getting Ack)   | 320 us |
  -- | Case 4   | Address + Ack + 2 Bytes Writing + Nack on 2nd   | 320 us |
  -- | Case 5   | Address + 1 Byte Reading + Sending NAck + Stop  | 240 us |
  -- | Case 6   | Address + 2 Bytes Reading + Ack 1st + NAck 2nd  | 330 us |
  -- | Case 7   | Address with Bus-Collision                      |  70 us |
  -- |---------------------------------------------------------------------|
  CONSTANT TestCase   : Integer := 6;
--------------------------------------------------------------------------------
-- Configuration done - don't edit the following
--------------------------------------------------------------------------------


  CONSTANT HalfClkPer : time    := (1 sec)/(2*InputFrequency); -- 100 MHz => 5 ns
  CONSTANT Divider    : integer := (InputFrequency / (200 * 1000));

  -- Component Declaration
  COMPONENT i2ccore IS
    Generic (Divider200_g   : integer := (100 * 1000 * 1000 / (200 * 1000))-1;
             Divider800_g   : integer := (100 * 1000 * 1000 / (800 * 1000))-1);

    Port ( Reset_i        : in      STD_LOGIC;
           Clk_i          : in      STD_LOGIC;
           Data_i         : in      STD_LOGIC_VECTOR (7 downto 0);
           Data_o         : out     STD_LOGIC_VECTOR (7 downto 0);
           DoTransfer_i   : in      STD_LOGIC;
           ReadWrite_n_i  : in      STD_LOGIC;
           AckTx_i        : in      STD_LOGIC;
           AckRx_o        : out     STD_LOGIC;
           Busy_o         : out     STD_LOGIC;
           ByteReady_o    : out     STD_LOGIC;
           BusErr_o       : out     STD_LOGIC;
           SDA_o          : out     STD_LOGIC;
           SDA_i          : in      STD_LOGIC;
           SCL_o          : out     STD_LOGIC;
           F100_400_n_i   : in      STD_LOGIC);
  END COMPONENT;

  -- Signal Declaration
  TYPE ByteArray is array(3 downto 0) of std_logic_vector(7 downto 0);
  SIGNAL WriteMemory  : ByteArray := ("10011010","11010010","11010010","01010101");
  SIGNAL ReadMemory   : ByteArray;

  SIGNAL Clk_i        : std_logic;
  SIGNAL Reset_i      : std_logic;
  SIGNAL Data_i       : std_logic_vector (7 downto 0);
  SIGNAL Data_o       : std_logic_vector (7 downto 0);
  SIGNAL DoTransfer_i : std_logic := '0';
  SIGNAL ReadWrite_n_i: std_logic := '0';
  SIGNAL AckRx_o      : std_logic := '0';
  SIGNAL AckTx_i      : std_logic := '0';
  SIGNAL Busy_o       : std_logic := '0';
  SIGNAL ByteReady_o  : std_logic := '0';
  SIGNAL BusErr_o     : std_logic := '0';
  SIGNAL SDA_o        : std_logic := '1';
  SIGNAL SDA_i        : std_logic := '1';
  SIGNAL SCL_o        : std_logic := '1';
  SIGNAL F100_400_n_i : std_logic := '1';


   -- Wating Procedure
  PROCEDURE wait4risingEdges(CONSTANT edges : IN INTEGER) IS
  BEGIN
   FOR edge IN 1 TO Divider*edges LOOP
      wait until rising_edge(Clk_i);
   END LOOP;
  END PROCEDURE;

  BEGIN

    -- Component Instantiation
    mycore : i2ccore
      GENERIC MAP ( Divider200_g   => Divider-1,
                    Divider800_g   => (Divider/4)-1)
      PORT MAP (    Reset_i        => Reset_i,
                    Clk_i          => Clk_i,
                    Data_i         => Data_i,
                    Data_o         => Data_o,
                    DoTransfer_i   => DoTransfer_i,
                    ReadWrite_n_i  => ReadWrite_n_i,
                    AckRx_o        => AckRx_o,
                    AckTx_i        => AckTx_i,
                    Busy_o         => Busy_o,
                    ByteReady_o    => ByteReady_o,
                    BusErr_o       => BusErr_o,
                    SDA_o          => SDA_o,
                    SDA_i          => SDA_i,
                    SCL_o          => SCL_o,
                    F100_400_n_i   => F100_400_n_i);

    -- PROCESS to generate Clock Signal (e.g. 100 MHz)
    clock: PROCESS
      BEGIN
        Clk_i <= '0';
        wait for HalfClkPer;
        Clk_i <= '1';
        wait for HalfClkPer;
    END PROCESS clock;

    -- Process to generate ControlCommands
    CTRL : PROCESS
      BEGIN
        -- Reset
        Reset_i       <= '1';
        wait until rising_edge(Clk_i);
        Reset_i       <= '0';

        -- Apply Addressdata to Data_i
        Data_i        <= WriteMemory(3);

        -- Check if all Outputs are set proper after Reset
        assert SCL_o = '1' AND SDA_o = '1' AND AckRx_o = '0' AND ByteReady_o = '0' AND BusErr_o = '0' AND Busy_o = '0'
          report "Output Signals not proper after Reset!"
          severity warning;

        DoTransfer_i   <= '1';
        wait4risingEdges(1);
        -- Checking Start-Signal
        assert SCL_o = '1' AND SDA_o = '1'
          report "Error in 1st part of Start-Signal!"
          severity warning;
        wait4risingEdges(1);
        assert SCL_o = '1' AND SDA_o = '0'
          report "Error in 2nd part of Start-Signal!"
          severity warning;

        -- Running selected Testcase --
        -- ========================= --
        case TestCase is
            -- Case 1: Address + Nack + Stop                        T_Sim 150 us
            when 1 =>   DoTransfer_i   <= '0';
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after core sent the last Bit!"
                           severity warning;

                        SDA_i          <='1'; -- NAck
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '0'
                           report "Core did set AckRx_o unexpected!"
                           severity warning;
            --------------------------------------------------------------------

            -- Case 2: Address + Ack + DoTransfer_i=0               T_Sim 150 us
            when 2 =>   DoTransfer_i   <= '0';
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit!"
                           severity warning;

                        SDA_i          <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time)!"
                           severity warning;
            --------------------------------------------------------------------

            -- Case 3: Address + Ack + 2 Bytes Writing (with Acks)  T_Sim 320 us
            when 3 =>   for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Addressbyte)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Addressbyte)!"
                           severity warning;

                        Data_i      <= WriteMemory(2); -- Applying 1st Databyte to write
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(2)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Databyte 1)!"
                           severity warning;
                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Databyte 1)!"
                           severity warning;

                        Data_i      <= WriteMemory(1); -- Applying 2nd Databyte to write
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(1)(i);
                           wait4risingEdges(2);
                        end loop;
                        DoTransfer_i <= '0';

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Databyte 2)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Databyte 2)!"
                           severity warning;
                        SDA_i       <= '1';
            --------------------------------------------------------------------

            -- Case 4: Addr. + Ack + 2 Bytes Writing + Nack on 2nd  T_Sim 320 us
            when 4 =>   for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Addressbyte)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Addressbyte)!"
                           severity warning;

                        Data_i      <= WriteMemory(2); -- Applying 1st Databyte to write
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(2)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Databyte 1)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Databyte 1)!"
                           severity warning;

                        Data_i      <= WriteMemory(1); -- Applying 2nd Databyte to write
                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(1)(i);
                           wait4risingEdges(2);
                        end loop;
                        DoTransfer_i <= '0';

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Databyte 2)!"
                           severity warning;

                        SDA_i       <= '1'; -- NAck
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '0'
                           report "Core did set AckRx_o unexpected (Databyte 2)!"
                           severity warning;
            --------------------------------------------------------------------

            -- Case 5: Addr + 1 Byte Reading + Sending NAck + Stop  T_Sim 240 us
            when 5 =>   ReadWrite_n_i  <= '1'; -- Reading MODE
                        AckTx_i <= '0'; -- Send NAck after reading first Databyte

                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Addressbyte)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Addressbyte)!"
                           severity warning;

                        -- Data to read: 11100111
                        SDA_i <= '1';
                        wait4risingEdges(6);                -- Data to read: 3 x '1'
                        SDA_i       <= '0';
                        wait4risingEdges(4);                 -- Data to read: 2 x '0'
                        SDA_i       <= '1';
                        wait4risingEdges(6);                -- Data to read: 3 x '1'

                        -- Check if SDA_o was stable at '1' during reading-phase
                        assert SDA_o = '1' AND SDA_o'stable(32*HalfClkPer*Divider)
                           report "SDA_o not stable at '1' while reading!"
                           severity warning;

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core read the last Bit!"
                           severity warning;

                        -- Check if Data was read correctly
                        assert Data_o = "11100111"
                           report "Data was not read correctly!"
                           severity warning;
                        DoTransfer_i   <= '0';
                        ReadMemory(3) <= Data_o;

                        -- Check if Core writes Ack
                        wait4risingEdges(2);
                        assert SDA_o = '1'
                           report "Core did write Ack unexpected!"
                           severity warning;
            --------------------------------------------------------------------

            -- Case 6: Addr + 2 Bytes Reading + Ack 1st + NAck 2nd  T_Sim 330 us
            when 6 =>   ReadWrite_n_i     <= '1'; -- Reading MODE
                        AckTx_i <= '1'; -- Send Ack after reading first Databyte

                        for i in 7 downto 0 loop
                           SDA_i       <= WriteMemory(3)(i);
                           wait4risingEdges(2);
                        end loop;

                        -- Check ByteReady_o
                        wait until rising_edge(Clk_i);
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core sent the last Bit (Addressbyte)!"
                           severity warning;

                        SDA_i       <= '0'; -- Ack
                        wait4risingEdges(2);

                        -- Check if Core sets AckRx_o
                        assert AckRx_o = '1'
                           report "Core did not set AckRx_o (in time) (Addressbyte)!"
                           severity warning;

                        -- Reading 1st Byte: Data to read: 11100111
                        SDA_i <= '1';
                        wait4risingEdges(6);                 -- Data to read: 3 x '1'
                        SDA_i       <= '0';
                        wait4risingEdges(4);                 -- Data to read: 2 x '0'
                        SDA_i       <= '1';
                        wait4risingEdges(6);                 -- Data to read: 3 x '1'

                        -- Check if SDA_o was stable at '1' during reading-phase
                        assert SDA_o = '1' AND SDA_o'stable(32*HalfClkPer*Divider)
                           report "SDA_o not stable at '1' while reading!"
                           severity warning;

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core read the last Bit!"
                           severity warning;

                        -- Check if Data was read correctly
                        assert Data_o = "11100111"
                           report "Data was not read correctly!"
                           severity warning;
                        ReadMemory(3) <= Data_o;

                        -- Check if Core writes Ack
                        wait4risingEdges(2);
                        assert SDA_o = '0'
                           report "Core did not write Ack!"
                           severity warning;

                        -- Reading 2nd Byte: Data to read: 11111111
                        AckTx_i <= '0'; -- Send NAck after reading 2nd Databyte
                        SDA_i <= '1';
                        wait4risingEdges(16);                -- Data to read: 8 x '1'
                        DoTransfer_i      <= '0';

                        -- Check if SDA_o was stable at '1' during reading-phase
                        assert SDA_o = '1' AND SDA_o'stable(32*HalfClkPer*Divider)
                           report "SDA_o not stable at '1' while reading!"
                           severity warning;

                        -- Check ByteReady_o
                        assert ByteReady_o = '1'
                           report "ByteReady_o is not set after Core read the last Bit!"
                           severity warning;

                        -- Check if Data was read correctly
                        assert Data_o = "11111111"
                           report "Data was not read correctly!"
                           severity warning;
                        ReadMemory(2) <= Data_o;

                        -- Check if Core writes Ack
                        wait4risingEdges(2);
                        assert SDA_o = '1'
                           report "Core did write Ack unexpected!"
                           severity warning;
            --------------------------------------------------------------------

            -- Case 7: Address with Bus-Collision                    T_Sim 50 us
            when 7 =>   DoTransfer_i   <= '0';

                        wait4risingEdges(2);
                        SDA_i <= '0';
                        wait4risingEdges(4);
                        SDA_i <= '1';
                        wait4risingEdges(2);
                        SDA_i <= '0';
                        wait4risingEdges(1);

                        -- Checking if Core recognizes Collision on 2nd Bit and sets BusErr_o
                        wait until rising_edge(Clk_i);
                        assert BusErr_o = '1'
                           report "Bus Collision was not detected/reported (in time)!"
                           severity warning;

            when others =>

         END CASE;

         case TestCase is
            when 7 =>      --nothing to do

            when others => -- Checking Stop-Signal
                           wait4risingEdges(1);
                           assert SCL_o = '0' AND SDA_o = '0'
                              report "Error in 1st part of Stop-Signal!"
                              severity warning;

                           wait4risingEdges(1);
                           assert SCL_o = '1' AND SDA_o = '0'
                              report "Error in 2nd part of Stop-Signal!"
                              severity warning;

                          -- Check if all Outputs are set proper after Operation finished
                          wait4risingEdges(1);
                          assert SCL_o = '1' AND SDA_o = '1' AND AckRx_o = '0' AND ByteReady_o = '0' AND Busy_o = '0'
                            report "Output Signals not proper after testcase / Core did not finish!"
                            severity warning;
         END CASE;

         wait;

    END PROCESS;

END;


