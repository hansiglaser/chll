

ARCHITECTURE behavior OF tb_I2CBusMaster IS

  -- DEFINE FREQUENCY HERE!
  CONSTANT InputFrequency : integer := 100 * 1000 * 1000; -- 100 MHz

  -- DEFINE WIDTH OF ReadCount_i HERE!
  CONSTANT ReadCountWidth_c : integer := 4; -- 4 Bit ReadCount_i Vector

  -- SELECT TESTCASE HERE!

  -- | TestCase | Description                                     | T_sim  |
  -- |---------------------------------------------------------------------|
  -- | Case 1   | StartProcess but no (Adress)Byte in Buffer      | 0.5 us |
  -- | Case 2   | Writing AdressByte + BusColl + ErrAck           |  50 us |
  -- | Case 3   | Writing AdressByte + Nack => ErrDevNotPresent   | 120 us |
  -- | Case 4   | Writing AdressByte + 1 Byte + Ack               | 250 us |
  -- | Case 5   | Writing AdressByte + 1 Byte + Nack              | 250 us |
  -- | Case 6   | Reading + BytesToRead=0                         |  10 us |
  -- | Case 7   | Writing AdressByte + Reading 2 Bytes            | 330 us |
  -- | Case 8   | Writing AdressByte + Reading + BufferFull       | 250 us |
  -- |---------------------------------------------------------------------|
  CONSTANT TestCase   : Integer := 8;
--------------------------------------------------------------------------------
-- Configuration done - don't edit the following
--------------------------------------------------------------------------------


  CONSTANT HalfClkPer : time    := (1 sec)/(2*InputFrequency); -- 100 MHz => 5 ns
  CONSTANT Divider    : integer := (InputFrequency / (200 * 1000));

  -- Component Declaration
  COMPONENT i2cbusmaster IS
    Generic ( ReadCountWidth_g    : INTEGER := ReadCountWidth_c; -- 4 Bit ReadCount_i Vector
              FIFOAddressWidth_g  : INTEGER := 4; -- 2^4 Memory Cells
              Divider200_g        : INTEGER := (100 * 1000 * 1000 / (200 * 1000))-1;
              Divider800_g        : INTEGER := (100 * 1000 * 1000 / (800 * 1000))-1);
    Port    ( Reset_i             : in  STD_LOGIC;
              Clk_i               : in  STD_LOGIC;
              F100_400_n_i        : in  STD_LOGIC;

              StartProcess_i      : in  STD_LOGIC;
              ReceiveSend_n_i     : in  STD_LOGIC;
              Busy_o              : out STD_LOGIC;
              ReadCount_i         : in  STD_LOGIC_VECTOR (ReadCountWidth_g-1 downto 0);

              FIFOReadNext_i      : in  STD_LOGIC;
              FIFOWrite_i         : in  STD_LOGIC;
              FIFOEmpty_o         : out STD_LOGIC;
              FIFOFull_o          : out STD_LOGIC;

              Data_i              : in  STD_LOGIC_VECTOR(7 downto 0);
              Data_o              : out STD_LOGIC_VECTOR(7 downto 0);

              ErrAck_i            : in  STD_LOGIC;
              ErrBusColl_o        : out STD_LOGIC;
              ErrFIFOFull_o       : out STD_LOGIC;
              ErrGotNAck_o        : out STD_LOGIC;
              ErrCoreBusy_o       : out STD_LOGIC;
              ErrFIFOEmpty_o      : out STD_LOGIC;
              ErrCoreStopped_o    : out STD_LOGIC;
              ErrDevNotPresent_o  : out STD_LOGIC;
              ErrReadCountZero_o  : out STD_LOGIC;

              SDA_i               : in  STD_LOGIC;
              SDA_o               : out STD_LOGIC;
              SCL_o               : out STD_LOGIC);
  END COMPONENT;

  -- Signal Declaration

   -- General Signals
  SIGNAL Reset_i            : STD_LOGIC;
  SIGNAL Clk_i              : STD_LOGIC;
  SIGNAL F100_400_n_i       : STD_LOGIC;

   -- TransferController Signals
  SIGNAL StartProcess_i     : STD_LOGIC;
  SIGNAL ReceiveSend_n_i    : STD_LOGIC;
  SIGNAL Busy_o             : STD_LOGIC;
  SIGNAL ReadCount_i        : STD_LOGIC_VECTOR (ReadCountWidth_c-1 downto 0);

   -- FIFO Signals
  SIGNAL FIFOReadNext_i     : STD_LOGIC;
  SIGNAL FIFOWrite_i        : STD_LOGIC;
  SIGNAL FIFOEmpty_o        : STD_LOGIC;
  SIGNAL FIFOFull_o         : STD_LOGIC;

   -- Error-Handling Signals
  SIGNAL ErrAck_i           : STD_LOGIC;

  SIGNAL ErrBusColl_o       : STD_LOGIC;
  SIGNAL ErrFIFOFull_o      : STD_LOGIC;
  SIGNAL ErrGotNAck_o       : STD_LOGIC;
  SIGNAL ErrCoreBusy_o      : STD_LOGIC;
  SIGNAL ErrFIFOEmpty_o     : STD_LOGIC;
  SIGNAL ErrCoreStopped_o   : STD_LOGIC;
  SIGNAL ErrDevNotPresent_o : STD_LOGIC;
  SIGNAL ErrReadCountZero_o : STD_LOGIC;

   -- Data Signals
  SIGNAL Data_i         : std_logic_vector (7 downto 0);
  SIGNAL Data_o         : std_logic_vector (7 downto 0);

   -- I2C Bus Signals
  SIGNAL SDA_o        : std_logic;
  SIGNAL SDA_i        : std_logic;
  SIGNAL SCL_o        : std_logic;

  -- Variables
  SIGNAL SDA_Feedback : std_logic;
  SIGNAL SDA_s        : std_logic;

  -- Procedure declaration

   -- Wating Procedure to get next rising edge of clk_i
  PROCEDURE wait4risingEdges(CONSTANT edges : IN INTEGER) IS
  BEGIN
   FOR edge IN 1 TO edges LOOP
      wait until rising_edge(clk_i);
   END LOOP;
  END PROCEDURE;

   -- Wating Procedure to get next rising edge of i2c-core internal clock
  PROCEDURE wait4risingI2CEdges(CONSTANT edges : IN INTEGER) IS
  BEGIN
   FOR edge IN 1 TO Divider*edges LOOP
      wait until rising_edge(clk_i);
   END LOOP;
  END PROCEDURE;
--------------------------------------------------------------------------------

  BEGIN
    -- Component Instantiation
    mybusmaster : i2cbusmaster
      GENERIC MAP ( ReadCountWidth_g    => ReadCountWidth_c, -- 4 Bit ReadCount_i Vector
                    FIFOAddressWidth_g  => 2,               -- 2^2 Memory Cells
                    Divider200_g        => Divider-1,
                    Divider800_g        => (Divider/4)-1)

      PORT MAP (    Reset_i             => Reset_i,
                    Clk_i               => Clk_i,
                    F100_400_n_i        => F100_400_n_i,
                    StartProcess_i      => StartProcess_i,
                    ReceiveSend_n_i     => ReceiveSend_n_i,
                    Busy_o              => Busy_o,
                    ReadCount_i         => ReadCount_i,

                    FIFOReadNext_i      => FIFOReadNext_i,
                    FIFOWrite_i         => FIFOWrite_i,
                    FIFOEmpty_o         => FIFOEmpty_o,
                    FIFOFull_o          => FIFOFull_o,

                    Data_i              => Data_i,
                    Data_o              => Data_o,

                    ErrAck_i            => ErrAck_i,
                    ErrBusColl_o        => ErrBusColl_o,
                    ErrFIFOFull_o       => ErrFIFOFull_o,
                    ErrGotNAck_o        => ErrGotNAck_o,
                    ErrCoreBusy_o       => ErrCoreBusy_o,
                    ErrFIFOEmpty_o      => ErrFIFOEmpty_o,
                    ErrCoreStopped_o    => ErrCoreStopped_o,
                    ErrDevNotPresent_o  => ErrDevNotPresent_o,
                    ErrReadCountZero_o  => ErrReadCountZero_o,

                    SDA_i               => SDA_i,
                    SDA_o               => SDA_o,
                    SCL_o               => SCL_o);
    -- END OF COMPONENTS

    -- Process to generate Oszillation Signal (e.g. 100 MHz)
    clock: PROCESS
      BEGIN
        Clk_i <= '0';
        wait for HalfClkPer;
        Clk_i <= '1';
        wait for HalfClkPer;
    END PROCESS clock;

    -- Process to generate ControlCommands for DeviceUnderTest
    CTRL : PROCESS
      BEGIN
        -- Default Signals
        StartProcess_i  <= '0';
        ReceiveSend_n_i <= '0'; -- Sending Mode
        ErrAck_i        <= '0';
        ReadCount_i     <= "0000";
        FIFOReadNext_i  <= '0';
        FIFOWrite_i     <= '0';
        data_i          <= "00000000";
        F100_400_n_i    <= '1'; -- 100 kHz SCL-Frequency

        SDA_s           <= '1';
        SDA_Feedback    <= '1'; -- Enables feedback of SDA signal from SDA_o
                                -- to SDA_i to avoid BusCollision-Error

        -- Running selected Testcase --
        -- ========================= --

        -- Reset
        Reset_i       <= '1';
        wait4risingEdges(2);
        Reset_i       <= '0';

        case TestCase is
            --Case 1: StartProcess but no (Adress)Byte in Buffer      | 0.5 us |
            when 1 =>
              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              assert Busy_o = '1'
                report "Error: Busy-Output is not set proper to '1'!"
                severity warning;

              -- Check ErrFIFOEmpty
              wait4risingEdges(1);
              assert ErrFIFOEmpty_o = '1'
                report "ErrFIFOEmpty was not set in time!"
                severity warning;

              wait4risingEdges(10);

              -- Acknowledge Error Signal
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';
              wait4risingEdges(1);

              -- Check if Busy- and Error-Signals are proper reset
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
              assert ErrFIFOEmpty_o = '0'
                report "Error: Busy-Output is not set proper!"
                severity warning;
            --------------------------------------------------------------------

            --Case 2: Writing AdressByte + BusColl + ErrAck            | 50 us |
            when 2 =>
              -- Filling FIFO with 1 Byte
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              FIFOWrite_i <= '0';

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              wait until SDA_o'event;
              wait4risingI2CEdges(5);
              SDA_Feedback <= '0';

              wait4risingI2CEdges(1);
              wait4risingEdges(1);
              assert ErrBusColl_o = '1'
                report "ErrBusColl_o was not set proper to '1'!"
                severity warning;

              wait4risingI2CEdges(1);
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';
              wait4risingEdges(1);
              assert ErrBusColl_o = '0'
                report "ErrBusColl_o was not set proper to '0'!"
                severity warning;
            --------------------------------------------------------------------

            --Case 3: Writing AdressByte + Nack => ErrDevNotPresent   | 150 us |
            when 3 =>
              -- Filling FIFO with 1 Byte
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              FIFOWrite_i <= '0';

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              wait until SCL_o'event;
              -- wait until all 8 bits are transfered
              wait4risingI2CEdges(16);

              -- Check if FIFO is empty now
              wait4risingEdges(1);
              assert FIFOEmpty_o = '1'
                report "FIFOEmpty_o was not set proper to '1'!"
                severity warning;

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '1'; -- Writing NACK to SDA_i
              wait4risingI2CEdges(2);

              -- Acknowledge Error-Signal
              wait4risingI2CEdges(2);
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';

              -- Check if Busy- and Error-Signals are proper reset
              wait4risingEdges(1);
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
              assert ErrFIFOEmpty_o = '0'
                report "Error: Busy-Output is not set proper!"
                severity warning;
            --------------------------------------------------------------------

            --Case 4: Writing AdressByte + 1 Byte + Ack               | 250 us |
            when 4 =>
              -- Filling FIFO with two Bytes (Adress + 1 Data)
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              Data_i <= "10101010";
              wait4risingEdges(1);
              FIFOWrite_i <= '0';

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              -- FIRST BYTE
              wait until SCL_o'event;
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '0'; -- Writing ACK to SDA_i
              wait4risingI2CEdges(2);
              SDA_Feedback <= '1'; -- enabling feedback of SDA

              -- SECOND BYTE
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '0'; -- Writing ACK to SDA_i
              wait4risingI2CEdges(2);
              SDA_Feedback <= '1'; -- enabling feedback of SDA

              wait4risingI2CEdges(2);
              -- Check if FIFO is empty now
              assert FIFOEmpty_o = '1'
                report "FIFOEmpty_o was not set proper to '1'!"
                severity warning;

              -- Check if operation finished without error
              assert ErrBusColl_o='0' AND ErrFIFOFull_o='0' AND ErrGotNAck_o='0'
              AND ErrCoreBusy_o='0' AND ErrFIFOEmpty_o='0' AND ErrCoreStopped_o='0'
              AND ErrDevNotPresent_o='0' AND ErrReadCountZero_o='0'
                report "There was an unexpected Error-Signal set during the operation!"
                severity warning;

              -- Check if Busy- and Error-Signals are proper reset
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
            --------------------------------------------------------------------

            --Case 5: Writing AdressByte + 1 Byte + Nack              | 250 us |
            when 5 =>
              -- Filling FIFO with two Bytes (Adress + 1 Data)
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              Data_i <= "10101010";
              wait4risingEdges(1);
              FIFOWrite_i <= '0';

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              -- FIRST BYTE
              wait until SCL_o'event;
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '0'; -- Writing ACK to SDA_i
              wait4risingI2CEdges(2);
              SDA_Feedback <= '1'; -- enabling feedback of SDA

              -- SECOND BYTE
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '1'; -- Writing NACK to SDA_i
              wait4risingI2CEdges(2);
              SDA_Feedback <= '1'; -- enabling feedback of SDA

              -- Check if FIFO is empty now
              assert FIFOEmpty_o = '1'
                report "FIFOEmpty_o was not set proper to '1'!"
                severity warning;

              -- Check if operation finished with right error signals
              assert ErrBusColl_o='0' AND ErrFIFOFull_o='0' AND ErrGotNAck_o='1'
              AND ErrCoreBusy_o='0' AND ErrFIFOEmpty_o='0' AND ErrCoreStopped_o='0'
              AND ErrDevNotPresent_o='0' AND ErrReadCountZero_o='0'
                report "Unexpected Error-Signal were set during the operation!"
                severity warning;

              -- Acknowleding the Error-Signal
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';
              wait4risingEdges(1);

              -- Check if Busy- and Error-Signals are proper reset
              assert ErrBusColl_o='0' AND ErrFIFOFull_o='0' AND ErrGotNAck_o='0'
              AND ErrCoreBusy_o='0' AND ErrFIFOEmpty_o='0' AND ErrCoreStopped_o='0'
              AND ErrDevNotPresent_o='0' AND ErrReadCountZero_o='0'
                report "Not all Error-Signals are reset!"
                severity warning;

              wait4risingI2CEdges(2);

              assert Busy_o = '0'
                report "Busy-Output is not set proper to '0'!"
                severity warning;
            --------------------------------------------------------------------

            --Case 6: Reading + BytesToRead=0                          | 10 us |
            when 6 =>
              ReceiveSend_n_i <= '1';
              -- Filling FIFO with 1 Byte
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              FIFOWrite_i <= '0';
              wait4risingEdges(1);

              --Check if FIFO stores data
              assert FIFOEmpty_o = '0'
                report "Error: FIFO is empty!"
                severity warning;

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';
              wait4risingEdges(1);

              --Check if Errorsignal was set proper
              assert ErrReadCountZero_o = '1'
                report "ErrReadCountZero was not set to '1'!"
                severity warning;

              wait4risingI2CEdges(1);

              -- Acknowledge Error Signal
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';

              -- Check if Busy- and Error-Signals are proper reset
              wait4risingEdges(1);
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
              assert ErrFIFOEmpty_o = '0'
                report "Error: Busy-Output is not set proper!"
                severity warning;
            --------------------------------------------------------------------

            --Case 7: Writing AdressByte + Reading 2 Bytes            | 330 us |
            when 7 =>
              ReadCount_i <= "0010";
              ReceiveSend_n_i <= '1';

              -- Filling FIFO with 1 AdressByte
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(1);
              FIFOWrite_i <= '0';
              wait4risingEdges(1);

              --Check if FIFO stores data
              assert FIFOEmpty_o = '0'
                report "Error: FIFO is empty!"
                severity warning;

              -- Start Process
              StartProcess_i <= '1';
              wait4risingEdges(1);
              StartProcess_i <= '0';

              -- Writing Adress-Byte
              wait until SCL_o'event;
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '0'; -- Writing ACK to SDA_i
              wait4risingI2CEdges(2);

              --Reading first Databyte from SDA_i ("10001101")
              SDA_s <= '1';
              wait4risingI2CEdges(2);
              SDA_s <= '0';
              wait4risingI2CEdges(6);
              SDA_s <= '1';
              wait4risingI2CEdges(4);
              SDA_s <= '0';
              wait4risingI2CEdges(2);
              SDA_s <= '1';
              wait4risingI2CEdges(2);

              --Check if Core writes ACK to SDA_o
              wait4risingEdges(2);
              assert SDA_o = '0'
                report "Error: Core did not set ACK!"
                severity warning;

              -- Reading second Databyte from SDA_i ("11111111")
              wait4risingI2CEdges(18);

              --Check if Core writes NACK to SDA_o
              assert SDA_o = '1'
                report "Error: Core did not set NACK!"
                severity warning;

              wait4risingI2CEdges(4);

              --Check if process finished in time
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
            --------------------------------------------------------------------

            --Case 8: Writing AdressByte + Reading + BufferFull       | 250 us |
            when 8 =>
              ReadCount_i <= "0100";
              ReceiveSend_n_i <= '1';

              -- Filling FIFO with 4 Bytes = FULL
              Data_i <= "10010110";
              FIFOWrite_i <= '1';
              wait4risingEdges(4);
              FIFOWrite_i <= '0';

              --Check if FIFO stores data
              assert FIFOEmpty_o = '0'
                report "Error: FIFO is empty!"
                severity warning;

              -- Start Process
              StartProcess_i <= '1';
              wait4risingI2CEdges(1);
              StartProcess_i <= '0';

              -- Writing Adress-Byte
              wait until SCL_o'event;
              -- wait until all 8 bits of first byte are transfered
              wait4risingI2CEdges(16);

              SDA_Feedback <= '0'; -- disabling feedback of SDA
              SDA_s <= '0'; -- Writing ACK to SDA_i
              wait4risingI2CEdges(2);

              --Reading first Databyte from SDA_i ("10001101")
              SDA_s <= '1';
              wait4risingI2CEdges(2);
              SDA_s <= '0';
              wait4risingI2CEdges(6);
              SDA_s <= '1';
              wait4risingI2CEdges(4);
              SDA_s <= '0';
              wait4risingI2CEdges(2);
              SDA_s <= '1';
              wait4risingI2CEdges(2);

              --Check if FIFO is full after receiving Byte
              wait4risingEdges(1);
              assert FIFOFull_o = '1'
                report "Error: FIFO is not full!"
                severity warning;

              wait4risingI2CEdges(1);

              --Check if ErrFIFOFull_o is full after receiving Byte
              assert ErrFIFOFull_o = '1'
                report "Error ErrFIFOFull_o was not set (in time)!"
                severity warning;

              wait4risingI2CEdges(1);

              -- Acknowledge Error Signal
              ErrAck_i <= '1';
              wait4risingEdges(1);
              ErrAck_i <= '0';

              wait4risingI2CEdges(2);

              -- Check if Busy- and Error-Signals are proper reset
              assert Busy_o = '0'
                report "Error: Busy-Output is not set proper to '0'!"
                severity warning;
              assert ErrFIFOEmpty_o = '0'
                report "Error: Busy-Output is not set proper!"
                severity warning;

            when others =>

         END CASE;
         wait;

    END PROCESS;

    -- Feedback of SDA_o to SDA_i - else set SDA_i by TestCase
    SDA_i <= SDA_o when (SDA_Feedback = '1') else SDA_s;

END;


