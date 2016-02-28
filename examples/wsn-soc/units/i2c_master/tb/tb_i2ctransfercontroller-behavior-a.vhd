

ARCHITECTURE behavior OF tb_I2CTransferController IS


  CONSTANT HalfClkPer : time      := 5 ns; -- 100 MHz
  CONSTANT ReadCountWidth_c : integer := 4;

  CONSTANT TestCase   : Integer := 1;

  -- Component Declaration
  COMPONENT i2ctransfercontroller IS
    Generic (ReadCountWidth_g :        INTEGER  := ReadCountWidth_c); -- 4 Bit ReadCount_i Vector
    Port ( Reset_i            : in     STD_LOGIC;
           Clk_i              : in     STD_LOGIC;
           ReadCount_i        : in     STD_LOGIC_VECTOR (ReadCountWidth_g_1 downto 0);
           StartProcess_i     : in     STD_LOGIC;
           ReceiveSend_n_i    : in     STD_LOGIC;
           Busy_o             : out    STD_LOGIC;
           FiFoReadNext_o     : out    STD_LOGIC;
           FiFoWrite_o        : out    STD_LOGIC;
           FiFoEmpty_i        : in     STD_LOGIC;
           FiFoFull_i         : in     STD_LOGIC;

           CoreDoTransfer_o   : out    STD_LOGIC;
           CoreReadWrite_n_o  : out    STD_LOGIC;
           CoreAckTx_o        : out    STD_LOGIC;
           CoreAckRx_i        : in     STD_LOGIC;
           CoreBusy_i         : in     STD_LOGIC;
           CoreByteReady_i    : in     STD_LOGIC;
           CoreBusErr_i       : in     STD_LOGIC;

           ErrAck_i           : in     STD_LOGIC;
           ErrBusColl_o       : out    STD_LOGIC;
           ErrFiFoFull_o      : out    STD_LOGIC;
           ErrGotNAck_o       : out    STD_LOGIC;
           ErrCoreBusy_o      : out    STD_LOGIC;
           ErrFiFoEmpty_o     : out    STD_LOGIC;
           ErrCoreStopped_o   : out    STD_LOGIC;
           ErrDevNotPresent_o : out    STD_LOGIC;
           ErrReadCountZero_o : out    STD_LOGIC);
  END COMPONENT;

  -- Signal Declaration

   -- General Signals
  SIGNAL Reset_i           : STD_LOGIC;
  SIGNAL Clk_i             : STD_LOGIC;

   -- TransferController Signals
  SIGNAL ReadCount_i       : STD_LOGIC_VECTOR (ReadCountWidth_c-1 downto 0);
  SIGNAL StartProcess_i    : STD_LOGIC;
  SIGNAL ReceiveSend_n_i   : STD_LOGIC;
  SIGNAL Busy_o            : STD_LOGIC;
  SIGNAL FIFOWrite       : STD_LOGIC;
  SIGNAL FIFOReadNext    : STD_LOGIC;

   -- FiFo Signals
  SIGNAL FIFOEmpty         : STD_LOGIC;
  SIGNAL FIFOFull          : STD_LOGIC;

   -- Signals between TransferController and Core
  SIGNAL CoreDoTransfer  : STD_LOGIC;
  SIGNAL CoreReadWrite_n : STD_LOGIC;
  SIGNAL CoreAckTx       : STD_LOGIC;
  SIGNAL CoreAckRx       : STD_LOGIC;
  SIGNAL CoreBusy        : STD_LOGIC;
  SIGNAL CoreByteReady   : STD_LOGIC;
  SIGNAL CoreBusErr      : STD_LOGIC;

   -- Error handling of TransferController
  SIGNAL ErrAck_i          : STD_LOGIC;
  SIGNAL Errors_o          : STD_LOGIC_VECTOR(7 downto 0);


  -- Wating Procedure
  PROCEDURE wait4risingEdges(CONSTANT edges : IN INTEGER) IS
   --VARIABLE edge: INTEGER:=1;
  BEGIN
   FOR edge IN 1 TO edges LOOP
      wait until rising_edge(clk_i);
   END LOOP;
  END PROCEDURE;

  BEGIN
    -- Component Instantiation
    mycontroller : i2ctransfercontroller
      GENERIC MAP(ReadCountWidth_g => ReadCountWidth_c) -- 4 Bit ReadCount_i Vector
      PORT MAP(Reset_i            => Reset_i,
               Clk_i              => Clk_i,
               ReadCount_i        => ReadCount_i,
               StartProcess_i     => StartProcess_i,
               ReceiveSend_n_i    => ReceiveSend_n_i,
               Busy_o             => Busy_o,
               FiFoReadNext_o     => FIFOReadNext,
               FiFoWrite_o        => FIFOWrite,
               FiFoEmpty_i        => FIFOEmpty,
               FiFoFull_i         => FIFOFull,

               CoreDoTransfer_o   => CoreDoTransfer,
               CoreReadWrite_n_o  => CoreReadWrite_n,
               CoreAckTx_o        => CoreAckTx,
               CoreAckRx_i        => CoreAckRx,
               CoreBusy_i         => CoreBusy,
               CoreByteReady_i    => CoreByteReady,
               CoreBusErr_i       => CoreBusErr,

               ErrAck_i           => ErrAck_i,
               ErrBusColl_o       => Errors_o(7),
               ErrFiFoFull_o      => Errors_o(6),
               ErrGotNAck_o       => Errors_o(5),
               ErrCoreBusy_o      => Errors_o(4),
               ErrFiFoEmpty_o     => Errors_o(3),
               ErrCoreStopped_o   => Errors_o(2),
               ErrDevNotPresent_o => Errors_o(1),
               ErrReadCountZero_o => Errors_o(0));
    -- END OF COMPONENTS

    -- PROCESS to generate 200(800)-kHz Clock Signal
    clock: PROCESS
      BEGIN
        Clk_i <= '0';
        wait for HalfClkPer;
        Clk_i <= '1';
        wait for HalfClkPer;
    END PROCESS clock;

    -- Process to generate ControllCommands
    CTRL : PROCESS

      BEGIN
        -- Default Signals
        ReadCount_i <= "0010";
        ReceiveSend_n_i <= '0';
        StartProcess_i <= '0';
        ErrAck_i <= '0';
        FIFOFull <= '0';
        FIFOEmpty <= '0';
        CoreBusy <= '0';
        CoreByteReady <= '0';
        CoreAckRx <= '0';

        -- Reset
        Reset_i       <= '1';
        wait for HalfClkPer;
        Reset_i       <= '0';

        -- Start Process
        wait for HalfClkPer;
        StartProcess_i <= '1';
        wait4risingEdges(edges => 1);
        StartProcess_i <= '0';
        CoreBusy <= '1';

        wait4risingEdges(edges => 3);
        CoreByteReady <= '1';

        wait4risingEdges(edges => 1);
        CoreByteReady <= '0';
        FIFOEmpty <= '1';
        wait4risingEdges(edges => 1);
        CoreAckRx <= '1';
        wait4risingEdges(edges => 1);
        CoreAckRx <= '0';
        CoreBusy <= '0';

         wait;

    END PROCESS;


END;

