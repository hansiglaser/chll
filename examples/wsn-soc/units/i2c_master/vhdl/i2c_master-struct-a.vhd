

ARCHITECTURE struct OF i2c_master IS
  -- Component Declaration
  COMPONENT i2ccore
    Generic ( DividerWidth_g : integer range 4 to 32);

    Port (    Reset_i        : in  STD_LOGIC;
              Clk_i          : in  STD_LOGIC;
              Data_i         : in  STD_LOGIC_VECTOR (7 downto 0);
              Data_o         : out STD_LOGIC_VECTOR (7 downto 0);
              DoTransfer_i   : in  STD_LOGIC;
              ReadWrite_n_i  : in  STD_LOGIC;
              AckTx_i        : in  STD_LOGIC;
              AckRx_o        : out STD_LOGIC;
              AckValid_o     : out STD_LOGIC;
              Busy_o         : out STD_LOGIC;
              ByteReady_o    : out STD_LOGIC;
              BusErr_o       : out STD_LOGIC;
              SDA_o          : out STD_LOGIC;
              SDA_i          : in  STD_LOGIC;
              SCL_o          : out STD_LOGIC;
              F100_400_n_i   : in  STD_LOGIC;
              Divider800_i   : in  std_logic_vector(DividerWidth_g-1 downto 0)
              );
  END COMPONENT;

  COMPONENT i2ctransfercontroller
    Generic ( ReadCountWidth_g   :        INTEGER);
    Port (    Reset_i            : in     STD_LOGIC;
              Clk_i              : in     STD_LOGIC;
              ReadCount_i        : in     STD_LOGIC_VECTOR (ReadCountWidth_g-1 downto 0);
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
              CoreAckValid_i     : in     STD_LOGIC;
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

  COMPONENT FIFOSyncTop
    Generic ( DataWidth   : integer range 2 to 64 := 8;
              AdressWidth : integer range 2 to 10 := 4);
    Port (    Reset_n     : in  STD_LOGIC;
              Clk         : in  STD_LOGIC;
              DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
              WriteA_i    : in  STD_LOGIC;
              DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
              ReadNextB_i : in  STD_LOGIC;
              FIFOFull_o  : out STD_LOGIC;
              FIFOEmpty_o : out STD_LOGIC);
  END COMPONENT;

  -- Signal Declaration

   -- TransferController Signals
  SIGNAL TCFIFOWrite_s       : STD_LOGIC;
  SIGNAL TCFIFOReadNext_s    : STD_LOGIC;

   -- FIFO Signals
  SIGNAL FIFOReadNext_s      : STD_LOGIC;
  SIGNAL FIFOWrite_s         : STD_LOGIC;
  SIGNAL FIFOFull_s          : STD_LOGIC;
  SIGNAL FIFOEmpty_s         : STD_LOGIC;

   -- Signals between TransferController and Core
  SIGNAL CoreDoTransfer_s    : STD_LOGIC;
  SIGNAL CoreReadWrite_n_s   : STD_LOGIC;
  SIGNAL CoreAckTx_s         : STD_LOGIC;
  SIGNAL CoreAckRx_s         : STD_LOGIC;
  SIGNAL CoreAckValid_s      : STD_LOGIC;
  SIGNAL CoreBusy_s          : STD_LOGIC;
  SIGNAL CoreByteReady_s     : STD_LOGIC;
  SIGNAL CoreBusErr_s        : STD_LOGIC;

   -- Data Signals
  SIGNAL Data_s              : std_logic_vector (7 downto 0);
  SIGNAL CoreData_o_s        : std_logic_vector (7 downto 0);
  SIGNAL CoreData_i_s        : std_logic_vector (7 downto 0);

  BEGIN
    -- Component Instantiation

    mycore : i2ccore
      GENERIC MAP ( DividerWidth_g => DividerWidth_g)
      PORT MAP (    Reset_i        => Reset_i,
                    Clk_i          => Clk_i,
                    Data_i         => CoreData_i_s,
                    Data_o         => CoreData_o_s,
                    DoTransfer_i   => CoreDoTransfer_s,
                    ReadWrite_n_i  => CoreReadWrite_n_s,
                    AckRx_o        => CoreAckRx_s,
                    AckValid_o     => CoreAckValid_s,
                    AckTx_i        => CoreAckTx_s,
                    Busy_o         => CoreBusy_s,
                    ByteReady_o    => CoreByteReady_s,
                    BusErr_o       => CoreBusErr_s,
                    SDA_o          => SDA_o,
                    SDA_i          => SDA_i,
                    SCL_o          => SCL_o,
                    Divider800_i   => Divider800_i,
                    F100_400_n_i   => F100_400_n_i);

    mycontroller : i2ctransfercontroller
      GENERIC MAP ( ReadCountWidth_g   => ReadCountWidth_g)
      PORT MAP (    Reset_i            => Reset_i,
                    Clk_i              => Clk_i,
                    ReadCount_i        => ReadCount_i,
                    StartProcess_i     => StartProcess_i,
                    ReceiveSend_n_i    => ReceiveSend_n_i,
                    Busy_o             => Busy_o,
                    FiFoReadNext_o     => TCFIFOReadNext_s,
                    FiFoWrite_o        => TCFIFOWrite_s,
                    FiFoEmpty_i        => FIFOEmpty_s,
                    FiFoFull_i         => FIFOFull_s,

                    CoreDoTransfer_o   => CoreDoTransfer_s,
                    CoreReadWrite_n_o  => CoreReadWrite_n_s,
                    CoreAckTx_o        => CoreAckTx_s,
                    CoreAckRx_i        => CoreAckRx_s,
                    CoreAckValid_i     => CoreAckValid_s,
                    CoreBusy_i         => CoreBusy_s,
                    CoreByteReady_i    => CoreByteReady_s,
                    CoreBusErr_i       => CoreBusErr_s,

                    ErrAck_i           => ErrAck_i,
                    ErrBusColl_o       => ErrBusColl_o,
                    ErrFiFoFull_o      => ErrFiFoFull_o,
                    ErrGotNAck_o       => ErrGotNAck_o,
                    ErrCoreBusy_o      => ErrCoreBusy_o,
                    ErrFiFoEmpty_o     => ErrFiFoEmpty_o,
                    ErrCoreStopped_o   => ErrCoreStopped_o,
                    ErrDevNotPresent_o => ErrDevNotPresent_o,
                    ErrReadCountZero_o => ErrReadCountZero_o);

    myfifo : FIFOSyncTop
      GENERIC MAP(
        DataWidth     => 8,
        AdressWidth   => FIFOAddressWidth_g)
      PORT MAP (
        Reset_n       => "not"(Reset_i), -- calls "not"-function (A => not B would be a static assignment)
        Clk           => Clk_i,
        DataA_i       => Data_s,
        WriteA_i      => FIFOWrite_s,
        DataB_o       => CoreData_i_s,
        ReadNextB_i   => FIFOReadNext_s,
        FIFOFull_o    => FIFOFull_s,
        FIFOEmpty_o   => FIFOEmpty_s);

    -- END OF COMPONENTS

    Data_s <= Data_i when FIFOWrite_i = '1' else CoreData_o_s;

    FIFOReadNext_s <= TCFIFOReadNext_s or FIFOReadNext_i;
    FIFOWrite_s    <= TCFIFOWrite_s    or FIFOWrite_i;
    
    Data_o      <= CoreData_i_s;
    FIFOFull_o  <= FIFOFull_s;
    FIFOEmpty_o <= FIFOEmpty_s;

END;
