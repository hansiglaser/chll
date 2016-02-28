

architecture structure of TxModule is

-- signal declaration

  -- signals for Tx module
  signal TxBGEnable                 : std_logic;
  signal TxBaudClk                  : std_logic;
  signal TxStateMachineParallelData : std_logic_vector((MaxDataWidth-1) downto 0);
  signal TxLoadData                 : std_logic;
  signal TxFifoEmpty                : std_logic;
  signal DummyBaudSamplingClk       : std_logic;

-- component declaration

  component BaudGenerator
    generic ( MaxSpeedDividerWidth : integer;
              Oversampling         : integer range 0 to 3);
    Port    ( Clk_i                : in  STD_LOGIC;
              Reset_i_n            : in  STD_LOGIC;
              BGEnable_i           : in  STD_LOGIC;
              SpeedDivider_i       : in  STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
              BaudClk_o            : out STD_LOGIC;
              BaudSamplingClk_o    : out STD_LOGIC);
  end component;

  component TxDataStateMachine
    generic ( MaxDataWidth         : integer range 2 to 64);
    Port    ( Reset_i_n            : in  STD_LOGIC;
              Clk_i                : in  STD_LOGIC;
              BaudClk_i            : in  STD_LOGIC;
              ParallelData_i       : in  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
              FifoEmpty_i          : in  STD_LOGIC;
              BitSelect_i          : in  BitSelectionType;
              ParityOn_i           : in  STD_LOGIC;
              ParityEvenOdd_i      : in  ParityType;
              LoadData_o           : out STD_LOGIC;
              BGEnable_o           : out STD_LOGIC;
              TxD_o                : out STD_LOGIC);
  end component;

  component FIFOSyncTop
    Generic ( DataWidth   : integer range 2 to 64;
              AdressWidth : integer range 2 to 10);
    Port    ( Reset_n     : in  STD_LOGIC;
              Clk         : in  STD_LOGIC;
              DataA_i     : in  STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
              WriteA_i    : in  STD_LOGIC;
              DataB_o     : out STD_LOGIC_VECTOR (DataWidth - 1 downto 0);
              ReadNextB_i : in  STD_LOGIC;
              FIFOFull_o  : out STD_LOGIC;
              FIFOEmpty_o : out STD_LOGIC);
  end component;

begin

  -- Tx BaudGenerator
  TXBAUD: BaudGenerator
    generic map ( MaxSpeedDividerWidth => MaxSpeedDividerWidth,
                  Oversampling         => Oversampling)
    port map    ( Clk_i                => Clk_i,
                  Reset_i_n            => Reset_i_n,
                  BGEnable_i           => TxBGEnable,
                  SpeedDivider_i       => SpeedDivider_i,
                  BaudClk_o            => TxBaudClk,
                  BaudSamplingClk_o    => DummyBaudSamplingClk);

  -- Tx StateMachine
  TXSM: TxDataStateMachine
    generic map ( MaxDataWidth     => MaxDataWidth)
    port map    ( Reset_i_n        => Reset_i_n,
                  Clk_i            => Clk_i,
                  BaudClk_i        => TxBaudClk,
                  ParallelData_i   => TxStateMachineParallelData,
                  FifoEmpty_i      => TxFifoEmpty,
                  BitSelect_i      => BitsSelect_i,
                  ParityOn_i       => ParityOn_i,
                  ParityEvenOdd_i  => ParityEvenOdd_i,
                  LoadData_o       => TxLoadData,
                  BGEnable_o       => TxBGEnable,
                  TxD_o            => TxD_o);

  -- Tx FIFO
  TXFIFO: FIFOSyncTop
    generic map ( DataWidth   => MaxDataWidth,
                  AdressWidth => TxFifoAdressWidth)
    port map    ( Reset_n     => Reset_i_n,
                  Clk         => Clk_i,
                  DataA_i     => TxData_i,
                  WriteA_i    => TxWr_i,
                  DataB_o     => TxStateMachineParallelData,
                  ReadNextB_i => TxLoadData,
                  FIFOFull_o  => TxFull_o,
                  FIFOEmpty_o => TxFifoEmpty);

  TxEmpty_o <= TxFifoEmpty;

end structure;

