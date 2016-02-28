

architecture structure of Uart is

-- component declaration

  component RxModule
    generic (
      MaxDataWidth                 : integer range 2 to 64;
      MaxSpeedDividerWidth         : integer;
      RxFifoAdressWidth            : integer range 2 to 10;
      Oversampling                 : integer range 2 to 2);
    Port (
      -- Parallel data inputs; CPU sided
      RxData_o                     : out  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      RxRd_i                       : in   STD_LOGIC;
      RxFull_o                     : out  STD_LOGIC;
      RxEmpty_o                    : out  STD_LOGIC;
      -- Configuration bits
      BitsSelect_i                 : in   BitSelectionType;
      ParityOn_i                   : in   STD_LOGIC;
      ParityEvenOdd_i              : in   ParityType;
      SpeedDivider_i               : in   STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
      -- Global Signals
      Clk_i                        : in   STD_LOGIC;
      Reset_i_n                    : in   STD_LOGIC;
      ErrorReset_i                 : in   STD_LOGIC;
      -- Error Signals
      RxParityErrorIndicator_o     : out  STD_LOGIC;
      RxStopBitErrorIndicator_o    : out  STD_LOGIC;
      RxBufferFullErrorIndicator_o : out  STD_LOGIC;
      -- Seriell input port
      RxD_i                        : in   STD_LOGIC);
  end component;

  component TxModule
    generic (
      MaxDataWidth         : integer range 2 to 64;
      MaxSpeedDividerWidth : integer;
      TxFifoAdressWidth    : integer range 2 to 10;
      Oversampling         : integer range 0 to 3);
    port (
      -- Parallel data inputs; CPU sided
      TxData_i             : in   STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      TxWr_i               : in   STD_LOGIC;
      TxEmpty_o            : out  STD_LOGIC;
      TxFull_o             : out  STD_LOGIC;
      -- Configuration bits
      BitsSelect_i         : in   BitSelectionType;
      ParityOn_i           : in   STD_LOGIC;
      ParityEvenOdd_i      : in   ParityType;
      SpeedDivider_i       : in   STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
      -- Global Signals
      Clk_i                : in   STD_LOGIC;
      Reset_i_n            : in   STD_LOGIC;
      -- Seriell in/output ports
      TxD_o                : out  STD_LOGIC);
  end component;

begin

  RXMOD: RxModule
    generic map (
      MaxDataWidth         => MaxDataWidth,
      MaxSpeedDividerWidth => MaxSpeedDividerWidth,
      RxFifoAdressWidth    => RxFifoAdressWidth,
      Oversampling         => Oversampling)
    port map(
      -- Parallel data inputs; CPU sided
      RxData_o                     => RxData_o,
      RxRd_i                       => RxRd_i,
      RxFull_o                     => RxFull_o,
      RxEmpty_o                    => RxEmpty_o,
      -- Configuration bits
      BitsSelect_i                 => BitsSelect_i,
      ParityOn_i                   => ParityOn_i,
      ParityEvenOdd_i              => ParityEvenOdd_i,
      SpeedDivider_i               => SpeedDivider_i,
      -- Global Signals
      Clk_i                        => Clk_i,
      Reset_i_n                    => Reset_i_n,
      -- Error Signals
      ErrorReset_i                 => ErrorReset_i,
      RxParityErrorIndicator_o     => RxParityErrorIndicator_o,
      RxStopBitErrorIndicator_o    => RxStopBitErrorIndicator_o,
      RxBufferFullErrorIndicator_o => RxBufferFullErrorIndicator_o,
      -- Seriell input port
      RxD_i                        => RxD_i);

  TXMOD: TxModule
    generic map(
      MaxDataWidth         => MaxDataWidth,
      MaxSpeedDividerWidth => MaxSpeedDividerWidth,
      TxFifoAdressWidth    => TxFifoAdressWidth,
      Oversampling         => Oversampling)
    port map(
      -- Parallel data inputs; CPU sided
      TxData_i             => TxData_i,
      TxWr_i               => TxWr_i,
      TxEmpty_o            => TxEmpty_o,
      TxFull_o             => TxFull_o,
      -- Configuration bits
      BitsSelect_i         => BitsSelect_i,
      ParityOn_i           => ParityOn_i,
      ParityEvenOdd_i      => ParityEvenOdd_i,
      SpeedDivider_i       => SpeedDivider_i,
      -- Global Signals
      Clk_i                => Clk_i,
      Reset_i_n            => Reset_i_n,
      -- Seriell in/output ports
      TxD_o                => TxD_o);

end structure;

