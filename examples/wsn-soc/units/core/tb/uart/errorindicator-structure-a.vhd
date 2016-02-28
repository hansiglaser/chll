

architecture structure of ErrorIndicator is

  component ErrorBit
    Port ( Clk_i               : in  STD_LOGIC;
           Reset_i_n           : in  STD_LOGIC;
           ErrorReset_i        : in  STD_LOGIC;
           ErrorBit_i          : in  STD_LOGIC;
           ErrorIndicatorBit_o : out STD_LOGIC);
  end component;

begin
    -- Parity Error
    PARITYERR: ErrorBit
    port map ( Clk_i               => Clk_i,
               Reset_i_n           => Reset_i_n,
               ErrorReset_i        => ErrorReset_i,
               ErrorBit_i          => ParityError_i,
               ErrorIndicatorBit_o => ParityErrorIndicator_o);

    -- StopBit Error
    STOPERR:   ErrorBit
    port map ( Clk_i               => Clk_i,
               Reset_i_n           => Reset_i_n,
               ErrorReset_i        => ErrorReset_i,
               ErrorBit_i          => StopBitError_i,
               ErrorIndicatorBit_o => StopBitErrorIndicator_o);

    -- RxBufferFullError
    RXBUFFERR: ErrorBit
    port map ( Clk_i               => Clk_i,
               Reset_i_n           => Reset_i_n,
               ErrorReset_i        => ErrorReset_i,
               ErrorBit_i          => RxBufferFullError_i,
               ErrorIndicatorBit_o => RxBufferFullErrorIndicator_o);

end structure;

