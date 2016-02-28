library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity Chip_tb is
end Chip_tb;

architecture behavior of Chip_tb is

  component Chip
    port (
      Reset_n_i         : in    std_logic;
      Clk_i             : in    std_logic;
      Cpu_En_i          : in    std_logic;
      Dbg_En_i          : in    std_logic;
--      Dbg_UART_RxD_i    : in    std_logic;
--      Dbg_UART_TxD_o    : out   std_logic;
      Dbg_SCL_i         : in    std_logic;
      Dbg_SDA_b         : inout std_logic;
      P1_b              : inout std_logic_vector(7 downto 0);
      P2_b              : inout std_logic_vector(7 downto 0);
      UartRxD_i         : in    std_logic;
      UartTxD_o         : out   std_logic;
      SCK_o             : out std_logic;
      MOSI_o            : out std_logic;
      MISO_i            : in  std_logic;
      Inputs_i          : in    std_logic_vector(7 downto 0);
      Outputs_o         : out   std_logic_vector(7 downto 0);
      SPIMISO_i         : in    std_logic;
      SPIMOSI_o         : out   std_logic;
      SPISCK_o          : out   std_logic;
      I2CSCL_b          : out   std_logic;
      I2CSDA_b          : inout std_logic;
--      OneWire_b         : inout std_logic;
--      PWM_i             : in    std_logic;
--      SENT_i            : in    std_logic;
--      SPC_b             : inout std_logic;
      AdcConvComplete_i : in    std_logic;
      AdcDoConvert_o    : out   std_logic;
      AdcValue_i        : in    std_logic_vector(9 downto 0));
  end component;

  -- Reset
  signal Reset_n_i         : std_logic := '0';
  -- Clock
  signal Clk_i             : std_logic := '1';
  signal Cpu_En_i          : std_logic := '1';
  signal Dbg_En_i          : std_logic := '0';
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic;
  signal Dbg_SDA_b         : std_logic;
  signal P1_b              : std_logic_vector(7 downto 0);
  signal P2_b              : std_logic_vector(7 downto 0);
  signal UartRxD_i         : std_logic;
  signal UartTxD_o         : std_logic;
  signal SCK_o             : std_logic;
  signal MOSI_o            : std_logic;
  signal MISO_i            : std_logic := '0';
  signal Inputs_i          : std_logic_vector(7 downto 0);
  signal Outputs_o         : std_logic_vector(7 downto 0);
  signal SPIMISO_i         : std_logic := '0';
  signal SPIMOSI_o         : std_logic;
  signal SPISCK_o          : std_logic;
  signal I2CSCL_b          : std_logic;
  signal I2CSDA_b          : std_logic;
--  signal OneWire_b         : std_logic;
--  signal PWM_i             : std_logic;
--  signal SENT_i            : std_logic;
--  signal SPC_b             : std_logic;
  signal AdcConvComplete_i : std_logic := '0';
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

  constant ClkPeriode : time := 10 ns;

begin

  DUT: Chip
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => Dbg_SCL_i,
      Dbg_SDA_b         => Dbg_SDA_b,
      P1_b              => P1_b,
      P2_b              => P2_b,
      UartRxD_i         => UartRxD_i,
      UartTxD_o         => UartTxD_o,
      SCK_o             => SCK_o,
      MOSI_o            => MOSI_o,
      MISO_i            => MISO_i,
      Inputs_i          => Inputs_i,
      Outputs_o         => Outputs_o,
      SPIMISO_i         => SPIMISO_i,
      SPIMOSI_o         => SPIMOSI_o,
      SPISCK_o          => SPISCK_o,
      I2CSCL_b          => I2CSCL_b,
      I2CSDA_b          => I2CSDA_b,
--      OneWire_b         => OneWire_b,
--      PWM_i             => PWM_i,
--      SENT_i            => SENT_i,
--      SPC_b             => SPC_b,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

  Inputs_i <= (others => '0');

--  Dbg_UART_RxD_i <= '1';
  Dbg_SCL_i <= 'H';
  Dbg_SDA_b <= 'H';
  P1_b      <= (others => 'H');
  P2_b      <= (others => 'H');
  UartRxD_i <= '1';
  MISO_i    <= '0';
  I2CSCL_b  <= 'H';
  I2CSDA_b  <= 'H';
--  OneWire_b <= 'H';
--  SPC_b     <= 'H';

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    wait for 2.3*ClkPeriode;

    -- deassert Reset
    Reset_n_i <= '1';

    wait for 5000*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
