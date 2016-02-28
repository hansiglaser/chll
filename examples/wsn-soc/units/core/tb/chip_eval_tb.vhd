-------------------------------------------------------------------------------
-- Title      : Testbench for design "Core"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Core_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-12-21
-- Last update: 2013-12-21
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-12-21  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.UartPkg.all;

-------------------------------------------------------------------------------

entity Chip_Eval_tb is

end Chip_Eval_tb;

-------------------------------------------------------------------------------

architecture behavior of Chip_Eval_tb is

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

  component adt7310_model
    port (
      SCLK_i  : in  std_logic;
      DOUT_o  : out std_logic;
      DIN_i   : in  std_logic;
      CS_n_i  : in  std_logic;
      CT_n_o  : out std_logic;
      INT_n_o : out std_logic;
      Temp_i  : in  real);
  end component;

  -- component ports
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
  signal MISO_i            : std_logic;
  signal Inputs_i          : std_logic_vector(7 downto 0);
  signal Outputs_o         : std_logic_vector(7 downto 0);
  signal SPIMISO_i         : std_logic;
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
  signal AdcValue_i        : std_logic_vector(9 downto 0) := (others => '0');

  signal INCLK_s           : std_logic := '1';

  -- ADT7310 component ports
  signal ADT7310CS_n_o : std_logic;
  signal CT_n_s        : std_logic;
  signal INT_n_s       : std_logic;
  signal Temp_s        : real := 23.7;

  component Uart
    generic (
      MaxDataWidth         : integer range 5 to 9;
      MaxSpeedDividerWidth : integer range 2 to 32;
      TxFifoAdressWidth    : integer range 2 to 10;
      RxFifoAdressWidth    : integer range 2 to 10;
      Oversampling         : integer range 2 to 2);
    port (
      TxData_i                     : in  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      TxWr_i                       : in  STD_LOGIC;
      TxEmpty_o                    : out STD_LOGIC;
      TxFull_o                     : out STD_LOGIC;
      RxData_o                     : out STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      RxRd_i                       : in  STD_LOGIC;
      RxFull_o                     : out STD_LOGIC;
      RxEmpty_o                    : out STD_LOGIC;
      BitsSelect_i                 : in  BitSelectionType;
      ParityOn_i                   : in  STD_LOGIC;
      ParityEvenOdd_i              : in  ParityType;
      SpeedDivider_i               : in  STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
      Clk_i                        : in  STD_LOGIC;
      Reset_i_n                    : in  STD_LOGIC;
      ErrorReset_i                 : in  STD_LOGIC;
      RxParityErrorIndicator_o     : out STD_LOGIC;
      RxStopBitErrorIndicator_o    : out STD_LOGIC;
      RxBufferFullErrorIndicator_o : out STD_LOGIC;
      TxD_o                        : out STD_LOGIC;
      RxD_i                        : in  STD_LOGIC;
      ScanEnable_i                 : in  std_logic;
      ScanClk_i                    : in  std_logic;
      ScanDataIn_i                 : in  std_logic;
      ScanDataOut_o                : out std_logic
    );
  end component;

  constant MaxDataWidth         : integer range 5 to 9  := 8;
  constant MaxSpeedDividerWidth : integer range 2 to 32 := 16;
  constant TxFifoAdressWidth    : integer range 2 to 10 := 4;
  constant RxFifoAdressWidth    : integer range 2 to 10 := 4;
  constant Oversampling         : integer range 2 to 2  := 2;

  -- clock_freq/(2**Oversampling * Baudrate)
  -- 4MHz/(2**2 * 9600) = 104.1666 rounded --> 104
  -- baudrate = clock_frequency / (2**Oversampling * SpeedDivider) = 4MHz / (2**2 * 104) = 9615.4
  -- error: (9615.4-9600)/9600 = -0.0016
  constant SpeedDivider : integer := 104;

  signal TxData_i                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0) := (others => '0');
  signal TxWr_i                       : STD_LOGIC := '0';
  signal TxEmpty_o                    : STD_LOGIC;
  signal TxFull_o                     : STD_LOGIC;
  signal RxData_o                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
  signal RxRd_i                       : STD_LOGIC;
  signal RxFull_o                     : STD_LOGIC;
  signal RxEmpty_o                    : STD_LOGIC;
  signal BitsSelect_i                 : BitSelectionType := Sel8Bits;
  signal ParityOn_i                   : STD_LOGIC := '0';
  signal ParityEvenOdd_i              : ParityType := Even;
  signal SpeedDivider_i               : STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0) := std_logic_vector(to_unsigned(SpeedDivider,MaxSpeedDividerWidth));
  signal ErrorReset_i                 : STD_LOGIC := '0';
  signal RxParityErrorIndicator_o     : STD_LOGIC;
  signal RxStopBitErrorIndicator_o    : STD_LOGIC;
  signal RxBufferFullErrorIndicator_o : STD_LOGIC;
  signal ScanEnable_i                 : std_logic := '0';
  signal ScanClk_i                    : std_logic := '0';
  signal ScanDataIn_i                 : std_logic := '0';
  signal ScanDataOut_o                : std_logic;
  signal UartRx                       : std_logic_vector(7 downto 0) := (others => '0');
  signal UartCh                       : character := NUL;

  constant ClkPeriode : time := 250 ns;  -- 4 MHz

begin  -- behavior

  -- component instantiation
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

  ADT7310CS_n_o <= to_X01(P1_b(0));

  adt7310_1: adt7310_model
    port map (
      SCLK_i  => SCK_o,
      DOUT_o  => MISO_i,
      DIN_i   => MOSI_o,
      CS_n_i  => ADT7310CS_n_o,
      CT_n_o  => CT_n_s,
      INT_n_o => INT_n_s,
      Temp_i  => Temp_s);

  Uart_1: Uart
    generic map (
      MaxDataWidth         => MaxDataWidth,
      MaxSpeedDividerWidth => MaxSpeedDividerWidth,
      TxFifoAdressWidth    => TxFifoAdressWidth,
      RxFifoAdressWidth    => RxFifoAdressWidth,
      Oversampling         => Oversampling
    )
    port map (
      TxData_i                     => TxData_i,
      TxWr_i                       => TxWr_i,
      TxEmpty_o                    => TxEmpty_o,
      TxFull_o                     => TxFull_o,
      RxData_o                     => RxData_o,
      RxRd_i                       => RxRd_i,
      RxFull_o                     => RxFull_o,
      RxEmpty_o                    => RxEmpty_o,
      BitsSelect_i                 => BitsSelect_i,
      ParityOn_i                   => ParityOn_i,
      ParityEvenOdd_i              => ParityEvenOdd_i,
      SpeedDivider_i               => SpeedDivider_i,
      Clk_i                        => Clk_i,
      Reset_i_n                    => Reset_n_i,
      ErrorReset_i                 => ErrorReset_i,
      RxParityErrorIndicator_o     => RxParityErrorIndicator_o,
      RxStopBitErrorIndicator_o    => RxStopBitErrorIndicator_o,
      RxBufferFullErrorIndicator_o => RxBufferFullErrorIndicator_o,
      TxD_o                        => UartRxD_i,
      RxD_i                        => UartTxD_o,
      ScanEnable_i                 => ScanEnable_i,
      ScanClk_i                    => ScanClk_i,
      ScanDataIn_i                 => ScanDataIn_i,
      ScanDataOut_o                => ScanDataOut_o
    );

--  Dbg_UART_RxD_i <= '1';
  Dbg_SCL_i <= 'H';
  Dbg_SDA_b <= 'H';
  P1_b      <= (others => 'H');
  P2_b      <= (1 => INCLK_s, others => 'H');
  Inputs_i  <= (others => '0');
  -- Strange, chip-chip_top-a.vhd instantiates a BUDD16P (Open Drain Output
  -- Buffer). In c35_IOLIB_4M.v its "PAD" is defined as "output", and so is the
  -- I2CSCL_b port of the chip entity in chip-e.v and in routing/output/chip.v.
  -- The pad cell drives either '0' or 'Z', but the additional driver of 'H' on
  -- that wire (see below) does not change its value to 'H'. :-( With I2CSDA_b
  -- this works. Probably the ports should be defined as inout, but I don't
  -- want to modify the IOLIB.
  I2CSCL_b  <= 'H';
  I2CSDA_b  <= 'H';
--  OneWire_b <= 'H';
--  SPC_b     <= 'H';

  -- clock generation
  Clk_i <= not Clk_i after ClkPeriode/2.0;

  -- set RTC clock as TimerA INCLK, SmartFusion uses a divider of 3051 --> f = 100MHz/(3051+1) = 32765.4 Hz
  INCLK_s <= not INCLK_s after (1.0 sec/32765.4/2.0);

  -- read Uart_1 FIFO
  UartRx_Proc: process
  begin
    RxRd_i <= '0';
    wait for 0.2*ClkPeriode;
    while true loop
      RxRd_i <= not RxEmpty_o;
      if RxEmpty_o = '0' then
        -- store to variable, because as soon as RxRd_i is '1', RxData_o gives the next value from the FIFO
        UartRx <= RxData_o;
        UartCh <= character'val(to_integer(unsigned(RxData_o)));
        --report "Received '" & character'val(to_integer(unsigned(RxData_o))) & "'" severity note;
      end if;
      wait for ClkPeriode;
    end loop;
  end process UartRx_Proc;

  -- waveform generation
  WaveGen_Proc: process

    procedure PutChar (
      constant Ch : in character) is
    begin  -- PutChar
      TxWr_i <= '1';
      TxData_i <= std_logic_vector(to_unsigned(character'pos(Ch),8));
      wait for ClkPeriode;
      TxWr_i <= '0';
    end PutChar;

  begin
    wait for 5.2*ClkPeriode;
    Reset_n_i <= '1';

    Temp_s <= 23.7;                     -- degree C

    wait until UartCh = 'I';   -- 'I'nit complete
    wait for 100*ClkPeriode;

    -- configure SPI bus divider
--    PutChar('y');   -- set busdivider =  2   -->  4 Clk_i cycles for 1 SCK_o cycle
--    PutChar('x');   -- set busdivider =  4   -->  8 Clk_i cycles for 1 SCK_o cycle
--    PutChar('c');   -- set busdivider =  8   --> 16 Clk_i cycles for 1 SCK_o cycle
    PutChar('v');   -- set busdivider = 16   --> 32 Clk_i cycles for 1 SCK_o cycle
    wait for (1+8+1+1) * (2**2 * SpeedDivider) * ClkPeriode;
    wait for 100*ClkPeriode;

    -- configure measurement cycle time
--    PutChar('0');   -- set cycletime =   0
    PutChar('a');   -- set cycletime =   5ms
--    PutChar('s');   -- set cycletime =  10ms
--    PutChar('d');   -- set cycletime =  20ms
--    PutChar('f');   -- set cycletime =  33ms
--    PutChar('g');   -- set cycletime =  40ms
--    PutChar('h');   -- set cycletime =  50ms
--    PutChar('j');   -- set cycletime = 100ms
--    PutChar('k');   -- set cycletime = 200ms
--    PutChar('l');   -- set cycletime = 255
    wait for (1+8+1+1) * (2**2 * SpeedDivider) * ClkPeriode;
    wait for 100*ClkPeriode;
    
    -- configure threshold
    -- TODO

    -- configure main frequency (used to simulate ADT7310 measurement delay)
    -- the value is only important for the PIC16LF727

    -- done with setup, start measurement
    PutChar('C');  -- setup 'C'omplete
    wait until UartCh = 'R';   -- 'R'eady

--    wait for 5000*ClkPeriode;
--
--    -- The digital value is 128*Temp_s (plus/minus rounding to nearest
--    -- modulo 8). The threshold for too large changes is 30 (see
--    -- sensorfsm.vhd).
--    -- 23.7°C --> 3032
--    -- 25.7°C --> 3288  (delta: | 256| >  30)
--    -- 25.6°C --> 3280  (delta: |  -8| <  30)
--    -- 25.5°C --> 3264  (delta: | -24| <  30)
--    -- 25.4°C --> 3248  (delta: | -40| >= 30)
--
--    -- new sensor value with large difference -> notify required
--    wait for 3*ClkPeriode;              -- 3 cycle
--    Temp_s <= 25.7;
--
--    -- new sensor value with small difference -> no notification
--    wait for 3*ClkPeriode;              -- 3 cycle
--    Temp_s <= 25.6;
--
--    -- new sensor value with small difference -> no notification
--    wait for 3*ClkPeriode;              -- 3 cycle
--    Temp_s <= 25.5;
--
--    -- new sensor value with large difference -> notify required
--    wait for 3*ClkPeriode;              -- 3 cycle
--    Temp_s <= 25.4;

    wait for 50 ms;

    report "### Simulation Finished ###" severity failure;
  end process WaveGen_Proc;

end behavior;
