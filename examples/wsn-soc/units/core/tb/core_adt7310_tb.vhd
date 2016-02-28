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

entity Core_ADT7310_tb is

end Core_ADT7310_tb;

-------------------------------------------------------------------------------

architecture behavior of Core_ADT7310_tb is

  component Core
    port (
      Reset_n_i         : in  std_logic;
      Clk_i             : in  std_logic;
      LFXT_Clk_i        : in  std_logic;
      Cpu_En_i          : in  std_logic;
      Dbg_En_i          : in  std_logic;
--      Dbg_UART_RxD_i    : in  std_logic;
--      Dbg_UART_TxD_o    : out std_logic;
      Dbg_SCL_i         : in  std_logic;
      Dbg_SDA_Out_o     : out std_logic;
      Dbg_SDA_In_i      : in  std_logic;
      P1_DOut_o         : out std_logic_vector(7 downto 0);
      P1_En_o           : out std_logic_vector(7 downto 0);
      P1_DIn_i          : in  std_logic_vector(7 downto 0);
      P2_DOut_o         : out std_logic_vector(7 downto 0);
      P2_En_o           : out std_logic_vector(7 downto 0);
      P2_DIn_i          : in  std_logic_vector(7 downto 0);
      UartRxD_i         : in  std_logic;
      UartTxD_o         : out std_logic;
      SCK_o             : out std_logic;
      MOSI_o            : out std_logic;
      MISO_i            : in  std_logic;
      Inputs_i          : in  std_logic_vector(7 downto 0);
      Outputs_o         : out std_logic_vector(7 downto 0);
      SPIMISO_i         : in  std_logic;
      SPIMOSI_o         : out std_logic;
      SPISCK_o          : out std_logic;
      I2CSCL_o          : out std_logic;
      I2CSDA_i          : in  std_logic;
      I2CSDA_o          : out std_logic;
--      OneWire_i         : in  std_logic;
--      OneWire_o         : out std_logic;
--      PWMInput_i        : in  std_logic;
--      SENTInput_i       : in  std_logic;
--      SPCInput_i        : in  std_logic;
--      SPCTrigger_o      : out std_logic;
      AdcConvComplete_i : in  std_logic;
      AdcDoConvert_o    : out std_logic;
      AdcValue_i        : in  std_logic_vector(9 downto 0));
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
  signal Reset_n_i         : std_logic := '0';
  signal Clk_i             : std_logic := '1';
  signal LFXT_Clk_i        : std_logic := '0';
  signal Cpu_En_i          : std_logic := '1';
  signal Dbg_En_i          : std_logic := '0';
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic := '1';
  signal Dbg_SDA_Out_o     : std_logic;
  signal Dbg_SDA_In_i      : std_logic := '1';
  signal P1_DOut_o         : std_logic_vector(7 downto 0);
  signal P1_En_o           : std_logic_vector(7 downto 0);
  signal P1_DIn_i          : std_logic_vector(7 downto 0) := (others => '0');
  signal P2_DOut_o         : std_logic_vector(7 downto 0);
  signal P2_En_o           : std_logic_vector(7 downto 0);
  signal P2_DIn_i          : std_logic_vector(7 downto 0) := (others => '0');
  signal UartRxD_i         : std_logic;
  signal UartTxD_o         : std_logic;
  signal SCK_o             : std_logic;
  signal MOSI_o            : std_logic;
  signal MISO_i            : std_logic := '0';
  signal Inputs_i          : std_logic_vector(7 downto 0) := (others => '0');
  signal Outputs_o         : std_logic_vector(7 downto 0);
  signal SPIMISO_i         : std_logic := '0';
  signal SPIMOSI_o         : std_logic;
  signal SPISCK_o          : std_logic;
  signal I2CSCL_o          : std_logic;
  signal I2CSDA_i          : std_logic := '0';
  signal I2CSDA_o          : std_logic;
--  signal OneWire_i         : std_logic := '0';
--  signal OneWire_o         : std_logic;
--  signal PWMInput_i        : std_logic := '0';
--  signal SENTInput_i       : std_logic := '0';
--  signal SPCInput_i        : std_logic := '0';
--  signal SPCTrigger_o      : std_logic;
  signal AdcConvComplete_i : std_logic := '0';
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0) := (others => '0');

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
  -- 10MHz/(2**2 * 115200) = 21.70139 rounded --> 22
  -- baudrate = clock_frequency / (2**Oversampling * SpeedDivider) = 10MHz / (2**2 * 22) = 113636.4
  -- error: (113636.4-115200)/115200 = -0.0136
  constant SpeedDivider : integer := 22;

  signal TxData_i                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0) := (others => '0');
  signal TxWr_i                       : STD_LOGIC := '0';
  signal TxEmpty_o                    : STD_LOGIC;
  signal TxFull_o                     : STD_LOGIC;
  signal RxData_o                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
  signal RxRd_i                       : STD_LOGIC := '0';
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

  constant ClkPeriode : time := 100 ns;  -- 10 MHz

begin  -- behavior

  -- component instantiation
  DUT: Core
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      LFXT_Clk_i        => LFXT_Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => Dbg_SCL_i,
      Dbg_SDA_Out_o     => Dbg_SDA_Out_o,
      Dbg_SDA_In_i      => Dbg_SDA_In_i,
      P1_DOut_o         => P1_DOut_o,
      P1_En_o           => P1_En_o,
      P1_DIn_i          => P1_DIn_i,
      P2_DOut_o         => P2_DOut_o,
      P2_En_o           => P2_En_o,
      P2_DIn_i          => P2_DIn_i,
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
      I2CSCL_o          => I2CSCL_o,
      I2CSDA_i          => I2CSDA_i,
      I2CSDA_o          => I2CSDA_o,
--      OneWire_i         => OneWire_i,
--      OneWire_o         => OneWire_o,
--      PWMInput_i        => PWMInput_i,
--      SENTInput_i       => SENTInput_i,
--      SPCInput_i        => SPCInput_i,
--      SPCTrigger_o      => SPCTrigger_o,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

  ADT7310CS_n_o <= P1_DOut_o(0);

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

  -- clock generation
  Clk_i <= not Clk_i after ClkPeriode/2.0;

  -- read Uart_1 FIFO
  UartRx_Proc: process
    variable Ch : std_logic_vector(7 downto 0) := (others => '0');
  begin
    wait for 0.2*ClkPeriode;
    while true loop
      RxRd_i <= not RxEmpty_o;
      if RxEmpty_o = '0' then
        -- store to variable, because as soon as RxRd_i is '1', RxData_o gives the next value from the FIFO
        Ch := RxData_o;
        --report "Received '" & character'val(to_integer(unsigned(RxData_o))) & "'" severity note;
      end if;
      wait for ClkPeriode;
    end loop;
  end process UartRx_Proc;

  -- waveform generation
  WaveGen_Proc: process
  begin
    wait for 5.2*ClkPeriode;
    Reset_n_i <= '1';

    Temp_s <= 23.7;                     -- degree C

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

    wait for 5 sec;

    report "### Simulation Finished ###" severity failure;
  end process WaveGen_Proc;

end behavior;
