-------------------------------------------------------------------------------
-- Title      : Testbench for design "Core"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : core_uart_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-12-21
-- Last update: 2014-08-02
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

entity Core_Uart_tb is

end Core_Uart_tb;

-------------------------------------------------------------------------------

architecture behavior of Core_Uart_tb is

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

  -- component ports
  signal Reset_n_i         : std_logic := '0';
  signal Clk_i             : std_logic := '1';
  signal LFXT_Clk_i        : std_logic := '0';
  signal Cpu_En_i          : std_logic := '1';
  signal Dbg_En_i          : std_logic := '0';
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic := '0';
  signal Dbg_SDA_Out_o     : std_logic;
  signal Dbg_SDA_In_i      : std_logic := '0';
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

  constant ClkPeriod : time := 100 ns;  -- 10 MHz

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
  Clk_i <= not Clk_i after ClkPeriod/2.0;

  -- waveform generation
  WaveGen_Proc: process

    procedure PutChar (
      constant Ch : in character) is
    begin  -- PutChar
      TxWr_i <= '1';
      TxData_i <= std_logic_vector(to_unsigned(character'pos(Ch),8));
      wait for ClkPeriod;
      TxWr_i <= '0';
    end PutChar;

    -- can't use "wait" in functions, so we need a procedure here
    procedure GetChar (
      variable Ch : out character) is
    begin  -- GetChar
      wait until RxEmpty_o /= '1';
      wait for 0.3*ClkPeriod;
      Ch := character'val(to_integer(unsigned(RxData_o)));
      RxRd_i <= '1';
      wait for ClkPeriod;
      RxRd_i <= '0';
    end GetChar;

    variable Ch : character;

  begin
    wait for 5.2*ClkPeriod;
    Reset_n_i <= '1';

    -- give the CPU some time to get to the main loop
    wait for 500*ClkPeriod;

    -- receive "Hello World!\n"
    Ch := 'A';
    while Ch /= LF loop
      GetChar(Ch);
      report "Received '" & Ch & "'" severity note;
    end loop;

    wait for 1000*ClkPeriod;

    -- try echo of upcased letters
    PutChar('a');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;

    PutChar('b');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;

    PutChar('c');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('d');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('0');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('1');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('A');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('B');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    
    PutChar('#');
    GetChar(Ch);
    report "Received '" & Ch & "'" severity note;
    

    report "Sending 9 characters as fast as possible" severity note;
    TxWr_i <= '1';
    TxData_i <= std_logic_vector(to_unsigned(character'pos('z'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('y'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('x'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('w'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('9'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('8'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('Z'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('X'),8));
    wait for ClkPeriod;
    TxData_i <= std_logic_vector(to_unsigned(character'pos('%'),8));
    wait for ClkPeriod;
    TxWr_i <= '0';
    
    report "Waiting to receive echoed characters" severity note;
    wait for (9+3)*10*(1.0/115200)*1000 ms;
    report "Done waiting to receive echoed characters" severity note;
    -- since there is no flow control and the firmware is slower when
    -- transmitting (compared to receiving), a few characters will get lost.
    RxRd_i <= '1';
    while RxEmpty_o /= '1' loop
      report "loop: Received '" & character'val(to_integer(unsigned(RxData_o))) & "'" severity note;
      wait for ClkPeriod;
    end loop;


    report "### Simulation Finished ###" severity failure;
  end process WaveGen_Proc;

end behavior;
