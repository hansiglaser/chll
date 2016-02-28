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

-------------------------------------------------------------------------------

entity Core_tb is

end Core_tb;

-------------------------------------------------------------------------------

architecture behavior of Core_tb is

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
  signal Dbg_SCL_i         : std_logic := '1';
  signal Dbg_SDA_Out_o     : std_logic;
  signal Dbg_SDA_In_i      : std_logic := '1';
  signal P1_DOut_o         : std_logic_vector(7 downto 0);
  signal P1_En_o           : std_logic_vector(7 downto 0);
  signal P1_DIn_i          : std_logic_vector(7 downto 0) := (others => '0');
  signal P2_DOut_o         : std_logic_vector(7 downto 0);
  signal P2_En_o           : std_logic_vector(7 downto 0);
  signal P2_DIn_i          : std_logic_vector(7 downto 0) := (others => '0');
  signal UartRxD_i         : std_logic := '0';
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

  -- clock generation
  Clk_i <= not Clk_i after ClkPeriod/2.0;

  -- waveform generation
  WaveGen_Proc: process
  begin
    wait for 5.2*ClkPeriod;
    Reset_n_i <= '1';

    wait for 5000*ClkPeriod;

    report "### Simulation Finished ###" severity failure;
  end process WaveGen_Proc;

end behavior;
