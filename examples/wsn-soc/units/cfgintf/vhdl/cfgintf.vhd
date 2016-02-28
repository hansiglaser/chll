-------------------------------------------------------------------------------
-- Title      : Config Interface
-- Project    : 
-------------------------------------------------------------------------------
-- File       : cfgintf.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-08-22
-- Last update: 2014-08-29
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: Configuration Interface of the Reconfigurable Module
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-08-22  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.Utils.all;
use work.Config.all;

entity CfgIntf is
  generic (
    NumCfgs      : integer := 3;
    BaseAddr     : integer := 16#0180#
  );
  port (
    Reset_n_i    : in  std_logic;
    Clk_i        : in  std_logic;
    -- OpenMSP430 Interface
    PerAddr_i    : in  std_logic_vector(13 downto 0);  -- in reality this is 14 downto 1
    PerDIn_i     : in  std_logic_vector(15 downto 0);
    PerDOut_o    : out std_logic_vector(15 downto 0);
    PerWr_i      : in  std_logic_vector( 1 downto 0);  -- byte select
    PerEn_i      : in  std_logic;
    -- Config Interface
    CfgClk_o     : out std_logic_vector(NumCfgs-1 downto 0);  -- shifting clock for current cfg. chain
    CfgMode_o    : out std_logic;                             -- select config mode for all config registers
    CfgShift_o   : out std_logic_vector(NumCfgs-1 downto 0);  -- enable shifting for current cfg. chain
    CfgDataOut_o : out std_logic;                             -- bitstream to chain
    CfgDataIn_i  : in  std_logic_vector(NumCfgs-1 downto 0)   -- return data
  );
end CfgIntf;

-------------------------------------------------------------------------------
-- Registers:
--
--   0x00 CSR Config and Status Register
--            15: Busy Bit
--            14: CfgMode
--            ... reserved
--            ChainNumWidth-1:0: Destination
--
--   0x02 CBS Config Bitstream Size
--            15:0: Bitstream Size (2^16-1 = 65,535 possible)
--
--   0x04 CDR Config Data Register
--            15:0: Bitstream Data (Shifted OUT to chain LSB first)
--
-------------------------------------------------------------------------------

architecture rtl of CfgIntf is
  -- addressing this module
  constant AddrWidth    : integer := 3;    -- number of bits considered for (full) address decoding
  constant MaskAddrRegs : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**AddrWidth-1,15));
  constant MaskAddrBase : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**15-1,15)) and (not MaskAddrRegs);
  constant BaseAddrVec  : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(BaseAddr,15)) and MaskAddrBase;
  -- addressing individual registers
  constant AddrCSR : integer := 0;  -- Config and Status Register
  constant AddrCBS : integer := 2;  -- Config Bitstream Size
  constant AddrCDR : integer := 4;  -- Config Data Register
  -- ...
  constant ChainNumWidth : integer := WidthFromMax(NumCfgs);
  -- ...
  signal Busy          : std_logic;
  signal CfgMode       : std_logic;
  signal ChainNum      : unsigned(ChainNumWidth-1 downto 0);
  signal BitstreamSize : unsigned(15 downto 0);
  signal Bitstream     : std_logic_vector(15 downto 0);
  signal ChunkCounter  : unsigned(4 downto 0);

  signal CfgShift      : std_logic_vector(NumCfgs-1 downto 0);
  signal CfgClk        : std_logic_vector(NumCfgs-1 downto 0);

  -- only used when work.Config.CfgClkGating is true
  component omsp_clock_gate
    port (
      gclk        : out std_logic;
      clk         : in  std_logic;
      enable      : in  std_logic;
      scan_enable : in  std_logic
    );
  end component;

begin  -- rtl

  -- purpose: peripheral read via bus interface
  -- type   : combinational
  -- inputs : PerAddr_i,PerEn_i
  -- outputs: PerDOut_o
  PerRead: process (PerAddr_i,PerEn_i,PerWr_i,Busy,CfgMode,ChainNum,BitstreamSize,Bitstream)
  begin  -- process PerRead
    PerDOut_o <= (others => '0');
    if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "00" then
      case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
        when AddrCSR => PerDOut_o <= Busy & CfgMode & FillVec(16-1-1-ChainNumWidth,'0') & std_logic_vector(ChainNum);
        when AddrCBS => PerDOut_o <= std_logic_vector(BitstreamSize);
        when AddrCDR => PerDOut_o <= Bitstream;
        when others => null;
      end case;
    end if;
  end process PerRead;

  -- purpose: Peripheral write, shift config
  -- type   : sequential
  -- inputs : Clk_i, Reset_n_i
  -- outputs: 
  PerWrite: process (Clk_i, Reset_n_i)
  begin  -- process PerWrite
    if Reset_n_i = '0' then             -- asynchronous reset (active low)
      ChainNum      <= (others => '0');
      BitstreamSize <= (others => '0');
      Bitstream     <= (others => '0');
      ChunkCounter  <= (others => '0');
      Busy          <= '0';
      CfgMode       <= '0';
      CfgShift      <= (others => '0');
      CfgDataOut_o  <= '0';
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge

      -- Peripheral write via bus interface
      if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "11" and Busy = '0' then
        case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
          when AddrCSR =>
            -- 15: don't write Busy
            -- 14: CfgMode
            CfgMode  <= PerDIn_i(14);
            -- ... reserved
            -- x:0: Chain Number
            ChainNum <= unsigned(PerDIn_i(ChainNumWidth-1 downto 0));
          when AddrCBS =>
            BitstreamSize <= unsigned(PerDIn_i);
          when AddrCDR =>
            Bitstream <= PerDIn_i;
            if BitstreamSize > 16 then
              ChunkCounter  <= to_unsigned(16,ChunkCounter'length);
              BitstreamSize <= BitstreamSize - 16;
            else
              ChunkCounter  <= BitstreamSize(ChunkCounter'range);
              BitstreamSize <= (others => '0');
            end if;
          when others => null;
        end case;
      end if;

      -- Shift config data to the config chain
      if ChunkCounter > 0 then
        Busy         <= '1';
        CfgDataOut_o <= Bitstream(0);
        Bitstream    <= CfgDataIn_i(to_integer(ChainNum)) & Bitstream(Bitstream'left downto 1);
        ChunkCounter <= ChunkCounter - 1;
        CfgShift(to_integer(ChainNum)) <= '1';
      else
        -- done with this chunk
        Busy         <= '0';
        CfgDataOut_o <= '0';            -- just clean up
        CfgShift     <= (others => '0');
        if BitstreamSize = 0 then 
          -- done with configuration, nothing special to do here because the
          -- user will deactivate CfgMode on his own
        end if;
      end if;

    end if;
  end process PerWrite;

  CfgMode_o <= CfgMode;

  CfgClk <= (others => Clk_i);
  CfgClkGatingOff: if not CfgClkGating generate
    -- no clock gating: just distribute the clock signal
    CfgClk_o <= CfgClk;
  end generate CfgClkGatingOff;
  CfgClkGatingOn: if CfgClkGating generate
    -- clock gating enabled: activate clock only when desired
--    CfgClk_o <= CfgClk and CfgShift;   -- CfgShift directly comes from a D-FF output
    CfgClkGate: for i in CfgClk_o'range generate
      Gate_1: omsp_clock_gate
        port map (
          clk         => CfgClk(i),
          enable      => CfgShift(i),
          scan_enable => '0',
          gclk        => CfgClk_o(i)
        );
    end generate CfgClkGate;
  end generate CfgClkGatingOn;

  CfgShift_o <= CfgShift;

end rtl;
