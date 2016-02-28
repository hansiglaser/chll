-------------------------------------------------------------------------------
-- Title      : Parameterization Interface
-- Project    : 
-------------------------------------------------------------------------------
-- File       : paramintf.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-10-24
-- Last update: 2013-10-24
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: Parameterization Interface of the Reconfigurable Module
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-10-24  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ParamIntf is
  
  generic (
    WrAddrWidth  : integer range 1 to 15 := 4;  -- max. 15 bits
    RdAddrWidth  : integer range 1 to 15 := 4;
    BaseAddr     : integer := 16#0188#
  );
  port (
    Reset_n_i      : in  std_logic;
    Clk_i          : in  std_logic;
    -- OpenMSP430 Interface
    PerAddr_i      : in  std_logic_vector(13 downto 0);  -- in reality this is 14 downto 1
    PerDIn_i       : in  std_logic_vector(15 downto 0);
    PerDOut_o      : out std_logic_vector(15 downto 0);
    PerWr_i        : in  std_logic_vector( 1 downto 0);  -- byte select
    PerEn_i        : in  std_logic;
    -- Param Out
    ParamWrAddr_o  : out std_logic_vector(WrAddrWidth-1 downto 0);
    ParamWrData_o  : out std_logic_vector(15 downto 0);
    ParamWr_o      : out std_logic;
    -- optional: Present_i : in  std_logic_vector(2**WrAddrWidth-1 downto 0);
    -- Param In
    ParamRdAddr_o  : out std_logic_vector(RdAddrWidth-1 downto 0);
    ParamRdData_i  : in  std_logic_vector(15 downto 0)
  );

end ParamIntf;

-------------------------------------------------------------------------------
-- Registers:
--
--   0x00 PCA Param Config and Address
--            15:   Auto-Increment (when '1')
--            14:0: Address  
--
--   0x02 PDR Param Data Register
--            15:0: Param Data
--
-------------------------------------------------------------------------------

architecture rtl of ParamIntf is
  -- addressing this module
  constant AddrWidth    : integer := 2;    -- number of bits considered for (full) address decoding
  constant MaskAddrRegs : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**AddrWidth-1,15));
  constant MaskAddrBase : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**15-1,15)) and (not MaskAddrRegs);
  constant BaseAddrVec  : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(BaseAddr,15)) and MaskAddrBase;
  -- addressing individual registers
  constant AddrPCA : integer := 0;  -- Param Config and Address
  constant AddrPDR : integer := 2;  -- Param Data Register
  -- ...
  -- ...
  signal AutoIncrement : std_logic;
  signal Addr          : unsigned(14 downto 0);
  signal PDR           : std_logic_vector(15 downto 0);
  signal DoIncrRd      : std_logic;

begin  -- rtl

  -- purpose: peripheral read via bus interface
  -- type   : combinational
  -- inputs : PerAddr_i,PerEn_i
  -- outputs: PerDOut_o
  PerRead: process (PerAddr_i,PerEn_i,PerWr_i,Addr,AutoIncrement,ParamRdData_i)
  begin  -- process PerRead
    DoIncrRd <= '0';
    PerDOut_o <= (others => '0');
    if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "00" then
      case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
        when AddrPCA => PerDOut_o <= AutoIncrement & std_logic_vector(Addr);
        when AddrPDR =>
          PerDOut_o <= ParamRdData_i;
          DoIncrRd  <= '1';
        when others => null;
      end case;
    end if;
  end process PerRead;

  -- purpose: Peripheral write, parameter read/write
  -- type   : sequential
  -- inputs : Clk_i, Reset_n_i
  -- outputs: 
  PerWrite: process (Clk_i, Reset_n_i)
    variable DoIncrWr : std_logic;
  begin  -- process PerWrite
    if Reset_n_i = '0' then             -- asynchronous reset (active low)
      AutoIncrement <= '0';
      Addr          <= (others => '0');
      PDR           <= (others => '0');
      ParamWrAddr_o <= (others => '0');
      ParamWrData_o <= (others => '0');
      ParamWr_o     <= '0';
      DoIncrWr      := '0';
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge
      ParamWr_o <= '0';
      DoIncrWr  := '0';

      -- Peripheral write via bus interface
      if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "11" then
        case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
          when AddrPCA =>
            AutoIncrement <= PerDIn_i(15);
            Addr          <= unsigned(PerDIn_i(14 downto 0));
          when AddrPDR =>
            ParamWrAddr_o <= std_logic_vector(Addr(WrAddrWidth-1 downto 0));
            ParamWrData_o <= PerDIn_i;
            ParamWr_o     <= '1';
            DoIncrWr      := '1';
          when others => null;
        end case;
      end if;

      -- auto-increment
      if (DoIncrRd = '1' or DoIncrWr = '1') and AutoIncrement = '1'then
        Addr <= Addr + 1;
      end if;

    end if;
  end process PerWrite;

  ParamRdAddr_o <= std_logic_vector(Addr(RdAddrWidth-1 downto 0));

end rtl;
