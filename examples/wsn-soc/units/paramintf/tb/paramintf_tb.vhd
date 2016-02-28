-------------------------------------------------------------------------------
-- Title      : Testbench for design "ParamIntf"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ParamIntf_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-10-24
-- Last update: 2013-11-11
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
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

-------------------------------------------------------------------------------

entity ParamIntf_tb is

end ParamIntf_tb;

-------------------------------------------------------------------------------

architecture behavior of ParamIntf_tb is

  component ParamIntf
    generic (
      WrAddrWidth : integer range 1 to 15;
      RdAddrWidth : integer range 1 to 15;
      BaseAddr    : integer);
    port (
      Reset_n_i     : in  std_logic;
      Clk_i         : in  std_logic;
      PerAddr_i     : in  std_logic_vector(13 downto 0);
      PerDIn_i      : in  std_logic_vector(15 downto 0);
      PerDOut_o     : out std_logic_vector(15 downto 0);
      PerWr_i       : in  std_logic_vector(1 downto 0);
      PerEn_i       : in  std_logic;
      ParamWrAddr_o : out std_logic_vector(WrAddrWidth-1 downto 0);
      ParamWrData_o : out std_logic_vector(15 downto 0);
      ParamWr_o     : out std_logic;
      ParamRdAddr_o : out std_logic_vector(RdAddrWidth-1 downto 0);
      ParamRdData_i : in  std_logic_vector(15 downto 0));
  end component;

  component ParamOutReg
    generic (
      Width : integer);
    port (
      Reset_n_i     : in  std_logic;
      Clk_i         : in  std_logic;
      Enable_i      : in  std_logic;
      ParamWrData_i : in  std_logic_vector(Width-1 downto 0);
      Param_o       : out std_logic_vector(Width-1 downto 0));
  end component;

  -- component generics
  constant WrAddrWidth : integer range 1 to 15 := 4;
  constant RdAddrWidth : integer range 1 to 15 := 4;
  constant BaseAddr    : integer               := 16#0188#;

  -- component ports
  signal Reset_n_i     : std_logic := '0';
  signal Clk_i         : std_logic := '1';
  signal PerAddr_i     : std_logic_vector(13 downto 0);
  signal PerDIn_i      : std_logic_vector(15 downto 0);
  signal PerDOut_o     : std_logic_vector(15 downto 0);
  signal PerWr_i       : std_logic_vector(1 downto 0);
  signal PerEn_i       : std_logic;
  signal ParamWrAddr_o : std_logic_vector(WrAddrWidth-1 downto 0);
  signal ParamWrData_o : std_logic_vector(15 downto 0);
  signal ParamWr_o     : std_logic;
  signal ParamRdAddr_o : std_logic_vector(RdAddrWidth-1 downto 0);
  signal ParamRdData_i : std_logic_vector(15 downto 0);

  -- clock
  constant ClkPeriode : time := 10 ns;

  signal ParamRdData : std_logic_vector(15 downto 0);
  type Params_t is array(0 to 2**WrAddrWidth-1) of std_logic_vector(15 downto 0);
  signal Params : Params_t;
  signal ParamEnable_s : std_logic_vector(2**WrAddrWidth-1 downto 0);

begin  -- behavior

  -- component instantiation
  DUT: ParamIntf
    generic map (
      WrAddrWidth => WrAddrWidth,
      RdAddrWidth => RdAddrWidth,
      BaseAddr    => BaseAddr)
    port map (
      Reset_n_i     => Reset_n_i,
      Clk_i         => Clk_i,
      PerAddr_i     => PerAddr_i,
      PerDIn_i      => PerDIn_i,
      PerDOut_o     => PerDOut_o,
      PerWr_i       => PerWr_i,
      PerEn_i       => PerEn_i,
      ParamWrAddr_o => ParamWrAddr_o,
      ParamWrData_o => ParamWrData_o,
      ParamWr_o     => ParamWr_o,
      ParamRdAddr_o => ParamRdAddr_o,
      ParamRdData_i => ParamRdData_i);

  ParamOutRegs: for i in 0 to 2**WrAddrWidth-1 generate

    ParamEnable_s(i) <= ParamWr_o when ParamWrAddr_o = std_logic_vector(to_unsigned(i,WrAddrWidth)) else
                       '0';

    ParamOutReg_1: ParamOutReg
      generic map (
        Width => 16
      )
      port map (
        Reset_n_i     => Reset_n_i,
        Clk_i         => Clk_i,
        Enable_i      => ParamEnable_s(i),
        ParamWrData_i => ParamWrData_o,
        Param_o       => Params(i)
      );
    
  end generate ParamOutRegs;  -- i
 
  -- clock generation
  Clk_i <= not Clk_i after ClkPeriode/2.0;

  -- waveform generation
  WaveGen_Proc: process
    variable Result : std_logic_vector(15 downto 0);

    procedure ClkCycle (
      constant Count : in integer) is
    begin  -- ClkCycle
      for i in 0 to Count-1 loop
        wait until rising_edge(Clk_i);
        wait for 0.2*ClkPeriode;
      end loop;  -- i
    end ClkCycle;

    procedure WriteWord (
      constant Addr  : in integer;
      constant Value : in std_logic_vector) is
    begin  -- WriteWord
      assert Addr mod 2 = 0 report "Only word-aligned access possible" severity failure;
      PerAddr_i <= std_logic_vector(to_unsigned(Addr, 15)(14 downto 1));
      PerDIn_i  <= Value;
      PerWr_i   <= "11";
      PerEn_i   <= '1';
      ClkCycle(1);
      PerWr_i   <= "00";
      PerEn_i   <= '0';
    end WriteWord;

    procedure WriteWord (
      constant Addr  : in integer;
      constant Value : in integer) is
    begin  -- WriteWord
      WriteWord(Addr,std_logic_vector(to_unsigned(Value,16)));
    end WriteWord;

    procedure ReadWord (
      constant Addr : integer) is
    begin  -- ReadWord
      assert Addr mod 2 = 0 report "Only word-aligned access possible" severity failure;
      PerAddr_i <= std_logic_vector(to_unsigned(Addr, 15)(14 downto 1));
      PerEn_i   <= '1';
      wait for 0.1*ClkPeriode;          -- give simulator time to update signals
      Result    := PerDOut_o;
      ClkCycle(1);
      PerEn_i   <= '0';
    end ReadWord;

    procedure CheckWord (
      constant Addr  : in integer;
      constant Value : in std_logic_vector) is
    begin  -- CheckWord
      assert Addr mod 2 = 0 report "Only word-aligned access possible" severity failure;
      PerAddr_i <= std_logic_vector(to_unsigned(Addr, 15)(14 downto 1));
      PerEn_i   <= '1';
      wait for 0.1*ClkPeriode;          -- give simulator time to update signals
      assert PerDOut_o = Value
        report "Read resulted in wrong value" severity error;
      Result    := PerDOut_o;
      ClkCycle(1);
      PerEn_i   <= '0';
    end CheckWord;

    procedure CheckWord (
      constant Addr  : in integer;
      constant Value : in integer) is
    begin  -- CheckWord
      CheckWord(Addr,std_logic_vector(to_unsigned(Value,16)));
    end CheckWord;

  begin
    Reset_n_i <= '0';
    wait for 5.2*ClkPeriode;
    Reset_n_i <= '1';

    ---------------------------------------------------------------------------
    -- silly write cycles
    WriteWord(BaseAddr+0,16#5555#);
    ClkCycle(3);
    WriteWord(BaseAddr+0,16#AAAA#);
    ClkCycle(3);
    WriteWord(BaseAddr+2,16#5555#);
    ClkCycle(3);
    WriteWord(BaseAddr+2,16#AAAA#);
    ClkCycle(3);
    -- read back PCA
    CheckWord(BaseAddr+0,16#AAAC#);     -- auto-increment for two write-accesses above
    -- write parameters
    WriteWord(BaseAddr+0,16#0000#);
    WriteWord(BaseAddr+2,16#AFFE#);
    ClkCycle(1);
    CheckWord(BaseAddr+2,16#AFFE#);
    WriteWord(BaseAddr+0,16#0001#);
    WriteWord(BaseAddr+2,16#BEEF#);
    ClkCycle(1);
    CheckWord(BaseAddr+2,16#BEEF#);
    WriteWord(BaseAddr+0,16#0000#);
    CheckWord(BaseAddr+2,16#AFFE#);
    WriteWord(BaseAddr+0,16#0001#);
    CheckWord(BaseAddr+2,16#BEEF#);
    -- write full parameter list
    for i in 0 to 2**WrAddrWidth-1 loop
      WriteWord(BaseAddr+0,i);
      WriteWord(BaseAddr+2,16#AFFE#-(i*23));
    end loop;  -- i in 0 to 2**WrAddrWidth-1
    -- read full parameter list
    for i in 0 to 2**RdAddrWidth-1 loop
      WriteWord(BaseAddr+0,i);
      CheckWord(BaseAddr+2,16#AFFE#-(i*23));
    end loop;  -- i in 0 to 2**RdAddrWidth-1
    -- write full parameter list with auto-increment
    WriteWord(BaseAddr+0,16#8000#);
    for i in 0 to 2**WrAddrWidth-1 loop
      WriteWord(BaseAddr+2,16#BEEF#-(i*43));
    end loop;  -- i in 0 to 2**WrAddrWidth-1
    -- read full parameter list with auto-increment
    WriteWord(BaseAddr+0,16#8000#);
    for i in 0 to 2**RdAddrWidth-1 loop
      CheckWord(BaseAddr+2,16#BEEF#-(i*43));
    end loop;  -- i in 0 to 2**RdAddrWidth-1

    ---------------------------------------------------------------------------

    ClkCycle(3);

    report "### Simulation Finished ###" severity failure;
    wait;
  end process WaveGen_Proc;

  ParamRdData <= Params(to_integer(unsigned(ParamRdAddr_o)));
  ParamRdData_i <= ParamRdData after 0.2*ClkPeriode;

end behavior;

-------------------------------------------------------------------------------

configuration ParamIntf_tb_behavior_cfg of ParamIntf_tb is
  for behavior
  end for;
end ParamIntf_tb_behavior_cfg;

-------------------------------------------------------------------------------
