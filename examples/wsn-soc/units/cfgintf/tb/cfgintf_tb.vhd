-------------------------------------------------------------------------------
-- Title      : Testbench for design "CfgIntf"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : CfgIntf_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-08-22
-- Last update: 2013-12-30
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
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

-------------------------------------------------------------------------------

entity CfgIntf_tb is

end CfgIntf_tb;

-------------------------------------------------------------------------------

architecture behavior of CfgIntf_tb is

  component CfgIntf
    generic (
      NumCfgs      : integer := 3;
      BaseAddr     : integer := 16#0180#
    );
    port (
      Reset_n_i    : in  std_logic;
      Clk_i        : in  std_logic;
      PerAddr_i    : in  std_logic_vector(13 downto 0);
      PerDIn_i     : in  std_logic_vector(15 downto 0);
      PerDOut_o    : out std_logic_vector(15 downto 0);
      PerWr_i      : in  std_logic_vector(1 downto 0);
      PerEn_i      : in  std_logic;
      CfgMode_o    : out std_logic;
      CfgShift_o   : out std_logic_vector(NumCfgs-1 downto 0);
      CfgDataOut_o : out std_logic;
      CfgDataIn_i  : in  std_logic_vector(NumCfgs-1 downto 0)
    );
  end component;

  -- component generics
  constant NumCfgs    : integer := 3;
  constant BaseAddr   : integer := 16#0180#;

  -- component ports
  signal Reset_n_i    : std_logic := '0';
  signal Clk_i        : std_logic := '1';
  signal PerAddr_i    : std_logic_vector(13 downto 0) := (others => '0');
  signal PerDIn_i     : std_logic_vector(15 downto 0) := (others => '0');
  signal PerDOut_o    : std_logic_vector(15 downto 0) := (others => '0');
  signal PerWr_i      : std_logic_vector(1 downto 0) := (others => '0');
  signal PerEn_i      : std_logic := '0';
  signal CfgMode_o    : std_logic;
  signal CfgShift_o   : std_logic_vector(NumCfgs-1 downto 0);
  signal CfgDataOut_o : std_logic;
  signal CfgDataIn_i  : std_logic_vector(NumCfgs-1 downto 0) := (others => '0');

  -- clock
  constant ClkPeriode : time := 10 ns;

  -- simulated config register
  signal ConfigRegister : std_logic_vector(4*16-1 downto 0);  -- be long enough for all test cases

begin  -- behavior

  -- component instantiation
  DUT: CfgIntf
    generic map (
      NumCfgs  => NumCfgs,
      BaseAddr => BaseAddr
    )
    port map (
      Reset_n_i    => Reset_n_i,
      Clk_i        => Clk_i,
      PerAddr_i    => PerAddr_i,
      PerDIn_i     => PerDIn_i,
      PerDOut_o    => PerDOut_o,
      PerWr_i      => PerWr_i,
      PerEn_i      => PerEn_i,
      CfgMode_o    => CfgMode_o,
      CfgShift_o   => CfgShift_o,
      CfgDataOut_o => CfgDataOut_o,
      CfgDataIn_i  => CfgDataIn_i
    );

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

    procedure CheckBitstream (
      constant Bitstream : in std_logic_vector
    ) is
      variable Length : integer;
      variable BitstreamDownto : std_logic_vector(Bitstream'length-1 downto 0);
      variable Chunk : std_logic_vector(15 downto 0);
      variable ChunkLength : integer;
      variable Pos : integer;
      variable First : boolean;
    begin  -- CheckBitstream
      Length := Bitstream'length;

      -- we need a "downto" vector, but immedate constants are "to", so create
      -- a copy
      if Bitstream'ascending then
        -- "to" vector --> revert
        for i in 0 to Length-1 loop  -- from left to right
          BitstreamDownto(Length-i-1) := Bitstream(i);
        end loop;  -- i
      else
        -- "downto" vector --> just copy
        BitstreamDownto := Bitstream;
      end if;

      Pos := 0;

      -- set CfgMode = '0', select chain number 2
      WriteWord(BaseAddr+0,16#4002#);
      ClkCycle(3);
      -- set total length
      WriteWord(BaseAddr+2,Length);
      ClkCycle(3);

      First := true;
      while Pos < Length loop
        ChunkLength := work.Utils.min(Length-Pos,16);
        Chunk := (others => '0');
        Chunk(ChunkLength-1 downto 0) := BitstreamDownto(Pos+ChunkLength-1 downto Pos);
        Pos := Pos + ChunkLength;

        -- write data word
        WriteWord(BaseAddr+4,Chunk);
        -- first edge after write
        if First then
          -- of very first chunk
          assert CfgMode_o    = '1'   report "First edge after first write data, CfgMode_o should still be active" severity error;
        else
          -- of following chunks
          assert CfgMode_o    = '1'   report "First edge after write data, CfgMode_o should still be active" severity error;
        end if;
        assert CfgShift_o   = "000" report "First edge after write data, CfgShift_o should still be inactive" severity error;
        assert CfgDataOut_o = '0'   report "First edge after write data, CfgDataOut_o should still be inactive" severity error;
        ClkCycle(1);
        -- second edge, outputs of CfgIntf are high now, but not yet propagated to
        -- the ConfigRegisters
        -- now look at all Clk_i edges
        for i in 1 to ChunkLength loop
          assert CfgMode_o    = '1'   report "During shifting, CfgMode_o should be active" severity error;
          assert CfgShift_o   = "100" report "During shifting, CfgShift_o should be active" severity error;
          assert CfgDataOut_o = Chunk(i-1) report "During shifting, CfgDataOut_o should be active" severity error;
          ClkCycle(1);
        end loop;  -- i
        if Pos < Length then
          -- during breaks, only CfgMode_o must stay active
          assert CfgMode_o    = '1'   report "After shifting, CfgMode_o should be active" severity error;
        else
          -- done shifting, signals should be deactivated
          assert CfgMode_o    = '1'   report "After shifting, CfgMode_o should be active" severity error;
        end if;
        assert CfgShift_o   = "000" report "After shifting, CfgShift_o should be inactive" severity error;
        assert CfgDataOut_o = '0'   report "After shifting, CfgDataOut_o should be inactive" severity error;
        -- check that CBS is 0
        CheckWord(BaseAddr+2,Length-Pos);
        
        First := false;
      end loop;

    end CheckBitstream;

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
    -- read back
    CheckWord(BaseAddr+0,'0' & '0' & "000000000000" & "10");
    CheckWord(BaseAddr+2,16#AAAA#);

    ---------------------------------------------------------------------------
    -- test CfgMode
    WriteWord(BaseAddr+0,16#0002#);       -- set CfgMode = '0', select chain number 2
    assert CfgMode_o    = '0'   report "After setting CfgMode='0', CfgMode_o should be inactive" severity error;
    WriteWord(BaseAddr+0,16#4002#);       -- set CfgMode = '1', select chain number 2
    assert CfgMode_o    = '1'   report "After setting CfgMode='1', CfgMode_o should be active" severity error;
    WriteWord(BaseAddr+0,16#0002#);       -- set CfgMode = '0', select chain number 2
    assert CfgMode_o    = '0'   report "After setting CfgMode='0', CfgMode_o should be inactive" severity error;

    ---------------------------------------------------------------------------
    -- test short bitstream (13 bits) with all '1's
    WriteWord(BaseAddr+0,16#4002#);       -- set CfgMode = '1', select chain number 2
    ClkCycle(3);
    WriteWord(BaseAddr+2,13);
    ClkCycle(3);
    WriteWord(BaseAddr+4,"000" & "1111111111111");
    -- first edge after write
    assert CfgMode_o    = '1'   report "First edge after write data, CfgMode_o should still be active" severity error;
    assert CfgShift_o   = "000" report "First edge after write data, CfgShift_o should still be inactive" severity error;
    assert CfgDataOut_o = '0'   report "First edge after write data, CfgDataOut_o should still be inactive" severity error;
    ClkCycle(1);
    -- second edge, outputs of CfgIntf are high now, but not yet propagated to
    -- the ConfigRegisters
    -- now look at 13 Clk_i edges
    for i in 1 to 13 loop
      assert CfgMode_o    = '1'   report "During shifting, CfgMode_o should be active" severity error;
      assert CfgShift_o   = "100" report "During shifting, CfgShift_o should be active" severity error;
      assert CfgDataOut_o = '1'   report "During shifting, CfgDataOut_o should be active" severity error;
      ClkCycle(1);
    end loop;  -- i
    -- done shifting, signals should be deactivated
    assert CfgMode_o    = '1'   report "After shifting, CfgMode_o should still be active" severity error;
    assert CfgShift_o   = "000" report "After shifting, CfgShift_o should be inactive" severity error;
    assert CfgDataOut_o = '0'   report "After shifting, CfgDataOut_o should be inactive" severity error;
    -- check that CBS is 0
    CheckWord(BaseAddr+2,0);

    ---------------------------------------------------------------------------
    -- test short bitstream (13 bits) with pseudo-random data
    Result := "000" & "1010110011110";  -- misuse this variable
    WriteWord(BaseAddr+0,16#4002#);       -- set CfgMode = '1', select chain number 2
    ClkCycle(3);
    WriteWord(BaseAddr+2,13);
    ClkCycle(3);
    WriteWord(BaseAddr+4,Result);
    -- first edge after write
    assert CfgMode_o    = '1'   report "First edge after write data, CfgMode_o should still be active" severity error;
    assert CfgShift_o   = "000" report "First edge after write data, CfgShift_o should still be inactive" severity error;
    assert CfgDataOut_o = '0'   report "First edge after write data, CfgDataOut_o should still be inactive" severity error;
    ClkCycle(1);
    -- second edge, outputs of CfgIntf are high now, but not yet propagated to
    -- the ConfigRegisters
    -- now look at 13 Clk_i edges
    for i in 1 to 13 loop
      assert CfgMode_o    = '1'   report "During shifting, CfgMode_o should be active" severity error;
      assert CfgShift_o   = "100" report "During shifting, CfgShift_o should be active" severity error;
      assert CfgDataOut_o = Result(i-1) report "During shifting, CfgDataOut_o should be active" severity error;
      ClkCycle(1);
    end loop;  -- i
    -- done shifting, signals should be deactivated
    assert CfgMode_o    = '1'   report "After shifting, CfgMode_o should still be active" severity error;
    assert CfgShift_o   = "000" report "After shifting, CfgShift_o should be inactive" severity error;
    assert CfgDataOut_o = '0'   report "After shifting, CfgDataOut_o should be inactive" severity error;
    -- check that CBS is 0
    CheckWord(BaseAddr+2,0);

    ---------------------------------------------------------------------------
    -- test long bitstream (3*16+13 bits) with pseudo-random data
    CheckBitstream(B"1110010111100_0010101100101010_0001000101011000_0111000111101000");

    ---------------------------------------------------------------------------

    WriteWord(BaseAddr+0,16#0000#);       -- set CfgMode = '0', select chain number 0
    ClkCycle(3);

    report "### Simulation Finished ###" severity failure;
    wait;
  end process WaveGen_Proc;

  -- emulate an actual configuration register where the configuration data is
  -- shifted in
  ConfigReg: process (Clk_i, Reset_n_i)
  begin  -- process ConfigReg
    if Reset_n_i = '0' then             -- asynchronous reset (active low)
      ConfigRegister <= (others => '0');
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge
      if CfgMode_o = '1' and CfgShift_o(2) = '1' then
        ConfigRegister <= CfgDataOut_o & ConfigRegister(ConfigRegister'left downto 1);
      end if;
    end if;
  end process ConfigReg;

end behavior;

-------------------------------------------------------------------------------

configuration CfgIntf_tb_behavior_cfg of CfgIntf_tb is
  for behavior
  end for;
end CfgIntf_tb_behavior_cfg;

-------------------------------------------------------------------------------
