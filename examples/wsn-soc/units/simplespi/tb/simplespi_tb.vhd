-------------------------------------------------------------------------------
-- Title      : Testbench for design "SimpleSPI"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : simplespi_tb.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2014-08-25
-- Last update: 2014-08-25
-- Platform   : 
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2014-08-25  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------

entity SimpleSPI_tb is

end SimpleSPI_tb;

-------------------------------------------------------------------------------

architecture behavior of SimpleSPI_tb is

  component SimpleSPI
    generic (
      BaseAddr : integer
    );
    port (
      Reset_n_i : in  std_logic;
      Clk_i     : in  std_logic;
      PerAddr_i : in  std_logic_vector(13 downto 0);
      PerDIn_i  : in  std_logic_vector(15 downto 0);
      PerDOut_o : out std_logic_vector(15 downto 0);
      PerWr_i   : in  std_logic_vector(1 downto 0);
      PerEn_i   : in  std_logic;
      Intr_o    : out std_logic;
      SCK_o     : out std_logic;
      MOSI_o    : out std_logic;
      MISO_i    : in  std_logic
    );
  end component;

  -- component generics
  constant BaseAddr    : integer := 16#0188#;

  -- component ports
  signal Reset_n_i : std_logic := '0';
  signal Clk_i     : std_logic := '1';
  signal PerAddr_i : std_logic_vector(13 downto 0);
  signal PerDIn_i  : std_logic_vector(15 downto 0);
  signal PerDOut_o : std_logic_vector(15 downto 0);
  signal PerWr_i   : std_logic_vector(1 downto 0);
  signal PerEn_i   : std_logic;
  signal Intr_o    : std_logic;
  signal SCK_o     : std_logic;
  signal MOSI_o    : std_logic;
  signal MISO_i    : std_logic := '0';

  -- clock
  constant ClkPeriode : time := 100 ns;

begin  -- behavior

  -- component instantiation
  DUT: SimpleSPI
    generic map (
      BaseAddr => BaseAddr
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i     => Clk_i,
      PerAddr_i => PerAddr_i,
      PerDIn_i  => PerDIn_i,
      PerDOut_o => PerDOut_o,
      PerWr_i   => PerWr_i,
      PerEn_i   => PerEn_i,
      Intr_o    => Intr_o,
      SCK_o     => SCK_o,
      MOSI_o    => MOSI_o,
      MISO_i    => MISO_i
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

    procedure CheckTransfer (
      constant IntEn     : in std_logic;
      constant BRDE      : in std_logic_vector(3 downto 0);
      constant BRDM      : in std_logic_vector(3 downto 0);
      constant CPHA      : in std_logic;
      constant CPOL      : in std_logic;
      constant MOSI_Byte : in std_logic_vector(7 downto 0);
      constant MISO_Byte : in std_logic_vector(7 downto 0)
    ) is
      variable NumCycles : integer;  -- number of cycles per half-periode
    begin
      NumCycles := (to_integer(unsigned(BRDM))+1)*2**(to_integer(unsigned(BRDE)));
      --                   Busy   res.
      WriteWord(BaseAddr+0,"0" & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL);
      ClkCycle(1);                              
      WriteWord(BaseAddr+2,"00000000" & MOSI_Byte);
      -- 1st cycle after write: Busy, Xfer and XferPhase were set at
      -- rising_edge(Clk_i), PrescaleSet = '1' because PrescalePrev was held to
      -- all '1', but nothing to see at the outputs
      assert MOSI_o = '0' report "MOSI_o should still be '0'" severity error;
      assert SCK_o  = CPOL report "SCK_o  should still be '" & std_logic'image(CPOL) & "'" severity error;
      CheckWord(BaseAddr+0,"1" & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL);   -- Busy bit is set
      -- now the real transfer starts
      for BitIdx in 7 downto 0 loop
        -- first half-periode
        MISO_i <= MISO_Byte(BitIdx);
        for i in 1 to NumCycles loop
          assert MOSI_o = MOSI_Byte(BitIdx) report "MOSI_o should be bit " & integer'image(BitIdx) & " = '" & std_logic'image(MOSI_Byte(BitIdx)) & "'" severity error;
          assert SCK_o  = (CPHA xor CPOL) report "SCK_o  should be '" & std_logic'image(CPHA xor CPOL) & "' in first half" severity error;
          CheckWord(BaseAddr+0,"1" & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL);   -- Busy bit is set
        end loop;
        for i in 1 to NumCycles loop
          assert MOSI_o = MOSI_Byte(BitIdx) report "MOSI_o should be bit " & integer'image(BitIdx) & " = '" & std_logic'image(MOSI_Byte(BitIdx)) & "'" severity error;
          assert SCK_o  = not (CPHA xor CPOL) report "SCK_o  should be '" & std_logic'image(not (CPHA xor CPOL)) & "' in second half" severity error;
          CheckWord(BaseAddr+0,"1" & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL);   -- Busy bit is set
        end loop;
      end loop;
      -- first cycle after last bit
      assert MOSI_o = '0' report "MOSI_o should agaion be '0'" severity error;
      assert SCK_o  = CPOL report "SCK_o  should still be '" & std_logic'image(CPOL) & "'" severity error;
      assert Intr_o = IntEn report "Intr_o should be '" & std_logic'image(IntEn) & "'" severity error;
      CheckWord(BaseAddr+0,"0" & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL);   -- Busy bit is set
      -- check received value
      CheckWord(BaseAddr+2,"00000000" & MISO_Byte);
      ClkCycle(4);
    end CheckTransfer;

  begin
    Reset_n_i <= '0';
    wait for 5.2*ClkPeriode;
    Reset_n_i <= '1';

    ---------------------------------------------------------------------------
    -- silly write cycles to CSR
    WriteWord(BaseAddr+0,16#5555#);
    ClkCycle(3);
    CheckWord(BaseAddr+0,16#0555#);
    ClkCycle(3);
    WriteWord(BaseAddr+0,16#AAAA#);
    ClkCycle(3);
    CheckWord(BaseAddr+0,16#02AA#);
    ClkCycle(3);

    ---------------------------------------------------------------------------
    -- Transfer bytes
    CheckTransfer('0', "0000", "0001", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- CPHA=0, CPOL=0
    CheckTransfer('1', "0000", "0001", '1', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- CPHA=1, CPOL=0
    CheckTransfer('1', "0000", "0001", '0', '1', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- CPHA=0, CPOL=1
    CheckTransfer('1', "0000", "0001", '1', '1', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- CPHA=1, CPOL=1

    CheckTransfer('1', "0000", "0000", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 0, BRDM = 0 --> ClkPeriod/2
    CheckTransfer('1', "0000", "0001", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 0, BRDM = 1 --> ClkPeriod/4
    CheckTransfer('1', "0000", "0010", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 0, BRDM = 2 --> ClkPeriod/6
    CheckTransfer('1', "0000", "0011", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 0, BRDM = 3 --> ClkPeriod/8

    CheckTransfer('1', "0001", "0000", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 1, BRDM = 0 --> ClkPeriod/4   
    CheckTransfer('1', "0001", "0001", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 1, BRDM = 1 --> ClkPeriod/8   
    CheckTransfer('1', "0001", "0010", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 1, BRDM = 2 --> ClkPeriod/12  
    CheckTransfer('1', "0001", "0011", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 1, BRDM = 3 --> ClkPeriod/16  

    CheckTransfer('1', "0101", "0000", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 5, BRDM = 0 --> ClkPeriod/64  
    CheckTransfer('1', "0101", "0001", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 5, BRDM = 1 --> ClkPeriod/128 
    CheckTransfer('1', "0101", "0010", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 5, BRDM = 2 --> ClkPeriod/192 
    CheckTransfer('1', "0101", "0011", '0', '0', std_logic_vector(to_unsigned(16#00C9#,8)), "10100110");   -- BRDE = 5, BRDM = 3 --> ClkPeriod/256 

    ---------------------------------------------------------------------------

    ClkCycle(3);

    report "### Simulation Finished ###" severity failure;
    wait;
  end process WaveGen_Proc;

end behavior;
