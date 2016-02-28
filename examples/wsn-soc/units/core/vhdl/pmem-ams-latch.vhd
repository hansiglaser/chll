library ieee;
use ieee.std_logic_1164.all;

entity PMem is
  port (
    ram_rstn : in  std_logic;
    ram_clk  : in  std_logic;
    ram_addr : in  std_logic_vector(11 downto 0);
    ram_cen  : in  std_logic;
    ram_dout : out std_logic_vector(15 downto 0);
    ram_din  : in  std_logic_vector(15 downto 0);
    ram_wen  : in  std_logic_vector( 1 downto 0)
  );
end PMem;

architecture struct of PMem is

  component sram2kx8
    generic(
      bankSize  : integer := 2048;
      AddrBits  : integer := 11;
      dataWidth : integer := 8;
      tOE       : integer := 4;
      tWE       : integer := 10;
      tOH       : integer := 2
    );
    port(
      A   : in  std_logic_vector(10 downto 0);
      I   : in  std_logic_vector( 7 downto 0);
      OEB : in  std_logic;
      O   : out std_logic_vector( 7 downto 0);
      RB  : in  std_logic;
      WEB : in  std_logic
    );
  end component;

  component omsp_clock_gate
    port (
      gclk        : out std_logic;
      clk         : in  std_logic;
      enable      : in  std_logic;
      scan_enable : in  std_logic
    );
  end component;

  signal ram_addr_latch : std_logic_vector(11 downto 0);
  signal ram_din_latch  : std_logic_vector(15 downto 0);
  signal OE_0_Enable    : std_logic;
  signal OE_1_Enable    : std_logic;
  signal OE_0           : std_logic;
  signal OE_1           : std_logic;
  signal OEB_0          : std_logic;
  signal OEB_1          : std_logic;
  signal WE_0_Enable    : std_logic;
  signal WE_1_Enable    : std_logic;
  signal WE_2_Enable    : std_logic;
  signal WE_3_Enable    : std_logic;
  signal WE_0           : std_logic;
  signal WE_1           : std_logic;
  signal WE_2           : std_logic;
  signal WE_3           : std_logic;
  signal WEB_0          : std_logic;
  signal WEB_1          : std_logic;
  signal WEB_2          : std_logic;
  signal WEB_3          : std_logic;
  signal dout_0         : std_logic_vector(15 downto 0);
  signal dout_1         : std_logic_vector(15 downto 0);
  signal DataClkEnable   : std_logic;
  signal DataClk         : std_logic;

begin

  -- keep address valid after rising_edge(Clk_i); this can't be done by a D-FF
  -- because the address setup time before the active (falling) edges of OEB
  -- and WEB would be too short.
  AddrLatch: process (ram_clk, ram_addr, ram_cen)
  begin
    if ram_clk = '0' then
      ram_addr_latch <= ram_addr;
    end if;
  end process AddrLatch;

  ram_din_latch  <= ram_din;

  -- use clock gates to generate OEB and WEB so that synthesis correctly
  -- identifies these signals as clocks

  OE_0_Enable <= '1' when ram_cen = '0' and ram_wen = "11" and ram_addr(11) = '0' else
                 '0';
  OE_1_Enable <= '1' when ram_cen = '0' and ram_wen = "11" and ram_addr(11) = '1' else
                 '0';

  OEB_0_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => OE_0_Enable,
      scan_enable => '0',
      gclk        => OE_0
    );

  OEB_1_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => OE_1_Enable,
      scan_enable => '0',
      gclk        => OE_1
    );

  OEB_0 <= not OE_0;
  OEB_1 <= not OE_1;

  WE_0_Enable <= '1' when ram_cen = '0' and ram_wen(0) = '0' and ram_addr(11) = '0' else
                 '0';
  WE_1_Enable <= '1' when ram_cen = '0' and ram_wen(1) = '0' and ram_addr(11) = '0' else
                 '0';
  WE_2_Enable <= '1' when ram_cen = '0' and ram_wen(0) = '0' and ram_addr(11) = '1' else
                 '0';
  WE_3_Enable <= '1' when ram_cen = '0' and ram_wen(1) = '0' and ram_addr(11) = '1' else
                 '0';


  WEB_0_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => WE_0_Enable,
      scan_enable => '0',
      gclk        => WE_0
    );

  WEB_1_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => WE_1_Enable,
      scan_enable => '0',
      gclk        => WE_1
    );

  WEB_2_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => WE_2_Enable,
      scan_enable => '0',
      gclk        => WE_2
    );

  WEB_3_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => WE_3_Enable,
      scan_enable => '0',
      gclk        => WE_3
    );

  WEB_0 <= not WE_0;
  WEB_1 <= not WE_1;
  WEB_2 <= not WE_2;
  WEB_3 <= not WE_3;

  DataClkEnable <= not ram_cen;

  Data_Gate: omsp_clock_gate
    port map (
      clk         => ram_clk,
      enable      => DataClkEnable,
      scan_enable => '0',
      gclk        => DataClk
    );

  DataReg: process(ram_rstn, DataClk)
  begin
    if ram_rstn = '0' then
      ram_dout <= (others => '0');
    elsif falling_edge(DataClk) then
      ram_dout <= dout_0 or dout_1;   -- sram2kx8 O is "0" while OEB = '1'
    end if;
  end process DataReg;

  ram_0: sram2kx8
    port map (
      A   => ram_addr_latch(10 downto 0),
      I   => ram_din_latch(7 downto 0),
      OEB => OEB_0,
      O   => dout_0(7 downto 0),
      RB  => '1',
      WEB => WEB_0
    );

  ram_1: sram2kx8
    port map (
      A   => ram_addr_latch(10 downto 0),
      I   => ram_din_latch(15 downto 8),
      OEB => OEB_0,
      O   => dout_0(15 downto 8),
      RB  => '1',
      WEB => WEB_1
    );

  ram_2: sram2kx8
    port map (
      A   => ram_addr_latch(10 downto 0),
      I   => ram_din_latch(7 downto 0),
      OEB => OEB_1,
      O   => dout_1(7 downto 0),
      RB  => '1',
      WEB => WEB_2
    );

  ram_3: sram2kx8
    port map (
      A   => ram_addr_latch(10 downto 0),
      I   => ram_din_latch(15 downto 8),
      OEB => OEB_1,
      O   => dout_1(15 downto 8),
      RB  => '1',
      WEB => WEB_3
    );

end struct;
