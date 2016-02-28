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

  signal cen_0  : std_logic;
  signal cen_1  : std_logic;
  signal wen_0  : std_logic_vector(1 downto 0);
  signal wen_1  : std_logic_vector(1 downto 0);
  signal dout_0 : std_logic_vector(15 downto 0);
  signal dout_1 : std_logic_vector(15 downto 0);
  signal ram_dout_s : std_logic_vector(15 downto 0);

  signal OEB : std_logic;
  signal WEB : std_logic_vector(1 downto 0);
begin

  OEB <= ram_cen or ram_clk;
  WEB <= ram_wen or (ram_clk & ram_clk);
  
  cen_0 <= OEB or     ram_addr(11);
  cen_1 <= OEB or not ram_addr(11);
  wen_0 <= WEB or     (ram_addr(11) & ram_addr(11));
  wen_1 <= WEB or not (ram_addr(11) & ram_addr(11));

--  ram_dout_s <= dout_0 when ram_addr(11) = '0' else
--                dout_1 after 1 ns;

  process (ram_rstn,ram_clk)
  begin  -- process
    if ram_rstn = '0' then
      ram_dout <= (others => '0');
    elsif rising_edge(ram_clk) then
      if ram_cen = '0' then
        ram_dout <= dout_0 or dout_1;   -- sram2kx8 O is "0" while OEB = '1'
      end if;
    end if;
  end process;

  ram_0: sram2kx8
    port map (
      A   => ram_addr(10 downto 0),
      I   => ram_din(7 downto 0),
      OEB => cen_0,
      O   => dout_0(7 downto 0),
      RB  => '1',
      WEB => wen_0(0)
    );

  ram_1: sram2kx8
    port map (
      A   => ram_addr(10 downto 0),
      I   => ram_din(15 downto 8),
      OEB => cen_0,
      O   => dout_0(15 downto 8),
      RB  => '1',
      WEB => wen_0(1)
    );

  ram_2: sram2kx8
    port map (
      A   => ram_addr(10 downto 0),
      I   => ram_din(7 downto 0),
      OEB => cen_1,
      O   => dout_1(7 downto 0),
      RB  => '1',
      WEB => wen_1(0)
    );

  ram_3: sram2kx8
    port map (
      A   => ram_addr(10 downto 0),
      I   => ram_din(15 downto 8),
      OEB => cen_1,
      O   => dout_1(15 downto 8),
      RB  => '1',
      WEB => wen_1(1)
    );

end struct;
