library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
-------------------------------------------------------------------------------
-- Don't use the VHDL component provided in the package memory_components but
-- use a local component definition. For more explanation see at the end of
-- this file.
--
--library work;
--use work.memory_components.all;

entity DMem is
  port (
    ram_rstn : in  std_logic;
    ram_clk  : in  std_logic;
    ram_addr : in  std_logic_vector( 6 downto 0);
    ram_cen  : in  std_logic;
    ram_dout : out std_logic_vector(15 downto 0);
    ram_din  : in  std_logic_vector(15 downto 0);
    ram_wen  : in  std_logic_vector( 1 downto 0)
  );
end DMem;

architecture struct of DMem is

  component sram128x8
    generic (
      TimingChecksOn  : BOOLEAN := True
    );
    port (
      CS   : in  std_logic;
      EN   : in  std_logic;
      RD   : in  std_logic;
      WR   : in  std_logic;
      NRST : in  std_logic;
      AD   : in  std_logic_VECTOR(6 downto 0);
      DI   : in  std_logic_VECTOR(7 downto 0);
      DO   : out std_logic_VECTOR(7 downto 0)
    );
  end component;
  -- private 

  signal ram_addr_s : std_logic_vector(6 downto 0);
  signal ram_cen_s  : std_logic;
  signal ram_din_s  : std_logic_vector(15 downto 0);
  signal ram_wen_s  : std_logic_vector(1 downto 0);
  signal RD_s : std_logic;
begin
  ram_addr_s <= ram_addr;-- after 2 ns;
  ram_cen_s  <= ram_cen ;-- after 2 ns;
  ram_din_s  <= ram_din ;-- after 2 ns;
  ram_wen_s  <= ram_wen ;-- after 2 ns;

  -- RD_s must be '0' during writes
  RD_s <= (not ram_cen_s) and and_reduce(ram_wen_s);
  
  sram128x8_0: sram128x8
    generic map (
      TimingChecksON => false
    )
    port map (
      CS   => ram_clk,
      EN   => '0',
      RD   => RD_s,
      WR   => "not"(ram_wen_s(0)),
      NRST => ram_rstn,
      AD   => ram_addr_s,
      DI   => ram_din_s (7 downto 0),
      DO   => ram_dout(7 downto 0)
    );

  sram128x8_1: sram128x8
    generic map (
      TimingChecksON => false
    )
    port map (
      CS   => ram_clk,
      EN   => '0',
      RD   => RD_s,
      WR   => "not"(ram_wen_s(1)),
      NRST => ram_rstn,
      AD   => ram_addr_s,
      DI   => ram_din_s (15 downto 8),
      DO   => ram_dout(15 downto 8)
    );

end struct;

-------------------------------------------------------------------------------
-- The sram128x8 vital_mem architecture in sram128x8.vhd does rigorous timing
-- checks which lead to an enormous amount of simulation messages and the
-- memory doesn't work. Most of these messages deal with hold time violations
-- during functional simulation, because AD, DI, RD and WR change at the very
-- same time as the edges of CS.
--
-- I tried a few ways to get around these problems.
--
-- 1) Delay AD, DI, RD and WR:
-- ---------------------------
-- Therefore I've added the four ram_*_s signals and assigned the inputs with a
-- delay of 2ns. The time was chosen to be most time delays specified in
-- sram128x8.vhd.
--
-- I didn't try yet, but these delayed assignements using "after" are most
-- probably not allowed in synthesis. Therefore I tried another way.
--
-- 2) Set sram128x8 generic TimingChecksON to false via configuration:
-- -------------------------------------------------------------------
-- I found that the RAM model has a generic TimingChecksON which can be used to
-- disable the timing checks. Unfortunately, the component definition in the
-- package memory_components only exposes the ports but no generics. Therefore
-- I used a configuration (see below) to set this generic.
--
-- This was a bit tricky because I didn't find how to use a configuration for
-- an inner module, except when simulating a top-level configuration. Therefore
-- I created Core_tb_amsram_cfg in ./tb/core_tb-amsram-cfg-c.vhd. Unfortunately
-- it was not possibel to "configure into the Core Verilog module", therefore I
-- created another Verilog configuration CoreAMSRAM in
-- ./verilog/core-amsram-c.v. This finaly used DMem_TimingChecksOFF_cfg.
-- 
-- Then another problem arised: the generic was set, but the port map as
-- specified above in the instantiations was removed, i.e. the instances were
-- not connected.
--
-- 3) Use local VHDL component:
-- ----------------------------
-- The above problem could be solved by adding a port map to
-- DMem_TimingChecksOFF_cfg (more precisely: adding it two times because both
-- instances have slightly different connections). This was too tedious, so I
-- just added a local VHDL component here which includes the generic
-- TimingChecksON. All configurations were removed.
--
-- Now simulations behaves well.
--


--configuration DMem_TimingChecksOFF_cfg of DMem is
--  for struct
--    for all : sram128x8
--      use entity work.sram128x8(vital_mem)
--        generic map (
--          TimingChecksON => false
--        );
--    end for;
--  end for;
--end DMem_TimingChecksOFF_cfg;
