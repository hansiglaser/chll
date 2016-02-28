-------------------------------------------------------------------------------
-- Title      : Param Out Register
-- Project    : 
-------------------------------------------------------------------------------
-- File       : paramoutreg.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2013-10-24
-- Last update: 2013-10-24
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: Parameter Register which stores values written by the CPU
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2013-10-24  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity ParamOutReg is
  
  generic (
    Width : integer := 16);

  port (
    Reset_n_i     : in  std_logic;
    Clk_i         : in  std_logic;
    Enable_i      : in  std_logic;
    ParamWrData_i : in  std_logic_vector(Width-1 downto 0);
    Param_o       : out std_logic_vector(Width-1 downto 0)
  );

end ParamOutReg;

-------------------------------------------------------------------------------
-- Outside of this module, i.e. in its parent, the Enable_i signal
-- is created using RTL coding style, so that the synthesis and P&R can
-- optimize the large address decoder.
--
-- E.g.
--   Param17Enable_s <= ParamWr_o when ParamWrAddr_o = "10001" else
--                      '0';
--   ParamOutReg_17: ParamOutReg
--     generic map (
--       Width => 16
--     )
--     port map (
--       Reset_n_i     => Reset_n_i,
--       Clk_i         => Clk_i,
--       Enable_i      => Param17Enable,
--       ParamWrData_i => ParamWrData_i,
--       Param_o       => Param17_s
--     );
--   
-------------------------------------------------------------------------------

architecture rtl of ParamOutReg is
begin  -- rtl


  ParamReg: process (Clk_i, Reset_n_i)
  begin  -- process ParamReg
    if Reset_n_i = '0' then             -- asynchronous reset (active low)
      Param_o <= (others => '0');
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge
      if Enable_i = '1' then
        Param_o <= ParamWrData_i;
      end if;
    end if;
  end process ParamReg;

end rtl;
