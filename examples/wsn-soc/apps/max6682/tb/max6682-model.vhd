-------------------------------------------------------------------------------
-- Title      : Entity for simulation model of MAX6682 temperature sensor
-- Project    : 
-------------------------------------------------------------------------------
-- File       : max6682-e.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2011-04-14
-- Last update: 2011-04-14
-- Platform   : 
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2011 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2011-04-14  1.0      glasejoh	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity MAX6682_Model is
  port (
    ChipSelect_n_i : in  std_logic;
    SCLK_i         : in  std_logic;
    SO_o           : out std_logic;
    Value_i        : in  std_logic_vector(10 downto 0));
end MAX6682_Model;

architecture behavior of MAX6682_Model is
begin  -- behavior

  SPIProc: process (ChipSelect_n_i,SCLK_i,Value_i)
    variable Value : std_logic_vector(10 downto 0);
  begin  -- process SPIProc
    if ChipSelect_n_i = '1' then
      Value := Value_i;
      SO_o  <= 'Z';
    else
      --falling_edge(ChipSelect_n_i) then
      --wait for 30 ns;                     -- t_DV <= 35 ns : CS Fall to Output Data Valid
      if falling_edge(SCLK_i) then
        --wait for 30 ns;                   -- t_DO <= 35 ns : SCK Fall to Output Data Valid
        Value := Value(Value'high-1 downto 0) & 'Z';
      end if;
      SO_o <= Value(Value'high);
    end if;
  end process SPIProc;

end behavior;
