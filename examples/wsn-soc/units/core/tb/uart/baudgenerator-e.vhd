----------------------------------------------------------------------------------
-- Company:  TU Vienna
-- Engineer: Armin FALTINGER
-- 
-- Create Date:  17:27:11 11/26/2009
-- Module Name:  BaudGenerator - RTL
-- Project Name: Uart
-- Description:  generate baud clock for serial data i/o
--
-- Dependencies: pure RTL, no reference to other modules
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity BaudGenerator is
 generic ( MaxSpeedDividerWidth : integer              := 16;
           Oversampling         : integer range 0 to 3 := 2); -- 0 means use sampling divider output directly
    Port ( Clk_i                : in  STD_LOGIC;
           Reset_i_n            : in  STD_LOGIC;
           BGEnable_i           : in  STD_LOGIC;
           SpeedDivider_i       : in  STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
           BaudClk_o            : out STD_LOGIC;
           BaudSamplingClk_o    : out STD_LOGIC);
end BaudGenerator;

