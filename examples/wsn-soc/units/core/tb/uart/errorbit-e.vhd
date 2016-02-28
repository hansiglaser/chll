----------------------------------------------------------------------------------
-- Company:   TU Vienna
-- Engineer:  Armin FALTINGER
-- 
-- Create Date:   10:21:09 12/25/2009
-- Module Name:   ErrorBit - RTL
-- Project Name:  Uart
-- Description:   Indicate possible errors,
--                  clearing the error with a global reset (Reset_i_n)
--                  or the dedicated error reset (ErrorReset)
--
-- Dependencies:  pure RTL no dependencies
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ErrorBit is
    Port ( Clk_i               : in  STD_LOGIC;
           Reset_i_n           : in  STD_LOGIC;
           ErrorReset_i        : in  STD_LOGIC;
           ErrorBit_i          : in  STD_LOGIC;
           ErrorIndicatorBit_o : out STD_LOGIC);
end ErrorBit;

