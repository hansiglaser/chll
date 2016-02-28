--------------------------------------------------------------------------------
-- Company:        vienna university of technology
-- Engineer:       mario faschang
-- Create Date:    14:24:06 11/30/2009
-- Module Name:    ClkDiv - rtl
-- Project Name:   i2c master controller
-- Description:  * This module generates the necessary frequency for all the
--                 other modules to finally result in an I2C-Bus SCL-frequency
--                 of 100 kHz or 400 kHz.
--               * F100_400_n_i: defines either 100 kHz or 400 kHz as final I2C-
--                 Bus SCL-frequency. (Which results in an Clk_o frequency of
--                 200 kHz or 800 kHz)
--               * Clk_i is the input-port for the fast oscillation-frequency
--               * Reset_i is an assynchronous reset input
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ClkDiv is
   -- Generates 200 / 800 kHz Clk
   Generic (DividerWidth_g : integer range 4 to 32);
            
   Port (   F100_400_n_i : in      STD_LOGIC;
            Divider800_i : in      std_logic_vector(DividerWidth_g-1 downto 0);
            Clk_i        : in      STD_LOGIC;
            Reset_i      : in      STD_LOGIC;
            Clk_o        : out     STD_LOGIC);
end ClkDiv;
