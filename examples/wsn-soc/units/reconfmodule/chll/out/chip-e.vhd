-- Automatically generated: write_netlist -chip -vhdl -entity chip-e.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity chip is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    Cpu_En_i : in std_logic;
    Dbg_En_i : in std_logic;
    Dbg_SCL_i : in std_logic;
    Dbg_SDA_b : inout std_logic;
    P1_b : inout std_logic_vector(7 downto 0);
    P2_b : inout std_logic_vector(7 downto 0);
    UartRxD_i : in std_logic;
    UartTxD_o : out std_logic;
    MISO_i : in std_logic;
    MOSI_o : out std_logic;
    SCK_o : out std_logic;
    Inputs_i : in std_logic_vector(7 downto 0);
    Outputs_o : out std_logic_vector(7 downto 0);
    SPIMISO_i : in std_logic;
    SPIMOSI_o : out std_logic;
    SPISCK_o : out std_logic;
    I2CSCL_b : out std_logic;
    I2CSDA_b : inout std_logic;
    AdcConvComplete_i : in std_logic;
    AdcDoConvert_o : out std_logic;
    AdcValue_i : in std_logic_vector(9 downto 0)
  );
end chip;

