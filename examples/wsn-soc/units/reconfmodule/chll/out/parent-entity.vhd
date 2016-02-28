-- Automatically generated: write_netlist -parent -vhdl -entity parent-entity.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Core is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    Cpu_En_i : in std_logic;
    LFXT_Clk_i : in std_logic;
    Dbg_En_i : in std_logic;
    Dbg_SCL_i : in std_logic;
    Dbg_SDA_Out_o : out std_logic;
    Dbg_SDA_In_i : in std_logic;
    P1_DOut_o : out std_logic_vector(7 downto 0);
    P1_En_o : out std_logic_vector(7 downto 0);
    P1_DIn_i : in std_logic_vector(7 downto 0);
    P2_DOut_o : out std_logic_vector(7 downto 0);
    P2_En_o : out std_logic_vector(7 downto 0);
    P2_DIn_i : in std_logic_vector(7 downto 0);
    UartRxD_i : in std_logic;
    UartTxD_o : out std_logic;
    SCK_o : out std_logic;
    MOSI_o : out std_logic;
    MISO_i : in std_logic;
    Inputs_i : in std_logic_vector(7 downto 0);
    Outputs_o : out std_logic_vector(7 downto 0);
    SPIMISO_i : in std_logic;
    SPIMOSI_o : out std_logic;
    SPISCK_o : out std_logic;
    I2CSCL_o : out std_logic;
    I2CSDA_i : in std_logic;
    I2CSDA_o : out std_logic;
    AdcConvComplete_i : in std_logic;
    AdcDoConvert_o : out std_logic;
    AdcValue_i : in std_logic_vector(9 downto 0)
  );
  attribute src of Core : entity is "../../../core/verilog/core.v:22";
  attribute src of Reset_n_i : signal is "../../../core/verilog/core.v:24";
  attribute src of Clk_i : signal is "../../../core/verilog/core.v:25";
  attribute src of Cpu_En_i : signal is "../../../core/verilog/core.v:28";
  attribute src of LFXT_Clk_i : signal is "../../../core/verilog/core.v:30";
  attribute src of Dbg_En_i : signal is "../../../core/verilog/core.v:32";
  attribute src of Dbg_SCL_i : signal is "../../../core/verilog/core.v:38";
  attribute src of Dbg_SDA_Out_o : signal is "../../../core/verilog/core.v:39";
  attribute src of Dbg_SDA_In_i : signal is "../../../core/verilog/core.v:40";
  attribute src of P1_DOut_o : signal is "../../../core/verilog/core.v:43";
  attribute src of P1_En_o : signal is "../../../core/verilog/core.v:44";
  attribute src of P1_DIn_i : signal is "../../../core/verilog/core.v:45";
  attribute src of P2_DOut_o : signal is "../../../core/verilog/core.v:46";
  attribute src of P2_En_o : signal is "../../../core/verilog/core.v:47";
  attribute src of P2_DIn_i : signal is "../../../core/verilog/core.v:48";
  attribute src of UartRxD_i : signal is "../../../core/verilog/core.v:50";
  attribute src of UartTxD_o : signal is "../../../core/verilog/core.v:51";
  attribute src of SCK_o : signal is "../../../core/verilog/core.v:53";
  attribute src of MOSI_o : signal is "../../../core/verilog/core.v:54";
  attribute src of MISO_i : signal is "../../../core/verilog/core.v:55";
  attribute src of Inputs_i : signal is "../../../core/verilog/core.v:58";
  attribute src of Outputs_o : signal is "../../../core/verilog/core.v:59";
  attribute src of SPIMISO_i : signal is "../../../core/verilog/core.v:61";
  attribute src of SPIMOSI_o : signal is "../../../core/verilog/core.v:62";
  attribute src of SPISCK_o : signal is "../../../core/verilog/core.v:63";
  attribute src of I2CSCL_o : signal is "../../../core/verilog/core.v:65";
  attribute src of I2CSDA_i : signal is "../../../core/verilog/core.v:66";
  attribute src of I2CSDA_o : signal is "../../../core/verilog/core.v:67";
  attribute src of AdcConvComplete_i : signal is "../../../core/verilog/core.v:87";
  attribute src of AdcDoConvert_o : signal is "../../../core/verilog/core.v:88";
  attribute src of AdcValue_i : signal is "../../../core/verilog/core.v:89";
end Core;

