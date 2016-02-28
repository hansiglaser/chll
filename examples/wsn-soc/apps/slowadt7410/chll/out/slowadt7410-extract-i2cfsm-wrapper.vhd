library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity I2CFSM is
  port (
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    In0_i : in std_logic;
    In1_i : in std_logic;
    In2_i : in std_logic;
    In3_i : in std_logic;
    In4_i : in std_logic;
    In5_i : in std_logic;
    In6_i : in std_logic;
    In7_i : in std_logic;
    Out0_o : out std_logic;
    Out1_o : out std_logic;
    Out2_o : out std_logic;
    Out3_o : out std_logic;
    Out4_o : out std_logic;
    Out5_o : out std_logic;
    Out6_o : out std_logic;
    Out7_o : out std_logic;
    Out8_o : out std_logic;
    Out9_o : out std_logic;
    Out10_o : out std_logic;
    Out11_o : out std_logic;
    Out12_o : out std_logic;
    Out13_o : out std_logic;
    Out14_o : out std_logic;
    CfgMode_i : in std_logic;
    CfgClk_i : in std_logic;
    CfgShift_i : in std_logic;
    CfgDataIn_i : in std_logic;
    CfgDataOut_o : out std_logic
  );
end I2CFSM;

architecture struct of I2CFSM is

  component TRFSM
    generic (
      InputWidth : integer;
      OutputWidth : integer;
      StateWidth : integer;
      UseResetRow : integer;
      NumRows0 : integer;
      NumRows1 : integer;
      NumRows2 : integer;
      NumRows3 : integer;
      NumRows4 : integer;
      NumRows5 : integer;
      NumRows6 : integer;
      NumRows7 : integer;
      NumRows8 : integer;
      NumRows9 : integer
    );
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Input_i : in std_logic_vector(InputWidth-1 downto 0);
      Output_o : out std_logic_vector(OutputWidth-1 downto 0);
      CfgMode_i : in std_logic;
      CfgClk_i : in std_logic;
      CfgShift_i : in std_logic;
      CfgDataIn_i : in std_logic;
      CfgDataOut_o : out std_logic;
      ScanEnable_i : in std_logic;
      ScanClk_i : in std_logic;
      ScanDataIn_i : in std_logic;
      ScanDataOut_o : out std_logic
    );
  end component;

  signal Input_s : std_logic_vector(7 downto 0);
  signal Output_s : std_logic_vector(14 downto 0);
  signal ScanEnable_s : std_logic;
  signal ScanClk_s : std_logic;
  signal ScanDataIn_s : std_logic;
  signal ScanDataOut_s : std_logic;

begin

  TRFSM_1: TRFSM
    generic map (
      InputWidth => 8,
      OutputWidth => 15,
      StateWidth => 5,
      UseResetRow => 0,
      NumRows0 => 5,
      NumRows1 => 10,
      NumRows2 => 10,
      NumRows3 => 5,
      NumRows4 => 5,
      NumRows5 => 0,
      NumRows6 => 0,
      NumRows7 => 0,
      NumRows8 => 0,
      NumRows9 => 0
    )
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Input_i => Input_s,
      Output_o => Output_s,
      CfgMode_i => CfgMode_i,
      CfgClk_i => CfgClk_i,
      CfgShift_i => CfgShift_i,
      CfgDataIn_i => CfgDataIn_i,
      CfgDataOut_o => CfgDataOut_o,
      ScanEnable_i => ScanEnable_s,
      ScanClk_i => ScanClk_s,
      ScanDataIn_i => ScanDataIn_s,
      ScanDataOut_o => ScanDataOut_s
    );

  Input_s <= In7_i & In6_i & In5_i & In4_i & In3_i & In2_i & In1_i & In0_i;
  Out0_o <= Output_s(0);
  Out1_o <= Output_s(1);
  Out2_o <= Output_s(2);
  Out3_o <= Output_s(3);
  Out4_o <= Output_s(4);
  Out5_o <= Output_s(5);
  Out6_o <= Output_s(6);
  Out7_o <= Output_s(7);
  Out8_o <= Output_s(8);
  Out9_o <= Output_s(9);
  Out10_o <= Output_s(10);
  Out11_o <= Output_s(11);
  Out12_o <= Output_s(12);
  Out13_o <= Output_s(13);
  Out14_o <= Output_s(14);
  ScanEnable_s <= '0';
  ScanClk_s <= '0';
  ScanDataIn_s <= '0';

end struct;
