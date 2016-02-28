-- Automatically generated: write_netlist -parent -vhdl -component parent-component.vhd

  component Core
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
  end component;

