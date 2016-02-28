-- Automatically generated: write_netlist -preliminary -vhdl -component reconflogic-cmp.vhd

  component MyReconfigLogic
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      AdcConvComplete_i : in std_logic;
      AdcDoConvert_o : out std_logic;
      AdcValue_i : in std_logic_vector(9 downto 0);
      I2C_Busy_i : in std_logic;
      I2C_DataIn_o : out std_logic_vector(7 downto 0);
      I2C_DataOut_i : in std_logic_vector(7 downto 0);
      I2C_Divider800_o : out std_logic_vector(15 downto 0);
      I2C_ErrAckParam_o : out std_logic;
      I2C_Error_i : in std_logic;
      I2C_F100_400_n_o : out std_logic;
      I2C_FIFOEmpty_i : in std_logic;
      I2C_FIFOFull_i : in std_logic;
      I2C_FIFOReadNext_o : out std_logic;
      I2C_FIFOWrite_o : out std_logic;
      I2C_ReadCount_o : out std_logic_vector(3 downto 0);
      I2C_ReceiveSend_n_o : out std_logic;
      I2C_StartProcess_o : out std_logic;
      Inputs_i : in std_logic_vector(7 downto 0);
      Outputs_o : out std_logic_vector(7 downto 0);
      ReconfModuleIRQs_o : out std_logic_vector(4 downto 0);
      SPI_CPHA_o : out std_logic;
      SPI_CPOL_o : out std_logic;
      SPI_DataIn_o : out std_logic_vector(7 downto 0);
      SPI_DataOut_i : in std_logic_vector(7 downto 0);
      SPI_FIFOEmpty_i : in std_logic;
      SPI_FIFOFull_i : in std_logic;
      SPI_LSBFE_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_SPPR_SPR_o : out std_logic_vector(7 downto 0);
      SPI_Transmission_i : in std_logic;
      SPI_Write_o : out std_logic;
      ReconfModuleIn_i : in std_logic_vector(7 downto 0);
      ReconfModuleOut_o : out std_logic_vector(7 downto 0);
      I2C_Errors_i : in std_logic_vector(7 downto 0);
      PerAddr_i : in std_logic_vector(13 downto 0);
      PerDIn_i : in std_logic_vector(15 downto 0);
      PerWr_i : in std_logic_vector(1 downto 0);
      PerEn_i : in std_logic;
      CfgIntfDOut_o : out std_logic_vector(15 downto 0);
      ParamIntfDOut_o : out std_logic_vector(15 downto 0)
    );
  end component;

