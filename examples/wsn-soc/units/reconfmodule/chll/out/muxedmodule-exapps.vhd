library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MuxedModuleExApps is
  port (
    Select_i : in std_logic_vector(2 downto 0);
    Reset_n_i : in std_logic;
    Clk_i : in std_logic;
    ReconfModuleIn_s : in std_logic_vector(7 downto 0);
    ReconfModuleIRQs_s : out std_logic_vector(4 downto 0);
    Outputs_o : out std_logic_vector(7 downto 0);
    SPI_DataOut : in std_logic_vector(7 downto 0);
    SPI_Write : out std_logic;
    SPI_ReadNext : out std_logic;
    SPI_DataIn : out std_logic_vector(7 downto 0);
    SPI_FIFOFull : in std_logic;
    SPI_FIFOEmpty : in std_logic;
    SPI_Transmission : in std_logic;
    ParamWordIn0_i : in std_logic_vector(15 downto 0);
    ParamWordIn1_i : in std_logic_vector(15 downto 0);
    ParamWordIn2_i : in std_logic_vector(15 downto 0);
    ParamWordOut0_o : in std_logic_vector(15 downto 0);
    ParamWordIn3_i : in std_logic_vector(15 downto 0);
    SPI_CPOL : out std_logic;
    SPI_CPHA : out std_logic;
    SPI_LSBFE : out std_logic;
    SPI_SPPR_SPR : out std_logic_vector(7 downto 0);
    I2C_ReceiveSend_n : out std_logic;
    I2C_ReadCount : out std_logic_vector(3 downto 0);
    I2C_StartProcess : out std_logic;
    I2C_Busy : in std_logic;
    I2C_FIFOReadNext : out std_logic;
    I2C_FIFOWrite : out std_logic;
    I2C_DataIn : out std_logic_vector(7 downto 0);
    I2C_DataOut : in std_logic_vector(7 downto 0);
    I2C_Error : in std_logic;
    I2C_F100_400_n : out std_logic;
    I2C_Divider800 : out std_logic_vector(15 downto 0);
    Inputs_i : in std_logic_vector(7 downto 0);
    AdcDoConvert_o : out std_logic;
    AdcConvComplete_i : in std_logic;
    AdcValue_i : in std_logic_vector(9 downto 0);
    ParamWordIn4_i : in std_logic_vector(15 downto 0)
  );
end MuxedModuleExApps;

architecture struct of MuxedModuleExApps is

  component ADT7310
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      ADT7310CS_n_o : out std_logic;
      SPI_Data_i : in std_logic_vector(7 downto 0);
      SPI_Write_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_Data_o : out std_logic_vector(7 downto 0);
      SPI_FIFOFull_i : in std_logic;
      SPI_FIFOEmpty_i : in std_logic;
      SPI_Transmission_i : in std_logic;
      PeriodCounterPreset_i : in std_logic_vector(15 downto 0);
      SPICounterPresetH_i : in std_logic_vector(15 downto 0);
      SPICounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7410
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      I2C_ReceiveSend_n_o : out std_logic;
      I2C_ReadCount_o : out std_logic_vector(7 downto 0);
      I2C_StartProcess_o : out std_logic;
      I2C_Busy_i : in std_logic;
      I2C_FIFOReadNext_o : out std_logic;
      I2C_FIFOWrite_o : out std_logic;
      I2C_Data_o : out std_logic_vector(7 downto 0);
      I2C_Data_i : in std_logic_vector(7 downto 0);
      I2C_Error_i : in std_logic;
      PeriodCounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      WaitCounterPreset_i : in std_logic_vector(15 downto 0)
    );
  end component;

  component ExtADC
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      SensorPower_o : out std_logic;
      SensorStart_o : out std_logic;
      SensorReady_i : in std_logic;
      AdcStart_o : out std_logic;
      AdcDone_i : in std_logic;
      AdcValue_i : in std_logic_vector(15 downto 0);
      PeriodCounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0)
    );
  end component;

  component MAX6682Mean
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      MAX6682CS_n_o : out std_logic;
      SPI_Data_i : in std_logic_vector(7 downto 0);
      SPI_Write_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_Data_o : out std_logic_vector(7 downto 0);
      SPI_FIFOFull_i : in std_logic;
      SPI_FIFOEmpty_i : in std_logic;
      SPI_Transmission_i : in std_logic;
      PauseCounterPreset_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component MAX6682
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      MAX6682CS_n_o : out std_logic;
      SPI_Data_i : in std_logic_vector(7 downto 0);
      SPI_Write_o : out std_logic;
      SPI_ReadNext_o : out std_logic;
      SPI_Data_o : out std_logic_vector(7 downto 0);
      SPI_FIFOFull_i : in std_logic;
      SPI_FIFOEmpty_i : in std_logic;
      SPI_Transmission_i : in std_logic;
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component SlowADT7410
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      Enable_i : in std_logic;
      CpuIntr_o : out std_logic;
      I2C_ReceiveSend_n_o : out std_logic;
      I2C_ReadCount_o : out std_logic_vector(7 downto 0);
      I2C_StartProcess_o : out std_logic;
      I2C_Busy_i : in std_logic;
      I2C_FIFOReadNext_o : out std_logic;
      I2C_FIFOWrite_o : out std_logic;
      I2C_Data_o : out std_logic_vector(7 downto 0);
      I2C_Data_i : in std_logic_vector(7 downto 0);
      I2C_Error_i : in std_logic;
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      WaitCounterPresetH_i : in std_logic_vector(15 downto 0);
      WaitCounterPresetL_i : in std_logic_vector(15 downto 0)
    );
  end component;

  signal ADT7310_Reset_n_i : std_logic;
  signal ADT7310_Clk_i : std_logic;
  signal ADT7310_Enable_i : std_logic;
  signal ADT7310_CpuIntr_o : std_logic;
  signal ADT7310_ADT7310CS_n_o : std_logic;
  signal ADT7310_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310_SPI_Write_o : std_logic;
  signal ADT7310_SPI_ReadNext_o : std_logic;
  signal ADT7310_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310_SPI_FIFOFull_i : std_logic;
  signal ADT7310_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310_SPI_Transmission_i : std_logic;
  signal ADT7310_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310_SPICounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310_SPICounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310_SPI_CPOL_o : std_logic;
  signal ADT7310_SPI_CPHA_o : std_logic;
  signal ADT7310_SPI_LSBFE_o : std_logic;
  signal MAX6682_Reset_n_i : std_logic;
  signal MAX6682_Clk_i : std_logic;
  signal MAX6682_Enable_i : std_logic;
  signal MAX6682_CpuIntr_o : std_logic;
  signal MAX6682_MAX6682CS_n_o : std_logic;
  signal MAX6682_SPI_Data_i : std_logic_vector(7 downto 0);
  signal MAX6682_SPI_Write_o : std_logic;
  signal MAX6682_SPI_ReadNext_o : std_logic;
  signal MAX6682_SPI_Data_o : std_logic_vector(7 downto 0);
  signal MAX6682_SPI_FIFOFull_i : std_logic;
  signal MAX6682_SPI_FIFOEmpty_i : std_logic;
  signal MAX6682_SPI_Transmission_i : std_logic;
  signal MAX6682_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal MAX6682_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal MAX6682_SensorValue_o : std_logic_vector(15 downto 0);
  signal MAX6682_Threshold_i : std_logic_vector(15 downto 0);
  signal MAX6682_SPI_CPOL_o : std_logic;
  signal MAX6682_SPI_CPHA_o : std_logic;
  signal MAX6682_SPI_LSBFE_o : std_logic;
  signal MAX6682Mean_Reset_n_i : std_logic;
  signal MAX6682Mean_Clk_i : std_logic;
  signal MAX6682Mean_Enable_i : std_logic;
  signal MAX6682Mean_CpuIntr_o : std_logic;
  signal MAX6682Mean_MAX6682CS_n_o : std_logic;
  signal MAX6682Mean_SPI_Data_i : std_logic_vector(7 downto 0);
  signal MAX6682Mean_SPI_Write_o : std_logic;
  signal MAX6682Mean_SPI_ReadNext_o : std_logic;
  signal MAX6682Mean_SPI_Data_o : std_logic_vector(7 downto 0);
  signal MAX6682Mean_SPI_FIFOFull_i : std_logic;
  signal MAX6682Mean_SPI_FIFOEmpty_i : std_logic;
  signal MAX6682Mean_SPI_Transmission_i : std_logic;
  signal MAX6682Mean_PauseCounterPreset_i : std_logic_vector(15 downto 0);
  signal MAX6682Mean_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal MAX6682Mean_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal MAX6682Mean_SensorValue_o : std_logic_vector(15 downto 0);
  signal MAX6682Mean_Threshold_i : std_logic_vector(15 downto 0);
  signal MAX6682Mean_SPI_CPOL_o : std_logic;
  signal MAX6682Mean_SPI_CPHA_o : std_logic;
  signal MAX6682Mean_SPI_LSBFE_o : std_logic;
  signal ADT7410_Reset_n_i : std_logic;
  signal ADT7410_Clk_i : std_logic;
  signal ADT7410_Enable_i : std_logic;
  signal ADT7410_CpuIntr_o : std_logic;
  signal ADT7410_I2C_ReceiveSend_n_o : std_logic;
  signal ADT7410_I2C_ReadCount_o : std_logic_vector(7 downto 0);
  signal ADT7410_I2C_StartProcess_o : std_logic;
  signal ADT7410_I2C_Busy_i : std_logic;
  signal ADT7410_I2C_FIFOReadNext_o : std_logic;
  signal ADT7410_I2C_FIFOWrite_o : std_logic;
  signal ADT7410_I2C_Data_o : std_logic_vector(7 downto 0);
  signal ADT7410_I2C_Data_i : std_logic_vector(7 downto 0);
  signal ADT7410_I2C_Error_i : std_logic;
  signal ADT7410_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7410_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7410_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7410_WaitCounterPreset_i : std_logic_vector(15 downto 0);
  signal ExtADC_Reset_n_i : std_logic;
  signal ExtADC_Clk_i : std_logic;
  signal ExtADC_Enable_i : std_logic;
  signal ExtADC_CpuIntr_o : std_logic;
  signal ExtADC_SensorPower_o : std_logic;
  signal ExtADC_SensorStart_o : std_logic;
  signal ExtADC_SensorReady_i : std_logic;
  signal ExtADC_AdcStart_o : std_logic;
  signal ExtADC_AdcDone_i : std_logic;
  signal ExtADC_AdcValue_i : std_logic_vector(15 downto 0);
  signal ExtADC_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ExtADC_SensorValue_o : std_logic_vector(15 downto 0);
  signal ExtADC_Threshold_i : std_logic_vector(15 downto 0);
  signal SlowADT7410_Reset_n_i : std_logic;
  signal SlowADT7410_Clk_i : std_logic;
  signal SlowADT7410_Enable_i : std_logic;
  signal SlowADT7410_CpuIntr_o : std_logic;
  signal SlowADT7410_I2C_ReceiveSend_n_o : std_logic;
  signal SlowADT7410_I2C_ReadCount_o : std_logic_vector(7 downto 0);
  signal SlowADT7410_I2C_StartProcess_o : std_logic;
  signal SlowADT7410_I2C_Busy_i : std_logic;
  signal SlowADT7410_I2C_FIFOReadNext_o : std_logic;
  signal SlowADT7410_I2C_FIFOWrite_o : std_logic;
  signal SlowADT7410_I2C_Data_o : std_logic_vector(7 downto 0);
  signal SlowADT7410_I2C_Data_i : std_logic_vector(7 downto 0);
  signal SlowADT7410_I2C_Error_i : std_logic;
  signal SlowADT7410_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal SlowADT7410_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal SlowADT7410_SensorValue_o : std_logic_vector(15 downto 0);
  signal SlowADT7410_Threshold_i : std_logic_vector(15 downto 0);
  signal SlowADT7410_WaitCounterPresetH_i : std_logic_vector(15 downto 0);
  signal SlowADT7410_WaitCounterPresetL_i : std_logic_vector(15 downto 0);

begin

  ADT7310_1: ADT7310
    port map (
      Reset_n_i => ADT7310_Reset_n_i,
      Clk_i => ADT7310_Clk_i,
      Enable_i => ADT7310_Enable_i,
      CpuIntr_o => ADT7310_CpuIntr_o,
      ADT7310CS_n_o => ADT7310_ADT7310CS_n_o,
      SPI_Data_i => ADT7310_SPI_Data_i,
      SPI_Write_o => ADT7310_SPI_Write_o,
      SPI_ReadNext_o => ADT7310_SPI_ReadNext_o,
      SPI_Data_o => ADT7310_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310_SPI_Transmission_i,
      PeriodCounterPreset_i => ADT7310_PeriodCounterPreset_i,
      SPICounterPresetH_i => ADT7310_SPICounterPresetH_i,
      SPICounterPresetL_i => ADT7310_SPICounterPresetL_i,
      SensorValue_o => ADT7310_SensorValue_o,
      Threshold_i => ADT7310_Threshold_i,
      SPI_CPOL_o => ADT7310_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310_SPI_LSBFE_o
    );

  ADT7310_Reset_n_i <= Reset_n_i when Select_i = "000" else 
                       '0';
  ADT7310_Clk_i <= Clk_i when Select_i = "000" else 
                   '0';
  ADT7310_Enable_i <= ReconfModuleIn_s(0) when Select_i = "000" else 
                      '0';
  ReconfModuleIRQs_s <= '0' & '0' & '0' & '0' & ADT7310_CpuIntr_o when Select_i = "000" else 
                        '0' & '0' & '0' & '0' & MAX6682_CpuIntr_o when Select_i = "001" else 
                        '0' & '0' & '0' & '0' & MAX6682Mean_CpuIntr_o when Select_i = "010" else 
                        '0' & '0' & '0' & '0' & ADT7410_CpuIntr_o when Select_i = "011" else 
                        '0' & '0' & '0' & '0' & ExtADC_CpuIntr_o when Select_i = "100" else 
                        '0' & '0' & '0' & '0' & SlowADT7410_CpuIntr_o when Select_i = "101" else 
                        "00000";
  Outputs_o <= '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310_ADT7310CS_n_o when Select_i = "000" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & MAX6682_MAX6682CS_n_o when Select_i = "001" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & MAX6682Mean_MAX6682CS_n_o when Select_i = "010" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADC_SensorStart_o & ExtADC_SensorPower_o when Select_i = "100" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADC_SensorStart_o & ExtADC_SensorPower_o when Select_i = "100" else 
               "00000000";
  ADT7310_SPI_Data_i <= SPI_DataOut when Select_i = "000" else 
                        "00000000";
  SPI_Write <= ADT7310_SPI_Write_o when Select_i = "000" else 
               MAX6682_SPI_Write_o when Select_i = "001" else 
               MAX6682Mean_SPI_Write_o when Select_i = "010" else 
               '0';
  SPI_ReadNext <= ADT7310_SPI_ReadNext_o when Select_i = "000" else 
                  MAX6682_SPI_ReadNext_o when Select_i = "001" else 
                  MAX6682Mean_SPI_ReadNext_o when Select_i = "010" else 
                  '0';
  SPI_DataIn <= ADT7310_SPI_Data_o when Select_i = "000" else 
                MAX6682_SPI_Data_o when Select_i = "001" else 
                MAX6682Mean_SPI_Data_o when Select_i = "010" else 
                "00000000";
  ADT7310_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "000" else 
                            '0';
  ADT7310_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "000" else 
                             '0';
  ADT7310_SPI_Transmission_i <= SPI_Transmission when Select_i = "000" else 
                                '0';
  ADT7310_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "000" else 
                                   "0000000000000000";
  ADT7310_SPICounterPresetH_i <= ParamWordIn1_i when Select_i = "000" else 
                                 "0000000000000000";
  ADT7310_SPICounterPresetL_i <= ParamWordIn2_i when Select_i = "000" else 
                                 "0000000000000000";
  ADT7310_SensorValue_o <= ParamWordOut0_o when Select_i = "000" else 
                           "0000000000000000";
  ADT7310_Threshold_i <= ParamWordIn3_i when Select_i = "000" else 
                         "0000000000000000";
  SPI_CPOL <= ADT7310_SPI_CPOL_o when Select_i = "000" else 
              MAX6682_SPI_CPOL_o when Select_i = "001" else 
              MAX6682Mean_SPI_CPOL_o when Select_i = "010" else 
              '0';
  SPI_CPHA <= ADT7310_SPI_CPHA_o when Select_i = "000" else 
              MAX6682_SPI_CPHA_o when Select_i = "001" else 
              MAX6682Mean_SPI_CPHA_o when Select_i = "010" else 
              '0';
  SPI_LSBFE <= ADT7310_SPI_LSBFE_o when Select_i = "000" else 
               MAX6682_SPI_LSBFE_o when Select_i = "001" else 
               MAX6682Mean_SPI_LSBFE_o when Select_i = "010" else 
               '0';
  SPI_SPPR_SPR <= "00000000" when Select_i = "000" else 
                  "00000000" when Select_i = "001" else 
                  "00000000" when Select_i = "010" else 
                  "00000000";


  MAX6682_1: MAX6682
    port map (
      Reset_n_i => MAX6682_Reset_n_i,
      Clk_i => MAX6682_Clk_i,
      Enable_i => MAX6682_Enable_i,
      CpuIntr_o => MAX6682_CpuIntr_o,
      MAX6682CS_n_o => MAX6682_MAX6682CS_n_o,
      SPI_Data_i => MAX6682_SPI_Data_i,
      SPI_Write_o => MAX6682_SPI_Write_o,
      SPI_ReadNext_o => MAX6682_SPI_ReadNext_o,
      SPI_Data_o => MAX6682_SPI_Data_o,
      SPI_FIFOFull_i => MAX6682_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => MAX6682_SPI_FIFOEmpty_i,
      SPI_Transmission_i => MAX6682_SPI_Transmission_i,
      PeriodCounterPresetH_i => MAX6682_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => MAX6682_PeriodCounterPresetL_i,
      SensorValue_o => MAX6682_SensorValue_o,
      Threshold_i => MAX6682_Threshold_i,
      SPI_CPOL_o => MAX6682_SPI_CPOL_o,
      SPI_CPHA_o => MAX6682_SPI_CPHA_o,
      SPI_LSBFE_o => MAX6682_SPI_LSBFE_o
    );

  MAX6682_Reset_n_i <= Reset_n_i when Select_i = "001" else 
                       '0';
  MAX6682_Clk_i <= Clk_i when Select_i = "001" else 
                   '0';
  MAX6682_Enable_i <= ReconfModuleIn_s(0) when Select_i = "001" else 
                      '0';
  MAX6682_SPI_Data_i <= SPI_DataOut when Select_i = "001" else 
                        "00000000";
  MAX6682_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "001" else 
                            '0';
  MAX6682_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "001" else 
                             '0';
  MAX6682_SPI_Transmission_i <= SPI_Transmission when Select_i = "001" else 
                                '0';
  MAX6682_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "001" else 
                                    "0000000000000000";
  MAX6682_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "001" else 
                                    "0000000000000000";
  MAX6682_SensorValue_o <= ParamWordOut0_o when Select_i = "001" else 
                           "0000000000000000";
  MAX6682_Threshold_i <= ParamWordIn2_i when Select_i = "001" else 
                         "0000000000000000";


  MAX6682Mean_1: MAX6682Mean
    port map (
      Reset_n_i => MAX6682Mean_Reset_n_i,
      Clk_i => MAX6682Mean_Clk_i,
      Enable_i => MAX6682Mean_Enable_i,
      CpuIntr_o => MAX6682Mean_CpuIntr_o,
      MAX6682CS_n_o => MAX6682Mean_MAX6682CS_n_o,
      SPI_Data_i => MAX6682Mean_SPI_Data_i,
      SPI_Write_o => MAX6682Mean_SPI_Write_o,
      SPI_ReadNext_o => MAX6682Mean_SPI_ReadNext_o,
      SPI_Data_o => MAX6682Mean_SPI_Data_o,
      SPI_FIFOFull_i => MAX6682Mean_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => MAX6682Mean_SPI_FIFOEmpty_i,
      SPI_Transmission_i => MAX6682Mean_SPI_Transmission_i,
      PauseCounterPreset_i => MAX6682Mean_PauseCounterPreset_i,
      PeriodCounterPresetH_i => MAX6682Mean_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => MAX6682Mean_PeriodCounterPresetL_i,
      SensorValue_o => MAX6682Mean_SensorValue_o,
      Threshold_i => MAX6682Mean_Threshold_i,
      SPI_CPOL_o => MAX6682Mean_SPI_CPOL_o,
      SPI_CPHA_o => MAX6682Mean_SPI_CPHA_o,
      SPI_LSBFE_o => MAX6682Mean_SPI_LSBFE_o
    );

  MAX6682Mean_Reset_n_i <= Reset_n_i when Select_i = "010" else 
                           '0';
  MAX6682Mean_Clk_i <= Clk_i when Select_i = "010" else 
                       '0';
  MAX6682Mean_Enable_i <= ReconfModuleIn_s(0) when Select_i = "010" else 
                          '0';
  MAX6682Mean_SPI_Data_i <= SPI_DataOut when Select_i = "010" else 
                            "00000000";
  MAX6682Mean_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "010" else 
                                '0';
  MAX6682Mean_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "010" else 
                                 '0';
  MAX6682Mean_SPI_Transmission_i <= SPI_Transmission when Select_i = "010" else 
                                    '0';
  MAX6682Mean_PauseCounterPreset_i <= ParamWordIn0_i when Select_i = "010" else 
                                      "0000000000000000";
  MAX6682Mean_PeriodCounterPresetH_i <= ParamWordIn1_i when Select_i = "010" else 
                                        "0000000000000000";
  MAX6682Mean_PeriodCounterPresetL_i <= ParamWordIn2_i when Select_i = "010" else 
                                        "0000000000000000";
  MAX6682Mean_SensorValue_o <= ParamWordOut0_o when Select_i = "010" else 
                               "0000000000000000";
  MAX6682Mean_Threshold_i <= ParamWordIn3_i when Select_i = "010" else 
                             "0000000000000000";


  ADT7410_1: ADT7410
    port map (
      Reset_n_i => ADT7410_Reset_n_i,
      Clk_i => ADT7410_Clk_i,
      Enable_i => ADT7410_Enable_i,
      CpuIntr_o => ADT7410_CpuIntr_o,
      I2C_ReceiveSend_n_o => ADT7410_I2C_ReceiveSend_n_o,
      I2C_ReadCount_o => ADT7410_I2C_ReadCount_o,
      I2C_StartProcess_o => ADT7410_I2C_StartProcess_o,
      I2C_Busy_i => ADT7410_I2C_Busy_i,
      I2C_FIFOReadNext_o => ADT7410_I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => ADT7410_I2C_FIFOWrite_o,
      I2C_Data_o => ADT7410_I2C_Data_o,
      I2C_Data_i => ADT7410_I2C_Data_i,
      I2C_Error_i => ADT7410_I2C_Error_i,
      PeriodCounterPreset_i => ADT7410_PeriodCounterPreset_i,
      SensorValue_o => ADT7410_SensorValue_o,
      Threshold_i => ADT7410_Threshold_i,
      WaitCounterPreset_i => ADT7410_WaitCounterPreset_i
    );

  ADT7410_Reset_n_i <= Reset_n_i when Select_i = "011" else 
                       '0';
  ADT7410_Clk_i <= Clk_i when Select_i = "011" else 
                   '0';
  ADT7410_Enable_i <= ReconfModuleIn_s(0) when Select_i = "011" else 
                      '0';
  I2C_ReceiveSend_n <= ADT7410_I2C_ReceiveSend_n_o when Select_i = "011" else 
                       SlowADT7410_I2C_ReceiveSend_n_o when Select_i = "101" else 
                       '0';
  I2C_ReadCount <= ADT7410_I2C_ReadCount_o(3 downto 0) when Select_i = "011" else 
                   SlowADT7410_I2C_ReadCount_o(3 downto 0) when Select_i = "101" else 
                   "0000";
  I2C_StartProcess <= ADT7410_I2C_StartProcess_o when Select_i = "011" else 
                      SlowADT7410_I2C_StartProcess_o when Select_i = "101" else 
                      '0';
  ADT7410_I2C_Busy_i <= I2C_Busy when Select_i = "011" else 
                        '0';
  I2C_FIFOReadNext <= ADT7410_I2C_FIFOReadNext_o when Select_i = "011" else 
                      SlowADT7410_I2C_FIFOReadNext_o when Select_i = "101" else 
                      '0';
  I2C_FIFOWrite <= ADT7410_I2C_FIFOWrite_o when Select_i = "011" else 
                   SlowADT7410_I2C_FIFOWrite_o when Select_i = "101" else 
                   '0';
  I2C_DataIn <= ADT7410_I2C_Data_o when Select_i = "011" else 
                SlowADT7410_I2C_Data_o when Select_i = "101" else 
                "00000000";
  ADT7410_I2C_Data_i <= I2C_DataOut when Select_i = "011" else 
                        "00000000";
  ADT7410_I2C_Error_i <= I2C_Error when Select_i = "011" else 
                         '0';
  ADT7410_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "011" else 
                                   "0000000000000000";
  ADT7410_SensorValue_o <= ParamWordOut0_o when Select_i = "011" else 
                           "0000000000000000";
  ADT7410_Threshold_i <= ParamWordIn1_i when Select_i = "011" else 
                         "0000000000000000";
  ADT7410_WaitCounterPreset_i <= ParamWordIn2_i when Select_i = "011" else 
                                 "0000000000000000";
  I2C_F100_400_n <= '1' when Select_i = "011" else 
                    '1' when Select_i = "101" else 
                    '0';
  I2C_Divider800 <= "0000000001111100" when Select_i = "011" else 
                    "0000000001111100" when Select_i = "101" else 
                    "0000000000000000";


  ExtADC_1: ExtADC
    port map (
      Reset_n_i => ExtADC_Reset_n_i,
      Clk_i => ExtADC_Clk_i,
      Enable_i => ExtADC_Enable_i,
      CpuIntr_o => ExtADC_CpuIntr_o,
      SensorPower_o => ExtADC_SensorPower_o,
      SensorStart_o => ExtADC_SensorStart_o,
      SensorReady_i => ExtADC_SensorReady_i,
      AdcStart_o => ExtADC_AdcStart_o,
      AdcDone_i => ExtADC_AdcDone_i,
      AdcValue_i => ExtADC_AdcValue_i,
      PeriodCounterPreset_i => ExtADC_PeriodCounterPreset_i,
      SensorValue_o => ExtADC_SensorValue_o,
      Threshold_i => ExtADC_Threshold_i
    );

  ExtADC_Reset_n_i <= Reset_n_i when Select_i = "100" else 
                      '0';
  ExtADC_Clk_i <= Clk_i when Select_i = "100" else 
                  '0';
  ExtADC_Enable_i <= ReconfModuleIn_s(0) when Select_i = "100" else 
                     '0';
  ExtADC_SensorReady_i <= Inputs_i(0) when Select_i = "100" else 
                          '0';
  AdcDoConvert_o <= ExtADC_AdcStart_o when Select_i = "100" else 
                    '0';
  ExtADC_AdcDone_i <= AdcConvComplete_i when Select_i = "100" else 
                      '0';
  ExtADC_AdcValue_i <= "000000" & AdcValue_i when Select_i = "100" else 
                       "0000000000000000";
  ExtADC_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "100" else 
                                  "0000000000000000";
  ExtADC_SensorValue_o <= ParamWordOut0_o when Select_i = "100" else 
                          "0000000000000000";
  ExtADC_Threshold_i <= ParamWordIn1_i when Select_i = "100" else 
                        "0000000000000000";


  SlowADT7410_1: SlowADT7410
    port map (
      Reset_n_i => SlowADT7410_Reset_n_i,
      Clk_i => SlowADT7410_Clk_i,
      Enable_i => SlowADT7410_Enable_i,
      CpuIntr_o => SlowADT7410_CpuIntr_o,
      I2C_ReceiveSend_n_o => SlowADT7410_I2C_ReceiveSend_n_o,
      I2C_ReadCount_o => SlowADT7410_I2C_ReadCount_o,
      I2C_StartProcess_o => SlowADT7410_I2C_StartProcess_o,
      I2C_Busy_i => SlowADT7410_I2C_Busy_i,
      I2C_FIFOReadNext_o => SlowADT7410_I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => SlowADT7410_I2C_FIFOWrite_o,
      I2C_Data_o => SlowADT7410_I2C_Data_o,
      I2C_Data_i => SlowADT7410_I2C_Data_i,
      I2C_Error_i => SlowADT7410_I2C_Error_i,
      PeriodCounterPresetH_i => SlowADT7410_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => SlowADT7410_PeriodCounterPresetL_i,
      SensorValue_o => SlowADT7410_SensorValue_o,
      Threshold_i => SlowADT7410_Threshold_i,
      WaitCounterPresetH_i => SlowADT7410_WaitCounterPresetH_i,
      WaitCounterPresetL_i => SlowADT7410_WaitCounterPresetL_i
    );

  SlowADT7410_Reset_n_i <= Reset_n_i when Select_i = "101" else 
                           '0';
  SlowADT7410_Clk_i <= Clk_i when Select_i = "101" else 
                       '0';
  SlowADT7410_Enable_i <= ReconfModuleIn_s(0) when Select_i = "101" else 
                          '0';
  SlowADT7410_I2C_Busy_i <= I2C_Busy when Select_i = "101" else 
                            '0';
  SlowADT7410_I2C_Data_i <= I2C_DataOut when Select_i = "101" else 
                            "00000000";
  SlowADT7410_I2C_Error_i <= I2C_Error when Select_i = "101" else 
                             '0';
  SlowADT7410_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "101" else 
                                        "0000000000000000";
  SlowADT7410_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "101" else 
                                        "0000000000000000";
  SlowADT7410_SensorValue_o <= ParamWordOut0_o when Select_i = "101" else 
                               "0000000000000000";
  SlowADT7410_Threshold_i <= ParamWordIn2_i when Select_i = "101" else 
                             "0000000000000000";
  SlowADT7410_WaitCounterPresetH_i <= ParamWordIn3_i when Select_i = "101" else 
                                      "0000000000000000";
  SlowADT7410_WaitCounterPresetL_i <= ParamWordIn4_i when Select_i = "101" else 
                                      "0000000000000000";

end struct;
