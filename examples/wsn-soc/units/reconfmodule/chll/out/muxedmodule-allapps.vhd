library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MuxedModuleAllApps is
  port (
    Select_i : in std_logic_vector(4 downto 0);
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
    ParamWordIn4_i : in std_logic_vector(15 downto 0);
    ParamWordOut3_o : in std_logic_vector(15 downto 0)
  );
end MuxedModuleAllApps;

architecture struct of MuxedModuleAllApps is

  component ADT7310P16LS16L
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
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P16LS32L
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

  component ADT7310P16S16
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
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32LS16L
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32LS16
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32LS32L
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPresetH_i : in std_logic_vector(15 downto 0);
      SPICounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32S16L
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32S16
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPreset_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

  component ADT7310P32S32
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
      PeriodCounterPresetH_i : in std_logic_vector(15 downto 0);
      PeriodCounterPresetL_i : in std_logic_vector(15 downto 0);
      SPICounterPresetH_i : in std_logic_vector(15 downto 0);
      SPICounterPresetL_i : in std_logic_vector(15 downto 0);
      SensorValue_o : out std_logic_vector(15 downto 0);
      Threshold_i : in std_logic_vector(15 downto 0);
      SPI_CPOL_o : out std_logic;
      SPI_CPHA_o : out std_logic;
      SPI_LSBFE_o : out std_logic
    );
  end component;

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

  component ExtADCSimple
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
      SensorValue_o : out std_logic_vector(15 downto 0)
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

  component ExtIntr
    port (
      ExtIntrOut_o : out std_logic;
      ExtIntrIn_i : in std_logic
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

  component TMP421
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
      SensorValueL_o : out std_logic_vector(15 downto 0);
      SensorValueR_o : out std_logic_vector(15 downto 0)
    );
  end component;

  component blinki
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      LED_o : out std_logic;
      PeriodH_i : in std_logic_vector(15 downto 0);
      PeriodL_i : in std_logic_vector(15 downto 0)
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
  signal ADT7310P32S16_Reset_n_i : std_logic;
  signal ADT7310P32S16_Clk_i : std_logic;
  signal ADT7310P32S16_Enable_i : std_logic;
  signal ADT7310P32S16_CpuIntr_o : std_logic;
  signal ADT7310P32S16_ADT7310CS_n_o : std_logic;
  signal ADT7310P32S16_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32S16_SPI_Write_o : std_logic;
  signal ADT7310P32S16_SPI_ReadNext_o : std_logic;
  signal ADT7310P32S16_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32S16_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32S16_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32S16_SPI_Transmission_i : std_logic;
  signal ADT7310P32S16_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32S16_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16_SPI_CPOL_o : std_logic;
  signal ADT7310P32S16_SPI_CPHA_o : std_logic;
  signal ADT7310P32S16_SPI_LSBFE_o : std_logic;
  signal ExtADCSimple_Reset_n_i : std_logic;
  signal ExtADCSimple_Clk_i : std_logic;
  signal ExtADCSimple_Enable_i : std_logic;
  signal ExtADCSimple_CpuIntr_o : std_logic;
  signal ExtADCSimple_SensorPower_o : std_logic;
  signal ExtADCSimple_SensorStart_o : std_logic;
  signal ExtADCSimple_SensorReady_i : std_logic;
  signal ExtADCSimple_AdcStart_o : std_logic;
  signal ExtADCSimple_AdcDone_i : std_logic;
  signal ExtADCSimple_AdcValue_i : std_logic_vector(15 downto 0);
  signal ExtADCSimple_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ExtADCSimple_SensorValue_o : std_logic_vector(15 downto 0);
  signal TMP421_Reset_n_i : std_logic;
  signal TMP421_Clk_i : std_logic;
  signal TMP421_Enable_i : std_logic;
  signal TMP421_CpuIntr_o : std_logic;
  signal TMP421_I2C_ReceiveSend_n_o : std_logic;
  signal TMP421_I2C_ReadCount_o : std_logic_vector(7 downto 0);
  signal TMP421_I2C_StartProcess_o : std_logic;
  signal TMP421_I2C_Busy_i : std_logic;
  signal TMP421_I2C_FIFOReadNext_o : std_logic;
  signal TMP421_I2C_FIFOWrite_o : std_logic;
  signal TMP421_I2C_Data_o : std_logic_vector(7 downto 0);
  signal TMP421_I2C_Data_i : std_logic_vector(7 downto 0);
  signal TMP421_I2C_Error_i : std_logic;
  signal TMP421_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal TMP421_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal TMP421_SensorValueL_o : std_logic_vector(15 downto 0);
  signal TMP421_SensorValueR_o : std_logic_vector(15 downto 0);
  signal ADT7310P16LS16L_Reset_n_i : std_logic;
  signal ADT7310P16LS16L_Clk_i : std_logic;
  signal ADT7310P16LS16L_Enable_i : std_logic;
  signal ADT7310P16LS16L_CpuIntr_o : std_logic;
  signal ADT7310P16LS16L_ADT7310CS_n_o : std_logic;
  signal ADT7310P16LS16L_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P16LS16L_SPI_Write_o : std_logic;
  signal ADT7310P16LS16L_SPI_ReadNext_o : std_logic;
  signal ADT7310P16LS16L_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P16LS16L_SPI_FIFOFull_i : std_logic;
  signal ADT7310P16LS16L_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P16LS16L_SPI_Transmission_i : std_logic;
  signal ADT7310P16LS16L_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS16L_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS16L_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P16LS16L_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS16L_SPI_CPOL_o : std_logic;
  signal ADT7310P16LS16L_SPI_CPHA_o : std_logic;
  signal ADT7310P16LS16L_SPI_LSBFE_o : std_logic;
  signal blinki_Reset_n_i : std_logic;
  signal blinki_Clk_i : std_logic;
  signal blinki_LED_o : std_logic;
  signal blinki_PeriodH_i : std_logic_vector(15 downto 0);
  signal blinki_PeriodL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_Reset_n_i : std_logic;
  signal ADT7310P32LS16L_Clk_i : std_logic;
  signal ADT7310P32LS16L_Enable_i : std_logic;
  signal ADT7310P32LS16L_CpuIntr_o : std_logic;
  signal ADT7310P32LS16L_ADT7310CS_n_o : std_logic;
  signal ADT7310P32LS16L_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32LS16L_SPI_Write_o : std_logic;
  signal ADT7310P32LS16L_SPI_ReadNext_o : std_logic;
  signal ADT7310P32LS16L_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32LS16L_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32LS16L_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32LS16L_SPI_Transmission_i : std_logic;
  signal ADT7310P32LS16L_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16L_SPI_CPOL_o : std_logic;
  signal ADT7310P32LS16L_SPI_CPHA_o : std_logic;
  signal ADT7310P32LS16L_SPI_LSBFE_o : std_logic;
  signal ADT7310P16LS32L_Reset_n_i : std_logic;
  signal ADT7310P16LS32L_Clk_i : std_logic;
  signal ADT7310P16LS32L_Enable_i : std_logic;
  signal ADT7310P16LS32L_CpuIntr_o : std_logic;
  signal ADT7310P16LS32L_ADT7310CS_n_o : std_logic;
  signal ADT7310P16LS32L_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P16LS32L_SPI_Write_o : std_logic;
  signal ADT7310P16LS32L_SPI_ReadNext_o : std_logic;
  signal ADT7310P16LS32L_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P16LS32L_SPI_FIFOFull_i : std_logic;
  signal ADT7310P16LS32L_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P16LS32L_SPI_Transmission_i : std_logic;
  signal ADT7310P16LS32L_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS32L_SPICounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS32L_SPICounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS32L_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P16LS32L_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P16LS32L_SPI_CPOL_o : std_logic;
  signal ADT7310P16LS32L_SPI_CPHA_o : std_logic;
  signal ADT7310P16LS32L_SPI_LSBFE_o : std_logic;
  signal ADT7310P32LS32L_Reset_n_i : std_logic;
  signal ADT7310P32LS32L_Clk_i : std_logic;
  signal ADT7310P32LS32L_Enable_i : std_logic;
  signal ADT7310P32LS32L_CpuIntr_o : std_logic;
  signal ADT7310P32LS32L_ADT7310CS_n_o : std_logic;
  signal ADT7310P32LS32L_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32LS32L_SPI_Write_o : std_logic;
  signal ADT7310P32LS32L_SPI_ReadNext_o : std_logic;
  signal ADT7310P32LS32L_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32LS32L_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32LS32L_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32LS32L_SPI_Transmission_i : std_logic;
  signal ADT7310P32LS32L_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_SPICounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_SPICounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS32L_SPI_CPOL_o : std_logic;
  signal ADT7310P32LS32L_SPI_CPHA_o : std_logic;
  signal ADT7310P32LS32L_SPI_LSBFE_o : std_logic;
  signal ADT7310P32LS16_Reset_n_i : std_logic;
  signal ADT7310P32LS16_Clk_i : std_logic;
  signal ADT7310P32LS16_Enable_i : std_logic;
  signal ADT7310P32LS16_CpuIntr_o : std_logic;
  signal ADT7310P32LS16_ADT7310CS_n_o : std_logic;
  signal ADT7310P32LS16_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32LS16_SPI_Write_o : std_logic;
  signal ADT7310P32LS16_SPI_ReadNext_o : std_logic;
  signal ADT7310P32LS16_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32LS16_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32LS16_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32LS16_SPI_Transmission_i : std_logic;
  signal ADT7310P32LS16_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32LS16_SPI_CPOL_o : std_logic;
  signal ADT7310P32LS16_SPI_CPHA_o : std_logic;
  signal ADT7310P32LS16_SPI_LSBFE_o : std_logic;
  signal ADT7310P32S32_Reset_n_i : std_logic;
  signal ADT7310P32S32_Clk_i : std_logic;
  signal ADT7310P32S32_Enable_i : std_logic;
  signal ADT7310P32S32_CpuIntr_o : std_logic;
  signal ADT7310P32S32_ADT7310CS_n_o : std_logic;
  signal ADT7310P32S32_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32S32_SPI_Write_o : std_logic;
  signal ADT7310P32S32_SPI_ReadNext_o : std_logic;
  signal ADT7310P32S32_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32S32_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32S32_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32S32_SPI_Transmission_i : std_logic;
  signal ADT7310P32S32_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_SPICounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_SPICounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S32_SPI_CPOL_o : std_logic;
  signal ADT7310P32S32_SPI_CPHA_o : std_logic;
  signal ADT7310P32S32_SPI_LSBFE_o : std_logic;
  signal ADT7310P32S16L_Reset_n_i : std_logic;
  signal ADT7310P32S16L_Clk_i : std_logic;
  signal ADT7310P32S16L_Enable_i : std_logic;
  signal ADT7310P32S16L_CpuIntr_o : std_logic;
  signal ADT7310P32S16L_ADT7310CS_n_o : std_logic;
  signal ADT7310P32S16L_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P32S16L_SPI_Write_o : std_logic;
  signal ADT7310P32S16L_SPI_ReadNext_o : std_logic;
  signal ADT7310P32S16L_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P32S16L_SPI_FIFOFull_i : std_logic;
  signal ADT7310P32S16L_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P32S16L_SPI_Transmission_i : std_logic;
  signal ADT7310P32S16L_PeriodCounterPresetH_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16L_PeriodCounterPresetL_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16L_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16L_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P32S16L_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P32S16L_SPI_CPOL_o : std_logic;
  signal ADT7310P32S16L_SPI_CPHA_o : std_logic;
  signal ADT7310P32S16L_SPI_LSBFE_o : std_logic;
  signal ExtIntr_ExtIntrOut_o : std_logic;
  signal ExtIntr_ExtIntrIn_i : std_logic;
  signal ADT7310P16S16_Reset_n_i : std_logic;
  signal ADT7310P16S16_Clk_i : std_logic;
  signal ADT7310P16S16_Enable_i : std_logic;
  signal ADT7310P16S16_CpuIntr_o : std_logic;
  signal ADT7310P16S16_ADT7310CS_n_o : std_logic;
  signal ADT7310P16S16_SPI_Data_i : std_logic_vector(7 downto 0);
  signal ADT7310P16S16_SPI_Write_o : std_logic;
  signal ADT7310P16S16_SPI_ReadNext_o : std_logic;
  signal ADT7310P16S16_SPI_Data_o : std_logic_vector(7 downto 0);
  signal ADT7310P16S16_SPI_FIFOFull_i : std_logic;
  signal ADT7310P16S16_SPI_FIFOEmpty_i : std_logic;
  signal ADT7310P16S16_SPI_Transmission_i : std_logic;
  signal ADT7310P16S16_PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P16S16_SPICounterPreset_i : std_logic_vector(15 downto 0);
  signal ADT7310P16S16_SensorValue_o : std_logic_vector(15 downto 0);
  signal ADT7310P16S16_Threshold_i : std_logic_vector(15 downto 0);
  signal ADT7310P16S16_SPI_CPOL_o : std_logic;
  signal ADT7310P16S16_SPI_CPHA_o : std_logic;
  signal ADT7310P16S16_SPI_LSBFE_o : std_logic;

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

  ADT7310_Reset_n_i <= Reset_n_i when Select_i = "00000" else 
                       '0';
  ADT7310_Clk_i <= Clk_i when Select_i = "00000" else 
                   '0';
  ADT7310_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00000" else 
                      '0';
  ReconfModuleIRQs_s <= '0' & '0' & '0' & '0' & ADT7310_CpuIntr_o when Select_i = "00000" else 
                        '0' & '0' & '0' & '0' & MAX6682_CpuIntr_o when Select_i = "00001" else 
                        '0' & '0' & '0' & '0' & MAX6682Mean_CpuIntr_o when Select_i = "00010" else 
                        '0' & '0' & '0' & '0' & ADT7410_CpuIntr_o when Select_i = "00011" else 
                        '0' & '0' & '0' & '0' & ExtADC_CpuIntr_o when Select_i = "00100" else 
                        '0' & '0' & '0' & '0' & SlowADT7410_CpuIntr_o when Select_i = "00101" else 
                        '0' & '0' & '0' & '0' & ADT7310P32S16_CpuIntr_o when Select_i = "00110" else 
                        '0' & '0' & '0' & '0' & ExtADCSimple_CpuIntr_o when Select_i = "00111" else 
                        '0' & '0' & '0' & '0' & TMP421_CpuIntr_o when Select_i = "01000" else 
                        '0' & '0' & '0' & '0' & ADT7310P16LS16L_CpuIntr_o when Select_i = "01001" else 
                        '0' & '0' & '0' & '0' & ADT7310P32LS16L_CpuIntr_o when Select_i = "01011" else 
                        '0' & '0' & '0' & '0' & ADT7310P16LS32L_CpuIntr_o when Select_i = "01100" else 
                        '0' & '0' & '0' & '0' & ADT7310P32LS32L_CpuIntr_o when Select_i = "01101" else 
                        '0' & '0' & '0' & '0' & ADT7310P32LS16_CpuIntr_o when Select_i = "01110" else 
                        '0' & '0' & '0' & '0' & ADT7310P32S32_CpuIntr_o when Select_i = "01111" else 
                        '0' & '0' & '0' & '0' & ADT7310P32S16L_CpuIntr_o when Select_i = "10000" else 
                        '0' & '0' & '0' & ExtIntr_ExtIntrOut_o & '0' when Select_i = "10001" else 
                        '0' & '0' & '0' & '0' & ADT7310P16S16_CpuIntr_o when Select_i = "10010" else 
                        "00000";
  Outputs_o <= '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310_ADT7310CS_n_o when Select_i = "00000" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & MAX6682_MAX6682CS_n_o when Select_i = "00001" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & MAX6682Mean_MAX6682CS_n_o when Select_i = "00010" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADC_SensorStart_o & ExtADC_SensorPower_o when Select_i = "00100" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADC_SensorStart_o & ExtADC_SensorPower_o when Select_i = "00100" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32S16_ADT7310CS_n_o when Select_i = "00110" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADCSimple_SensorStart_o & ExtADCSimple_SensorPower_o when Select_i = "00111" else 
               '0' & '0' & '0' & '0' & '0' & '0' & ExtADCSimple_SensorStart_o & ExtADCSimple_SensorPower_o when Select_i = "00111" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P16LS16L_ADT7310CS_n_o when Select_i = "01001" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & blinki_LED_o when Select_i = "01010" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32LS16L_ADT7310CS_n_o when Select_i = "01011" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P16LS32L_ADT7310CS_n_o when Select_i = "01100" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32LS32L_ADT7310CS_n_o when Select_i = "01101" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32LS16_ADT7310CS_n_o when Select_i = "01110" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32S32_ADT7310CS_n_o when Select_i = "01111" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P32S16L_ADT7310CS_n_o when Select_i = "10000" else 
               '0' & '0' & '0' & '0' & '0' & '0' & '0' & ADT7310P16S16_ADT7310CS_n_o when Select_i = "10010" else 
               "00000000";
  ADT7310_SPI_Data_i <= SPI_DataOut when Select_i = "00000" else 
                        "00000000";
  SPI_Write <= ADT7310_SPI_Write_o when Select_i = "00000" else 
               MAX6682_SPI_Write_o when Select_i = "00001" else 
               MAX6682Mean_SPI_Write_o when Select_i = "00010" else 
               ADT7310P32S16_SPI_Write_o when Select_i = "00110" else 
               ADT7310P16LS16L_SPI_Write_o when Select_i = "01001" else 
               ADT7310P32LS16L_SPI_Write_o when Select_i = "01011" else 
               ADT7310P16LS32L_SPI_Write_o when Select_i = "01100" else 
               ADT7310P32LS32L_SPI_Write_o when Select_i = "01101" else 
               ADT7310P32LS16_SPI_Write_o when Select_i = "01110" else 
               ADT7310P32S32_SPI_Write_o when Select_i = "01111" else 
               ADT7310P32S16L_SPI_Write_o when Select_i = "10000" else 
               ADT7310P16S16_SPI_Write_o when Select_i = "10010" else 
               '0';
  SPI_ReadNext <= ADT7310_SPI_ReadNext_o when Select_i = "00000" else 
                  MAX6682_SPI_ReadNext_o when Select_i = "00001" else 
                  MAX6682Mean_SPI_ReadNext_o when Select_i = "00010" else 
                  ADT7310P32S16_SPI_ReadNext_o when Select_i = "00110" else 
                  ADT7310P16LS16L_SPI_ReadNext_o when Select_i = "01001" else 
                  ADT7310P32LS16L_SPI_ReadNext_o when Select_i = "01011" else 
                  ADT7310P16LS32L_SPI_ReadNext_o when Select_i = "01100" else 
                  ADT7310P32LS32L_SPI_ReadNext_o when Select_i = "01101" else 
                  ADT7310P32LS16_SPI_ReadNext_o when Select_i = "01110" else 
                  ADT7310P32S32_SPI_ReadNext_o when Select_i = "01111" else 
                  ADT7310P32S16L_SPI_ReadNext_o when Select_i = "10000" else 
                  ADT7310P16S16_SPI_ReadNext_o when Select_i = "10010" else 
                  '0';
  SPI_DataIn <= ADT7310_SPI_Data_o when Select_i = "00000" else 
                MAX6682_SPI_Data_o when Select_i = "00001" else 
                MAX6682Mean_SPI_Data_o when Select_i = "00010" else 
                ADT7310P32S16_SPI_Data_o when Select_i = "00110" else 
                ADT7310P16LS16L_SPI_Data_o when Select_i = "01001" else 
                ADT7310P32LS16L_SPI_Data_o when Select_i = "01011" else 
                ADT7310P16LS32L_SPI_Data_o when Select_i = "01100" else 
                ADT7310P32LS32L_SPI_Data_o when Select_i = "01101" else 
                ADT7310P32LS16_SPI_Data_o when Select_i = "01110" else 
                ADT7310P32S32_SPI_Data_o when Select_i = "01111" else 
                ADT7310P32S16L_SPI_Data_o when Select_i = "10000" else 
                ADT7310P16S16_SPI_Data_o when Select_i = "10010" else 
                "00000000";
  ADT7310_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "00000" else 
                            '0';
  ADT7310_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "00000" else 
                             '0';
  ADT7310_SPI_Transmission_i <= SPI_Transmission when Select_i = "00000" else 
                                '0';
  ADT7310_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "00000" else 
                                   "0000000000000000";
  ADT7310_SPICounterPresetH_i <= ParamWordIn1_i when Select_i = "00000" else 
                                 "0000000000000000";
  ADT7310_SPICounterPresetL_i <= ParamWordIn2_i when Select_i = "00000" else 
                                 "0000000000000000";
  ADT7310_SensorValue_o <= ParamWordOut0_o when Select_i = "00000" else 
                           "0000000000000000";
  ADT7310_Threshold_i <= ParamWordIn3_i when Select_i = "00000" else 
                         "0000000000000000";
  SPI_CPOL <= ADT7310_SPI_CPOL_o when Select_i = "00000" else 
              MAX6682_SPI_CPOL_o when Select_i = "00001" else 
              MAX6682Mean_SPI_CPOL_o when Select_i = "00010" else 
              ADT7310P32S16_SPI_CPOL_o when Select_i = "00110" else 
              ADT7310P16LS16L_SPI_CPOL_o when Select_i = "01001" else 
              ADT7310P32LS16L_SPI_CPOL_o when Select_i = "01011" else 
              ADT7310P16LS32L_SPI_CPOL_o when Select_i = "01100" else 
              ADT7310P32LS32L_SPI_CPOL_o when Select_i = "01101" else 
              ADT7310P32LS16_SPI_CPOL_o when Select_i = "01110" else 
              ADT7310P32S32_SPI_CPOL_o when Select_i = "01111" else 
              ADT7310P32S16L_SPI_CPOL_o when Select_i = "10000" else 
              ADT7310P16S16_SPI_CPOL_o when Select_i = "10010" else 
              '0';
  SPI_CPHA <= ADT7310_SPI_CPHA_o when Select_i = "00000" else 
              MAX6682_SPI_CPHA_o when Select_i = "00001" else 
              MAX6682Mean_SPI_CPHA_o when Select_i = "00010" else 
              ADT7310P32S16_SPI_CPHA_o when Select_i = "00110" else 
              ADT7310P16LS16L_SPI_CPHA_o when Select_i = "01001" else 
              ADT7310P32LS16L_SPI_CPHA_o when Select_i = "01011" else 
              ADT7310P16LS32L_SPI_CPHA_o when Select_i = "01100" else 
              ADT7310P32LS32L_SPI_CPHA_o when Select_i = "01101" else 
              ADT7310P32LS16_SPI_CPHA_o when Select_i = "01110" else 
              ADT7310P32S32_SPI_CPHA_o when Select_i = "01111" else 
              ADT7310P32S16L_SPI_CPHA_o when Select_i = "10000" else 
              ADT7310P16S16_SPI_CPHA_o when Select_i = "10010" else 
              '0';
  SPI_LSBFE <= ADT7310_SPI_LSBFE_o when Select_i = "00000" else 
               MAX6682_SPI_LSBFE_o when Select_i = "00001" else 
               MAX6682Mean_SPI_LSBFE_o when Select_i = "00010" else 
               ADT7310P32S16_SPI_LSBFE_o when Select_i = "00110" else 
               ADT7310P16LS16L_SPI_LSBFE_o when Select_i = "01001" else 
               ADT7310P32LS16L_SPI_LSBFE_o when Select_i = "01011" else 
               ADT7310P16LS32L_SPI_LSBFE_o when Select_i = "01100" else 
               ADT7310P32LS32L_SPI_LSBFE_o when Select_i = "01101" else 
               ADT7310P32LS16_SPI_LSBFE_o when Select_i = "01110" else 
               ADT7310P32S32_SPI_LSBFE_o when Select_i = "01111" else 
               ADT7310P32S16L_SPI_LSBFE_o when Select_i = "10000" else 
               ADT7310P16S16_SPI_LSBFE_o when Select_i = "10010" else 
               '0';
  SPI_SPPR_SPR <= "00000000" when Select_i = "00000" else 
                  "00000000" when Select_i = "00001" else 
                  "00000000" when Select_i = "00010" else 
                  "00000000" when Select_i = "00110" else 
                  "00000000" when Select_i = "01001" else 
                  "00000000" when Select_i = "01011" else 
                  "00000000" when Select_i = "01100" else 
                  "00000000" when Select_i = "01101" else 
                  "00000000" when Select_i = "01110" else 
                  "00000000" when Select_i = "01111" else 
                  "00000000" when Select_i = "10000" else 
                  "00000000" when Select_i = "10010" else 
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

  MAX6682_Reset_n_i <= Reset_n_i when Select_i = "00001" else 
                       '0';
  MAX6682_Clk_i <= Clk_i when Select_i = "00001" else 
                   '0';
  MAX6682_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00001" else 
                      '0';
  MAX6682_SPI_Data_i <= SPI_DataOut when Select_i = "00001" else 
                        "00000000";
  MAX6682_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "00001" else 
                            '0';
  MAX6682_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "00001" else 
                             '0';
  MAX6682_SPI_Transmission_i <= SPI_Transmission when Select_i = "00001" else 
                                '0';
  MAX6682_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "00001" else 
                                    "0000000000000000";
  MAX6682_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "00001" else 
                                    "0000000000000000";
  MAX6682_SensorValue_o <= ParamWordOut0_o when Select_i = "00001" else 
                           "0000000000000000";
  MAX6682_Threshold_i <= ParamWordIn2_i when Select_i = "00001" else 
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

  MAX6682Mean_Reset_n_i <= Reset_n_i when Select_i = "00010" else 
                           '0';
  MAX6682Mean_Clk_i <= Clk_i when Select_i = "00010" else 
                       '0';
  MAX6682Mean_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00010" else 
                          '0';
  MAX6682Mean_SPI_Data_i <= SPI_DataOut when Select_i = "00010" else 
                            "00000000";
  MAX6682Mean_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "00010" else 
                                '0';
  MAX6682Mean_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "00010" else 
                                 '0';
  MAX6682Mean_SPI_Transmission_i <= SPI_Transmission when Select_i = "00010" else 
                                    '0';
  MAX6682Mean_PauseCounterPreset_i <= ParamWordIn0_i when Select_i = "00010" else 
                                      "0000000000000000";
  MAX6682Mean_PeriodCounterPresetH_i <= ParamWordIn1_i when Select_i = "00010" else 
                                        "0000000000000000";
  MAX6682Mean_PeriodCounterPresetL_i <= ParamWordIn2_i when Select_i = "00010" else 
                                        "0000000000000000";
  MAX6682Mean_SensorValue_o <= ParamWordOut0_o when Select_i = "00010" else 
                               "0000000000000000";
  MAX6682Mean_Threshold_i <= ParamWordIn3_i when Select_i = "00010" else 
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

  ADT7410_Reset_n_i <= Reset_n_i when Select_i = "00011" else 
                       '0';
  ADT7410_Clk_i <= Clk_i when Select_i = "00011" else 
                   '0';
  ADT7410_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00011" else 
                      '0';
  I2C_ReceiveSend_n <= ADT7410_I2C_ReceiveSend_n_o when Select_i = "00011" else 
                       SlowADT7410_I2C_ReceiveSend_n_o when Select_i = "00101" else 
                       TMP421_I2C_ReceiveSend_n_o when Select_i = "01000" else 
                       '0';
  I2C_ReadCount <= ADT7410_I2C_ReadCount_o(3 downto 0) when Select_i = "00011" else 
                   SlowADT7410_I2C_ReadCount_o(3 downto 0) when Select_i = "00101" else 
                   TMP421_I2C_ReadCount_o(3 downto 0) when Select_i = "01000" else 
                   "0000";
  I2C_StartProcess <= ADT7410_I2C_StartProcess_o when Select_i = "00011" else 
                      SlowADT7410_I2C_StartProcess_o when Select_i = "00101" else 
                      TMP421_I2C_StartProcess_o when Select_i = "01000" else 
                      '0';
  ADT7410_I2C_Busy_i <= I2C_Busy when Select_i = "00011" else 
                        '0';
  I2C_FIFOReadNext <= ADT7410_I2C_FIFOReadNext_o when Select_i = "00011" else 
                      SlowADT7410_I2C_FIFOReadNext_o when Select_i = "00101" else 
                      TMP421_I2C_FIFOReadNext_o when Select_i = "01000" else 
                      '0';
  I2C_FIFOWrite <= ADT7410_I2C_FIFOWrite_o when Select_i = "00011" else 
                   SlowADT7410_I2C_FIFOWrite_o when Select_i = "00101" else 
                   TMP421_I2C_FIFOWrite_o when Select_i = "01000" else 
                   '0';
  I2C_DataIn <= ADT7410_I2C_Data_o when Select_i = "00011" else 
                SlowADT7410_I2C_Data_o when Select_i = "00101" else 
                TMP421_I2C_Data_o when Select_i = "01000" else 
                "00000000";
  ADT7410_I2C_Data_i <= I2C_DataOut when Select_i = "00011" else 
                        "00000000";
  ADT7410_I2C_Error_i <= I2C_Error when Select_i = "00011" else 
                         '0';
  ADT7410_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "00011" else 
                                   "0000000000000000";
  ADT7410_SensorValue_o <= ParamWordOut0_o when Select_i = "00011" else 
                           "0000000000000000";
  ADT7410_Threshold_i <= ParamWordIn1_i when Select_i = "00011" else 
                         "0000000000000000";
  ADT7410_WaitCounterPreset_i <= ParamWordIn2_i when Select_i = "00011" else 
                                 "0000000000000000";
  I2C_F100_400_n <= '1' when Select_i = "00011" else 
                    '1' when Select_i = "00101" else 
                    '0' when Select_i = "01000" else 
                    '0';
  I2C_Divider800 <= "0000000001111100" when Select_i = "00011" else 
                    "0000000001111100" when Select_i = "00101" else 
                    "0000000000001100" when Select_i = "01000" else 
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

  ExtADC_Reset_n_i <= Reset_n_i when Select_i = "00100" else 
                      '0';
  ExtADC_Clk_i <= Clk_i when Select_i = "00100" else 
                  '0';
  ExtADC_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00100" else 
                     '0';
  ExtADC_SensorReady_i <= Inputs_i(0) when Select_i = "00100" else 
                          '0';
  AdcDoConvert_o <= ExtADC_AdcStart_o when Select_i = "00100" else 
                    ExtADCSimple_AdcStart_o when Select_i = "00111" else 
                    '0';
  ExtADC_AdcDone_i <= AdcConvComplete_i when Select_i = "00100" else 
                      '0';
  ExtADC_AdcValue_i <= "000000" & AdcValue_i when Select_i = "00100" else 
                       "0000000000000000";
  ExtADC_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "00100" else 
                                  "0000000000000000";
  ExtADC_SensorValue_o <= ParamWordOut0_o when Select_i = "00100" else 
                          "0000000000000000";
  ExtADC_Threshold_i <= ParamWordIn1_i when Select_i = "00100" else 
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

  SlowADT7410_Reset_n_i <= Reset_n_i when Select_i = "00101" else 
                           '0';
  SlowADT7410_Clk_i <= Clk_i when Select_i = "00101" else 
                       '0';
  SlowADT7410_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00101" else 
                          '0';
  SlowADT7410_I2C_Busy_i <= I2C_Busy when Select_i = "00101" else 
                            '0';
  SlowADT7410_I2C_Data_i <= I2C_DataOut when Select_i = "00101" else 
                            "00000000";
  SlowADT7410_I2C_Error_i <= I2C_Error when Select_i = "00101" else 
                             '0';
  SlowADT7410_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "00101" else 
                                        "0000000000000000";
  SlowADT7410_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "00101" else 
                                        "0000000000000000";
  SlowADT7410_SensorValue_o <= ParamWordOut0_o when Select_i = "00101" else 
                               "0000000000000000";
  SlowADT7410_Threshold_i <= ParamWordIn2_i when Select_i = "00101" else 
                             "0000000000000000";
  SlowADT7410_WaitCounterPresetH_i <= ParamWordIn3_i when Select_i = "00101" else 
                                      "0000000000000000";
  SlowADT7410_WaitCounterPresetL_i <= ParamWordIn4_i when Select_i = "00101" else 
                                      "0000000000000000";


  ADT7310P32S16_1: ADT7310P32S16
    port map (
      Reset_n_i => ADT7310P32S16_Reset_n_i,
      Clk_i => ADT7310P32S16_Clk_i,
      Enable_i => ADT7310P32S16_Enable_i,
      CpuIntr_o => ADT7310P32S16_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32S16_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32S16_SPI_Data_i,
      SPI_Write_o => ADT7310P32S16_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32S16_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32S16_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32S16_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32S16_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32S16_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32S16_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32S16_PeriodCounterPresetL_i,
      SPICounterPreset_i => ADT7310P32S16_SPICounterPreset_i,
      SensorValue_o => ADT7310P32S16_SensorValue_o,
      Threshold_i => ADT7310P32S16_Threshold_i,
      SPI_CPOL_o => ADT7310P32S16_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32S16_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32S16_SPI_LSBFE_o
    );

  ADT7310P32S16_Reset_n_i <= Reset_n_i when Select_i = "00110" else 
                             '0';
  ADT7310P32S16_Clk_i <= Clk_i when Select_i = "00110" else 
                         '0';
  ADT7310P32S16_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00110" else 
                            '0';
  ADT7310P32S16_SPI_Data_i <= SPI_DataOut when Select_i = "00110" else 
                              "00000000";
  ADT7310P32S16_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "00110" else 
                                  '0';
  ADT7310P32S16_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "00110" else 
                                   '0';
  ADT7310P32S16_SPI_Transmission_i <= SPI_Transmission when Select_i = "00110" else 
                                      '0';
  ADT7310P32S16_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "00110" else 
                                          "0000000000000000";
  ADT7310P32S16_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "00110" else 
                                          "0000000000000000";
  ADT7310P32S16_SPICounterPreset_i <= ParamWordIn2_i when Select_i = "00110" else 
                                      "0000000000000000";
  ADT7310P32S16_SensorValue_o <= ParamWordOut0_o when Select_i = "00110" else 
                                 "0000000000000000";
  ADT7310P32S16_Threshold_i <= ParamWordIn3_i when Select_i = "00110" else 
                               "0000000000000000";


  ExtADCSimple_1: ExtADCSimple
    port map (
      Reset_n_i => ExtADCSimple_Reset_n_i,
      Clk_i => ExtADCSimple_Clk_i,
      Enable_i => ExtADCSimple_Enable_i,
      CpuIntr_o => ExtADCSimple_CpuIntr_o,
      SensorPower_o => ExtADCSimple_SensorPower_o,
      SensorStart_o => ExtADCSimple_SensorStart_o,
      SensorReady_i => ExtADCSimple_SensorReady_i,
      AdcStart_o => ExtADCSimple_AdcStart_o,
      AdcDone_i => ExtADCSimple_AdcDone_i,
      AdcValue_i => ExtADCSimple_AdcValue_i,
      PeriodCounterPreset_i => ExtADCSimple_PeriodCounterPreset_i,
      SensorValue_o => ExtADCSimple_SensorValue_o
    );

  ExtADCSimple_Reset_n_i <= Reset_n_i when Select_i = "00111" else 
                            '0';
  ExtADCSimple_Clk_i <= Clk_i when Select_i = "00111" else 
                        '0';
  ExtADCSimple_Enable_i <= ReconfModuleIn_s(0) when Select_i = "00111" else 
                           '0';
  ExtADCSimple_SensorReady_i <= Inputs_i(0) when Select_i = "00111" else 
                                '0';
  ExtADCSimple_AdcDone_i <= AdcConvComplete_i when Select_i = "00111" else 
                            '0';
  ExtADCSimple_AdcValue_i <= "000000" & AdcValue_i when Select_i = "00111" else 
                             "0000000000000000";
  ExtADCSimple_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "00111" else 
                                        "0000000000000000";
  ExtADCSimple_SensorValue_o <= ParamWordOut0_o when Select_i = "00111" else 
                                "0000000000000000";


  TMP421_1: TMP421
    port map (
      Reset_n_i => TMP421_Reset_n_i,
      Clk_i => TMP421_Clk_i,
      Enable_i => TMP421_Enable_i,
      CpuIntr_o => TMP421_CpuIntr_o,
      I2C_ReceiveSend_n_o => TMP421_I2C_ReceiveSend_n_o,
      I2C_ReadCount_o => TMP421_I2C_ReadCount_o,
      I2C_StartProcess_o => TMP421_I2C_StartProcess_o,
      I2C_Busy_i => TMP421_I2C_Busy_i,
      I2C_FIFOReadNext_o => TMP421_I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => TMP421_I2C_FIFOWrite_o,
      I2C_Data_o => TMP421_I2C_Data_o,
      I2C_Data_i => TMP421_I2C_Data_i,
      I2C_Error_i => TMP421_I2C_Error_i,
      PeriodCounterPresetH_i => TMP421_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => TMP421_PeriodCounterPresetL_i,
      SensorValueL_o => TMP421_SensorValueL_o,
      SensorValueR_o => TMP421_SensorValueR_o
    );

  TMP421_Reset_n_i <= Reset_n_i when Select_i = "01000" else 
                      '0';
  TMP421_Clk_i <= Clk_i when Select_i = "01000" else 
                  '0';
  TMP421_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01000" else 
                     '0';
  TMP421_I2C_Busy_i <= I2C_Busy when Select_i = "01000" else 
                       '0';
  TMP421_I2C_Data_i <= I2C_DataOut when Select_i = "01000" else 
                       "00000000";
  TMP421_I2C_Error_i <= I2C_Error when Select_i = "01000" else 
                        '0';
  TMP421_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "01000" else 
                                   "0000000000000000";
  TMP421_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "01000" else 
                                   "0000000000000000";
  TMP421_SensorValueL_o <= ParamWordOut0_o when Select_i = "01000" else 
                           "0000000000000000";
  TMP421_SensorValueR_o <= ParamWordOut3_o when Select_i = "01000" else 
                           "0000000000000000";


  ADT7310P16LS16L_1: ADT7310P16LS16L
    port map (
      Reset_n_i => ADT7310P16LS16L_Reset_n_i,
      Clk_i => ADT7310P16LS16L_Clk_i,
      Enable_i => ADT7310P16LS16L_Enable_i,
      CpuIntr_o => ADT7310P16LS16L_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P16LS16L_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P16LS16L_SPI_Data_i,
      SPI_Write_o => ADT7310P16LS16L_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P16LS16L_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P16LS16L_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P16LS16L_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P16LS16L_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P16LS16L_SPI_Transmission_i,
      PeriodCounterPreset_i => ADT7310P16LS16L_PeriodCounterPreset_i,
      SPICounterPreset_i => ADT7310P16LS16L_SPICounterPreset_i,
      SensorValue_o => ADT7310P16LS16L_SensorValue_o,
      Threshold_i => ADT7310P16LS16L_Threshold_i,
      SPI_CPOL_o => ADT7310P16LS16L_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P16LS16L_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P16LS16L_SPI_LSBFE_o
    );

  ADT7310P16LS16L_Reset_n_i <= Reset_n_i when Select_i = "01001" else 
                               '0';
  ADT7310P16LS16L_Clk_i <= Clk_i when Select_i = "01001" else 
                           '0';
  ADT7310P16LS16L_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01001" else 
                              '0';
  ADT7310P16LS16L_SPI_Data_i <= SPI_DataOut when Select_i = "01001" else 
                                "00000000";
  ADT7310P16LS16L_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01001" else 
                                    '0';
  ADT7310P16LS16L_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01001" else 
                                     '0';
  ADT7310P16LS16L_SPI_Transmission_i <= SPI_Transmission when Select_i = "01001" else 
                                        '0';
  ADT7310P16LS16L_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "01001" else 
                                           "0000000000000000";
  ADT7310P16LS16L_SPICounterPreset_i <= ParamWordIn1_i when Select_i = "01001" else 
                                        "0000000000000000";
  ADT7310P16LS16L_SensorValue_o <= ParamWordOut0_o when Select_i = "01001" else 
                                   "0000000000000000";
  ADT7310P16LS16L_Threshold_i <= ParamWordIn2_i when Select_i = "01001" else 
                                 "0000000000000000";


  blinki_1: blinki
    port map (
      Reset_n_i => blinki_Reset_n_i,
      Clk_i => blinki_Clk_i,
      LED_o => blinki_LED_o,
      PeriodH_i => blinki_PeriodH_i,
      PeriodL_i => blinki_PeriodL_i
    );

  blinki_Reset_n_i <= Reset_n_i when Select_i = "01010" else 
                      '0';
  blinki_Clk_i <= Clk_i when Select_i = "01010" else 
                  '0';
  blinki_PeriodH_i <= ParamWordIn0_i when Select_i = "01010" else 
                      "0000000000000000";
  blinki_PeriodL_i <= ParamWordIn1_i when Select_i = "01010" else 
                      "0000000000000000";


  ADT7310P32LS16L_1: ADT7310P32LS16L
    port map (
      Reset_n_i => ADT7310P32LS16L_Reset_n_i,
      Clk_i => ADT7310P32LS16L_Clk_i,
      Enable_i => ADT7310P32LS16L_Enable_i,
      CpuIntr_o => ADT7310P32LS16L_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32LS16L_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32LS16L_SPI_Data_i,
      SPI_Write_o => ADT7310P32LS16L_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32LS16L_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32LS16L_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32LS16L_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32LS16L_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32LS16L_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32LS16L_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32LS16L_PeriodCounterPresetL_i,
      SPICounterPreset_i => ADT7310P32LS16L_SPICounterPreset_i,
      SensorValue_o => ADT7310P32LS16L_SensorValue_o,
      Threshold_i => ADT7310P32LS16L_Threshold_i,
      SPI_CPOL_o => ADT7310P32LS16L_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32LS16L_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32LS16L_SPI_LSBFE_o
    );

  ADT7310P32LS16L_Reset_n_i <= Reset_n_i when Select_i = "01011" else 
                               '0';
  ADT7310P32LS16L_Clk_i <= Clk_i when Select_i = "01011" else 
                           '0';
  ADT7310P32LS16L_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01011" else 
                              '0';
  ADT7310P32LS16L_SPI_Data_i <= SPI_DataOut when Select_i = "01011" else 
                                "00000000";
  ADT7310P32LS16L_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01011" else 
                                    '0';
  ADT7310P32LS16L_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01011" else 
                                     '0';
  ADT7310P32LS16L_SPI_Transmission_i <= SPI_Transmission when Select_i = "01011" else 
                                        '0';
  ADT7310P32LS16L_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "01011" else 
                                            "0000000000000000";
  ADT7310P32LS16L_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "01011" else 
                                            "0000000000000000";
  ADT7310P32LS16L_SPICounterPreset_i <= ParamWordIn2_i when Select_i = "01011" else 
                                        "0000000000000000";
  ADT7310P32LS16L_SensorValue_o <= ParamWordOut0_o when Select_i = "01011" else 
                                   "0000000000000000";
  ADT7310P32LS16L_Threshold_i <= ParamWordIn3_i when Select_i = "01011" else 
                                 "0000000000000000";


  ADT7310P16LS32L_1: ADT7310P16LS32L
    port map (
      Reset_n_i => ADT7310P16LS32L_Reset_n_i,
      Clk_i => ADT7310P16LS32L_Clk_i,
      Enable_i => ADT7310P16LS32L_Enable_i,
      CpuIntr_o => ADT7310P16LS32L_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P16LS32L_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P16LS32L_SPI_Data_i,
      SPI_Write_o => ADT7310P16LS32L_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P16LS32L_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P16LS32L_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P16LS32L_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P16LS32L_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P16LS32L_SPI_Transmission_i,
      PeriodCounterPreset_i => ADT7310P16LS32L_PeriodCounterPreset_i,
      SPICounterPresetH_i => ADT7310P16LS32L_SPICounterPresetH_i,
      SPICounterPresetL_i => ADT7310P16LS32L_SPICounterPresetL_i,
      SensorValue_o => ADT7310P16LS32L_SensorValue_o,
      Threshold_i => ADT7310P16LS32L_Threshold_i,
      SPI_CPOL_o => ADT7310P16LS32L_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P16LS32L_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P16LS32L_SPI_LSBFE_o
    );

  ADT7310P16LS32L_Reset_n_i <= Reset_n_i when Select_i = "01100" else 
                               '0';
  ADT7310P16LS32L_Clk_i <= Clk_i when Select_i = "01100" else 
                           '0';
  ADT7310P16LS32L_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01100" else 
                              '0';
  ADT7310P16LS32L_SPI_Data_i <= SPI_DataOut when Select_i = "01100" else 
                                "00000000";
  ADT7310P16LS32L_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01100" else 
                                    '0';
  ADT7310P16LS32L_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01100" else 
                                     '0';
  ADT7310P16LS32L_SPI_Transmission_i <= SPI_Transmission when Select_i = "01100" else 
                                        '0';
  ADT7310P16LS32L_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "01100" else 
                                           "0000000000000000";
  ADT7310P16LS32L_SPICounterPresetH_i <= ParamWordIn1_i when Select_i = "01100" else 
                                         "0000000000000000";
  ADT7310P16LS32L_SPICounterPresetL_i <= ParamWordIn2_i when Select_i = "01100" else 
                                         "0000000000000000";
  ADT7310P16LS32L_SensorValue_o <= ParamWordOut0_o when Select_i = "01100" else 
                                   "0000000000000000";
  ADT7310P16LS32L_Threshold_i <= ParamWordIn3_i when Select_i = "01100" else 
                                 "0000000000000000";


  ADT7310P32LS32L_1: ADT7310P32LS32L
    port map (
      Reset_n_i => ADT7310P32LS32L_Reset_n_i,
      Clk_i => ADT7310P32LS32L_Clk_i,
      Enable_i => ADT7310P32LS32L_Enable_i,
      CpuIntr_o => ADT7310P32LS32L_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32LS32L_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32LS32L_SPI_Data_i,
      SPI_Write_o => ADT7310P32LS32L_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32LS32L_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32LS32L_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32LS32L_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32LS32L_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32LS32L_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32LS32L_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32LS32L_PeriodCounterPresetL_i,
      SPICounterPresetH_i => ADT7310P32LS32L_SPICounterPresetH_i,
      SPICounterPresetL_i => ADT7310P32LS32L_SPICounterPresetL_i,
      SensorValue_o => ADT7310P32LS32L_SensorValue_o,
      Threshold_i => ADT7310P32LS32L_Threshold_i,
      SPI_CPOL_o => ADT7310P32LS32L_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32LS32L_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32LS32L_SPI_LSBFE_o
    );

  ADT7310P32LS32L_Reset_n_i <= Reset_n_i when Select_i = "01101" else 
                               '0';
  ADT7310P32LS32L_Clk_i <= Clk_i when Select_i = "01101" else 
                           '0';
  ADT7310P32LS32L_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01101" else 
                              '0';
  ADT7310P32LS32L_SPI_Data_i <= SPI_DataOut when Select_i = "01101" else 
                                "00000000";
  ADT7310P32LS32L_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01101" else 
                                    '0';
  ADT7310P32LS32L_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01101" else 
                                     '0';
  ADT7310P32LS32L_SPI_Transmission_i <= SPI_Transmission when Select_i = "01101" else 
                                        '0';
  ADT7310P32LS32L_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "01101" else 
                                            "0000000000000000";
  ADT7310P32LS32L_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "01101" else 
                                            "0000000000000000";
  ADT7310P32LS32L_SPICounterPresetH_i <= ParamWordIn2_i when Select_i = "01101" else 
                                         "0000000000000000";
  ADT7310P32LS32L_SPICounterPresetL_i <= ParamWordIn3_i when Select_i = "01101" else 
                                         "0000000000000000";
  ADT7310P32LS32L_SensorValue_o <= ParamWordOut0_o when Select_i = "01101" else 
                                   "0000000000000000";
  ADT7310P32LS32L_Threshold_i <= ParamWordIn4_i when Select_i = "01101" else 
                                 "0000000000000000";


  ADT7310P32LS16_1: ADT7310P32LS16
    port map (
      Reset_n_i => ADT7310P32LS16_Reset_n_i,
      Clk_i => ADT7310P32LS16_Clk_i,
      Enable_i => ADT7310P32LS16_Enable_i,
      CpuIntr_o => ADT7310P32LS16_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32LS16_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32LS16_SPI_Data_i,
      SPI_Write_o => ADT7310P32LS16_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32LS16_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32LS16_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32LS16_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32LS16_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32LS16_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32LS16_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32LS16_PeriodCounterPresetL_i,
      SPICounterPreset_i => ADT7310P32LS16_SPICounterPreset_i,
      SensorValue_o => ADT7310P32LS16_SensorValue_o,
      Threshold_i => ADT7310P32LS16_Threshold_i,
      SPI_CPOL_o => ADT7310P32LS16_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32LS16_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32LS16_SPI_LSBFE_o
    );

  ADT7310P32LS16_Reset_n_i <= Reset_n_i when Select_i = "01110" else 
                              '0';
  ADT7310P32LS16_Clk_i <= Clk_i when Select_i = "01110" else 
                          '0';
  ADT7310P32LS16_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01110" else 
                             '0';
  ADT7310P32LS16_SPI_Data_i <= SPI_DataOut when Select_i = "01110" else 
                               "00000000";
  ADT7310P32LS16_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01110" else 
                                   '0';
  ADT7310P32LS16_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01110" else 
                                    '0';
  ADT7310P32LS16_SPI_Transmission_i <= SPI_Transmission when Select_i = "01110" else 
                                       '0';
  ADT7310P32LS16_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "01110" else 
                                           "0000000000000000";
  ADT7310P32LS16_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "01110" else 
                                           "0000000000000000";
  ADT7310P32LS16_SPICounterPreset_i <= ParamWordIn2_i when Select_i = "01110" else 
                                       "0000000000000000";
  ADT7310P32LS16_SensorValue_o <= ParamWordOut0_o when Select_i = "01110" else 
                                  "0000000000000000";
  ADT7310P32LS16_Threshold_i <= ParamWordIn3_i when Select_i = "01110" else 
                                "0000000000000000";


  ADT7310P32S32_1: ADT7310P32S32
    port map (
      Reset_n_i => ADT7310P32S32_Reset_n_i,
      Clk_i => ADT7310P32S32_Clk_i,
      Enable_i => ADT7310P32S32_Enable_i,
      CpuIntr_o => ADT7310P32S32_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32S32_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32S32_SPI_Data_i,
      SPI_Write_o => ADT7310P32S32_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32S32_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32S32_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32S32_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32S32_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32S32_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32S32_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32S32_PeriodCounterPresetL_i,
      SPICounterPresetH_i => ADT7310P32S32_SPICounterPresetH_i,
      SPICounterPresetL_i => ADT7310P32S32_SPICounterPresetL_i,
      SensorValue_o => ADT7310P32S32_SensorValue_o,
      Threshold_i => ADT7310P32S32_Threshold_i,
      SPI_CPOL_o => ADT7310P32S32_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32S32_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32S32_SPI_LSBFE_o
    );

  ADT7310P32S32_Reset_n_i <= Reset_n_i when Select_i = "01111" else 
                             '0';
  ADT7310P32S32_Clk_i <= Clk_i when Select_i = "01111" else 
                         '0';
  ADT7310P32S32_Enable_i <= ReconfModuleIn_s(0) when Select_i = "01111" else 
                            '0';
  ADT7310P32S32_SPI_Data_i <= SPI_DataOut when Select_i = "01111" else 
                              "00000000";
  ADT7310P32S32_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "01111" else 
                                  '0';
  ADT7310P32S32_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "01111" else 
                                   '0';
  ADT7310P32S32_SPI_Transmission_i <= SPI_Transmission when Select_i = "01111" else 
                                      '0';
  ADT7310P32S32_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "01111" else 
                                          "0000000000000000";
  ADT7310P32S32_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "01111" else 
                                          "0000000000000000";
  ADT7310P32S32_SPICounterPresetH_i <= ParamWordIn2_i when Select_i = "01111" else 
                                       "0000000000000000";
  ADT7310P32S32_SPICounterPresetL_i <= ParamWordIn3_i when Select_i = "01111" else 
                                       "0000000000000000";
  ADT7310P32S32_SensorValue_o <= ParamWordOut0_o when Select_i = "01111" else 
                                 "0000000000000000";
  ADT7310P32S32_Threshold_i <= ParamWordIn4_i when Select_i = "01111" else 
                               "0000000000000000";


  ADT7310P32S16L_1: ADT7310P32S16L
    port map (
      Reset_n_i => ADT7310P32S16L_Reset_n_i,
      Clk_i => ADT7310P32S16L_Clk_i,
      Enable_i => ADT7310P32S16L_Enable_i,
      CpuIntr_o => ADT7310P32S16L_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P32S16L_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P32S16L_SPI_Data_i,
      SPI_Write_o => ADT7310P32S16L_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P32S16L_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P32S16L_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P32S16L_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P32S16L_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P32S16L_SPI_Transmission_i,
      PeriodCounterPresetH_i => ADT7310P32S16L_PeriodCounterPresetH_i,
      PeriodCounterPresetL_i => ADT7310P32S16L_PeriodCounterPresetL_i,
      SPICounterPreset_i => ADT7310P32S16L_SPICounterPreset_i,
      SensorValue_o => ADT7310P32S16L_SensorValue_o,
      Threshold_i => ADT7310P32S16L_Threshold_i,
      SPI_CPOL_o => ADT7310P32S16L_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P32S16L_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P32S16L_SPI_LSBFE_o
    );

  ADT7310P32S16L_Reset_n_i <= Reset_n_i when Select_i = "10000" else 
                              '0';
  ADT7310P32S16L_Clk_i <= Clk_i when Select_i = "10000" else 
                          '0';
  ADT7310P32S16L_Enable_i <= ReconfModuleIn_s(0) when Select_i = "10000" else 
                             '0';
  ADT7310P32S16L_SPI_Data_i <= SPI_DataOut when Select_i = "10000" else 
                               "00000000";
  ADT7310P32S16L_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "10000" else 
                                   '0';
  ADT7310P32S16L_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "10000" else 
                                    '0';
  ADT7310P32S16L_SPI_Transmission_i <= SPI_Transmission when Select_i = "10000" else 
                                       '0';
  ADT7310P32S16L_PeriodCounterPresetH_i <= ParamWordIn0_i when Select_i = "10000" else 
                                           "0000000000000000";
  ADT7310P32S16L_PeriodCounterPresetL_i <= ParamWordIn1_i when Select_i = "10000" else 
                                           "0000000000000000";
  ADT7310P32S16L_SPICounterPreset_i <= ParamWordIn2_i when Select_i = "10000" else 
                                       "0000000000000000";
  ADT7310P32S16L_SensorValue_o <= ParamWordOut0_o when Select_i = "10000" else 
                                  "0000000000000000";
  ADT7310P32S16L_Threshold_i <= ParamWordIn3_i when Select_i = "10000" else 
                                "0000000000000000";


  ExtIntr_1: ExtIntr
    port map (
      ExtIntrOut_o => ExtIntr_ExtIntrOut_o,
      ExtIntrIn_i => ExtIntr_ExtIntrIn_i
    );

  ExtIntr_ExtIntrIn_i <= Inputs_i(7) when Select_i = "10001" else 
                         '0';


  ADT7310P16S16_1: ADT7310P16S16
    port map (
      Reset_n_i => ADT7310P16S16_Reset_n_i,
      Clk_i => ADT7310P16S16_Clk_i,
      Enable_i => ADT7310P16S16_Enable_i,
      CpuIntr_o => ADT7310P16S16_CpuIntr_o,
      ADT7310CS_n_o => ADT7310P16S16_ADT7310CS_n_o,
      SPI_Data_i => ADT7310P16S16_SPI_Data_i,
      SPI_Write_o => ADT7310P16S16_SPI_Write_o,
      SPI_ReadNext_o => ADT7310P16S16_SPI_ReadNext_o,
      SPI_Data_o => ADT7310P16S16_SPI_Data_o,
      SPI_FIFOFull_i => ADT7310P16S16_SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => ADT7310P16S16_SPI_FIFOEmpty_i,
      SPI_Transmission_i => ADT7310P16S16_SPI_Transmission_i,
      PeriodCounterPreset_i => ADT7310P16S16_PeriodCounterPreset_i,
      SPICounterPreset_i => ADT7310P16S16_SPICounterPreset_i,
      SensorValue_o => ADT7310P16S16_SensorValue_o,
      Threshold_i => ADT7310P16S16_Threshold_i,
      SPI_CPOL_o => ADT7310P16S16_SPI_CPOL_o,
      SPI_CPHA_o => ADT7310P16S16_SPI_CPHA_o,
      SPI_LSBFE_o => ADT7310P16S16_SPI_LSBFE_o
    );

  ADT7310P16S16_Reset_n_i <= Reset_n_i when Select_i = "10010" else 
                             '0';
  ADT7310P16S16_Clk_i <= Clk_i when Select_i = "10010" else 
                         '0';
  ADT7310P16S16_Enable_i <= ReconfModuleIn_s(0) when Select_i = "10010" else 
                            '0';
  ADT7310P16S16_SPI_Data_i <= SPI_DataOut when Select_i = "10010" else 
                              "00000000";
  ADT7310P16S16_SPI_FIFOFull_i <= SPI_FIFOFull when Select_i = "10010" else 
                                  '0';
  ADT7310P16S16_SPI_FIFOEmpty_i <= SPI_FIFOEmpty when Select_i = "10010" else 
                                   '0';
  ADT7310P16S16_SPI_Transmission_i <= SPI_Transmission when Select_i = "10010" else 
                                      '0';
  ADT7310P16S16_PeriodCounterPreset_i <= ParamWordIn0_i when Select_i = "10010" else 
                                         "0000000000000000";
  ADT7310P16S16_SPICounterPreset_i <= ParamWordIn1_i when Select_i = "10010" else 
                                      "0000000000000000";
  ADT7310P16S16_SensorValue_o <= ParamWordOut0_o when Select_i = "10010" else 
                                 "0000000000000000";
  ADT7310P16S16_Threshold_i <= ParamWordIn2_i when Select_i = "10010" else 
                               "0000000000000000";

end struct;
