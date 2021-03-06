library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.BusMasters.all;

entity MAX6682Mean_tb is
end MAX6682Mean_tb;

architecture behavior of MAX6682Mean_tb is

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

  component MAX6682_Model
    port (
      ChipSelect_n_i : in  std_logic;
      SCLK_i         : in  std_logic;
      SO_o           : out std_logic;
      Value_i        : in  std_logic_vector(10 downto 0)
    );
  end component;

  -- Reset
  signal Reset_n_i : std_logic := '0';
  -- Clock
  signal Clk_i : std_logic := '1';
  signal Enable_i : std_logic;
  signal CpuIntr_o : std_logic;
  signal MAX6682CS_n_o : std_logic;
  signal SPI_Data_i : std_logic_vector(7 downto 0);
  signal SPI_Write_o : std_logic;
  signal SPI_ReadNext_o : std_logic;
  signal SPI_Data_o : std_logic_vector(7 downto 0);
  signal SPI_FIFOFull_i : std_logic;
  signal SPI_FIFOEmpty_i : std_logic;
  signal SPI_Transmission_i : std_logic;
  signal PauseCounterPreset_i : std_logic_vector(15 downto 0);
  signal PeriodCounterPreset_i : std_logic_vector(31 downto 0);
  signal SensorValue_o : std_logic_vector(15 downto 0);
  signal Threshold_i : std_logic_vector(15 downto 0);
  signal SPI_SPPR_SPR_o : std_logic_vector(7 downto 0);

  constant ClkPeriode : time := 100 ns;

  signal SPI_CPOL_o : std_logic;
  signal SPI_CPHA_o : std_logic;
  signal SPI_LSBFE_o : std_logic;

  constant SPPRWidth : integer := 4;
  constant SPRWidth  : integer := 4;
  constant DataWidth : integer := 8;
  constant SPIFIFOReadWidth  : integer := 4;
  constant SPIFIFOWriteWidth : integer := 4;
  -- SPI signals
  signal SPI_SCK_s          : std_logic;
  signal SPI_MOSI_s         : std_logic;
  signal SPI_MISO_s         : std_logic;
  signal SPI_ScanEnable_s   : std_logic;
  signal SPI_ScanClk_s      : std_logic;
  signal SPI_ScanDataIn_s   : std_logic;
  signal SPI_ScanDataOut_s  : std_logic;
  -- MAX6682 simulation
  signal MAX6682Value       : unsigned(10 downto 0);

begin

  DUT: MAX6682Mean
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Enable_i,
      CpuIntr_o => CpuIntr_o,
      MAX6682CS_n_o => MAX6682CS_n_o,
      SPI_Data_i => SPI_Data_i,
      SPI_Write_o => SPI_Write_o,
      SPI_ReadNext_o => SPI_ReadNext_o,
      SPI_Data_o => SPI_Data_o,
      SPI_FIFOFull_i => SPI_FIFOFull_i,
      SPI_FIFOEmpty_i => SPI_FIFOEmpty_i,
      SPI_Transmission_i => SPI_Transmission_i,
      PauseCounterPreset_i => PauseCounterPreset_i,
      PeriodCounterPresetH_i => PeriodCounterPreset_i(31 downto 16),
      PeriodCounterPresetL_i => PeriodCounterPreset_i(15 downto 0),
      SensorValue_o => SensorValue_o,
      Threshold_i => Threshold_i,
      SPI_CPOL_o => SPI_CPOL_o,
      SPI_CPHA_o => SPI_CPHA_o,
      SPI_LSBFE_o => SPI_LSBFE_o
    );

  spi_master_1: spi_master
    generic map (
      DataWidth      => DataWidth,
      SPPRWidth      => SPPRWidth,
      SPRWidth       => SPRWidth,
      FIFOReadWidth  => SPIFIFOReadWidth,
      FIFOWriteWidth => SPIFIFOWriteWidth
    )
    port map (
      Reset_n        => Reset_n_i,
      Clk            => Clk_i,
      -- IO
      SCK_o          => SPI_SCK_s,
      MOSI_o         => SPI_MOSI_s,
      MISO_i         => SPI_MISO_s,
      -- control signals
      CPOL_i         => SPI_CPOL_o,
      CPHA_i         => SPI_CPHA_o,
      LSBFE_i        => SPI_LSBFE_o,
      SPPR_i         => SPI_SPPR_SPR_o(7 downto 4),
      SPR_i          => SPI_SPPR_SPR_o(3 downto 0),
      Transmission_o => SPI_Transmission_i,
      Write_i        => SPI_Write_o,
      ReadNext_i     => SPI_ReadNext_o,
      Data_i         => SPI_Data_o,
      Data_o         => SPI_Data_i,
      FIFOFull_o     => SPI_FIFOFull_i,
      FIFOEmpty_o    => SPI_FIFOEmpty_i,
      ScanEnable_i   => SPI_ScanEnable_s,
      ScanClk_i      => SPI_ScanClk_s,
      ScanDataIn_i   => SPI_ScanDataIn_s,
      ScanDataOut_o  => SPI_ScanDataOut_s
    );

  MAX6682_1: MAX6682_Model
    port map (
      ChipSelect_n_i => MAX6682CS_n_o,
      SCLK_i         => SPI_SCK_s,
      SO_o           => SPI_MISO_s,
      Value_i        => std_logic_vector(MAX6682Value));

  -- constant value for reconfig signal
  SPI_SPPR_SPR_o <= "00000000";

  -- SPI MISO Pull-down
  SPI_MISO_s <= 'L';

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    Enable_i <= '0';
    PauseCounterPreset_i <= "0000000001010000";  -- 80
    PeriodCounterPreset_i <= "00000000000000000000001111101000"; -- 1000
    Threshold_i <= "0000000000101000";
    MAX6682Value <= (others => '0');

    -- Check constant values of dynamic signals coming out of the application modules
    wait for 0.1*ClkPeriode;
    assert SPI_CPOL_o = '0'
      report "Dynamic signal SPI_CPOL_o should have constant value '0'" severity failure;
    assert SPI_CPHA_o = '0'
      report "Dynamic signal SPI_CPHA_o should have constant value '0'" severity failure;
    assert SPI_LSBFE_o = '0'
      report "Dynamic signal SPI_LSBFE_o should have constant value '0'" severity failure;

    wait for 2.2*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    -- enable SensorFSM
    MAX6682Value <= (others => '0');
    Enable_i <= '1';
    wait for 999*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1'" severity error;
    -- query first 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 1000 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query second 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query third 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query fourth 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(0,16)) report "SensorValue_o should be 0" severity error;
    wait for 1*ClkPeriode;    -- new SensorValue_o is set one cycle afterwards
    assert SensorValue_o = std_logic_vector(to_unsigned(0,16)) report "SensorValue_o should be 0" severity error;

    -- new sensor value: 38+39+40+41 -> large difference -> notify required
    wait for 3*ClkPeriode;
    MAX6682Value <= to_unsigned(38,11);
    wait for 996*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1'" severity error;
    -- query first 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 1000 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(39,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query second 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(40,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query third 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(41,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query fourth 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    wait for 1*ClkPeriode;    -- SensorValue_o is set one cycle afterwards
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38+39+40+41,16)) report "SensorValue_o should be 158" severity error;

    -- new sensor value: 32+32+32+32 = 128 -> small difference -> no notification
    wait for 3*ClkPeriode;
    MAX6682Value <= to_unsigned(32,11);
    wait for 1380*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;    -- SensorValue_o is set one cycle afterwards
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38+39+40+41,16)) report "SensorValue_o should be 158" severity error;

    -- new sensor value: 30+30+30+30 = 120 -> small difference -> no notification
    wait for 3*ClkPeriode;
    MAX6682Value <= to_unsigned(30,11);
    wait for 1380*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;                     -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;    -- SensorValue_o is set one cycle afterwards
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38+39+40+41,16)) report "SensorValue_o should be 158" severity error;

    -- new sensor value: 27+27+27+27 = 108 -> large difference -> notify required
    wait for 3*ClkPeriode;
    MAX6682Value <= to_unsigned(27,11);
    wait for 1380*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    wait for 1*ClkPeriode;    -- SensorValue_o is set one cycle afterwards
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(108,16)) report "SensorValue_o should be 158" severity error;

    -- new sensor value: 38+39+40+41 -> large difference -> notify required
    wait for 3*ClkPeriode;
    MAX6682Value <= to_unsigned(38,11);
    wait for 996*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1'" severity error;
    -- query first 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 1000 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(39,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query second 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(40,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query third 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    MAX6682Value <= to_unsigned(41,11);
    wait for (80-1)*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' at end of pause" severity error;
    -- query fourth 16 bits
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should be '0' after 80 cycles" severity error;
    wait for 35*ClkPeriode;
    assert MAX6682CS_n_o = '0' report "CS_n should still be '0'" severity error;
    wait for 1*ClkPeriode;
    assert MAX6682CS_n_o = '1' report "CS_n should be '1' after 16 SPI bits" severity error;
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    wait for 1*ClkPeriode;
    assert CpuIntr_o = '1' report "CpuIntr should be '1'" severity error;
    wait for 1*ClkPeriode;    -- SensorValue_o is set one cycle afterwards
    assert CpuIntr_o = '0' report "CpuIntr should be '0'" severity error;
    assert SensorValue_o = std_logic_vector(to_unsigned(38+39+40+41,16)) report "SensorValue_o should be 158" severity error;

    wait for 100*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;


end behavior;
