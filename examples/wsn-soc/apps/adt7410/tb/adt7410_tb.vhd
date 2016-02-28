library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.BusMasters.all;

entity ADT7410_tb is
end ADT7410_tb;

architecture behavior of ADT7410_tb is

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

  component adt7410_model
    port (
      scl_i      : in    std_logic;
      sda_io     : inout std_logic;
      i2c_addr_i : in    std_logic_vector(1 downto 0);
      int_o      : out   std_logic;
      ct_o       : out   std_logic;
      temp_i     : in    std_logic_vector(15 downto 0));
  end component;

  component ExtNames
    port (
      I2CFSM_Done : out std_logic
    );
  end component;

  -- Reset
  signal Reset_n_i : std_logic := '0';
  -- Clock
  signal Clk_i : std_logic := '1';
  signal Enable_i : std_logic;
  signal CpuIntr_o : std_logic;
  signal I2C_ReceiveSend_n_o : std_logic;
  signal I2C_ReadCount_o : std_logic_vector(7 downto 0);
  signal I2C_StartProcess_o : std_logic;
  signal I2C_Busy_i : std_logic;
  signal I2C_FIFOReadNext_o : std_logic;
  signal I2C_FIFOWrite_o : std_logic;
  signal I2C_Data_o : std_logic_vector(7 downto 0);
  signal I2C_Data_i : std_logic_vector(7 downto 0);
  signal I2C_Error_i : std_logic;
  signal PeriodCounterPreset_i : std_logic_vector(15 downto 0);
  signal SensorValue_o : std_logic_vector(15 downto 0);
  signal Threshold_i : std_logic_vector(15 downto 0);
  signal WaitCounterPreset_i : std_logic_vector(15 downto 0);
  signal I2C_F100_400_n : std_logic;
  signal I2C_Divider800 : std_logic_vector(15 downto 0);
  signal SensorValue_real : real;

  -- look into the ADT7310 app
  -- alias I2CFSM_Done_i is << signal .adt7310_tb.DUT.I2CFSM_Done_s : std_logic >>;
  -- ModelSim complains here, that the references signal is not a VHDL object.
  -- True, this is a Verilog object. As a workaround the module ExtNames is created
  -- which uses Verilog hierarchical names to reference the wire and assigns it to
  -- an output. This module is instantiated (and it seems ModelSim only adds
  -- Verilog<->VHDL signal converters on instance boundaries) and this output is
  -- connected with the I2CFSM_Done_i signal.
  signal I2CFSM_Done_i : std_logic;  -- directly from inside I2C_FSM
  -- Using the extracted Yosys FSM we get delta cycles and a glitch on
  -- I2CFSM_Done_i. Therefore we generate a slightly delayed version and wait
  -- on the ANDed value.
  signal I2CFSM_Done_d : std_logic;  -- sightly delayed
  signal I2CFSM_Done_a : std_logic;  -- I2CFSM_Done_i and I2CFSM_Done_d

  -- ADT7410 component ports
  signal I2C_SDA_i     : std_logic;
  signal I2C_SDA_o     : std_logic;
  signal I2C_SDA_s     : std_logic;
  signal I2C_SCL_o     : std_logic;
  signal CT_n_s        : std_logic;
  signal INT_n_s       : std_logic;
  signal Temp_s        : real := 23.7;
  signal TempBin_s     : std_logic_vector(15 downto 0);

  -- I2C Master generics
  constant I2C_FIFOAddressWidth_g : integer :=  4;
  constant I2C_ReadCountWidth_g   : integer :=  4;
  constant I2C_DividerWidth_g     : integer := 16;
  -- I2C Master component ports
  signal I2C_FIFOEmpty_s    : std_logic := '0';
  signal I2C_FIFOFull_s     : std_logic := '0';
  signal I2C_ErrBusColl_s       : std_logic;
  signal I2C_ErrCoreBusy_s      : std_logic;
  signal I2C_ErrCoreStopped_s   : std_logic;
  signal I2C_ErrDevNotPresent_s : std_logic;
  signal I2C_ErrFIFOEmpty_s     : std_logic;
  signal I2C_ErrFIFOFull_s      : std_logic;
  signal I2C_ErrGotNAck_s       : std_logic;
  signal I2C_ErrReadCountZero_s : std_logic;
  signal I2C_ScanEnable_s   : std_logic := '0';
  signal I2C_ScanClk_s      : std_logic := '0';
  signal I2C_ScanDataIn_s   : std_logic := '0';
  signal I2C_ScanDataOut_s  : std_logic := '0';

  -- The timer has to wait for 240ms. With a 16 bit resolution, the maximumn
  -- counting periode is 3.66us. Here we set the clock signal to 10us = 100kHz.
  -- The timer is preset to 24000.
  constant ClkPeriode : time := 10 us;

begin

  DUT: ADT7410
    port map (
      Reset_n_i => Reset_n_i,
      Clk_i => Clk_i,
      Enable_i => Enable_i,
      CpuIntr_o => CpuIntr_o,
      I2C_ReceiveSend_n_o => I2C_ReceiveSend_n_o,
      I2C_ReadCount_o => I2C_ReadCount_o,
      I2C_StartProcess_o => I2C_StartProcess_o,
      I2C_Busy_i => I2C_Busy_i,
      I2C_FIFOReadNext_o => I2C_FIFOReadNext_o,
      I2C_FIFOWrite_o => I2C_FIFOWrite_o,
      I2C_Data_o => I2C_Data_o,
      I2C_Data_i => I2C_Data_i,
      I2C_Error_i => I2C_Error_i,
      PeriodCounterPreset_i => PeriodCounterPreset_i,
      SensorValue_o => SensorValue_o,
      Threshold_i => Threshold_i,
      WaitCounterPreset_i => WaitCounterPreset_i
    );

  TempBin_s <= std_logic_vector(to_unsigned(integer(Temp_s*128.0),16));
  SensorValue_real <= real(to_integer(unsigned(SensorValue_o)))/128.0;

  ExtNames_1: ExtNames
    port map (
      I2CFSM_Done => I2CFSM_Done_i
    );
  I2CFSM_Done_d <= I2CFSM_Done_i after 1.0 ns;
  I2CFSM_Done_a <= I2CFSM_Done_i and I2CFSM_Done_d;

  i2c_master_1: i2c_master
    generic map (
      ReadCountWidth_g   => I2C_ReadCountWidth_g,
      FIFOAddressWidth_g => I2C_FIFOAddressWidth_g,
      DividerWidth_g     => I2C_DividerWidth_g)
    port map (
      Reset_i            => "not"(Reset_n_i),
      Clk_i              => Clk_i,
      Divider800_i       => I2C_Divider800,
      F100_400_n_i       => I2C_F100_400_n,
      StartProcess_i     => I2C_StartProcess_o,
      ReceiveSend_n_i    => I2C_ReceiveSend_n_o,
      Busy_o             => I2C_Busy_i,
      ReadCount_i        => I2C_ReadCount_o(I2C_ReadCountWidth_g-1 downto 0),
      FIFOReadNext_i     => I2C_FIFOReadNext_o,
      FIFOWrite_i        => I2C_FIFOWrite_o,
      FIFOEmpty_o        => I2C_FIFOEmpty_s,
      FIFOFull_o         => I2C_FIFOFull_s,
      Data_i             => I2C_Data_o,
      Data_o             => I2C_Data_i,
      ErrAck_i           => '0',
      ErrBusColl_o       => I2C_ErrBusColl_s,
      ErrFIFOFull_o      => I2C_ErrFIFOFull_s,
      ErrGotNAck_o       => I2C_ErrGotNAck_s,
      ErrCoreBusy_o      => I2C_ErrCoreBusy_s,
      ErrFIFOEmpty_o     => I2C_ErrFIFOEmpty_s,
      ErrCoreStopped_o   => I2C_ErrCoreStopped_s,
      ErrDevNotPresent_o => I2C_ErrDevNotPresent_s,
      ErrReadCountZero_o => I2C_ErrReadCountZero_s,
      SDA_i              => I2C_SDA_i,
      SDA_o              => I2C_SDA_o,
      SCL_o              => I2C_SCL_o,
      ScanEnable_i       => I2C_ScanEnable_s,
      ScanClk_i          => I2C_ScanClk_s,
      ScanDataIn_i       => I2C_ScanDataIn_s,
      ScanDataOut_o      => I2C_ScanDataOut_s
    );

  I2C_Error_i <= I2C_ErrBusColl_s or I2C_ErrCoreBusy_s or I2C_ErrCoreStopped_s or I2C_ErrDevNotPresent_s or I2C_ErrFIFOEmpty_s or I2C_ErrFIFOFull_s or I2C_ErrGotNAck_s or I2C_ErrReadCountZero_s;

  I2C_SDA_s <= 'H';      -- weak 1 -> simulate pull-up

  I2C_SDA_s <= '0' when I2C_SDA_o = '0' else 'Z';

  I2C_SDA_i <= to_X01(I2C_SDA_s) after 0.2 us;

  adt7410_1: adt7410_model
    port map (
      scl_i      => I2C_SCL_o,
      sda_io     => I2C_SDA_s,
      i2c_addr_i => "00",
      INT_o      => INT_n_s,
      CT_o       => CT_n_s,
      temp_i     => TempBin_s);


  -- constant value for reconfig signal
  I2C_F100_400_n <= '1';
  -- constant value for reconfig signal
  I2C_Divider800 <= "0000000001111100";
  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  StimulusProc: process
  begin
    Enable_i <= '0';
    PeriodCounterPreset_i <= "0000000000001010";
    Threshold_i <= "0000000000011110";
    WaitCounterPreset_i <= "0101110111000000";

    wait for 2.3*ClkPeriode;
    -- deassert Reset
    Reset_n_i <= '1';

    Temp_s <= 23.7;                     -- degree C

    -- three cycles with disabled SensorFSM
    wait for 3*ClkPeriode;

    -- enable SensorFSM
    Enable_i <= '1';
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- The digital value is 128*Temp_s (plus/minus rounding to nearest
    -- modulo 8). The threshold for too large changes is 30 (see
    -- sensorfsm.vhd).
    -- 23.7°C --> 3032
    -- 25.7°C --> 3288  (delta: | 256| >  30)
    -- 25.6°C --> 3280  (delta: |  -8| <  30)
    -- 25.5°C --> 3264  (delta: | -24| <  30)
    -- 25.4°C --> 3248  (delta: | -40| >= 30)

    -- new sensor value with large difference -> notify required
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.7;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.6;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after I2CFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with small difference -> no notification
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.5;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '0' report "CpuIntr should still be '0' one cycle after I2CFSM is done for small value change" severity error;
    assert abs(SensorValue_real - 25.7) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be old value " & real'image(25.7) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    -- new sensor value with large difference -> notify required
    wait for 3*ClkPeriode;              -- 3 cycle
    Temp_s <= 25.4;
    wait until I2CFSM_Done_d = '1';
    assert CpuIntr_o = '0' report "CpuIntr should be '0' directly after I2CFSM is done" severity error;
    wait until rising_edge(Clk_i); wait for 0.1*ClkPeriode;      -- 1 cycle
    assert CpuIntr_o = '1' report "CpuIntr should be '1' one cycle after I2CFSM is done" severity error;
    assert abs(SensorValue_real - Temp_s) <= 1.0/16.0/2.0
      report "Invalid temperature value: " & real'image(SensorValue_real) & "°C, should be " & real'image(Temp_s) & "°C"
      severity error;
    wait for 1*ClkPeriode;              -- 1 cycle

    wait for 100 ms;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
