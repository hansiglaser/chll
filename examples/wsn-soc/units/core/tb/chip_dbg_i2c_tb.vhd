library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.BusMasters.all;
use work.HexFile.all;

entity Chip_tb is
end Chip_tb;

architecture behavior of Chip_tb is

  component Chip
    port (
      Reset_n_i         : in    std_logic;
      Clk_i             : in    std_logic;
      Cpu_En_i          : in    std_logic;
      Dbg_En_i          : in    std_logic;
--      Dbg_UART_RxD_i    : in    std_logic;
--      Dbg_UART_TxD_o    : out   std_logic;
      Dbg_SCL_i         : in    std_logic;
      Dbg_SDA_b         : inout std_logic;
      P1_b              : inout std_logic_vector(7 downto 0);
      P2_b              : inout std_logic_vector(7 downto 0);
      UartRxD_i         : in    std_logic;
      UartTxD_o         : out   std_logic;
      SCK_o             : out std_logic;
      MOSI_o            : out std_logic;
      MISO_i            : in  std_logic;
      Inputs_i          : in    std_logic_vector(7 downto 0);
      Outputs_o         : out   std_logic_vector(7 downto 0);
      SPIMISO_i         : in    std_logic;
      SPIMOSI_o         : out   std_logic;
      SPISCK_o          : out   std_logic;
      I2CSCL_b          : out   std_logic;
      I2CSDA_b          : inout std_logic;
--      OneWire_b         : inout std_logic;
--      PWM_i             : in    std_logic;
--      SENT_i            : in    std_logic;
--      SPC_b             : inout std_logic;
      AdcConvComplete_i : in    std_logic;
      AdcDoConvert_o    : out   std_logic;
      AdcValue_i        : in    std_logic_vector(9 downto 0));
  end component;

  constant DbgI2CAddr : integer := 16#2A#;   -- see core.v
  constant PMemSize   : integer := 2**13;    -- 8 kByte
  constant DMemSize   : integer := 2**8;     -- 256 Bytes

-- Dbg interface register addresses
  constant CPU_ID_LO    : integer := 16#00#;
  constant CPU_ID_HI    : integer := 16#01#;
  constant CPU_CTL      : integer := 16#02#;
  constant CPU_STAT     : integer := 16#03#;
  constant MEM_CTL      : integer := 16#04#;
  constant MEM_ADDR     : integer := 16#05#;
  constant MEM_DATA     : integer := 16#06#;
  constant MEM_CNT      : integer := 16#07#;
  constant BRK0_CTL     : integer := 16#08#;
  constant BRK0_STAT    : integer := 16#09#;
  constant BRK0_ADDR0   : integer := 16#0A#;
  constant BRK0_ADDR1   : integer := 16#0B#;
  constant BRK1_CTL     : integer := 16#0C#;
  constant BRK1_STAT    : integer := 16#0D#;
  constant BRK1_ADDR0   : integer := 16#0E#;
  constant BRK1_ADDR1   : integer := 16#0F#;
  constant BRK2_CTL     : integer := 16#10#;
  constant BRK2_STAT    : integer := 16#11#;
  constant BRK2_ADDR0   : integer := 16#12#;
  constant BRK2_ADDR1   : integer := 16#13#;
  constant BRK3_CTL     : integer := 16#14#;
  constant BRK3_STAT    : integer := 16#15#;
  constant BRK3_ADDR0   : integer := 16#16#;
  constant BRK3_ADDR1   : integer := 16#17#;
  constant CPU_NR       : integer := 16#18#;

  -- Reset
  signal Reset_n_i         : std_logic := '0';
  -- Clock
  signal Clk_i             : std_logic := '1';
  signal Cpu_En_i          : std_logic := '0';
  signal Dbg_En_i          : std_logic;
--  signal Dbg_UART_RxD_i    : std_logic;
--  signal Dbg_UART_TxD_o    : std_logic;
  signal Dbg_SCL_i         : std_logic;
  signal Dbg_SDA_b         : std_logic;
  signal P1_b              : std_logic_vector(7 downto 0);
  signal P2_b              : std_logic_vector(7 downto 0);
  signal UartRxD_i         : std_logic;
  signal UartTxD_o         : std_logic;
  signal SCK_o             : std_logic;
  signal MOSI_o            : std_logic;
  signal MISO_i            : std_logic := '0';
  signal Inputs_i          : std_logic_vector(7 downto 0);
  signal Outputs_o         : std_logic_vector(7 downto 0);
  signal SPIMISO_i         : std_logic := '0';
  signal SPIMOSI_o         : std_logic;
  signal SPISCK_o          : std_logic;
  signal I2CSCL_b          : std_logic;
  signal I2CSDA_b          : std_logic;
--  signal OneWire_b         : std_logic;
--  signal PWM_i             : std_logic;
--  signal SENT_i            : std_logic;
--  signal SPC_b             : std_logic;
  signal AdcConvComplete_i : std_logic := '0';
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

  -- I2C Master generics
  constant I2C_FIFOAddressWidth_g : integer :=  4;
  constant I2C_ReadCountWidth_g   : integer :=  4;
  constant I2C_DividerWidth_g     : integer := 16;
  -- I2C Master component ports
  signal I2C_Divider800_i       : std_logic_vector(15 downto 0);
  signal I2C_F100_400_n_i       : std_logic;
  signal I2C_StartProcess_i     : std_logic;
  signal I2C_ReceiveSend_n_i    : std_logic;
  signal I2C_Busy_o             : std_logic;
  signal I2C_ReadCount_i        : std_logic_vector(I2C_ReadCountWidth_g-1 downto 0);
  signal I2C_FIFOReadNext_i     : std_logic;
  signal I2C_FIFOWrite_i        : std_logic;
  signal I2C_FIFOEmpty_o        : std_logic;
  signal I2C_FIFOFull_o         : std_logic;
  signal I2C_Data_i             : std_logic_vector(7 downto 0);
  signal I2C_Data_o             : std_logic_vector(7 downto 0);
  signal I2C_ErrAck_i           : std_logic;
  signal I2C_ErrBusColl_o       : std_logic;
  signal I2C_ErrCoreBusy_o      : std_logic;
  signal I2C_ErrCoreStopped_o   : std_logic;
  signal I2C_ErrDevNotPresent_o : std_logic;
  signal I2C_ErrFIFOEmpty_o     : std_logic;
  signal I2C_ErrFIFOFull_o      : std_logic;
  signal I2C_ErrGotNAck_o       : std_logic;
  signal I2C_ErrReadCountZero_o : std_logic;
  signal I2C_SDA_i              : std_logic;
  signal I2C_SDA_o              : std_logic;
  signal I2C_SDA_o_dly          : std_logic;
  signal I2C_SDA_s              : std_logic;
  signal I2C_SCL_o              : std_logic;
  signal I2C_SCL_o_dly          : std_logic;
  signal I2C_SCL_s              : std_logic;
  signal I2C_ScanEnable_i       : std_logic;
  signal I2C_ScanClk_i          : std_logic;
  signal I2C_ScanDataIn_i       : std_logic;
  signal I2C_ScanDataOut_o      : std_logic;

  -- The timer has to wait for 240ms. With a 16 bit resolution, the maximumn
  -- counting periode is 3.66us. Here we set the clock signal to 10us = 100kHz.
  -- The timer is preset to 24000.
  constant ClkPeriode : time := 100 ns;

begin

  DUT: Chip
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => I2C_SCL_s,
      Dbg_SDA_b         => I2C_SDA_s,
      P1_b              => P1_b,
      P2_b              => P2_b,
      UartRxD_i         => UartRxD_i,
      UartTxD_o         => UartTxD_o,
      SCK_o             => SCK_o,
      MOSI_o            => MOSI_o,
      MISO_i            => MISO_i,
      Inputs_i          => Inputs_i,
      Outputs_o         => Outputs_o,
      SPIMISO_i         => SPIMISO_i,
      SPIMOSI_o         => SPIMOSI_o,
      SPISCK_o          => SPISCK_o,
      I2CSCL_b          => I2CSCL_b,
      I2CSDA_b          => I2CSDA_b,
--      OneWire_b         => OneWire_b,
--      PWM_i             => PWM_i,
--      SENT_i            => SENT_i,
--      SPC_b             => SPC_b,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o    => AdcDoConvert_o,
      AdcValue_i        => AdcValue_i
    );

  Inputs_i <= (others => '0');

--  Dbg_UART_RxD_i <= '1';
  I2C_SCL_s <= 'H';
  I2C_SDA_s <= 'H';
  P1_b      <= (others => 'H');
  P2_b      <= (others => 'H');
  UartRxD_i <= '1';
  MISO_i    <= '0';
  I2CSCL_b  <= 'H';
  I2CSDA_b  <= 'H';
--  OneWire_b <= 'H';
--  PWM_i     <= 'H';
--  SENT_i    <= 'H';
--  SPC_b     <= 'H';
  AdcConvComplete_i <= '0';
  AdcValue_i <= (others => '0');

  i2c_master_1: i2c_master
    generic map (
      ReadCountWidth_g   => I2C_ReadCountWidth_g,
      FIFOAddressWidth_g => I2C_FIFOAddressWidth_g,
      DividerWidth_g     => I2C_DividerWidth_g)
    port map (
      Reset_i            => "not"(Reset_n_i),
      Clk_i              => Clk_i,
      Divider800_i       => I2C_Divider800_i,
      F100_400_n_i       => I2C_F100_400_n_i,
      StartProcess_i     => I2C_StartProcess_i,
      ReceiveSend_n_i    => I2C_ReceiveSend_n_i,
      Busy_o             => I2C_Busy_o,
      ReadCount_i        => I2C_ReadCount_i,
      FIFOReadNext_i     => I2C_FIFOReadNext_i,
      FIFOWrite_i        => I2C_FIFOWrite_i,
      FIFOEmpty_o        => I2C_FIFOEmpty_o,
      FIFOFull_o         => I2C_FIFOFull_o,
      Data_i             => I2C_Data_i,
      Data_o             => I2C_Data_o,
      ErrAck_i           => I2C_ErrAck_i,
      ErrBusColl_o       => I2C_ErrBusColl_o,
      ErrFIFOFull_o      => I2C_ErrFIFOFull_o,
      ErrGotNAck_o       => I2C_ErrGotNAck_o,
      ErrCoreBusy_o      => I2C_ErrCoreBusy_o,
      ErrFIFOEmpty_o     => I2C_ErrFIFOEmpty_o,
      ErrCoreStopped_o   => I2C_ErrCoreStopped_o,
      ErrDevNotPresent_o => I2C_ErrDevNotPresent_o,
      ErrReadCountZero_o => I2C_ErrReadCountZero_o,
      SDA_i              => I2C_SDA_i,
      SDA_o              => I2C_SDA_o,
      SCL_o              => I2C_SCL_o,
      ScanEnable_i       => I2C_ScanEnable_i,
      ScanClk_i          => I2C_ScanClk_i,
      ScanDataIn_i       => I2C_ScanDataIn_i,
      ScanDataOut_o      => I2C_ScanDataOut_o
    );

  -- delay SCL too, because (at least currently) for the minimum delay
  -- post-synthesis simulation, there are 75ps hold time problems for the SCL
  -- synchronizer cell.
  I2C_SCL_o_dly <= I2C_SCL_o after 1.0 ns;

  I2C_SCL_s <= 'H';      -- weak 1 -> simulate pull-up

  I2C_SCL_s <= '0' when I2C_SCL_o_dly = '0' else 'Z';

  -- delay SDA pull-down of I2C Master by a bit more than 1 clock periode,
  -- because the OpenMSP430 dbg_i2c samples the NACK at the end of an I2C read
  -- transfer two Clk_i cycles after a the falling edge of SCL, but the
  -- I2C_Master pulls it low one Clk_i cycle after it pulled low SCL.
  I2C_SDA_o_dly <= I2C_SDA_o after 1.222*ClkPeriode;

  I2C_SDA_s <= 'H';      -- weak 1 -> simulate pull-up

  I2C_SDA_s <= '0' when I2C_SDA_o_dly = '0' else 'Z';

  I2C_SDA_i <= to_X01(I2C_SDA_s) after 0.2 ns;

  -- Generate clock signal
  Clk_i <= not Clk_i after ClkPeriode*0.5;

  I2CErrProc: process(I2C_ErrBusColl_o, I2C_ErrFIFOFull_o, I2C_ErrGotNAck_o, I2C_ErrCoreBusy_o, I2C_ErrFIFOEmpty_o, I2C_ErrCoreStopped_o, I2C_ErrDevNotPresent_o, I2C_ErrReadCountZero_o)
    variable I2CErr : std_logic;
  begin
    I2CErr := I2C_ErrBusColl_o or I2C_ErrFIFOFull_o or I2C_ErrGotNAck_o or I2C_ErrCoreBusy_o or I2C_ErrFIFOEmpty_o or I2C_ErrCoreStopped_o or I2C_ErrDevNotPresent_o or I2C_ErrReadCountZero_o;
    assert I2CErr /= '1' report "I2C Error" severity failure;
  end process I2CErrProc;

  StimulusProc: process
    variable DbgData : std_logic_vector(15 downto 0);

    procedure DbgWrite8 (
      constant RegAddr : in integer;
      constant DbgData : in std_logic_vector(7 downto 0)
    ) is
    begin
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '1' & '1' & std_logic_vector(to_unsigned(RegAddr,6)); -- write 8 bit
      wait for ClkPeriode;
      I2C_Data_i <= DbgData(7 downto 0); -- LSB
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
    end DbgWrite8;

    procedure DbgWrite16 (
      constant RegAddr : in integer;
      constant DbgData : in std_logic_vector(15 downto 0)
    ) is
    begin
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '1' & '0' & std_logic_vector(to_unsigned(RegAddr,6)); -- write 16 bit
      wait for ClkPeriode;
      I2C_Data_i <= DbgData(7 downto 0); -- LSB
      wait for ClkPeriode;
      I2C_Data_i <= DbgData(15 downto 8); -- MSB
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
    end DbgWrite16;

    procedure DbgWrite16 (
      constant RegAddr : in integer;
      constant DbgData : in integer
    ) is
    begin
      DbgWrite16(RegAddr,std_logic_vector(to_unsigned(DbgData,16)));
    end DbgWrite16;
    
    procedure DbgRead8 (
      constant RegAddr : in integer;
      variable DbgData : out std_logic_vector(7 downto 0)
    ) is
    begin
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '0' & '1' & std_logic_vector(to_unsigned(RegAddr,6)); -- read 8 bit
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
      -- perform read
      I2C_ReadCount_i <= "0001";   -- 1 byte
      I2C_ReceiveSend_n_i <= '1';
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '1'; -- I2C address, read transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
      I2C_ReceiveSend_n_i <= '0';
      DbgData(7 downto 0) := I2C_Data_o;
      I2C_FIFOReadNext_i <= '1';
      wait for ClkPeriode;
      I2C_FIFOReadNext_i <= '0';
      wait for ClkPeriode;
    end DbgRead8;

    procedure DbgRead16 (
      constant RegAddr : in integer;
      variable DbgData : out std_logic_vector(15 downto 0)
    ) is
    begin
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '0' & '0' & std_logic_vector(to_unsigned(RegAddr,6)); -- read 16 bit
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
      -- perform read
      I2C_ReadCount_i <= "0010";   -- 2 bytes
      I2C_ReceiveSend_n_i <= '1';
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '1'; -- I2C address, read transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
      I2C_ReceiveSend_n_i <= '0';
      DbgData(7 downto 0) := I2C_Data_o;
      I2C_FIFOReadNext_i <= '1';
      wait for ClkPeriode;
      DbgData(15 downto 8) := I2C_Data_o;
      wait for ClkPeriode;
      I2C_FIFOReadNext_i <= '0';
      wait for ClkPeriode;
    end DbgRead16;
    
    subtype UInt16_t is std_logic_vector(15 downto 0);
    type DataArray_t is array(natural range <>) of UInt16_t;
    type PDataArray is access DataArray_t;

    procedure DbgWriteBurst (
      constant MemAddr : in integer;
      constant Data    : in DataArray_t
    ) is
      variable i : integer;
    begin
      DbgWrite16(MEM_ADDR,MemAddr);
      DbgWrite16(MEM_CNT, Data'length - 1);
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '1' & '1' & std_logic_vector(to_unsigned(MEM_CTL,6)); -- write 8 bit
      wait for ClkPeriode;
      I2C_Data_i <= "00000011";    -- 16 bits, memory, write, start
      wait for ClkPeriode;
      I2C_StartProcess_i <= '1';
      for i in Data'range loop
        if I2C_FIFOFull_o = '1' then
          I2C_FIFOWrite_i <= '0';
          wait until I2C_FIFOFull_o = '0';
          wait for 0.3*ClkPeriode;
        end if;
        I2C_FIFOWrite_i <= '1';
        I2C_Data_i <= Data(i)(7 downto 0);
        wait for ClkPeriode;
        if I2C_FIFOFull_o = '1' then
          I2C_FIFOWrite_i <= '0';
          wait until I2C_FIFOFull_o = '0';
          wait for 0.3*ClkPeriode;
        end if;
        I2C_FIFOWrite_i <= '1';
        I2C_Data_i <= Data(i)(15 downto 8);
        wait for ClkPeriode;
      end loop;
      I2C_FIFOWrite_i <= '0';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
    end DbgWriteBurst;
    
    procedure DbgReadBurst (
      constant MemAddr : in  integer;
      variable Data    : out DataArray_t
    ) is
      variable i : integer;
    begin
      assert Data'length <= 7 report "DbgReadBurst can read a maximum of 7 bytes" severity failure;
      DbgWrite16(MEM_ADDR,MemAddr);
      DbgWrite16(MEM_CNT, Data'length - 1);
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '0'; -- I2C address, write transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_Data_i <= '1' & '1' & std_logic_vector(to_unsigned(MEM_CTL,6)); -- write 8 bit
      wait for ClkPeriode;
      I2C_Data_i <= "00000001";    -- 16 bits, memory, read, start
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';

      -- perform read
      I2C_ReadCount_i <= std_logic_vector(to_unsigned(Data'length*2,I2C_ReadCountWidth_g));
      I2C_ReceiveSend_n_i <= '1';
      I2C_Data_i <= std_logic_vector(to_unsigned(DbgI2CAddr,7)) & '1'; -- I2C address, read transfer
      I2C_FIFOWrite_i <= '1';
      wait for ClkPeriode;
      I2C_FIFOWrite_i <= '0';
      I2C_StartProcess_i <= '1';
      wait for ClkPeriode;
      wait until I2C_Busy_o = '0'; wait for 0.3*ClkPeriode;
      I2C_StartProcess_i <= '0';
      I2C_ReceiveSend_n_i <= '0';

      DbgData(7 downto 0) := I2C_Data_o;
      I2C_FIFOReadNext_i <= '1';
      for i in Data'range loop
        Data(i)(7 downto 0) := I2C_Data_o;
        wait for ClkPeriode;
        Data(i)(15 downto 8) := I2C_Data_o;
        wait for ClkPeriode;
      end loop;
        --if i = Data'high then
          I2C_FIFOReadNext_i <= '0';
        --end if;
    end DbgReadBurst;

    procedure DbgWriteBurst (
      constant Data : in HexRecord) is
      variable DataArray : PDataArray;
    begin  -- DbgWriteBurst
      DataArray := new DataArray_t(0 to (Data.Len+1)/2-1);
      for i in 0 to Data.Len/2-1 loop
        DataArray.all(i) := Data.Data(i*2+1) & Data.Data(i*2);
      end loop;  -- i
      if Data.Len mod 2 /= 0 then
        DataArray.all((Data.Len+1)/2-1) := "00000000" & Data.Data(Data.Len-1);
      end if;
      DbgWriteBurst(Data.Addr,DataArray.all);
    end DbgWriteBurst;

    ---------------------------------------------------------------------------
    -- Execute a power-up clear (PUC) command
    -- see OpenMSP430 tools/lib/tcl-lib/dbg_functions.tcl
    procedure ExecutePOR is
      variable cpu_ctl_org  : std_logic_vector(7 downto 0);
      variable cpu_ctl_new  : std_logic_vector(7 downto 0);
      variable cpu_stat_val : std_logic_vector(7 downto 0);
    begin  -- ExecutePOR_Halt
      -- query CPU_CTL
      DbgRead8(CPU_CTL,cpu_ctl_org);
      -- Set PUC
      --report "Set PUC" severity note;
      cpu_ctl_new := cpu_ctl_org or  "01000000";  -- set CPU_RST
      DbgWrite8(CPU_CTL, cpu_ctl_new);
      -- Remove PUC, clear break after reset
      --report "Remove PUC, clear break after reset" severity note;
      cpu_ctl_org := cpu_ctl_org and "01011111";  -- reset RST_BRK_EN
      DbgWrite8(CPU_CTL, cpu_ctl_org);
      -- Check status: make sure a PUC occured
      --report "Check status: make sure a PUC occured" severity note;
      DbgRead8(CPU_STAT,cpu_stat_val);
      assert cpu_stat_val(2) = '1' report "No PUC occured" severity error;
      -- Clear PUC pending flag
      DbgWrite8(CPU_STAT, "00000100");
    end ExecutePOR;

    ---------------------------------------------------------------------------
    -- Same as ExecutePOR with the difference that the CPU automatically goes
    -- in Halt mode after reset.
    -- see OpenMSP430 tools/lib/tcl-lib/dbg_functions.tcl
    procedure ExecutePOR_Halt is
      variable cpu_ctl_org  : std_logic_vector(7 downto 0);
      variable cpu_ctl_new  : std_logic_vector(7 downto 0);
      variable cpu_stat_val : std_logic_vector(7 downto 0);
    begin  -- ExecutePOR_Halt
      -- query CPU_CTL
      DbgRead8(CPU_CTL,cpu_ctl_org);
      cpu_ctl_new := cpu_ctl_org or "01100000";  -- set CPU_RST and RST_BRK_EN
      DbgWrite8(CPU_CTL, cpu_ctl_new);
      DbgWrite8(CPU_CTL, cpu_ctl_org);
      -- Check status: make sure a PUC occured and that the CPU is halted
      DbgRead8(CPU_STAT,cpu_stat_val);
      assert cpu_stat_val(2) = '1' and cpu_stat_val(0) = '1' report "No PUC occured or CPU not halted" severity error;
      -- Clear PUC pending flag
      DbgWrite8(CPU_STAT, "00000100");
    end ExecutePOR_Halt;

    ---------------------------------------------------------------------------
    -- Releases the target device's CPU from the controlled, stopped state.
    -- (Does not release the target device from debug control.)
    -- see OpenMSP430 tools/lib/tcl-lib/dbg_functions.tcl
    procedure ReleaseCPU is
      variable cpu_ctl_org  : std_logic_vector(7 downto 0);
      variable cpu_ctl_new  : std_logic_vector(7 downto 0);
      variable cpu_stat_val : std_logic_vector(7 downto 0);
    begin  -- ExecutePOR_Halt
      -- query CPU_CTL
      DbgRead8(CPU_CTL,cpu_ctl_org);
      -- Start CPU
      --report "Start CPU" severity note;
      cpu_ctl_new := cpu_ctl_org or  "00000010";  -- set RUN
      DbgWrite8(CPU_CTL, cpu_ctl_new);
      -- Check status: make sure the CPU runs
      ----report "Check status: make sure the CPU runs" severity note;
      DbgRead8(CPU_STAT,cpu_stat_val);
      assert cpu_stat_val(0) = '0' report "CPU doesn't run" severity error;
    end ReleaseCPU;

    procedure ReleaseDevice is
    begin  -- ReleaseDevice
      ExecutePOR;
      ReleaseCPU;
    end ReleaseDevice;
    
    variable DataRd : DataArray_t(0 to 5);
    variable HexFile : PHexRecordArray;
  begin
    -- Chip
    Dbg_En_i <= '0';
    -- I2C Master
    I2C_Divider800_i       <= std_logic_vector(to_unsigned(9,I2C_DividerWidth_g));   -- 9+1 clock cycles per half bit at "400kHz enabled"
    I2C_F100_400_n_i       <= '0';   -- 400kHz enabled
    I2C_StartProcess_i     <= '0';
    I2C_ReceiveSend_n_i    <= '0';
    I2C_ReadCount_i        <= (others => '0');
    I2C_FIFOReadNext_i     <= '0';
    I2C_FIFOWrite_i        <= '0';
    I2C_Data_i             <= (others => '0');
    I2C_ErrAck_i           <= '0';
    I2C_ScanEnable_i       <= '0';
    I2C_ScanClk_i          <= '0';
    I2C_ScanDataIn_i       <= '0';
    wait for 2.3*ClkPeriode;

    -- deassert Reset
    Reset_n_i <= '1';
    wait for 50*ClkPeriode;

    Dbg_En_i <= '1';
    Cpu_En_i <= '1';
    DbgRead16(CPU_ID_LO,DbgData); -- read 16 bit from 0x00 (DBG_REG_CPU_ID_LO)
    report "CPU_ID_LO = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    -- 0x0202 = 514
    DbgRead16(CPU_ID_HI,DbgData); -- read 16 bit from 0x01 (DBG_REG_CPU_ID_HO)
    report "CPU_ID_JO = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    -- 0x2004 = 8196

    DbgWrite16(MEM_ADDR,std_logic_vector(to_unsigned(12345,16)));
    DbgRead16(MEM_ADDR,DbgData);
    report "MEM_ADDR = " & integer'image(to_integer(unsigned(DbgData))) severity note;

    -- write 0xF0CC 0xAA55
    DbgWriteBurst(16#E000#,("1111000011001100","1010101001010101"));

    report "written to memory" severity note;
    wait for 100*ClkPeriode;

    DbgReadBurst(16#E000#,DataRd);
    for i in DataRd'range loop
      report "DataRd(" & integer'image(i) & ") = " & integer'image(to_integer(unsigned(DataRd(i))));
    end loop;

    wait for 100*ClkPeriode;

    DbgRead16 (MEM_DATA,DbgData);
    report "MEM_DATA = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    -- this returns the latest burst read value

    wait for 100*ClkPeriode;

    -- read a few words from PMem
    DbgWrite16(MEM_ADDR,std_logic_vector(to_unsigned(16#E000#,16)));
    DbgWrite8 (MEM_CTL,"00000000");
    DbgWrite16(MEM_DATA,std_logic_vector(to_unsigned(54321,16)));
    DbgWrite16(MEM_DATA,std_logic_vector(to_unsigned(12345,16)));
    DbgWrite16(MEM_DATA,std_logic_vector(to_unsigned(19283,16)));
    DbgWrite16(MEM_ADDR,std_logic_vector(to_unsigned(16#E000#,16)));
    DbgRead16 (MEM_DATA,DbgData);
    report "MEM_DATA = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    DbgRead16 (MEM_DATA,DbgData);
    report "MEM_DATA = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    DbgRead16 (MEM_DATA,DbgData);
    report "MEM_DATA = " & integer'image(to_integer(unsigned(DbgData))) severity note;
    -- all three DbgRead16 return the latest value written to MEM_DATA (19283)

    wait for 100*ClkPeriode;

    -- write burst 6 words
    DbgWriteBurst(16#E000#,(x"1234",x"5678",x"9ABC",x"DEF0",x"4321",x"8765"));
    report "written 6 words to memory at 0xE000" severity note;
    DbgWriteBurst(16#F000#,(x"1234",x"5678",x"9ABC",x"DEF0",x"4321",x"8765"));
    report "written 6 words to memory at 0xF000" severity note;
    wait for 100*ClkPeriode;
    -- read back
    DbgReadBurst(16#E000#,DataRd);
    for i in DataRd'range loop
      report "DataRd(" & integer'image(i) & ") = " & integer'image(to_integer(unsigned(DataRd(i))));
    end loop;

    wait for 30*ClkPeriode;

    ---------------------------------------------------------------------------
    -- read firmware from Hex file
    ExecutePOR_Halt;

    report "Downloading firmware from Intel Hex file" severity note;
    HexFile := ReadHexFile("../firmware/blinki/blinki.hex");
    for i in HexFile.all'low to HexFile.all'high loop
      DbgWriteBurst(HexFile.all(i));
    end loop;  -- i
    report "  --> done" severity note;

    -- run CPU
    ReleaseDevice;

    wait for 10000*ClkPeriode;

    ---------------------------------------------------------------------------
    -- assert Reset
    report "Asserting Reset_n_i with Dbg_En_i = '1' ==> CPU should be stopped" severity note;
    Reset_n_i <= '0';
    Dbg_En_i  <= '1';
    Cpu_En_i  <= '1';
    wait for 5*ClkPeriode;
    -- deassert Reset
    Reset_n_i  <= '1';
    wait for 500*ClkPeriode;
    -- deassert Dbg_En_i
    report "Deasserting Dbg_En_i = '0' ==> CPU should start executing" severity note;
    Dbg_En_i  <= '0';
    wait for 5000*ClkPeriode;
    -- assert Reset
    report "Asserting Reset_n_i with Dbg_En_i = '0' ==> CPU should start executing immediately" severity note;
    Reset_n_i <= '0';
    Dbg_En_i  <= '0';
    Cpu_En_i  <= '1';
    wait for 5*ClkPeriode;
    -- deassert Reset
    Reset_n_i  <= '1';
    wait for 5000*ClkPeriode;
    -- assert Dbg_En_i
    report "Asserting Dbg_En_i = '1' ==> CPU should keep executing" severity note;
    Dbg_En_i  <= '1';
    wait for 5000*ClkPeriode;

    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

end behavior;
