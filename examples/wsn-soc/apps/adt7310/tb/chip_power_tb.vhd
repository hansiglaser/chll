library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.UartPkg.all;

entity Chip_Power_tb is
  generic (
    SPISelect  : integer :=    1;   -- 0: CPU, 1: CHLL
    ImpulseGen : integer :=    0    -- 0: no, 1: connect Inputs_i(7) to a signal with pulses for wakeup from LPM3
  );
end Chip_Power_tb;

architecture behavior of Chip_Power_tb is

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

  component adt7310_model
    port (
      SCLK_i  : in  std_logic;
      DOUT_o  : out std_logic;
      DIN_i   : in  std_logic;
      CS_n_i  : in  std_logic;
      CT_n_o  : out std_logic;
      INT_n_o : out std_logic;
      Temp_i  : in  real);
  end component;

--  component ExtNames
--    port (
--      SPIFSM_Done : out std_logic;
--      CpuIntr     : out std_logic;
--      SensorValue : out std_logic_vector(15 downto 0)
--    );
--  end component;

  -- Reset
  signal Reset_n_i         : std_logic := '0';
  -- Clock
  signal Clk_i             : std_logic;
  signal Cpu_En_i          : std_logic := '1';
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
  signal SPIMISO_i         : std_logic;
  signal SPIMOSI_o         : std_logic;
  signal SPISCK_o          : std_logic;
  signal I2CSCL_b          : std_logic;
  signal I2CSDA_b          : std_logic;
--  signal OneWire_b         : std_logic;
--  signal PWM_i             : std_logic;
--  signal SENT_i            : std_logic;
--  signal SPC_b             : std_logic;
  signal AdcConvComplete_i : std_logic;
  signal AdcDoConvert_o    : std_logic;
  signal AdcValue_i        : std_logic_vector(9 downto 0);

--  -- look into the ADT7310 app
--  -- alias SPIFSM_Done_i is << signal .adt7310_tb.DUT.SPIFSM_Done_s : std_logic >>;
--  -- ModelSim complains here, that the references signal is not a VHDL object.
--  -- True, this is a Verilog object. As a workaround the module ExtNames is created
--  -- which uses Verilog hierarchical names to reference the wire and assigns it to
--  -- an output. This module is instantiated (and it seems ModelSim only adds
--  -- Verilog<->VHDL signal converters on instance boundaries) and this output is
--  -- connected with the SPIFSM_Done_i signal.
--  signal SPIFSM_Done_e : std_logic;  -- directly from inside SPI_FSM
--  signal CpuIntr_e     : std_logic;  -- directly from inside SPI_FSM
--  signal SensorValue_e : std_logic_vector(15 downto 0);
--  -- Using the extracted Yosys FSM we get delta cycles and a glitch on
--  -- SPIFSM_Done_i. Therefore we generate a slightly delayed version and wait
--  -- on the ANDed value.
--  signal SPIFSM_Done_d : std_logic;  -- sightly delayed
--  signal CpuIntr_o     : std_logic;  -- sightly delayed
--  signal SensorValue_o : std_logic_vector(15 downto 0);  -- sightly delayed
--  signal SensorValue_real : real;

  signal Clk_s         : std_logic := '1';
  signal RTC_s         : std_logic := '1';
  signal ClkEnable_s   : std_logic := '0';
  signal RTCEnable_s   : std_logic := '0';
  signal RTC_i         : std_logic;

  type ImpulseGenState_t is (igsIdle,igsPulse1,igsWait1,igsPulse2,igsWait2);
  signal ImpulseGenState : ImpulseGenState_t := igsIdle;
  signal ImpulseGen_s  : std_logic := '0';

  -- default values, the firmware relies on these!
  constant MCUStdClkDiv    : integer :=   24;  -- standard frequency of 100/(24+1) = 4MHz for MCU UART for 9600 baud
  constant RTCClockDivider : integer := 3051;
  -- setup values for testpoint
  signal ClkDiv            : integer :=   99;   -- 100MHz/(99+1) = 1MHz
  signal ClkDivRTC         : integer := 3051;   -- 100MHz/(3051+1) = 32.765kHz
  signal Threshold         : integer :=  100;
  signal CycleTime         : integer :=    5;
  signal BusDivider        : integer :=    1;
  -- derived values from setup values
  signal ClkPeriode        : time    := 10 ns * (MCUStdClkDiv+1);
  signal RTCPeriode        : time    := 10 ns * (RTCClockDivider+1);
  -- simulation control signals: these will be set using "force" by the simulator
  signal HaveTestpoint     : boolean := true;

  -- Trigger to start/stop the power analysis
  signal TriggerEnable_s   : std_logic := '0';
  signal TriggerSPI_s      : std_logic := '0';
  signal TriggerManual_s   : std_logic := '0';
  signal Trigger_s         : std_logic := '0';
  signal Triggered_s       : std_logic := '0';   -- stays '1' until ResetTriggered_s is asserted
  signal ResetTriggered_s  : std_logic := '0';

  -- ADT7310 component ports
  signal ADT7310CS_n_s : std_logic;
  signal ADT7310MISO_s : std_logic;
  signal ADT7310MOSI_s : std_logic;
  signal ADT7310SCK_s  : std_logic;
  signal CT_n_s        : std_logic;
  signal INT_n_s       : std_logic;
  signal Temp_s        : real := 23.7;

  component Uart
    generic (
      MaxDataWidth         : integer range 5 to 9;
      MaxSpeedDividerWidth : integer range 2 to 32;
      TxFifoAdressWidth    : integer range 2 to 10;
      RxFifoAdressWidth    : integer range 2 to 10;
      Oversampling         : integer range 2 to 2);
    port (
      TxData_i                     : in  STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      TxWr_i                       : in  STD_LOGIC;
      TxEmpty_o                    : out STD_LOGIC;
      TxFull_o                     : out STD_LOGIC;
      RxData_o                     : out STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
      RxRd_i                       : in  STD_LOGIC;
      RxFull_o                     : out STD_LOGIC;
      RxEmpty_o                    : out STD_LOGIC;
      BitsSelect_i                 : in  BitSelectionType;
      ParityOn_i                   : in  STD_LOGIC;
      ParityEvenOdd_i              : in  ParityType;
      SpeedDivider_i               : in  STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0);
      Clk_i                        : in  STD_LOGIC;
      Reset_i_n                    : in  STD_LOGIC;
      ErrorReset_i                 : in  STD_LOGIC;
      RxParityErrorIndicator_o     : out STD_LOGIC;
      RxStopBitErrorIndicator_o    : out STD_LOGIC;
      RxBufferFullErrorIndicator_o : out STD_LOGIC;
      TxD_o                        : out STD_LOGIC;
      RxD_i                        : in  STD_LOGIC;
      ScanEnable_i                 : in  std_logic;
      ScanClk_i                    : in  std_logic;
      ScanDataIn_i                 : in  std_logic;
      ScanDataOut_o                : out std_logic
    );
  end component;

  constant MaxDataWidth         : integer range 5 to 9  := 8;
  constant MaxSpeedDividerWidth : integer range 2 to 32 := 16;
  constant TxFifoAdressWidth    : integer range 2 to 10 := 4;
  constant RxFifoAdressWidth    : integer range 2 to 10 := 4;
  constant Oversampling         : integer range 2 to 2  := 2;

  -- clock_freq/(2**Oversampling * Baudrate)
  -- 4MHz/(2**2 * 9600) = 104.1666 rounded --> 104
  -- baudrate = clock_frequency / (2**Oversampling * SpeedDivider) = 4MHz / (2**2 * 104) = 9615.4
  -- error: (9615.4-9600)/9600 = -0.0016
  -- 9600 Baud is very slow in simulation, so we use 115200 Baud
  -- 4MHz/(2**2 * 115200) = 8.68055 rounded --> 9
  -- baudrate = clock_frequency / (2**Oversampling * SpeedDivider) = 4MHz / (2**2 * 9) = 111111.1
  -- error: (111111.111111-115200)/115200 = -0.0354938271615
  constant SpeedDivider : integer := 9;

  signal TxData_i                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0) := (others => '0');
  signal TxWr_i                       : STD_LOGIC := '0';
  signal TxEmpty_o                    : STD_LOGIC;
  signal TxFull_o                     : STD_LOGIC;
  signal RxData_o                     : STD_LOGIC_VECTOR((MaxDataWidth-1) downto 0);
  signal RxRd_i                       : STD_LOGIC;
  signal RxFull_o                     : STD_LOGIC;
  signal RxEmpty_o                    : STD_LOGIC;
  signal BitsSelect_i                 : BitSelectionType := Sel8Bits;
  signal ParityOn_i                   : STD_LOGIC := '0';
  signal ParityEvenOdd_i              : ParityType := Even;
  signal SpeedDivider_i               : STD_LOGIC_VECTOR((MaxSpeedDividerWidth-1) downto 0) := std_logic_vector(to_unsigned(SpeedDivider,MaxSpeedDividerWidth));
  signal ErrorReset_i                 : STD_LOGIC := '0';
  signal RxParityErrorIndicator_o     : STD_LOGIC;
  signal RxStopBitErrorIndicator_o    : STD_LOGIC;
  signal RxBufferFullErrorIndicator_o : STD_LOGIC;
  signal ScanEnable_i                 : std_logic := '0';
  signal ScanClk_i                    : std_logic := '0';
  signal ScanDataIn_i                 : std_logic := '0';
  signal ScanDataOut_o                : std_logic;
  subtype Byte is std_logic_vector(7 downto 0);
  signal UartRx                       : Byte := (others => '0');
  signal UartCh                       : character := NUL;
  constant UartRxFifoAddrWidth        : integer := 4;
  type UartRxFifo_t is array(0 to (2**UartRxFifoAddrWidth-1)) of character;
  signal UartRxFifo                   : UartRxFifo_t;
  signal UartRxFifoWr                 : integer := 0;
  signal UartRxFifoRd                 : integer := 0;
begin

  DUT: Chip
    port map (
      Reset_n_i         => Reset_n_i,
      Clk_i             => Clk_i,
      Cpu_En_i          => Cpu_En_i,
      Dbg_En_i          => Dbg_En_i,
--      Dbg_UART_RxD_i    => Dbg_UART_RxD_i,
--      Dbg_UART_TxD_o    => Dbg_UART_TxD_o,
      Dbg_SCL_i         => Dbg_SCL_i,
      Dbg_SDA_b         => Dbg_SDA_b,
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

  SPISelectCPU: if SPISelect = 0 generate
    ADT7310CS_n_s <= P1_b(0);
    MISO_i        <= ADT7310MISO_s ;
    ADT7310MOSI_s <= MOSI_o;
    ADT7310SCK_s  <= SCK_o;
    SPIMISO_i     <= '0';
  end generate;
  SPISelectCHLL: if SPISelect = 1 generate
    ADT7310CS_n_s <= Outputs_o(0);
    SPIMISO_i     <= ADT7310MISO_s ;
    ADT7310MOSI_s <= SPIMOSI_o;
    ADT7310SCK_s  <= SPISCK_o;
    MISO_i        <= '0';
  end generate;

  ImpulseGenNo: if ImpulseGen = 0 generate
    Inputs_i <= (others => '0');
  end generate;
  ImpulseGenYes: if ImpulseGen = 1 generate

    ImpulseGenProc: process(TriggerEnable_s, Clk_s)
      variable NextEvent : time := 0.0 ns;
    begin
      if rising_edge(TriggerEnable_s) then
        -- start with pulse generation
        ImpulseGenState <= igsPulse1; -- trick: MCU is already waiting in the 2ms periode, so we skip the first pulse and directly enter in the waiting periode
        ImpulseGen_s    <= '0';
        NextEvent       := now + 5 * (10 ns * (ClkDiv+1));  -- 5 Clk cycles
      elsif falling_edge(TriggerEnable_s) then
        -- stop with pulse generation
        ImpulseGenState <= igsIdle;
        ImpulseGen_s    <= '0';
      end if;
      if now >= NextEvent then
        -- ok, we have something to do
        case ImpulseGenState is
          when igsIdle => null;
          when igsPulse1 =>
            -- end of first pulse
            ImpulseGen_s    <= '0';
            NextEvent       := now + 1.85 ms;
            ImpulseGenState <= igsWait1;
          when igsWait1 =>
            -- begin second pulse
            ImpulseGen_s    <= '1';
            NextEvent       := now + 5 * (10 ns * (ClkDiv+1));  -- 5 Clk cycles
            ImpulseGenState <= igsPulse2;
          when igsPulse2 =>
            -- end of second pulse
            ImpulseGen_s    <= '0';
            NextEvent       := now + (real(CycleTime)-1.85) * 1 ms;
            ImpulseGenState <= igsWait2;
          when igsWait2 =>
            -- begin of first pulse
            ImpulseGen_s    <= '1';
            NextEvent       := now + 5 * (10 ns * (ClkDiv+1));  -- 5 Clk cycles
            ImpulseGenState <= igsPulse1;
          when others =>
            report "Invalid ImpulseGenState" severity failure;
        end case;
      end if;
    end process ImpulseGenProc;

    Inputs_i <= (7 => ImpulseGen_s, others => '0');
  end generate;

  Cpu_En_i  <= '1';
  Dbg_En_i  <= '0';
--  Dbg_UART_RxD_i <= '1';
  Dbg_SCL_i <= 'H';
  Dbg_SDA_b <= 'H';
  P1_b      <= (others => 'H');
  P2_b      <= (1 => RTC_i, others => 'H');
  I2CSCL_b  <= 'H';
  I2CSDA_b  <= 'H';
--  OneWire_b <= 'H';
--  PWM_i     <= 'H';
--  SENT_i    <= 'H';
--  SPC_b     <= 'H';
  AdcConvComplete_i <= '0';
  AdcValue_i <= (others => '0');

--  ExtNames_1: ExtNames
--    port map (
--      SPIFSM_Done => SPIFSM_Done_e,
--      CpuIntr     => CpuIntr_e,
--      SensorValue => SensorValue_e
--    );
--  SPIFSM_Done_d <= SPIFSM_Done_e after 1.0 ns;
--  CpuIntr_o     <= CpuIntr_e     after 1.0 ns;
--  SensorValue_o <= SensorValue_e after 1.0 ns;
--
--  SensorValue_real <= real(to_integer(unsigned(SensorValue_o)))/128.0;

  adt7310_1: adt7310_model
    port map (
      SCLK_i  => ADT7310SCK_s,
      DOUT_o  => ADT7310MISO_s,
      DIN_i   => ADT7310MOSI_s,
      CS_n_i  => ADT7310CS_n_s,
      CT_n_o  => CT_n_s,
      INT_n_o => INT_n_s,
      Temp_i  => Temp_s);

  Uart_1: Uart
    generic map (
      MaxDataWidth         => MaxDataWidth,
      MaxSpeedDividerWidth => MaxSpeedDividerWidth,
      TxFifoAdressWidth    => TxFifoAdressWidth,
      RxFifoAdressWidth    => RxFifoAdressWidth,
      Oversampling         => Oversampling
    )
    port map (
      TxData_i                     => TxData_i,
      TxWr_i                       => TxWr_i,
      TxEmpty_o                    => TxEmpty_o,
      TxFull_o                     => TxFull_o,
      RxData_o                     => RxData_o,
      RxRd_i                       => RxRd_i,
      RxFull_o                     => RxFull_o,
      RxEmpty_o                    => RxEmpty_o,
      BitsSelect_i                 => BitsSelect_i,
      ParityOn_i                   => ParityOn_i,
      ParityEvenOdd_i              => ParityEvenOdd_i,
      SpeedDivider_i               => SpeedDivider_i,
      Clk_i                        => Clk_i,
      Reset_i_n                    => Reset_n_i,
      ErrorReset_i                 => ErrorReset_i,
      RxParityErrorIndicator_o     => RxParityErrorIndicator_o,
      RxStopBitErrorIndicator_o    => RxStopBitErrorIndicator_o,
      RxBufferFullErrorIndicator_o => RxBufferFullErrorIndicator_o,
      TxD_o                        => UartRxD_i,
      RxD_i                        => UartTxD_o,
      ScanEnable_i                 => ScanEnable_i,
      ScanClk_i                    => ScanClk_i,
      ScanDataIn_i                 => ScanDataIn_i,
      ScanDataOut_o                => ScanDataOut_o
    );

  -- Generate clock signal
  Clk_s <= not Clk_s after ClkPeriode*0.5;
  RTC_s <= not RTC_s after RTCPeriode*0.5;
  Clk_i <= Clk_s and ClkEnable_s;  -- TODO: currently there is no glitch avoidance
  RTC_i <= RTC_s and RTCEnable_s;

  StimulusProc: process

    procedure PutChar (
      constant Ch : in character) is
    begin  -- PutChar
      TxWr_i <= '1';
      TxData_i <= std_logic_vector(to_unsigned(character'pos(Ch),8));
      wait for ClkPeriode;
      TxWr_i <= '0';
    end PutChar;

    -- get received value in the global signal 
    procedure Recv(
      constant Timeout : in time;
      variable Ch      : out character
    ) is
      variable EndTime : time;
    begin
      EndTime := now + Timeout;
      while (UartRxFifoWr = UartRxFifoRd) and (now < EndTime) loop
        -- wait for 1 bit
        wait for (2**Oversampling * SpeedDivider) * ClkPeriode;
      end loop;
      if UartRxFifoWr = UartRxFifoRd then
        report "Recv timeout reached" severity failure;
      end if;
      -- received at least 1 byte
      Ch := UartRxFifo(UartRxFifoRd);
      UartRxFifoRd <= (UartRxFifoRd+1) mod (2**UartRxFifoAddrWidth); -- no "and" for integers :-(
    end Recv;

    procedure AssertMCUInitComplete is
      variable Ch : character;
    begin
      Recv(100.0 ms, Ch);
      if Ch /= 'I' then
        report "Received crap: '" & Ch & "'" severity failure;
      end if;
    end AssertMCUInitComplete;

    procedure SetThreshold(constant Threshold : integer) is
    begin
      case Threshold is
        when   0 => PutChar('q');
        when   1 => PutChar('w');
        when   2 => PutChar('e');
        when   3 => PutChar('r');
        when   4 => PutChar('t');
        when   5 => PutChar('z');
        when   6 => PutChar('u');
        when   7 => PutChar('i');
        when   8 => PutChar('o');
        when   9 => PutChar('p');
        when 100 => PutChar('m');
        when others =>
          report "Invalid threshold " & integer'image(Threshold) severity failure;
      end case;
    end SetThreshold;

    procedure SetCycleTime(constant CycleTime : integer) is
    begin
      case CycleTime is
        when    0 => PutChar('0'); -- no sleep
        when    5 => PutChar('a');
        when   10 => PutChar('s');
        when   20 => PutChar('d');
        when   33 => PutChar('f');
        when   40 => PutChar('g');
        when   50 => PutChar('h');
        when  100 => PutChar('j');
        when  200 => PutChar('k');
        when 2000 => PutChar('l'); -- sleep
        when others =>
          report "Invalid cycle time " & integer'image(CycleTime) severity failure;
      end case;
    end SetCycleTime;

    procedure SetBusDivider(constant BusDivider : integer) is
    begin
      case BusDivider is
        when  1 => PutChar('b');
        when  2 => PutChar('y');
        when  4 => PutChar('x');
        when  8 => PutChar('c');
        when 16 => PutChar('v');
        when others =>
          report "Invalid busdivider " & integer'image(BusDivider) severity failure;
      end case;
    end SetBusDivider;

    procedure SetFrequencyCompensation(constant ClkDiv : integer) is
    begin
      case ClkDiv is
        when 3051 => PutChar('A');
        when 1999 => PutChar('S');
        when  999 => PutChar('D');
        when  499 => PutChar('F');
        when  332 => PutChar('G');
        when  249 => PutChar('H');
        when  199 => PutChar('J');
        when  124 => PutChar('K');
        when   99 => PutChar('Q');
        when   49 => PutChar('W');
        when   32 => PutChar('E');
        when   24 => PutChar('R');
        when   19 => PutChar('T');
        when   15 => PutChar('Z');
        when   12 => PutChar('U');
        when   11 => PutChar('I');
        when   10 => PutChar('O');
        when    9 => PutChar('P');
        when    0 => SetCycleTime(2000); -- ClkDiv = 0 means frequency = 0, set MCU to sleep mode
        when others =>
          report "Invalid clock divider " & integer'image(ClkDiv) severity failure;
      end case;
    end SetFrequencyCompensation;

    procedure AssertMCUReady is
      variable Ch : character;
    begin
      PutChar('C'); -- setup complete - continue execution
      Recv(100.0 ms, Ch);
      if Ch /= 'R' then
        report "MCU not ready for measurement." severity failure;
      end if;
    end AssertMCUReady;

  begin
    -- TODO: setup SPI Simulator

    while HaveTestpoint loop
      HaveTestpoint <= false;   -- the simulator will set it back to to true if another loop execution is requested
      -- setup standard Clk (4MHz) for setup
      ClkPeriode <= 10 ns * (MCUStdClkDiv+1);
      RTCPeriode <= 10 ns * (RTCClockDivider+1);
      ClkEnable_s <= '1';
      RTCEnable_s <= '1';
      Temp_s <= 0.0; -- degree C, don't disturb the tests
      -- reset MCU (but first ensure a fixed relative position to the last Clk edge to avoid complaints of clock gate cells and SRAMs)
      wait until rising_edge(Clk_s);
      wait for 0.3*ClkPeriode;
      report "MCU reset" severity note;
      Reset_n_i <= '0';
      wait for 2.3*ClkPeriode;
      -- deassert Reset
      Reset_n_i <= '1';
      wait for 1.3*ClkPeriode;                     -- wait until spi_master's SCK_o goes '1' to conform to CPOL_i = '1'
  
      -- wait until MCU has initialized, it will send 'I'
      AssertMCUInitComplete;
      report "MCU startup complete, performing setup: " &
        "ClkDiv = "     & integer'image(ClkDiv)     & ", " &
        "ClkDivRTC = "  & integer'image(ClkDivRTC)  & ", " &
        "Threshold = "  & integer'image(Threshold)  & ", " &
        "CycleTime = "  & integer'image(CycleTime)  & ", " &
        "BusDivider = " & integer'image(BusDivider)
        severity note;
  
      -- change bus divider according to test point
      SetBusDivider(BusDivider);
      -- change cycle time according to test point
      SetCycleTime(CycleTime);
      -- change threshold according to test point
      SetThreshold(Threshold);
      -- change frequency compensation according to test point
      SetFrequencyCompensation(ClkDiv);
  
      -- wait for MCU ready, send 'R' to CPU, it will reply 'C'
      AssertMCUReady;
      report "MCU setup complete, now starting sensor measurement cycles" severity note;
  
      -- set final clock frequencies
      if ClkDiv /= 0 then
        ClkPeriode <= 10 ns * (ClkDiv+1);
      else
        ClkEnable_s <= '0';
      end if;
      if ClkDivRTC /= 0 then
        RTCPeriode <= 10 ns * (ClkDivRTC+1);
      else
        RTCEnable_s <= '0';
      end if;
      -- TODO: set impulse durations

      -- enable trigger to start the "measurement"
      if (CycleTime > 0) and (CycleTime < 2000) and (ClkDiv /= 0) then
        -- use trigger
        TriggerEnable_s  <= '1';
        -- reset latching Triggered_s
        ResetTriggered_s <= '1';
        wait for 1 ns;
        ResetTriggered_s <= '0';
        -- wait for trigger at most 100ms plus cycle time
        for i in 1 to (100+CycleTime) loop
          wait for 1 ms;
          if Triggered_s = '1' then
            exit;
          end if;
        end loop;
        assert Triggered_s = '1' report "Missing trigger" severity error;
        -- reset latching Triggered_s
        ResetTriggered_s <= '1';
        wait for 1 ns;
        ResetTriggered_s <= '0';
        -- let it do 5 measurement cycles at most
        for i in 1 to 5 loop
          wait for CycleTime * 1.05 ms;   -- wait a bit longer than 1 cycle
          if HaveTestpoint then
            -- the simulator already has set HaveTestpoint back to true, so we
            -- are done waiting and can continue with the outer loop to
            -- simulate the next testpoint
            exit;
          end if;
        end loop;
        TriggerEnable_s  <= '0';
      else
        -- start immediately, simulate a 200ms delay
        wait for 10 ns*(ClkDiv+1) * 100;   -- wait 100 Clk cycles until LPMx is established, can't use ClkPeriode in case ClkDiv = 0
        TriggerManual_s <= '1';
        wait for 10 ns;
        TriggerManual_s <= '0';
        wait for (200.0 ms - 10.0 ns);
        TriggerManual_s <= '1';
        wait for 10 ns;
        TriggerManual_s <= '0';
        wait for 1 us;
      end if;
    end loop;
      
    -- End of simulation
    report "### Simulation Finished ###"  severity failure;
    wait;
  end process StimulusProc;

  -- trigger detector
  TriggerProc: process(TriggerEnable_s, Clk_s, ResetTriggered_s)
    variable LastCS_n  : std_logic := 'X';
    variable LastEvent : time := 0.0 ns;
  begin
    if rising_edge(TriggerEnable_s) then
      LastEvent := now;
    end if;
    if rising_edge(Clk_s) then
      if LastCS_n = '1' and ADT7310CS_n_s = '0' then
        if ((now - LastEvent) > 2.8 ms) and (TriggerEnable_s = '1') then
          TriggerSPI_s <= '1';
          Triggered_s  <= '1';
        end if;
        LastEvent := now;
      elsif LastCS_n = '0' and ADT7310CS_n_s = '1' then
        TriggerSPI_s <= '0';
      end if;
      LastCS_n := ADT7310CS_n_s;
    end if;
    if rising_edge(ResetTriggered_s) then
      Triggered_s  <= '0';
    end if;
  end process TriggerProc;

  Trigger_s <= TriggerSPI_s or TriggerManual_s;

  -- read Uart_1 FIFO
  UartRx_Proc: process
  begin
    RxRd_i <= '0';
    wait for 0.2*ClkPeriode;
    while true loop
      RxRd_i <= not RxEmpty_o;
      if RxEmpty_o = '0' then
        -- store to variable, because as soon as RxRd_i is '1', RxData_o gives the next value from the FIFO
        UartRx <= RxData_o;
        UartCh <= character'val(to_integer(unsigned(RxData_o)));
        wait for 1 ns;  -- update signals
        UartRxFifo(UartRxFifoWr) <= UartCh;
        UartRxFifoWr <= (UartRxFifoWr + 1) mod (2**UartRxFifoAddrWidth); -- no "and" for integers :-(
        --report "Received '" & character'val(to_integer(unsigned(RxData_o))) & "'" severity note;
      end if;
      wait for ClkPeriode;
    end loop;
  end process UartRx_Proc;

end behavior;
