-- Automatically generated: write_netlist -parent -vhdl -module parent-module.vhd

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

architecture struct of Core is

  component $not
    generic (
      A_SIGNED : integer := 0;
      A_WIDTH : integer := 1;
      Y_WIDTH : integer := 1
    );
    port (
      A : in std_logic;
      Y : out std_logic
    );
  end component;

  component $logic_and
    generic (
      A_SIGNED : integer := 0;
      B_SIGNED : integer := 0;
      A_WIDTH : integer := 1;
      B_WIDTH : integer := 1;
      Y_WIDTH : integer := 1
    );
    port (
      A : in std_logic;
      B : in std_logic;
      Y : out std_logic
    );
  end component;

  component $or
    generic (
      A_SIGNED : integer := 0;
      B_SIGNED : integer := 0;
      A_WIDTH : integer := 1;
      B_WIDTH : integer := 1;
      Y_WIDTH : integer := 1
    );
    port (
      A : in std_logic;
      B : in std_logic;
      Y : out std_logic
    );
  end component;

  component $adff
    generic (
      WIDTH : integer := 0;
      CLK_POLARITY : std_logic := '1';
      ARST_POLARITY : std_logic := '1';
      ARST_VALUE : integer := 0
    );
    port (
      CLK : in std_logic;
      ARST : in std_logic;
      D : in std_logic;
      Q : out std_logic
    );
  end component;

  component $mux
    generic (
      WIDTH : integer := 0
    );
    port (
      A : in std_logic;
      B : in std_logic;
      S : in std_logic;
      Y : out std_logic
    );
  end component;

  component $reduce_or
    generic (
      A_SIGNED : integer := 0;
      A_WIDTH : integer := 0;
      Y_WIDTH : integer := 0
    );
    port (
      A : in std_logic;
      Y : out std_logic
    );
  end component;

  component ram
    port (
      ram_dout : out std_logic_vector(15 downto 0);
      ram_addr : in std_logic_vector(6 downto 0);
      ram_cen : in std_logic;
      ram_clk : in std_logic;
      ram_din : in std_logic_vector(15 downto 0);
      ram_wen : in std_logic_vector(1 downto 0)
    );
  end component;

  component omsp_gpio
    port (
      irq_port1 : out std_logic;
      irq_port2 : out std_logic;
      p1_dout : out std_logic_vector(7 downto 0);
      p1_dout_en : out std_logic_vector(7 downto 0);
      p1_sel : out std_logic_vector(7 downto 0);
      p2_dout : out std_logic_vector(7 downto 0);
      p2_dout_en : out std_logic_vector(7 downto 0);
      p2_sel : out std_logic_vector(7 downto 0);
      p3_dout : out std_logic_vector(7 downto 0);
      p3_dout_en : out std_logic_vector(7 downto 0);
      p3_sel : out std_logic_vector(7 downto 0);
      p4_dout : out std_logic_vector(7 downto 0);
      p4_dout_en : out std_logic_vector(7 downto 0);
      p4_sel : out std_logic_vector(7 downto 0);
      p5_dout : out std_logic_vector(7 downto 0);
      p5_dout_en : out std_logic_vector(7 downto 0);
      p5_sel : out std_logic_vector(7 downto 0);
      p6_dout : out std_logic_vector(7 downto 0);
      p6_dout_en : out std_logic_vector(7 downto 0);
      p6_sel : out std_logic_vector(7 downto 0);
      per_dout : out std_logic_vector(15 downto 0);
      mclk : in std_logic;
      p1_din : in std_logic_vector(7 downto 0);
      p2_din : in std_logic_vector(7 downto 0);
      p3_din : in std_logic_vector(7 downto 0);
      p4_din : in std_logic_vector(7 downto 0);
      p5_din : in std_logic_vector(7 downto 0);
      p6_din : in std_logic_vector(7 downto 0);
      per_addr : in std_logic_vector(13 downto 0);
      per_din : in std_logic_vector(15 downto 0);
      per_en : in std_logic;
      per_we : in std_logic_vector(1 downto 0);
      puc_rst : in std_logic
    );
  end component;

  component i2c_master
    port (
      Reset_i : in std_logic;
      Clk_i : in std_logic;
      F100_400_n_i : in std_logic;
      Divider800_i : in std_logic_vector(15 downto 0);
      StartProcess_i : in std_logic;
      ReceiveSend_n_i : in std_logic;
      Busy_o : out std_logic;
      ReadCount_i : in std_logic_vector(3 downto 0);
      FIFOReadNext_i : in std_logic;
      FIFOWrite_i : in std_logic;
      FIFOEmpty_o : out std_logic;
      FIFOFull_o : out std_logic;
      Data_i : in std_logic_vector(7 downto 0);
      Data_o : out std_logic_vector(7 downto 0);
      ErrAck_i : in std_logic;
      ErrBusColl_o : out std_logic;
      ErrFIFOFull_o : out std_logic;
      ErrGotNAck_o : out std_logic;
      ErrCoreBusy_o : out std_logic;
      ErrFIFOEmpty_o : out std_logic;
      ErrCoreStopped_o : out std_logic;
      ErrDevNotPresent_o : out std_logic;
      ErrReadCountZero_o : out std_logic;
      SDA_i : in std_logic;
      SDA_o : out std_logic;
      SCL_o : out std_logic;
      ScanEnable_i : in std_logic;
      ScanClk_i : in std_logic;
      ScanDataIn_i : in std_logic;
      ScanDataOut_o : out std_logic
    );
  end component;

  component openMSP430
    port (
      aclk : out std_logic;
      aclk_en : out std_logic;
      dbg_freeze : out std_logic;
      dbg_i2c_sda_out : out std_logic;
      dbg_uart_txd : out std_logic;
      dco_enable : out std_logic;
      dco_wkup : out std_logic;
      dmem_addr : out std_logic_vector(6 downto 0);
      dmem_cen : out std_logic;
      dmem_din : out std_logic_vector(15 downto 0);
      dmem_wen : out std_logic_vector(1 downto 0);
      irq_acc : out std_logic_vector(13 downto 0);
      lfxt_enable : out std_logic;
      lfxt_wkup : out std_logic;
      mclk : out std_logic;
      per_addr : out std_logic_vector(13 downto 0);
      per_din : out std_logic_vector(15 downto 0);
      per_we : out std_logic_vector(1 downto 0);
      per_en : out std_logic;
      pmem_addr : out std_logic_vector(11 downto 0);
      pmem_cen : out std_logic;
      pmem_din : out std_logic_vector(15 downto 0);
      pmem_wen : out std_logic_vector(1 downto 0);
      puc_rst : out std_logic;
      smclk : out std_logic;
      smclk_en : out std_logic;
      cpu_en : in std_logic;
      dbg_en : in std_logic;
      dbg_i2c_addr : in std_logic_vector(6 downto 0);
      dbg_i2c_broadcast : in std_logic_vector(6 downto 0);
      dbg_i2c_scl : in std_logic;
      dbg_i2c_sda_in : in std_logic;
      dbg_uart_rxd : in std_logic;
      dco_clk : in std_logic;
      dmem_dout : in std_logic_vector(15 downto 0);
      irq : in std_logic_vector(13 downto 0);
      lfxt_clk : in std_logic;
      nmi : in std_logic;
      per_dout : in std_logic_vector(15 downto 0);
      pmem_dout : in std_logic_vector(15 downto 0);
      reset_n : in std_logic;
      scan_enable : in std_logic;
      scan_mode : in std_logic;
      wkup : in std_logic
    );
  end component;

  component SimpleSPI
    port (
      Reset_n_i : in std_logic;
      Clk_i : in std_logic;
      PerAddr_i : in std_logic_vector(13 downto 0);
      PerDIn_i : in std_logic_vector(15 downto 0);
      PerDOut_o : out std_logic_vector(15 downto 0);
      PerWr_i : in std_logic_vector(1 downto 0);
      PerEn_i : in std_logic;
      Intr_o : out std_logic;
      SCK_o : out std_logic;
      MOSI_o : out std_logic;
      MISO_i : in std_logic
    );
  end component;

  component SPI_Master
    port (
      Reset_n : in std_logic;
      Clk : in std_logic;
      CPOL_i : in std_logic;
      CPHA_i : in std_logic;
      LSBFE_i : in std_logic;
      SPPR_i : in std_logic_vector(2 downto 0);
      SPR_i : in std_logic_vector(2 downto 0);
      SCK_o : out std_logic;
      MOSI_o : out std_logic;
      MISO_i : in std_logic;
      Transmission_o : out std_logic;
      Write_i : in std_logic;
      ReadNext_i : in std_logic;
      Data_i : in std_logic_vector(7 downto 0);
      Data_o : out std_logic_vector(7 downto 0);
      FIFOFull_o : out std_logic;
      FIFOEmpty_o : out std_logic;
      ScanEnable_i : in std_logic;
      ScanClk_i : in std_logic;
      ScanDataIn_i : in std_logic;
      ScanDataOut_o : out std_logic
    );
  end component;

  component omsp_timerA
    port (
      irq_ta0 : out std_logic;
      irq_ta1 : out std_logic;
      per_dout : out std_logic_vector(15 downto 0);
      ta_out0 : out std_logic;
      ta_out0_en : out std_logic;
      ta_out1 : out std_logic;
      ta_out1_en : out std_logic;
      ta_out2 : out std_logic;
      ta_out2_en : out std_logic;
      aclk_en : in std_logic;
      dbg_freeze : in std_logic;
      inclk : in std_logic;
      irq_ta0_acc : in std_logic;
      mclk : in std_logic;
      per_addr : in std_logic_vector(13 downto 0);
      per_din : in std_logic_vector(15 downto 0);
      per_en : in std_logic;
      per_we : in std_logic_vector(1 downto 0);
      puc_rst : in std_logic;
      smclk_en : in std_logic;
      ta_cci0a : in std_logic;
      ta_cci0b : in std_logic;
      ta_cci1a : in std_logic;
      ta_cci1b : in std_logic;
      ta_cci2a : in std_logic;
      ta_cci2b : in std_logic;
      taclk : in std_logic
    );
  end component;

  component omsp_uart
    port (
      irq_uart_rx : out std_logic;
      irq_uart_tx : out std_logic;
      per_dout : out std_logic_vector(15 downto 0);
      uart_txd : out std_logic;
      mclk : in std_logic;
      per_addr : in std_logic_vector(13 downto 0);
      per_din : in std_logic_vector(15 downto 0);
      per_en : in std_logic;
      per_we : in std_logic_vector(1 downto 0);
      puc_rst : in std_logic;
      smclk_en : in std_logic;
      uart_rxd : in std_logic
    );
  end component;

  signal $0\I2C_ErrAck[0:0] : std_logic;
  attribute src of $0\I2C_ErrAck[0:0] : signal is "../../../core/verilog/core.v:904";
  signal $eq$../../../core/verilog/core.v:914$45_Y : std_logic;
  attribute src of $eq$../../../core/verilog/core.v:914$45_Y : signal is "../../../core/verilog/core.v:914";
  signal $logic_and$../../../core/verilog/core.v:914$46_Y : std_logic;
  attribute src of $logic_and$../../../core/verilog/core.v:914$46_Y : signal is "../../../core/verilog/core.v:914";
  signal $or$../../../core/verilog/core.v:656$10_Y : std_logic_vector(15 downto 0);
  attribute src of $or$../../../core/verilog/core.v:656$10_Y : signal is "../../../core/verilog/core.v:656";
  signal $or$../../../core/verilog/core.v:656$11_Y : std_logic_vector(15 downto 0);
  attribute src of $or$../../../core/verilog/core.v:656$11_Y : signal is "../../../core/verilog/core.v:656";
  signal $or$../../../core/verilog/core.v:656$12_Y : std_logic_vector(15 downto 0);
  attribute src of $or$../../../core/verilog/core.v:656$12_Y : signal is "../../../core/verilog/core.v:656";
  signal $or$../../../core/verilog/core.v:656$13_Y : std_logic_vector(15 downto 0);
  attribute src of $or$../../../core/verilog/core.v:656$13_Y : signal is "../../../core/verilog/core.v:656";
  signal $or$../../../core/verilog/core.v:656$9_Y : std_logic_vector(15 downto 0);
  attribute src of $or$../../../core/verilog/core.v:656$9_Y : signal is "../../../core/verilog/core.v:656";
  signal AClk_En_s : std_logic;
  attribute src of AClk_En_s : signal is "../../../core/verilog/core.v:256";
  signal AClk_s : std_logic;
  attribute src of AClk_s : signal is "../../../core/verilog/core.v:255";
  signal CPU_Enable_s : std_logic;
  attribute src of CPU_Enable_s : signal is "../../../core/verilog/core.v:247";
  signal CfgIntf_DOut_s : std_logic_vector(15 downto 0);
  attribute src of CfgIntf_DOut_s : signal is "../../../core/verilog/core.v:616";
  signal DCO_Enable_s : std_logic;
  attribute src of DCO_Enable_s : signal is "../../../core/verilog/core.v:250";
  signal DCO_Wakeup_s : std_logic;
  attribute src of DCO_Wakeup_s : signal is "../../../core/verilog/core.v:251";
  signal DMem_Addr_s : std_logic_vector(6 downto 0);
  attribute src of DMem_Addr_s : signal is "../../../core/verilog/core.v:268";
  signal DMem_DIn_s : std_logic_vector(15 downto 0);
  attribute src of DMem_DIn_s : signal is "../../../core/verilog/core.v:270";
  signal DMem_DOut_s : std_logic_vector(15 downto 0);
  attribute src of DMem_DOut_s : signal is "../../../core/verilog/core.v:272";
  signal DMem_En_n_s : std_logic;
  attribute src of DMem_En_n_s : signal is "../../../core/verilog/core.v:269";
  signal DMem_Wr_n_s : std_logic_vector(1 downto 0);
  attribute src of DMem_Wr_n_s : signal is "../../../core/verilog/core.v:271";
  signal Dbg_Freeze_s : std_logic;
  attribute src of Dbg_Freeze_s : signal is "../../../core/verilog/core.v:286";
  signal Dbg_UART_TxD_s : std_logic;
  attribute src of Dbg_UART_TxD_s : signal is "../../../core/verilog/core.v:298";
  signal Gpio_DOut_s : std_logic_vector(15 downto 0);
  attribute src of Gpio_DOut_s : signal is "../../../core/verilog/core.v:440";
  signal Gpio_IRQ1_s : std_logic;
  attribute src of Gpio_IRQ1_s : signal is "../../../core/verilog/core.v:441";
  signal Gpio_IRQ2_s : std_logic;
  attribute src of Gpio_IRQ2_s : signal is "../../../core/verilog/core.v:442";
  signal I2C_Busy : std_logic;
  attribute src of I2C_Busy : signal is "../../../core/verilog/core.v:838";
  signal I2C_DataIn : std_logic_vector(7 downto 0);
  attribute src of I2C_DataIn : signal is "../../../core/verilog/core.v:844";
  signal I2C_DataOut : std_logic_vector(7 downto 0);
  attribute src of I2C_DataOut : signal is "../../../core/verilog/core.v:845";
  signal I2C_Divider800 : std_logic_vector(15 downto 0);
  attribute src of I2C_Divider800 : signal is "../../../core/verilog/core.v:835";
  signal I2C_ErrAck : std_logic;
  attribute src of I2C_ErrAck : signal is "../../../core/verilog/core.v:846";
  signal I2C_ErrAckParam : std_logic;
  attribute src of I2C_ErrAckParam : signal is "../../../core/verilog/core.v:902";
  signal I2C_ErrAckParamOld : std_logic;
  attribute src of I2C_ErrAckParamOld : signal is "../../../core/verilog/core.v:903";
  signal I2C_ErrBusColl : std_logic;
  attribute src of I2C_ErrBusColl : signal is "../../../core/verilog/core.v:847";
  signal I2C_ErrCoreBusy : std_logic;
  attribute src of I2C_ErrCoreBusy : signal is "../../../core/verilog/core.v:850";
  signal I2C_ErrCoreStopped : std_logic;
  attribute src of I2C_ErrCoreStopped : signal is "../../../core/verilog/core.v:852";
  signal I2C_ErrDevNotPresent : std_logic;
  attribute src of I2C_ErrDevNotPresent : signal is "../../../core/verilog/core.v:853";
  signal I2C_ErrFIFOEmpty : std_logic;
  attribute src of I2C_ErrFIFOEmpty : signal is "../../../core/verilog/core.v:851";
  signal I2C_ErrFIFOFull : std_logic;
  attribute src of I2C_ErrFIFOFull : signal is "../../../core/verilog/core.v:848";
  signal I2C_ErrGotNAck : std_logic;
  attribute src of I2C_ErrGotNAck : signal is "../../../core/verilog/core.v:849";
  signal I2C_ErrReadCountZero : std_logic;
  attribute src of I2C_ErrReadCountZero : signal is "../../../core/verilog/core.v:854";
  signal I2C_Error : std_logic;
  attribute keep of I2C_Error : signal is 1;
  attribute src of I2C_Error : signal is "../../../core/verilog/core.v:857";
  attribute unused_bits of I2C_Error : signal is "0";
  signal I2C_Errors : std_logic_vector(7 downto 0);
  attribute keep of I2C_Errors : signal is 1;
  attribute src of I2C_Errors : signal is "../../../core/verilog/core.v:855";
  signal I2C_F100_400_n : std_logic;
  attribute src of I2C_F100_400_n : signal is "../../../core/verilog/core.v:834";
  signal I2C_FIFOEmpty : std_logic;
  attribute src of I2C_FIFOEmpty : signal is "../../../core/verilog/core.v:842";
  signal I2C_FIFOFull : std_logic;
  attribute src of I2C_FIFOFull : signal is "../../../core/verilog/core.v:843";
  signal I2C_FIFOReadNext : std_logic;
  attribute src of I2C_FIFOReadNext : signal is "../../../core/verilog/core.v:840";
  signal I2C_FIFOWrite : std_logic;
  attribute src of I2C_FIFOWrite : signal is "../../../core/verilog/core.v:841";
  signal I2C_ReadCount : std_logic_vector(3 downto 0);
  attribute src of I2C_ReadCount : signal is "../../../core/verilog/core.v:839";
  signal I2C_ReceiveSend_n : std_logic;
  attribute src of I2C_ReceiveSend_n : signal is "../../../core/verilog/core.v:837";
  signal I2C_ScanDataOut : std_logic;
  attribute src of I2C_ScanDataOut : signal is "../../../core/verilog/core.v:862";
  signal I2C_StartProcess : std_logic;
  attribute src of I2C_StartProcess : signal is "../../../core/verilog/core.v:836";
  signal INClk_s : std_logic;
  attribute src of INClk_s : signal is "../../../core/verilog/core.v:522";
  signal IRQ_Ack_s : std_logic_vector(13 downto 0);
  attribute src of IRQ_Ack_s : signal is "../../../core/verilog/core.v:281";
  signal IRQ_s : std_logic_vector(13 downto 0);
  attribute src of IRQ_s : signal is "../../../core/verilog/core.v:280";
  signal LFXT_Enable_s : std_logic;
  attribute src of LFXT_Enable_s : signal is "../../../core/verilog/core.v:252";
  signal LFXT_Wakeup_s : std_logic;
  attribute src of LFXT_Wakeup_s : signal is "../../../core/verilog/core.v:253";
  signal MClk_s : std_logic;
  attribute src of MClk_s : signal is "../../../core/verilog/core.v:254";
  signal P1_DIn_s : std_logic_vector(7 downto 0);
  attribute src of P1_DIn_s : signal is "../../../core/verilog/core.v:446";
  signal P1_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P1_DOut_s : signal is "../../../core/verilog/core.v:443";
  signal P1_En_s : std_logic_vector(7 downto 0);
  attribute src of P1_En_s : signal is "../../../core/verilog/core.v:444";
  signal P1_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P1_Sel_s : signal is "../../../core/verilog/core.v:445";
  signal P2_DIn_s : std_logic_vector(7 downto 0);
  attribute src of P2_DIn_s : signal is "../../../core/verilog/core.v:450";
  signal P2_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P2_DOut_s : signal is "../../../core/verilog/core.v:447";
  signal P2_En_s : std_logic_vector(7 downto 0);
  attribute src of P2_En_s : signal is "../../../core/verilog/core.v:448";
  signal P2_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P2_Sel_s : signal is "../../../core/verilog/core.v:449";
  signal P3_DIn_s : std_logic_vector(7 downto 0);
  attribute src of P3_DIn_s : signal is "../../../core/verilog/core.v:454";
  signal P3_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P3_DOut_s : signal is "../../../core/verilog/core.v:451";
  signal P3_En_s : std_logic_vector(7 downto 0);
  attribute src of P3_En_s : signal is "../../../core/verilog/core.v:452";
  signal P3_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P3_Sel_s : signal is "../../../core/verilog/core.v:453";
  signal P4_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P4_DOut_s : signal is "../../../core/verilog/core.v:455";
  signal P4_En_s : std_logic_vector(7 downto 0);
  attribute src of P4_En_s : signal is "../../../core/verilog/core.v:456";
  signal P4_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P4_Sel_s : signal is "../../../core/verilog/core.v:457";
  signal P5_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P5_DOut_s : signal is "../../../core/verilog/core.v:459";
  signal P5_En_s : std_logic_vector(7 downto 0);
  attribute src of P5_En_s : signal is "../../../core/verilog/core.v:460";
  signal P5_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P5_Sel_s : signal is "../../../core/verilog/core.v:461";
  signal P6_DOut_s : std_logic_vector(7 downto 0);
  attribute src of P6_DOut_s : signal is "../../../core/verilog/core.v:463";
  signal P6_En_s : std_logic_vector(7 downto 0);
  attribute src of P6_En_s : signal is "../../../core/verilog/core.v:464";
  signal P6_Sel_s : std_logic_vector(7 downto 0);
  attribute src of P6_Sel_s : signal is "../../../core/verilog/core.v:465";
  signal PMem_Addr_s : std_logic_vector(11 downto 0);
  attribute src of PMem_Addr_s : signal is "../../../core/verilog/core.v:262";
  signal PMem_DIn_s : std_logic_vector(15 downto 0);
  attribute src of PMem_DIn_s : signal is "../../../core/verilog/core.v:264";
  signal PMem_DOut_s : std_logic_vector(15 downto 0);
  attribute src of PMem_DOut_s : signal is "../../../core/verilog/core.v:266";
  signal PMem_En_n_s : std_logic;
  attribute src of PMem_En_n_s : signal is "../../../core/verilog/core.v:263";
  signal PMem_Wr_n_s : std_logic_vector(1 downto 0);
  attribute src of PMem_Wr_n_s : signal is "../../../core/verilog/core.v:265";
  signal PUC_Reset_s : std_logic;
  attribute src of PUC_Reset_s : signal is "../../../core/verilog/core.v:246";
  signal ParamIntf_DOut_s : std_logic_vector(15 downto 0);
  attribute src of ParamIntf_DOut_s : signal is "../../../core/verilog/core.v:618";
  signal Per_Addr_s : std_logic_vector(13 downto 0);
  attribute src of Per_Addr_s : signal is "../../../core/verilog/core.v:274";
  signal Per_DIn_s : std_logic_vector(15 downto 0);
  attribute src of Per_DIn_s : signal is "../../../core/verilog/core.v:275";
  signal Per_DOut_s : std_logic_vector(15 downto 0);
  attribute src of Per_DOut_s : signal is "../../../core/verilog/core.v:276";
  signal Per_En_s : std_logic;
  attribute src of Per_En_s : signal is "../../../core/verilog/core.v:278";
  signal Per_Wr_s : std_logic_vector(1 downto 0);
  attribute src of Per_Wr_s : signal is "../../../core/verilog/core.v:277";
  signal ReconfModuleIRQs_s : std_logic_vector(4 downto 0);
  attribute keep of ReconfModuleIRQs_s : signal is 1;
  attribute src of ReconfModuleIRQs_s : signal is "../../../core/verilog/core.v:622";
  signal ReconfModuleIn_s : std_logic_vector(7 downto 0);
  attribute keep of ReconfModuleIn_s : signal is 1;
  attribute src of ReconfModuleIn_s : signal is "../../../core/verilog/core.v:623";
  signal ReconfModuleOut_s : std_logic_vector(7 downto 0);
  attribute keep of ReconfModuleOut_s : signal is 1;
  attribute src of ReconfModuleOut_s : signal is "../../../core/verilog/core.v:624";
  signal ResetSync : std_logic_vector(1 downto 0);
  attribute src of ResetSync : signal is "../../../core/verilog/core.v:113";
  signal Reset_n_s : std_logic;
  attribute src of Reset_n_s : signal is "../../../core/verilog/core.v:123";
  signal Reset_s : std_logic;
  attribute src of Reset_s : signal is "../../../core/verilog/core.v:125";
  signal SMClk_En_s : std_logic;
  attribute src of SMClk_En_s : signal is "../../../core/verilog/core.v:258";
  signal SMClk_s : std_logic;
  attribute src of SMClk_s : signal is "../../../core/verilog/core.v:257";
  signal SPI_CPHA : std_logic;
  attribute src of SPI_CPHA : signal is "../../../core/verilog/core.v:779";
  signal SPI_CPOL : std_logic;
  attribute src of SPI_CPOL : signal is "../../../core/verilog/core.v:778";
  signal SPI_DOut_s : std_logic_vector(15 downto 0);
  attribute src of SPI_DOut_s : signal is "../../../core/verilog/core.v:595";
  signal SPI_DataIn : std_logic_vector(7 downto 0);
  attribute src of SPI_DataIn : signal is "../../../core/verilog/core.v:787";
  signal SPI_DataOut : std_logic_vector(7 downto 0);
  attribute src of SPI_DataOut : signal is "../../../core/verilog/core.v:788";
  signal SPI_FIFOEmpty : std_logic;
  attribute src of SPI_FIFOEmpty : signal is "../../../core/verilog/core.v:790";
  signal SPI_FIFOFull : std_logic;
  attribute src of SPI_FIFOFull : signal is "../../../core/verilog/core.v:789";
  signal SPI_IRQ_s : std_logic;
  attribute src of SPI_IRQ_s : signal is "../../../core/verilog/core.v:596";
  signal SPI_LSBFE : std_logic;
  attribute src of SPI_LSBFE : signal is "../../../core/verilog/core.v:780";
  signal SPI_ReadNext : std_logic;
  attribute src of SPI_ReadNext : signal is "../../../core/verilog/core.v:786";
  signal SPI_SPPR_SPR : std_logic_vector(7 downto 0);
  attribute src of SPI_SPPR_SPR : signal is "../../../core/verilog/core.v:783";
  signal SPI_ScanDataOut : std_logic;
  attribute src of SPI_ScanDataOut : signal is "../../../core/verilog/core.v:794";
  signal SPI_Transmission : std_logic;
  attribute src of SPI_Transmission : signal is "../../../core/verilog/core.v:784";
  signal SPI_Write : std_logic;
  attribute src of SPI_Write : signal is "../../../core/verilog/core.v:785";
  signal TAClk_s : std_logic;
  attribute src of TAClk_s : signal is "../../../core/verilog/core.v:523";
  signal TimerA_CCI0A_s : std_logic;
  attribute src of TimerA_CCI0A_s : signal is "../../../core/verilog/core.v:524";
  signal TimerA_CCI0B_s : std_logic;
  attribute src of TimerA_CCI0B_s : signal is "../../../core/verilog/core.v:525";
  signal TimerA_CCI1A_s : std_logic;
  attribute src of TimerA_CCI1A_s : signal is "../../../core/verilog/core.v:528";
  signal TimerA_CCI1B_s : std_logic;
  attribute src of TimerA_CCI1B_s : signal is "../../../core/verilog/core.v:529";
  signal TimerA_CCI2A_s : std_logic;
  attribute src of TimerA_CCI2A_s : signal is "../../../core/verilog/core.v:532";
  signal TimerA_CCI2B_s : std_logic;
  attribute src of TimerA_CCI2B_s : signal is "../../../core/verilog/core.v:533";
  signal TimerA_DOut_s : std_logic_vector(15 downto 0);
  attribute src of TimerA_DOut_s : signal is "../../../core/verilog/core.v:519";
  signal TimerA_IRQ1_s : std_logic;
  attribute src of TimerA_IRQ1_s : signal is "../../../core/verilog/core.v:520";
  signal TimerA_IRQ2_s : std_logic;
  attribute src of TimerA_IRQ2_s : signal is "../../../core/verilog/core.v:521";
  signal TimerA_Out0_En_s : std_logic;
  attribute src of TimerA_Out0_En_s : signal is "../../../core/verilog/core.v:527";
  signal TimerA_Out0_s : std_logic;
  attribute src of TimerA_Out0_s : signal is "../../../core/verilog/core.v:526";
  signal TimerA_Out1_En_s : std_logic;
  attribute src of TimerA_Out1_En_s : signal is "../../../core/verilog/core.v:531";
  signal TimerA_Out1_s : std_logic;
  attribute src of TimerA_Out1_s : signal is "../../../core/verilog/core.v:530";
  signal TimerA_Out2_En_s : std_logic;
  attribute src of TimerA_Out2_En_s : signal is "../../../core/verilog/core.v:535";
  signal TimerA_Out2_s : std_logic;
  attribute src of TimerA_Out2_s : signal is "../../../core/verilog/core.v:534";
  signal UART_DOut_s : std_logic_vector(15 downto 0);
  attribute src of UART_DOut_s : signal is "../../../core/verilog/core.v:573";
  signal UART_IRQ_Rx_s : std_logic;
  attribute src of UART_IRQ_Rx_s : signal is "../../../core/verilog/core.v:574";
  signal UART_IRQ_Tx_s : std_logic;
  attribute src of UART_IRQ_Tx_s : signal is "../../../core/verilog/core.v:575";
  signal Wakeup_s : std_logic;
  attribute src of Wakeup_s : signal is "../../../core/verilog/core.v:260";

begin

  $eq$../../../core/verilog/core.v:914$45: $not
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 1,
      Y_WIDTH => 1
    )
    port map (
      A => I2C_ErrAckParamOld,
      Y => $eq$../../../core/verilog/core.v:914$45_Y
    );


  $logic_and$../../../core/verilog/core.v:914$46: $logic_and
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 1,
      B_SIGNED => 0,
      B_WIDTH => 1,
      Y_WIDTH => 1
    )
    port map (
      A => I2C_ErrAckParam,
      B => $eq$../../../core/verilog/core.v:914$45_Y,
      Y => $logic_and$../../../core/verilog/core.v:914$46_Y
    );


  $not$../../../core/verilog/core.v:126$6: $not
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 1,
      Y_WIDTH => 1
    )
    port map (
      A => ResetSync(1),
      Y => Reset_s
    );


  $or$../../../core/verilog/core.v:656$10: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => $or$../../../core/verilog/core.v:656$9_Y,
      B => UART_DOut_s,
      Y => $or$../../../core/verilog/core.v:656$10_Y
    );


  $or$../../../core/verilog/core.v:656$11: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => $or$../../../core/verilog/core.v:656$10_Y,
      B => SPI_DOut_s,
      Y => $or$../../../core/verilog/core.v:656$11_Y
    );


  $or$../../../core/verilog/core.v:656$12: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => $or$../../../core/verilog/core.v:656$11_Y,
      B => CfgIntf_DOut_s,
      Y => $or$../../../core/verilog/core.v:656$12_Y
    );


  $or$../../../core/verilog/core.v:656$13: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => $or$../../../core/verilog/core.v:656$12_Y,
      B => ParamIntf_DOut_s,
      Y => $or$../../../core/verilog/core.v:656$13_Y
    );


  $or$../../../core/verilog/core.v:656$14: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => $or$../../../core/verilog/core.v:656$13_Y,
      B => "0000000000000000",
      Y => Per_DOut_s
    );


  $or$../../../core/verilog/core.v:656$9: $or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 16,
      B_SIGNED => 0,
      B_WIDTH => 16,
      Y_WIDTH => 16
    )
    port map (
      A => Gpio_DOut_s,
      B => TimerA_DOut_s,
      Y => $or$../../../core/verilog/core.v:656$9_Y
    );


  $procdff$52: $adff
    generic map (
      ARST_POLARITY => '0',
      ARST_VALUE => "00",
      CLK_POLARITY => '1',
      WIDTH => 2
    )
    port map (
      ARST => Reset_n_i,
      CLK => Clk_i,
      D => ResetSync(0) & '1',
      Q => ResetSync
    );


  $procdff$53: $adff
    generic map (
      ARST_POLARITY => '0',
      ARST_VALUE => '0',
      CLK_POLARITY => '1',
      WIDTH => 1
    )
    port map (
      ARST => ResetSync(1),
      CLK => Clk_i,
      D => $0\I2C_ErrAck[0:0],
      Q => I2C_ErrAck
    );


  $procdff$54: $adff
    generic map (
      ARST_POLARITY => '0',
      ARST_VALUE => '0',
      CLK_POLARITY => '1',
      WIDTH => 1
    )
    port map (
      ARST => ResetSync(1),
      CLK => Clk_i,
      D => I2C_ErrAckParam,
      Q => I2C_ErrAckParamOld
    );


  $procmux$48: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => '0',
      B => '1',
      S => $logic_and$../../../core/verilog/core.v:914$46_Y,
      Y => $0\I2C_ErrAck[0:0]
    );


  $reduce_or$../../../core/verilog/core.v:282$7: $reduce_or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 12,
      Y_WIDTH => 1
    )
    port map (
      A => IRQ_s(6) & IRQ_s(7) & IRQ_s(8) & IRQ_s(9) & IRQ_s(4) & ReconfModuleIRQs_s & Gpio_IRQ2_s & Gpio_IRQ1_s,
      Y => Wakeup_s
    );


  $reduce_or$../../../core/verilog/core.v:858$41: $reduce_or
    generic map (
      A_SIGNED => 0,
      A_WIDTH => 8,
      Y_WIDTH => 1
    )
    port map (
      A => I2C_ErrReadCountZero & I2C_ErrGotNAck & I2C_ErrFIFOFull & I2C_ErrFIFOEmpty & I2C_ErrDevNotPresent & I2C_ErrCoreStopped & I2C_ErrCoreBusy & I2C_ErrBusColl,
      Y => I2C_Error
    );


  $ternary$../../../core/verilog/core.v:673$15: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(0),
      B => '0',
      S => P1_Sel_s(0),
      Y => P1_DOut_o(0)
    );


  $ternary$../../../core/verilog/core.v:674$16: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(1),
      B => TimerA_Out0_s,
      S => P1_Sel_s(1),
      Y => P1_DOut_o(1)
    );


  $ternary$../../../core/verilog/core.v:675$17: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(2),
      B => TimerA_Out1_s,
      S => P1_Sel_s(2),
      Y => P1_DOut_o(2)
    );


  $ternary$../../../core/verilog/core.v:676$18: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(3),
      B => TimerA_Out2_s,
      S => P1_Sel_s(3),
      Y => P1_DOut_o(3)
    );


  $ternary$../../../core/verilog/core.v:677$19: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(4),
      B => SMClk_s,
      S => P1_Sel_s(4),
      Y => P1_DOut_o(4)
    );


  $ternary$../../../core/verilog/core.v:678$20: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(5),
      B => TimerA_Out0_s,
      S => P1_Sel_s(5),
      Y => P1_DOut_o(5)
    );


  $ternary$../../../core/verilog/core.v:679$21: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(6),
      B => TimerA_Out1_s,
      S => P1_Sel_s(6),
      Y => P1_DOut_o(6)
    );


  $ternary$../../../core/verilog/core.v:680$22: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_DOut_s(7),
      B => TimerA_Out2_s,
      S => P1_Sel_s(7),
      Y => P1_DOut_o(7)
    );


  $ternary$../../../core/verilog/core.v:681$23: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(0),
      B => '0',
      S => P1_Sel_s(0),
      Y => P1_En_o(0)
    );


  $ternary$../../../core/verilog/core.v:682$24: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(1),
      B => TimerA_Out0_En_s,
      S => P1_Sel_s(1),
      Y => P1_En_o(1)
    );


  $ternary$../../../core/verilog/core.v:683$25: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(2),
      B => TimerA_Out1_En_s,
      S => P1_Sel_s(2),
      Y => P1_En_o(2)
    );


  $ternary$../../../core/verilog/core.v:684$26: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(3),
      B => TimerA_Out2_En_s,
      S => P1_Sel_s(3),
      Y => P1_En_o(3)
    );


  $ternary$../../../core/verilog/core.v:685$27: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(4),
      B => '1',
      S => P1_Sel_s(4),
      Y => P1_En_o(4)
    );


  $ternary$../../../core/verilog/core.v:686$28: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(5),
      B => TimerA_Out0_En_s,
      S => P1_Sel_s(5),
      Y => P1_En_o(5)
    );


  $ternary$../../../core/verilog/core.v:687$29: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(6),
      B => TimerA_Out1_En_s,
      S => P1_Sel_s(6),
      Y => P1_En_o(6)
    );


  $ternary$../../../core/verilog/core.v:688$30: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P1_En_s(7),
      B => TimerA_Out2_En_s,
      S => P1_Sel_s(7),
      Y => P1_En_o(7)
    );


  $ternary$../../../core/verilog/core.v:695$31: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_DOut_s(0),
      B => AClk_En_s,
      S => P2_Sel_s(0),
      Y => P2_DOut_o(0)
    );


  $ternary$../../../core/verilog/core.v:696$32: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_DOut_s(1),
      B => '0',
      S => P2_Sel_s(1),
      Y => P2_DOut_o(1)
    );


  $ternary$../../../core/verilog/core.v:697$33: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_DOut_s(2),
      B => TimerA_Out0_s,
      S => P2_Sel_s(2),
      Y => P2_DOut_o(2)
    );


  $ternary$../../../core/verilog/core.v:698$34: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_DOut_s(3),
      B => TimerA_Out1_s,
      S => P2_Sel_s(3),
      Y => P2_DOut_o(3)
    );


  $ternary$../../../core/verilog/core.v:699$35: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_DOut_s(4),
      B => TimerA_Out2_s,
      S => P2_Sel_s(4),
      Y => P2_DOut_o(4)
    );


  $ternary$../../../core/verilog/core.v:703$36: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_En_s(0),
      B => '1',
      S => P2_Sel_s(0),
      Y => P2_En_o(0)
    );


  $ternary$../../../core/verilog/core.v:704$37: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_En_s(1),
      B => '0',
      S => P2_Sel_s(1),
      Y => P2_En_o(1)
    );


  $ternary$../../../core/verilog/core.v:705$38: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_En_s(2),
      B => TimerA_Out0_En_s,
      S => P2_Sel_s(2),
      Y => P2_En_o(2)
    );


  $ternary$../../../core/verilog/core.v:706$39: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_En_s(3),
      B => TimerA_Out1_En_s,
      S => P2_Sel_s(3),
      Y => P2_En_o(3)
    );


  $ternary$../../../core/verilog/core.v:707$40: $mux
    generic map (
      WIDTH => 1
    )
    port map (
      A => P2_En_s(4),
      B => TimerA_Out2_En_s,
      S => P2_Sel_s(4),
      Y => P2_En_o(4)
    );


  DMem_0: ram
    generic map (
      $1 => 6,
      $2 => 256
    )
    port map (
      ram_addr => DMem_Addr_s,
      ram_cen => DMem_En_n_s,
      ram_clk => MClk_s,
      ram_din => DMem_DIn_s,
      ram_dout => DMem_DOut_s,
      ram_wen => DMem_Wr_n_s
    );


  PMem_0: ram
    generic map (
      $1 => 11,
      $2 => 8192
    )
    port map (
      ram_addr => PMem_Addr_s,
      ram_cen => PMem_En_n_s,
      ram_clk => MClk_s,
      ram_din => PMem_DIn_s,
      ram_dout => PMem_DOut_s,
      ram_wen => PMem_Wr_n_s
    );


  gpio_0: omsp_gpio
    generic map (
      P1_EN => '1',
      P2_EN => '1',
      P3_EN => '1',
      P4_EN => '0',
      P5_EN => '0',
      P6_EN => '0'
    )
    port map (
      irq_port1 => Gpio_IRQ1_s,
      irq_port2 => Gpio_IRQ2_s,
      mclk => SMClk_s,
      p1_din => P1_DIn_i,
      p1_dout => P1_DOut_s,
      p1_dout_en => P1_En_s,
      p1_sel => P1_Sel_s,
      p2_din => P2_DIn_i,
      p2_dout => P2_DOut_s,
      p2_dout_en => P2_En_s,
      p2_sel => P2_Sel_s,
      p3_din => P3_DIn_s,
      p3_dout => P3_DOut_s,
      p3_dout_en => P3_En_s,
      p3_sel => P3_Sel_s,
      p4_din => "00000000",
      p4_dout => P4_DOut_s,
      p4_dout_en => P4_En_s,
      p4_sel => P4_Sel_s,
      p5_din => "00000000",
      p5_dout => P5_DOut_s,
      p5_dout_en => P5_En_s,
      p5_sel => P5_Sel_s,
      p6_din => "00000000",
      p6_dout => P6_DOut_s,
      p6_dout_en => P6_En_s,
      p6_sel => P6_Sel_s,
      per_addr => Per_Addr_s,
      per_din => Per_DIn_s,
      per_dout => Gpio_DOut_s,
      per_en => Per_En_s,
      per_we => Per_Wr_s,
      puc_rst => PUC_Reset_s
    );


  i2c_master_1: i2c_master
    generic map (
      DividerWidth_g => 16,
      FIFOAddressWidth_g => 2,
      ReadCountWidth_g => 4
    )
    port map (
      Busy_o => I2C_Busy,
      Clk_i => Clk_i,
      Data_i => I2C_DataIn,
      Data_o => I2C_DataOut,
      Divider800_i => I2C_Divider800,
      ErrAck_i => I2C_ErrAck,
      ErrBusColl_o => I2C_ErrBusColl,
      ErrCoreBusy_o => I2C_ErrCoreBusy,
      ErrCoreStopped_o => I2C_ErrCoreStopped,
      ErrDevNotPresent_o => I2C_ErrDevNotPresent,
      ErrFIFOEmpty_o => I2C_ErrFIFOEmpty,
      ErrFIFOFull_o => I2C_ErrFIFOFull,
      ErrGotNAck_o => I2C_ErrGotNAck,
      ErrReadCountZero_o => I2C_ErrReadCountZero,
      F100_400_n_i => I2C_F100_400_n,
      FIFOEmpty_o => I2C_FIFOEmpty,
      FIFOFull_o => I2C_FIFOFull,
      FIFOReadNext_i => I2C_FIFOReadNext,
      FIFOWrite_i => I2C_FIFOWrite,
      ReadCount_i => I2C_ReadCount,
      ReceiveSend_n_i => I2C_ReceiveSend_n,
      Reset_i => Reset_s,
      SCL_o => I2CSCL_o,
      SDA_i => I2CSDA_i,
      SDA_o => I2CSDA_o,
      ScanClk_i => '0',
      ScanDataIn_i => '0',
      ScanDataOut_o => I2C_ScanDataOut,
      ScanEnable_i => '0',
      StartProcess_i => I2C_StartProcess
    );


  openMSP430_0: openMSP430
    port map (
      aclk => AClk_s,
      aclk_en => AClk_En_s,
      cpu_en => Cpu_En_i,
      dbg_en => Dbg_En_i,
      dbg_freeze => Dbg_Freeze_s,
      dbg_i2c_addr => "0101010",
      dbg_i2c_broadcast => "1111111",
      dbg_i2c_scl => Dbg_SCL_i,
      dbg_i2c_sda_in => Dbg_SDA_In_i,
      dbg_i2c_sda_out => Dbg_SDA_Out_o,
      dbg_uart_rxd => '0',
      dbg_uart_txd => Dbg_UART_TxD_s,
      dco_clk => Clk_i,
      dco_enable => DCO_Enable_s,
      dco_wkup => DCO_Wakeup_s,
      dmem_addr => DMem_Addr_s,
      dmem_cen => DMem_En_n_s,
      dmem_din => DMem_DIn_s,
      dmem_dout => DMem_DOut_s,
      dmem_wen => DMem_Wr_n_s,
      irq => ReconfModuleIRQs_s(4 downto 2) & '0' & IRQ_s(9 downto 6) & '0' & IRQ_s(4) & Gpio_IRQ2_s & Gpio_IRQ1_s & ReconfModuleIRQs_s(1 downto 0),
      irq_acc => IRQ_Ack_s,
      lfxt_clk => LFXT_Clk_i,
      lfxt_enable => LFXT_Enable_s,
      lfxt_wkup => LFXT_Wakeup_s,
      mclk => MClk_s,
      nmi => '0',
      per_addr => Per_Addr_s,
      per_din => Per_DIn_s,
      per_dout => Per_DOut_s,
      per_en => Per_En_s,
      per_we => Per_Wr_s,
      pmem_addr => PMem_Addr_s,
      pmem_cen => PMem_En_n_s,
      pmem_din => PMem_DIn_s,
      pmem_dout => PMem_DOut_s,
      pmem_wen => PMem_Wr_n_s,
      puc_rst => PUC_Reset_s,
      reset_n => Reset_n_i,
      scan_enable => '0',
      scan_mode => '0',
      smclk => SMClk_s,
      smclk_en => SMClk_En_s,
      wkup => Wakeup_s
    );


  spi_0: SimpleSPI
    generic map (
      BaseAddr => "000000010000000"
    )
    port map (
      Clk_i => SMClk_s,
      Intr_o => IRQ_s(4),
      MISO_i => MISO_i,
      MOSI_o => MOSI_o,
      PerAddr_i => Per_Addr_s,
      PerDIn_i => Per_DIn_s,
      PerDOut_o => SPI_DOut_s,
      PerEn_i => Per_En_s,
      PerWr_i => Per_Wr_s,
      Reset_n_i => ResetSync(1),
      SCK_o => SCK_o
    );


  spi_master_1: SPI_Master
    generic map (
      DataWidth => 8,
      FIFOReadWidth => 2,
      FIFOWriteWidth => 2,
      SPPRWidth => 4,
      SPRWidth => 4
    )
    port map (
      CPHA_i => SPI_CPHA,
      CPOL_i => SPI_CPOL,
      Clk => Clk_i,
      Data_i => SPI_DataIn,
      Data_o => SPI_DataOut,
      FIFOEmpty_o => SPI_FIFOEmpty,
      FIFOFull_o => SPI_FIFOFull,
      LSBFE_i => SPI_LSBFE,
      MISO_i => SPIMISO_i,
      MOSI_o => SPIMOSI_o,
      ReadNext_i => SPI_ReadNext,
      Reset_n => ResetSync(1),
      SCK_o => SPISCK_o,
      SPPR_i => SPI_SPPR_SPR(7 downto 4),
      SPR_i => SPI_SPPR_SPR(3 downto 0),
      ScanClk_i => '0',
      ScanDataIn_i => '0',
      ScanDataOut_o => SPI_ScanDataOut,
      ScanEnable_i => '0',
      Transmission_o => SPI_Transmission,
      Write_i => SPI_Write
    );


  timerA_0: omsp_timerA
    port map (
      aclk_en => AClk_En_s,
      dbg_freeze => Dbg_Freeze_s,
      inclk => P2_DIn_i(1),
      irq_ta0 => IRQ_s(9),
      irq_ta0_acc => IRQ_Ack_s(9),
      irq_ta1 => IRQ_s(8),
      mclk => SMClk_s,
      per_addr => Per_Addr_s,
      per_din => Per_DIn_s,
      per_dout => TimerA_DOut_s,
      per_en => Per_En_s,
      per_we => Per_Wr_s,
      puc_rst => PUC_Reset_s,
      smclk_en => SMClk_En_s,
      ta_cci0a => P1_DIn_i(1),
      ta_cci0b => P2_DIn_i(2),
      ta_cci1a => P1_DIn_i(2),
      ta_cci1b => P2_DIn_i(3),
      ta_cci2a => P1_DIn_i(3),
      ta_cci2b => P1_DIn_i(0),
      ta_out0 => TimerA_Out0_s,
      ta_out0_en => TimerA_Out0_En_s,
      ta_out1 => TimerA_Out1_s,
      ta_out1_en => TimerA_Out1_En_s,
      ta_out2 => TimerA_Out2_s,
      ta_out2_en => TimerA_Out2_En_s,
      taclk => P1_DIn_i(0)
    );


  uart_0: omsp_uart
    generic map (
      BASE_ADDR => "000000001110000"
    )
    port map (
      irq_uart_rx => IRQ_s(7),
      irq_uart_tx => IRQ_s(6),
      mclk => SMClk_s,
      per_addr => Per_Addr_s,
      per_din => Per_DIn_s,
      per_dout => UART_DOut_s,
      per_en => Per_En_s,
      per_we => Per_Wr_s,
      puc_rst => PUC_Reset_s,
      smclk_en => SMClk_En_s,
      uart_rxd => UartRxD_i,
      uart_txd => UartTxD_o
    );

  CPU_Enable_s <= Cpu_En_i;
  I2C_Errors <= I2C_ErrReadCountZero & I2C_ErrDevNotPresent & I2C_ErrCoreStopped & I2C_ErrFIFOEmpty & I2C_ErrCoreBusy & I2C_ErrGotNAck & I2C_ErrFIFOFull & I2C_ErrBusColl;
  INClk_s <= P2_DIn_i(1);
  IRQ_s(13 downto 10) & IRQ_s(5) & IRQ_s(3 downto 0) <= ReconfModuleIRQs_s(4 downto 2) & "00" & Gpio_IRQ2_s & Gpio_IRQ1_s & ReconfModuleIRQs_s(1 downto 0);
  P1_DIn_s <= P1_DIn_i;
  P2_DIn_s <= P2_DIn_i;
  P2_DOut_o(7 downto 5) <= P2_DOut_s(7 downto 5);
  P2_En_o(7 downto 5) <= P2_En_s(7 downto 5);
  P3_DOut_s <= ReconfModuleIn_s;
  P3_DIn_s <= ReconfModuleOut_s;
  Reset_n_s <= ResetSync(1);
  SPI_IRQ_s <= IRQ_s(4);
  TAClk_s <= P1_DIn_i(0);
  TimerA_CCI0A_s <= P1_DIn_i(1);
  TimerA_CCI0B_s <= P2_DIn_i(2);
  TimerA_CCI1A_s <= P1_DIn_i(2);
  TimerA_CCI1B_s <= P2_DIn_i(3);
  TimerA_CCI2A_s <= P1_DIn_i(3);
  TimerA_CCI2B_s <= P1_DIn_i(0);
  TimerA_IRQ1_s <= IRQ_s(9);
  TimerA_IRQ2_s <= IRQ_s(8);
  UART_IRQ_Rx_s <= IRQ_s(7);
  UART_IRQ_Tx_s <= IRQ_s(6);

end struct;

