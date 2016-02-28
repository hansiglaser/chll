/* This file requires some globally defined symbols.
 * 
 *   SIMULATION                  instantiate "msp_debug" to decode current
 *                               instructions
 *
 *   ReconfModuleConstDrivers    use constant drivers for otherwise undriven
 *                               signals, this is not (yet) implemented
 *
 *   ReconfModuleNone            Don't instantiate the Reconf.Module. This is
 *                               used with "flowcmd characterize-parent" to
 *                               find all undriven nets.
 *
 *   USE_AMS_RAM                 Instantiate modules "PMem" and "DMem" instead
 *                               of parameterized "ram" modules. Use either
 *                               [pd]mem-ram.v for testing (which inside use
 *                               "ram" modules), or use [pd]mem-ams.vhd for
 *                               the final chip.
 */

`include "openMSP430_defines.v"

module Core (
  // global
  input Reset_n_i,
  input Clk_i,
  //
  // CPU stuff
  input         Cpu_En_i,
  // CPU: secondary (slow) clock
  input LFXT_Clk_i,
  // CPU: Debug Interface
  input         Dbg_En_i,
`ifdef DBG_UART
  input         Dbg_UART_RxD_i,
  output        Dbg_UART_TxD_o,
`endif
`ifdef DBG_I2C
  input         Dbg_SCL_i,
  output        Dbg_SDA_Out_o,
  input         Dbg_SDA_In_i,
`endif
  // CPU: GPIOs
  output  [7:0] P1_DOut_o,
  output  [7:0] P1_En_o,
  input   [7:0] P1_DIn_i,
  output  [7:0] P2_DOut_o,
  output  [7:0] P2_En_o,
  input   [7:0] P2_DIn_i,
  // CPU: UART
  input         UartRxD_i,
  output        UartTxD_o,
  // CPU: SPI
  output        SCK_o,
  output        MOSI_o,
  input         MISO_i,
  // TODO
  // I/O
  input   [7:0] Inputs_i,
  output  [7:0] Outputs_o,
  // SPI
  input         SPIMISO_i,
  output        SPIMOSI_o,
  output        SPISCK_o,
  // I2C
  output        I2CSCL_o,
  input         I2CSDA_i,
  output        I2CSDA_o,
`ifdef INCLUDE_ONEWIRE
  // 1-Wire
  input         OneWire_i,
  output        OneWire_o,
`endif   // INCLUDE_ONEWIRE
`ifdef INCLUDE_PWM
  // PWM
  input         PWMInput_i,
`endif   // INCLUDE_PWM
`ifdef INCLUDE_SENT
  // SENT       
  input         SENTInput_i,
`endif   // INCLUDE_SENT
`ifdef INCLUDE_SPC
  // SPC
  input         SPCInput_i,
  output        SPCTrigger_o,
`endif   // INCLUDE_SPC
  // ADC
  input         AdcConvComplete_i,
  output        AdcDoConvert_o,
  input  [ 9:0] AdcValue_i
);

  ////////////////////////////////////////////////////////////////////////////
  // Reset Synchronizer
  // see e.g.
  //  - http://www.vlsiencyclopedia.com/2012/03/reset-synchronizer.html
  //  - http://logic-sense.com/2012/05/16/need-for-reset-synchronizer/
  //  - http://www.markharvey.info/fpga/reset/rst.html
  reg  [1:0] ResetSync;

  always @(posedge Clk_i or negedge Reset_n_i)
  begin
    if (!Reset_n_i)
      ResetSync <=  2'b00;
    else
      ResetSync <= {ResetSync[0], 1'b1};
  end
  
  wire   Reset_n_s;
  assign Reset_n_s = ResetSync[1];
  wire   Reset_s;
  assign Reset_s = ~Reset_n_s;

  ////////////////////////////////////////////////////////////////////////////
  /* ASIC Clocking / Clock Gating
   * ============================
   * Without the `define ASIC, the whole OpenMSP430 is a fully synchronous
   * design. MClk can't be disabled (but LPM0 and LPM1 do work) and disabling
   * SMClk is done by de-asserting SMClk_En_s. This, for example, disables
   * TimerA in LPM2 and LPM3.
   * Internally, the "smclk" (and "aclk") output is a straight through
   * connection with the "mclk" signal, which itself is "nodiv_mclk", which
   * directly comes from the "dco_clk" input.
   * 
   * With the `defines ASIC and ASIC_CLOCKING, many clock gates are added.
   * Clock dividers are implemented as clock gates, btw. Numerous other
   * `defines offer fine-granular control over the clock tree.
   * 
   *   no   CLOCK_GATING:    we don't want manual clock gating but let the
   *                         synthesis tool do this
   *   no   LFXT_DOMAIN:     we don't use the second clock
   *   no   MCLK_MUX:        MClk only directly from dco_clk
   *   no   MCLK_DIVIDER:    no clock divider for MClk
   *   no   SMCLK_MUX:       SMClk only directly from dco_clk
   *   no   SMCLK_DIVIDER:   no clock divider for SMClk
   *   no   WATCHDOG_MUX:    clock watchdog directly from dco_clk
   *   no   ACLK_DIVIDER:    clock AClk directly from dco_clk  (unused clock)
   *        CPUOFF_EN:       allow to switch of MClk (CPU, RAMs) for LPM0..4
   *   no   SCG0_EN:         we don't switch off the DCO oscillator, because
   *                         there is none (LPM1, 3, 4)
   *        SCG1_EN:         allow to switch off SMClk (peripherals) for
   *                         LPM2..4
   *   no   OSCOFF_EN        we don't switch off the LFXT oscillator, because
   *                         there is none (LPM4)
   *
   * Low Power Modes:
   * ----------------
   *   Flags                   |        | Clocks
   *   SCG1 SCG0 OSCOFF CPUOFF | Mode   | MClk DCO SMClk AClk
   *   ------------------------------------------------------
   *    0    0     0      0    | Active |   X   X    X     X
   *    0    0     0      1    |  LPM0  |   .   X    X     X
   *    0    1     0      1    |  LPM1  |   .   .    X     X
   *    1    0     0      1    |  LPM2  |   .   .    .     X
   *    1    1     0      1    |  LPM3  |   .   .    .     X
   *    1    1     1      1    |  LPM4  |   .   .    .     .
   *
   * CPUOFF: turns off MClk (CPU, RAMs)
   * OSCOFF: turns off the external LFXT oscillator
   * SCG0:   turns off the external DCO oscillator
   * SCG1:   turns off SMClk
   *
   * LPM0: just turns off MClk (CPU, RAMs)
   * LPM1: switches off external DCO, but we don't have any
   * LPM2: switch off SMClk (GPIO, TimerA, UART, SimpleSPI)
   * LPM3: switches off external DCO, but we don't have any
   * LPM4: switches off external LFXT, but we don't have any
   *   ==> For us only LPM0 (equivalent to LPM1) and LPM2 (=LPM3) are
   *       relevant.
   *
   * MClk_s vs. SMClk_s:
   * -------------------
   * Previously all peripherals (GPIO, TimerA, UART, SimpleSPI) were connected
   * to MClk_s. But in ASIC mode, this is switched off in all LPMx. Therefore
   * now all peripherals are connected to SMClk_s. In FPGA mode, this is the
   * same signal als MClk_s and is never disabled. On the other hand,
   * SMClk_En_s is disabled in LPM2-4, therefore effectively stopping the
   * TimerA.
   * In ASIC mode, SMClk_En_s is not used, and SMClk_s is disabled in LPM2-4.
   * The problem with the TimerA is, that it internally uses its "mclk" input
   * for all the timer stuff (counting, ...) as well as for the peripheral
   * interface, i.e. there are no separate clock domains. Therefore it doesn't
   * make sense to supply TimerA with a slower LFXT, therefore also SMCLK_MUX
   * doesn't make any sense. Additionally, for peripheral access, MClk_s and
   * SMClk_s must have the same frequency. Therefore, the simplest solution is
   * to use no clock dividers (`defines MCLK_DIVIDER, SMCLK_DIVIDER) at all.
   * We only want the MCLK_CGATE and SMCLK_CGATE, which are automatically
   * defined in openMSP430_defines.v if CPUOFF_EN and SCG1_EN are defined,
   * respectively.
   *
   * Wakeup CPU on Interrupts:
   * -------------------------
   * On interrupts, the MClk must be re-enabled ("wakeup"). This is done by
   * asserting the openMSP430 "wkup" input. Internally this signal is
   * synchronized with nodiv_mclk, which introduces two cycles delay. For the
   * single-cycle interrupts of the Reconf.Module, this doesn't work.
   * Therefore I removed the sync_cell in omsp_clock_module.v, because all
   * sources of wkup are synchronous anyways.
   * Another solution would be to to keep the bits of IRQ_s at '1' until the
   * according IRQ_Ack_s bits get '1', either by an apropriate FSM state in
   * the (Ex.)Apps (but this would require IRQ_Ack_s to be a reconf.signal) or
   * by a manual "always" block here in core.v.
   *
   */

  // CPU: OpenMSP430

/*

Program Memory: 16 bit data, separate byte-enable for write, use RAM, 
Data Memory: same interface as prog.mem.
build like MSP430F1232: 8kB Flash + 256B RAM, no HW-Mult. --> 2x parallel + 2x cascaded 2kB SRAM, 2x parallel 128B SRAM
build like MSP430F2232: 8kB Flash + 512B RAM, no HW-Mult. --> 2x parallel + 2x cascaded 2kB SRAM, 2x parallel + 2x cascaded 128B SRAM

for a RAM Entity see ./core/bench/verilog/ram.v, for its instance see .../tb_openMSP430.v

for a openMSP430 instance see .../tb_openMSP430.v, and also ./core/synthesis/xilinx/src/openMSP430_fpga.v

the debug interface assumes a DCO frequency of 20.0 MHz and an UART baud rate of 2MBaud

MSP430x11x2 has Port 1 (8 IOs) and Port 2 (6 IOs)
MSP430x12x2 additionally has Port 3 (8 IOs) with pins for the additional USART (UART+SPI) module

Test Program to blink:
  https://github.com/hansiglaser/prjblinkenlights/blob/master/workspace/blinki/blinki.c

*/

  // Reset
  wire               PUC_Reset_s;
  wire               CPU_Enable_s;
  assign CPU_Enable_s = Cpu_En_i; 
  // Clocks
  wire               DCO_Enable_s;   // unused
  wire               DCO_Wakeup_s;   // unused
  wire               LFXT_Enable_s;  // unused
  wire               LFXT_Wakeup_s;  // unused
  wire               MClk_s;
  wire               AClk_s;
  wire               AClk_En_s;
  wire               SMClk_s;
  wire               SMClk_En_s;
  // Wakeup from peripherals
  wire         [0:0] Wakeup_s;   // use one bit for each peripheral, will be reduce_or()ed for the OpenMSP430 core
  // Program Memory interface
  wire [`PMEM_MSB:0] PMem_Addr_s;
  wire               PMem_En_n_s;
  wire        [15:0] PMem_DIn_s;
  wire         [1:0] PMem_Wr_n_s;
  wire        [15:0] PMem_DOut_s;
  // Data Memory interface
  wire [`DMEM_MSB:0] DMem_Addr_s;
  wire               DMem_En_n_s;
  wire        [15:0] DMem_DIn_s;
  wire         [1:0] DMem_Wr_n_s;
  wire        [15:0] DMem_DOut_s;
  // Peripherals interface
  wire        [13:0] Per_Addr_s;
  wire        [15:0] Per_DIn_s;
  wire        [15:0] Per_DOut_s;
  wire         [1:0] Per_Wr_s;
  wire               Per_En_s;
  // Interrupts
  wire        [13:0] IRQ_s;
  wire        [13:0] IRQ_Ack_s;
  assign Wakeup_s = |IRQ_s;
  wire               NMI_s;
  assign NMI_s = 1'b0;
  // Debug Interface
  wire               Dbg_Freeze_s;
  parameter DBG_I2C_ADDR      = 7'h2A; // 42 decimal, these are bits 7:1 of the I2C address byte shifted to 6:0
  parameter DBG_I2C_BROADCAST = 7'h7F;
`ifdef DBG_UART
  wire               Dbg_SCL_s;
  wire               Dbg_SDA_Out_s;
  wire               Dbg_SDA_In_s;
  assign Dbg_SCL_s    = 1'b0;
  assign Dbg_SDA_In_s = 1'b0;
`endif
`ifdef DBG_I2C
  wire               Dbg_UART_RxD_s;
  wire               Dbg_UART_TxD_s;
  assign Dbg_UART_RxD_s = 1'b0;
`endif
  // DFT Scan Mode
  wire               Scan_Enable_s;
  assign Scan_Enable_s = 1'b0;
  wire               Scan_Mode_s;
  assign Scan_Mode_s = 1'b0;


  //
  // openMSP430 Instance
  //----------------------------------

  openMSP430 openMSP430_0 (
    // Reset
    .reset_n           (Reset_n_i),         // Reset input Pin (low active, asynchronous)
    .puc_rst           (PUC_Reset_s),       // Main system reset output (synchronized to mclk)
    .cpu_en            (CPU_Enable_s),      // Enable CPU code execution (asynchronous)
    // Clocks
    .dco_enable        (DCO_Enable_s),      // ASIC ONLY: Fast oscillator enable                          (output, to oscillator)
    .dco_wkup          (DCO_Wakeup_s),      // ASIC ONLY: Fast oscillator wake-up (asynchronous)          (output, to oscillator)
    .dco_clk           (Clk_i),             // Main clock input: Fast oscillator (fast clock) 
    .lfxt_enable       (LFXT_Enable_s),     // ASIC ONLY: Low frequency oscillator enable                 (output, to oscillator)
    .lfxt_wkup         (LFXT_Wakeup_s),     // ASIC ONLY: Low frequency oscillator wake-up (asynchronous) (output, to oscillator)
    .lfxt_clk          (LFXT_Clk_i),        // Low frequency oscillator (typ 32kHz) (input, from oscillator)
    .mclk              (MClk_s),            // Main system clock       (output, to peripherals)
    .aclk              (AClk_s),            // ASIC ONLY: ACLK         (output, to peripherals)
    .aclk_en           (AClk_En_s),         // FPGA ONLY: ACLK enable  (output, to peripherals)
    .smclk             (SMClk_s),           // ASIC ONLY: SMCLK        (output, to peripherals)
    .smclk_en          (SMClk_En_s),        // FPGA ONLY: SMCLK enable (output, to peripherals)
    // Wakeup from peripherals
    .wkup              (|Wakeup_s),         // ASIC ONLY: System Wake-up (asynchronous) (input, from peripherals)
    // Program Memory
    .pmem_addr         (PMem_Addr_s),       // output: Program Memory address
    .pmem_cen          (PMem_En_n_s),       // output: Program Memory chip enable (low active)
    .pmem_dout         (PMem_DOut_s),       // input:  Program Memory data output
    .pmem_din          (PMem_DIn_s),        // output: Program Memory data input (optional)
    .pmem_wen          (PMem_Wr_n_s),       // output: Program Memory write enable (low active) (optional)
    // Data Memory
    .dmem_addr         (DMem_Addr_s),       // output: Data Memory address
    .dmem_cen          (DMem_En_n_s),       // output: Data Memory chip enable (low active)
    .dmem_dout         (DMem_DOut_s),       // input:  Data Memory data output
    .dmem_din          (DMem_DIn_s),        // output: Data Memory data input
    .dmem_wen          (DMem_Wr_n_s),       // output: Data Memory write enable (low active)
    // Peripherals
    .per_addr          (Per_Addr_s),        // output: Peripheral address
    .per_en            (Per_En_s),          // output: Peripheral enable (high active)
    .per_dout          (Per_DOut_s),        // input:  Peripheral data output
    .per_din           (Per_DIn_s),         // output: Peripheral data input
    .per_we            (Per_Wr_s),          // output: Peripheral write enable (high active)
    // Interrupts
    .irq               (IRQ_s),             // Maskable interrupts
    .irq_acc           (IRQ_Ack_s),         // Interrupt request accepted (one-hot signal)
    .nmi               (NMI_s),             // Non-maskable interrupt (asynchronous)
    // Debug Interface
    .dbg_en            (Dbg_En_i),          // Debug interface enable (asynchronous)
    .dbg_freeze        (Dbg_Freeze_s),      // output: Freeze peripherals
`ifdef DBG_UART
    .dbg_uart_rxd      (Dbg_UART_RxD_i),    // Debug interface: UART RXD (asynchronous)
    .dbg_uart_txd      (Dbg_UART_TxD_o),    // Debug interface: UART TXD
`endif
`ifdef DBG_I2C
    .dbg_uart_rxd      (Dbg_UART_RxD_s),    // Debug interface: UART RXD (asynchronous)
    .dbg_uart_txd      (Dbg_UART_TxD_s),    // Debug interface: UART TXD
`endif
    .dbg_i2c_addr      (DBG_I2C_ADDR),      // Debug interface: I2C Address
    .dbg_i2c_broadcast (DBG_I2C_BROADCAST), // Debug interface: I2C Broadcast Address (for multicore systems)
`ifdef DBG_UART
    .dbg_i2c_scl       (Dbg_SCL_s),         // Debug interface: I2C SCL
    .dbg_i2c_sda_out   (Dbg_SDA_Out_s),     // Debug interface: I2C SDA OUT
    .dbg_i2c_sda_in    (Dbg_SDA_In_s),      // Debug interface: I2C SDA IN
`endif
`ifdef DBG_I2C
    .dbg_i2c_scl       (Dbg_SCL_i),         // Debug interface: I2C SCL
    .dbg_i2c_sda_out   (Dbg_SDA_Out_o),     // Debug interface: I2C SDA OUT
    .dbg_i2c_sda_in    (Dbg_SDA_In_i),      // Debug interface: I2C SDA IN
`endif
    // DFT Scan Mode
    .scan_enable       (Scan_Enable_s),     // ASIC ONLY: Scan enable (active during scan shifting)
    .scan_mode         (Scan_Mode_s)        // ASIC ONLY: Scan mode
  );

`ifndef USE_AMS_RAM
  // Directly use OpenMSP430 testench "ram" modules, see
  // ../../openmsp430/tb/ram.v.
  // This is implied by most testbenches for this unit "core" and for all
  // apps.
  // This RAM also works perfectly with FPGA synthesis for the ZedBoard,
  // because they are automatically replaced by BlockRAMs.

  // Program Memory
  ram #(`PMEM_MSB, `PMEM_SIZE) PMem_0 (
    .ram_clk     (MClk_s),                  // Program Memory clock
    .ram_addr    (PMem_Addr_s),             // Program Memory address
    .ram_cen     (PMem_En_n_s),             // Program Memory chip enable (low active)
    .ram_dout    (PMem_DOut_s),             // Program Memory data output
    .ram_din     (PMem_DIn_s),              // Program Memory data input
    .ram_wen     (PMem_Wr_n_s)              // Program Memory write enable (low active)
  );
  
  // Data Memory
  ram #(`DMEM_MSB, `DMEM_SIZE) DMem_0 (
    .ram_clk     (MClk_s),                  // Data Memory clock
    .ram_addr    (DMem_Addr_s),             // Data Memory address
    .ram_cen     (DMem_En_n_s),             // Data Memory chip enable (low active)
    .ram_dout    (DMem_DOut_s),             // Data Memory data output
    .ram_din     (DMem_DIn_s),              // Data Memory data input
    .ram_wen     (DMem_Wr_n_s)              // Data Memory write enable (low active)
  );

`else    // USE_AMS_RAM
  // Use the memory blocks from "ams", wrapped in two separate modules.
  //
  // Note: In pmem-ram.v and dmem-ram.v there are also versions of these
  // modules which use the OpenMSP430 testench "ram".

  // Program Memory
  PMem PMem_0 (
    .ram_rstn    (Reset_n_s),               // Program Memory reset (low active)
    .ram_clk     (MClk_s),                  // Program Memory clock
    .ram_addr    (PMem_Addr_s),             // Program Memory address
    .ram_cen     (PMem_En_n_s),             // Program Memory chip enable (low active)
    .ram_dout    (PMem_DOut_s),             // Program Memory data output
    .ram_din     (PMem_DIn_s),              // Program Memory data input
    .ram_wen     (PMem_Wr_n_s)              // Program Memory write enable (low active)
  );
  
  // Data Memory
  DMem DMem_0 (
    .ram_rstn    (Reset_n_s),               // Data Memory reset (low active)
    .ram_clk     (MClk_s),                  // Data Memory clock
    .ram_addr    (DMem_Addr_s),             // Data Memory address
    .ram_cen     (DMem_En_n_s),             // Data Memory chip enable (low active)
    .ram_dout    (DMem_DOut_s),             // Data Memory data output
    .ram_din     (DMem_DIn_s),              // Data Memory data input
    .ram_wen     (DMem_Wr_n_s)              // Data Memory write enable (low active)
  );

`endif

  // GPIO
  wire        [15:0] Gpio_DOut_s;
  wire               Gpio_IRQ1_s;
  wire               Gpio_IRQ2_s;
  wire         [7:0] P1_DOut_s;
  wire         [7:0] P1_En_s;  
  wire         [7:0] P1_Sel_s; 
  wire         [7:0] P1_DIn_s; 
  wire         [7:0] P2_DOut_s;
  wire         [7:0] P2_En_s;  
  wire         [7:0] P2_Sel_s; 
  wire         [7:0] P2_DIn_s; 
  wire         [7:0] P3_DOut_s;
  wire         [7:0] P3_En_s;
  wire         [7:0] P3_Sel_s;
  wire         [7:0] P3_DIn_s;
  wire         [7:0] P4_DOut_s;
  wire         [7:0] P4_En_s;
  wire         [7:0] P4_Sel_s;
  wire         [7:0] P4_DIn_s;
  wire         [7:0] P5_DOut_s;
  wire         [7:0] P5_En_s;
  wire         [7:0] P5_Sel_s;
  wire         [7:0] P5_DIn_s;
  wire         [7:0] P6_DOut_s;
  wire         [7:0] P6_En_s;
  wire         [7:0] P6_Sel_s;
  wire         [7:0] P6_DIn_s;
  // set defined values for unused port inputs (precisely, this is not
  // necessary, because omsp_gpio ANDs these input signals with the Px_EN
  // value)
  assign P4_DIn_s = 8'd0;
  assign P5_DIn_s = 8'd0;
  assign P6_DIn_s = 8'd0;

  omsp_gpio #(.P1_EN(1'b1),
              .P2_EN(1'b1),
              .P3_EN(1'b1),
              .P4_EN(1'b0),
              .P5_EN(1'b0),
              .P6_EN(1'b0)) gpio_0 (
    .puc_rst      (PUC_Reset_s),            // Main system reset
    .mclk         (SMClk_s),                // Main system clock
    // Peripheral Interface
    .per_addr     (Per_Addr_s),             // Peripheral address
    .per_en       (Per_En_s),               // Peripheral enable (high active)
    .per_dout     (Gpio_DOut_s),            // Peripheral data output
    .per_din      (Per_DIn_s),              // Peripheral data input
    .per_we       (Per_Wr_s),               // Peripheral write enable (high active)
    // Interrupts to the CPU
    .irq_port1    (Gpio_IRQ1_s),            // Port 1 interrupt
    .irq_port2    (Gpio_IRQ2_s),            // Port 2 interrupt
    // Ports
    .p1_dout      (P1_DOut_s),              // Port 1 data output to pin
    .p1_dout_en   (P1_En_s),                // Port 1 data output enable (from direction register)
    .p1_sel       (P1_Sel_s),               // Port 1 function select (GPIO or Special Fucntion)
    .p1_din       (P1_DIn_s),               // Port 1 data input from pin
    .p2_dout      (P2_DOut_s),              // Port 2 data output to pin
    .p2_dout_en   (P2_En_s),                // Port 2 data output enable (from direction register)
    .p2_sel       (P2_Sel_s),               // Port 2 function select (GPIO or Special Fucntion)
    .p2_din       (P2_DIn_s),               // Port 2 data input from pin
    .p3_dout      (P3_DOut_s),              // Port 3 data output to pin
    .p3_dout_en   (P3_En_s),                // Port 3 data output enable (from direction register)
    .p3_sel       (P3_Sel_s),               // Port 3 function select (GPIO or Special Fucntion)
    .p3_din       (P3_DIn_s),               // Port 3 data input from pin
    .p4_dout      (P4_DOut_s),              // Port 4 data output to pin
    .p4_dout_en   (P4_En_s),                // Port 4 data output enable (from direction register)
    .p4_sel       (P4_Sel_s),               // Port 4 function select (GPIO or Special Fucntion)
    .p4_din       (P4_DIn_s),               // Port 4 data input from pin
    .p5_dout      (P5_DOut_s),              // Port 5 data output to pin
    .p5_dout_en   (P5_En_s),                // Port 5 data output enable (from direction register)
    .p5_sel       (P5_Sel_s),               // Port 5 function select (GPIO or Special Fucntion)
    .p5_din       (P5_DIn_s),               // Port 5 data input from pin
    .p6_dout      (P6_DOut_s),              // Port 6 data output to pin
    .p6_dout_en   (P6_En_s),                // Port 6 data output enable (from direction register)
    .p6_sel       (P6_Sel_s),               // Port 6 function select (GPIO or Special Fucntion)
    .p6_din       (P6_DIn_s)                // Port 6 data input from pin
  );

  // Timer
  wire        [15:0] TimerA_DOut_s;
  wire               TimerA_IRQ1_s;
  wire               TimerA_IRQ2_s;
  wire               INClk_s;
  wire               TAClk_s;
  wire               TimerA_CCI0A_s;     
  wire               TimerA_CCI0B_s;     
  wire               TimerA_Out0_s;      
  wire               TimerA_Out0_En_s;   
  wire               TimerA_CCI1A_s;     
  wire               TimerA_CCI1B_s;     
  wire               TimerA_Out1_s;      
  wire               TimerA_Out1_En_s;   
  wire               TimerA_CCI2A_s;     
  wire               TimerA_CCI2B_s;     
  wire               TimerA_Out2_s;      
  wire               TimerA_Out2_En_s;   
  omsp_timerA timerA_0 (
    .puc_rst      (PUC_Reset_s),            // Main system reset
    .mclk         (SMClk_s),                // Main system clock
    .aclk_en      (AClk_En_s),              // ACLK enable (from CPU)
    .smclk_en     (SMClk_En_s),             // SMCLK enable (from CPU)
    .dbg_freeze   (Dbg_Freeze_s),           // Freeze Timer A counter
    // Additinal Clock Inputs (internally synchronized to MClk)
    .taclk        (TAClk_s),                // TACLK external timer clock (SLOW)
    .inclk        (INClk_s),                // INCLK external timer clock (SLOW)
    // Peripheral Interface
    .per_addr     (Per_Addr_s),             // Peripheral address
    .per_en       (Per_En_s),               // Peripheral enable (high active)
    .per_dout     (TimerA_DOut_s),          // Peripheral data output
    .per_din      (Per_DIn_s),              // Peripheral data input
    .per_we       (Per_Wr_s),               // Peripheral write enable (high active)
    // Interrupts to the CPU
    .irq_ta0      (TimerA_IRQ1_s),          // Timer A interrupt: TACCR0
    .irq_ta1      (TimerA_IRQ2_s),          // Timer A interrupt: TAIV, TACCR1, TACCR2
    .irq_ta0_acc  (IRQ_Ack_s[9]),           // Interrupt request TACCR0 accepted
    // Capture/Compare Unit 0
    .ta_cci0a     (TimerA_CCI0A_s),         // Timer A compare 0 input A
    .ta_cci0b     (TimerA_CCI0B_s),         // Timer A compare 0 input B
    .ta_out0      (TimerA_Out0_s),          // Timer A output 0
    .ta_out0_en   (TimerA_Out0_En_s),       // Timer A output 0 enable (for GPIO used as special function)
    // Capture/Compare Unit 1
    .ta_cci1a     (TimerA_CCI1A_s),         // Timer A compare 1 input A
    .ta_cci1b     (TimerA_CCI1B_s),         // Timer A compare 1 input B
    .ta_out1      (TimerA_Out1_s),          // Timer A output 1
    .ta_out1_en   (TimerA_Out1_En_s),       // Timer A output 1 enable (for GPIO used as special function)
    // Capture/Compare Unit 2
    .ta_cci2a     (TimerA_CCI2A_s),         // Timer A compare 2 input A
    .ta_cci2b     (TimerA_CCI2B_s),         // Timer A compare 2 input B
    .ta_out2      (TimerA_Out2_s),          // Timer A output 2
    .ta_out2_en   (TimerA_Out2_En_s)        // Timer A output 2 enable (for GPIO used as special function)
  );
   
  // UART
  wire        [15:0] UART_DOut_s;
  wire               UART_IRQ_Rx_s;
  wire               UART_IRQ_Tx_s;
  omsp_uart #(
    .BASE_ADDR(15'h0070)                    // use MSP430F1232 base address instead of default value 0x080
  ) uart_0 (
    .puc_rst      (PUC_Reset_s),            // Main system reset
    .mclk         (SMClk_s),                // Main system clock
    .smclk_en     (SMClk_En_s),             // SMCLK enable (from CPU)
    // Peripheral Interface
    .per_addr     (Per_Addr_s),             // Peripheral address
    .per_en       (Per_En_s),               // Peripheral enable (high active)
    .per_dout     (UART_DOut_s),            // Peripheral data output
    .per_din      (Per_DIn_s),              // Peripheral data input
    .per_we       (Per_Wr_s),               // Peripheral write enable (high active)
    .irq_uart_rx  (UART_IRQ_Rx_s),          // UART receive interrupt
    .irq_uart_tx  (UART_IRQ_Tx_s),          // UART transmit interrupt
    .uart_txd     (UartTxD_o),              // UART Data Transmit (TXD)
    .uart_rxd     (UartRxD_i)               // UART Data Receive (RXD)
  );

  // SPI
  wire        [15:0] SPI_DOut_s;
  wire               SPI_IRQ_s;
  SimpleSPI #(
    .BaseAddr('h0080)
  ) spi_0 (
    .Reset_n_i    (Reset_n_s),              // Main system reset
    .Clk_i        (SMClk_s),                // Main system clock
    // Peripheral Interface
    .PerAddr_i    (Per_Addr_s),             // Peripheral address
    .PerDIn_i     (Per_DIn_s),              // Peripheral data input
    .PerDOut_o    (SPI_DOut_s),             // Peripheral data output
    .PerWr_i      (Per_Wr_s),               // Peripheral write enable (high active)
    .PerEn_i      (Per_En_s),               // Peripheral enable (high active)
    .Intr_o       (SPI_IRQ_s),              // SPI interrupt
    // SPI Interface
    .SCK_o        (SCK_o),
    .MOSI_o       (MOSI_o),
    .MISO_i       (MISO_i)
  );

  // Configuration Interface
  wire        [15:0] CfgIntf_DOut_s;
  // Parameterization Interface
  wire        [15:0] ParamIntf_DOut_s;
  // instances are inside the reconfig module

  // interrupts from ReconfModule --> CPU
  (*keep*) wire [4:0] ReconfModuleIRQs_s;
  (*keep*) wire [7:0] ReconfModuleIn_s;
  (*keep*) wire [7:0] ReconfModuleOut_s;

  // ...

  /* Peripheral Address Space
   *  - datasheet msp430f1232.pdf p. 11:
   *     - 8-bit SFRs are 0x000 to 0x00F
   *     - 8-bit peripherals are 0x010 to 0x0FF
   *     - 16-bit peripherals are 0x100 to 0x1FF
   *  - datasheet msp430f1232.pdf p. 15:
   *     - USART0 is usually 0x70-0x77
   *     - P1: 0x20-0x26
   *     - P2: 0x28-0x2E
   *     - P3: 0x18-0x1B (no interrupts)
   *     - 0x80 is free according to this table
   *  - sources
   *     - omsp_sfr.v:          0x000
   *     - omsp_gpio.v:         0x000
   *     - omsp_clock_module.v: 0x050
   *     - omsp_uart.v:         0x070 (default: 0x080)
   *     - simplespi.vhd:       0x080
   *     - omsp_timerA.v:       0x100
   *     - omsp_multiplier.v:   0x130 (default)
   *     - omsp_watchdog.v:     0x120 (default)
   *  - ReconfModule:
   *     - CfgIntf:             0x180
   *     - ParamIntf:           0x188  (default: 0x180)
   *
   * Interrupt Vectors
   *  - see datasheet msp430f1232.pdf p. 9
   */
  // Inputs to the OpenMSP430 CPU
  assign Per_DOut_s = Gpio_DOut_s | TimerA_DOut_s | UART_DOut_s | SPI_DOut_s | CfgIntf_DOut_s | ParamIntf_DOut_s | 16'd0;
  assign IRQ_s  = {ReconfModuleIRQs_s[4],   // Vector 13  (0xFFFA) - unused in MSP430F1232, highest priority -
                   ReconfModuleIRQs_s[3],   // Vector 12  (0xFFF8) - unused in MSP430F1232 -
                   ReconfModuleIRQs_s[2],   // Vector 11  (0xFFF6) - unused in MSP430F1232 -
                   1'b0,                    // Vector 10  (0xFFF4) - Watchdog -
                   TimerA_IRQ1_s,           // Vector  9  (0xFFF2)
                   TimerA_IRQ2_s,           // Vector  8  (0xFFF0)
                   UART_IRQ_Rx_s,           // Vector  7  (0xFFEE) - UART Rx
                   UART_IRQ_Tx_s,           // Vector  6  (0xFFEC) - UART Tx
                   1'b0,                    // Vector  5  (0xFFEA) - ADC -
                   SPI_IRQ_s,               // Vector  4  (0xFFE8) - unused in MSP430F1232 -
                   Gpio_IRQ2_s,             // Vector  3  (0xFFE6)
                   Gpio_IRQ1_s,             // Vector  2  (0xFFE4)
                   ReconfModuleIRQs_s[1],   // Vector  1  (0xFFE2) - unused in MSP430F1232 -
                   ReconfModuleIRQs_s[0]};  // Vector  0  (0xFFE0) - unused in MSP430F1232, lowest priority -

  // GPIO Pads
  assign P1_DOut_o[0] = (P1_Sel_s[0] ? 1'b0             : P1_DOut_s[0]);
  assign P1_DOut_o[1] = (P1_Sel_s[1] ? TimerA_Out0_s    : P1_DOut_s[1]);
  assign P1_DOut_o[2] = (P1_Sel_s[2] ? TimerA_Out1_s    : P1_DOut_s[2]);
  assign P1_DOut_o[3] = (P1_Sel_s[3] ? TimerA_Out2_s    : P1_DOut_s[3]);
  assign P1_DOut_o[4] = (P1_Sel_s[4] ? SMClk_s          : P1_DOut_s[4]);
  assign P1_DOut_o[5] = (P1_Sel_s[5] ? TimerA_Out0_s    : P1_DOut_s[5]);
  assign P1_DOut_o[6] = (P1_Sel_s[6] ? TimerA_Out1_s    : P1_DOut_s[6]);
  assign P1_DOut_o[7] = (P1_Sel_s[7] ? TimerA_Out2_s    : P1_DOut_s[7]);
  assign P1_En_o  [0] = (P1_Sel_s[0] ? 1'b0             : P1_En_s[0]);
  assign P1_En_o  [1] = (P1_Sel_s[1] ? TimerA_Out0_En_s : P1_En_s[1]);
  assign P1_En_o  [2] = (P1_Sel_s[2] ? TimerA_Out1_En_s : P1_En_s[2]);
  assign P1_En_o  [3] = (P1_Sel_s[3] ? TimerA_Out2_En_s : P1_En_s[3]);
  assign P1_En_o  [4] = (P1_Sel_s[4] ? 1'b1             : P1_En_s[4]);
  assign P1_En_o  [5] = (P1_Sel_s[5] ? TimerA_Out0_En_s : P1_En_s[5]);
  assign P1_En_o  [6] = (P1_Sel_s[6] ? TimerA_Out1_En_s : P1_En_s[6]);
  assign P1_En_o  [7] = (P1_Sel_s[7] ? TimerA_Out2_En_s : P1_En_s[7]);
  assign P1_DIn_s       = P1_DIn_i;
  assign TAClk_s        = P1_DIn_i[0];
  assign TimerA_CCI0A_s = P1_DIn_i[1];
  assign TimerA_CCI1A_s = P1_DIn_i[2];
  assign TimerA_CCI2A_s = P1_DIn_i[3];
  // P1.[7:4] not used as inputs for peripherals
  assign P2_DOut_o[0] = (P2_Sel_s[0] ? AClk_En_s        : P2_DOut_s[0]);
  assign P2_DOut_o[1] = (P2_Sel_s[1] ? 1'b0             : P2_DOut_s[1]);
  assign P2_DOut_o[2] = (P2_Sel_s[2] ? TimerA_Out0_s    : P2_DOut_s[2]);
  assign P2_DOut_o[3] = (P2_Sel_s[3] ? TimerA_Out1_s    : P2_DOut_s[3]);
  assign P2_DOut_o[4] = (P2_Sel_s[4] ? TimerA_Out2_s    : P2_DOut_s[4]);
  assign P2_DOut_o[5] =                                   P2_DOut_s[5];
  assign P2_DOut_o[6] =                                   P2_DOut_s[6];
  assign P2_DOut_o[7] =                                   P2_DOut_s[7];
  assign P2_En_o  [0] = (P2_Sel_s[0] ? 1'b1             : P2_En_s[0]);
  assign P2_En_o  [1] = (P2_Sel_s[1] ? 1'b0             : P2_En_s[1]);
  assign P2_En_o  [2] = (P2_Sel_s[2] ? TimerA_Out0_En_s : P2_En_s[2]);
  assign P2_En_o  [3] = (P2_Sel_s[3] ? TimerA_Out1_En_s : P2_En_s[3]);
  assign P2_En_o  [4] = (P2_Sel_s[4] ? TimerA_Out2_En_s : P2_En_s[4]);
  assign P2_En_o  [5] =                                   P2_En_s[5];
  assign P2_En_o  [6] =                                   P2_En_s[6];
  assign P2_En_o  [7] =                                   P2_En_s[7];
  assign P2_DIn_s       = P2_DIn_i;
  assign INClk_s        = P2_DIn_i[1];
  assign TimerA_CCI0B_s = P2_DIn_i[2];
  assign TimerA_CCI1B_s = P2_DIn_i[3];
  assign TimerA_CCI2B_s = TAClk_s;
  // Port 3: connected to the Reconf.Module
  assign P3_DIn_s = ReconfModuleOut_s;
  assign ReconfModuleIn_s = P3_DOut_s;

  /* MSP430x11x2 and 12x2 Timer pin connections: 
     P1.0/TACLK/...: Timer_A clock signal TACLK input
     P1.1/TA0:       Timer_A capture CCI0A input, compare Out0 output
     P1.2/TA1:       Timer_A capture CCI1A input, compare Out1 output
     P1.3/TA2:       Timer_A capture CCI2A input, compare Out2 output
     P1.4/...
     P1.5/TA0/...:   Timer_A compare Out0 output
     P1.6/TA1/...:   Timer_A compare Out1 output
     P1.7/TA2/...:   Timer_A compare Out2 output
     P2.0/...
     P2.1/INCLK/...: Timer_A clock signal INCLK input
     P2.2/TA0/...:   Timer_A capture CCI0B input, compare Out0 output 
     P2.3/TA1/...:   Timer_A capture CCI1B input, compare Out1 output 
     P2.4/TA2/...:   Timer_A compare Out2 output 
     P2.5/...
     P2.6, P2.7: not defined
  */

`ifdef SIMULATION
  //
  // Debug utility signals
  //----------------------------------------
  // Core testbench debuging signals
  wire    [8*32-1:0] i_state;
  wire    [8*32-1:0] e_state;
  wire        [31:0] inst_cycle;
  wire    [8*32-1:0] inst_full;
  wire        [31:0] inst_number;
  wire        [15:0] inst_pc;
  wire    [8*32-1:0] inst_short;
     
  msp_debug msp_debug_0 (
  
  // OUTPUTs
      .e_state      (e_state),           // Execution state
      .i_state      (i_state),           // Instruction fetch state
      .inst_cycle   (inst_cycle),        // Cycle number within current instruction
      .inst_full    (inst_full),         // Currently executed instruction (full version)
      .inst_number  (inst_number),       // Instruction number since last system reset
      .inst_pc      (inst_pc),           // Instruction Program counter
      .inst_short   (inst_short),        // Currently executed instruction (short version)
  
  // INPUTs
      .mclk         (MClk_s),            // Main system clock
      .puc_rst      (PUC_Reset_s)        // Main system reset
  );
`endif

  // SPI
  parameter SPI_DataWidth      = 8;
  parameter SPI_SPPRWidth      = 4;
  parameter SPI_SPRWidth       = 4;
  parameter SPI_FIFOReadWidth  = 2;
  parameter SPI_FIFOWriteWidth = 2;
  wire SPI_CPOL;
  wire SPI_CPHA;
  wire SPI_LSBFE;
  wire [(SPI_SPPRWidth+SPI_SPRWidth-1):0]  SPI_SPPR_SPR;
  wire SPI_Transmission;
  wire SPI_Write;
  wire SPI_ReadNext;
  wire [(SPI_DataWidth-1):0] SPI_DataIn;
  wire [(SPI_DataWidth-1):0] SPI_DataOut;
  wire SPI_FIFOFull;
  wire SPI_FIFOEmpty;
  wire SPI_ScanEnable;
  wire SPI_ScanClk;
  wire SPI_ScanDataIn;
  wire SPI_ScanDataOut;
  assign SPI_ScanEnable = 1'b0;
  assign SPI_ScanClk    = 1'b0;
  assign SPI_ScanDataIn = 1'b0;
  SPI_Master #(
    .DataWidth     (SPI_DataWidth),
    .SPPRWidth     (SPI_SPPRWidth),
    .SPRWidth      (SPI_SPRWidth),
    .FIFOReadWidth (SPI_FIFOReadWidth),
    .FIFOWriteWidth(SPI_FIFOWriteWidth)
  ) spi_master_1 (
    .Reset_n       (Reset_n_s),
    .Clk           (Clk_i),
    .CPOL_i        (SPI_CPOL),
    .CPHA_i        (SPI_CPHA),
    .LSBFE_i       (SPI_LSBFE),
    .SPPR_i        (SPI_SPPR_SPR[7:4]),
    .SPR_i         (SPI_SPPR_SPR[3:0]),
    .SCK_o         (SPISCK_o),
    .MOSI_o        (SPIMOSI_o),
    .MISO_i        (SPIMISO_i),
    .Transmission_o(SPI_Transmission),
    .Write_i       (SPI_Write),
    .ReadNext_i    (SPI_ReadNext),
    .Data_i        (SPI_DataIn),
    .Data_o        (SPI_DataOut),
    .FIFOFull_o    (SPI_FIFOFull),
    .FIFOEmpty_o   (SPI_FIFOEmpty),
    .ScanEnable_i  (SPI_ScanEnable),
    .ScanClk_i     (SPI_ScanClk),
    .ScanDataIn_i  (SPI_ScanDataIn),
    .ScanDataOut_o (SPI_ScanDataOut)
  );

  // I2C
  parameter I2C_ReadCountWidth   =  4;
  parameter I2C_FIFOAddressWidth =  2;
  parameter I2C_DividerWidth     = 16;
  wire I2C_F100_400_n;
  wire [(I2C_DividerWidth-1):0] I2C_Divider800;
  wire I2C_StartProcess;
  wire I2C_ReceiveSend_n;
  wire I2C_Busy;
  wire [(I2C_ReadCountWidth-1):0] I2C_ReadCount;
  wire I2C_FIFOReadNext;
  wire I2C_FIFOWrite;
  wire I2C_FIFOEmpty;
  wire I2C_FIFOFull;
  wire [7:0] I2C_DataIn;
  wire [7:0] I2C_DataOut;
  reg  I2C_ErrAck;
  wire I2C_ErrBusColl;
  wire I2C_ErrFIFOFull;
  wire I2C_ErrGotNAck;
  wire I2C_ErrCoreBusy;
  wire I2C_ErrFIFOEmpty;
  wire I2C_ErrCoreStopped;
  wire I2C_ErrDevNotPresent;
  wire I2C_ErrReadCountZero;
  (* keep *) wire [7:0] I2C_Errors;   // use this as readable parameter
  assign I2C_Errors = {I2C_ErrReadCountZero, I2C_ErrDevNotPresent, I2C_ErrCoreStopped, I2C_ErrFIFOEmpty, I2C_ErrCoreBusy, I2C_ErrGotNAck, I2C_ErrFIFOFull, I2C_ErrBusColl};
  (* keep *) wire I2C_Error;
  assign I2C_Error = |I2C_Errors;
  wire I2C_ScanEnable;
  wire I2C_ScanClk;
  wire I2C_ScanDataIn;
  wire I2C_ScanDataOut;
  assign I2C_ScanEnable = 1'b0;
  assign I2C_ScanClk    = 1'b0;
  assign I2C_ScanDataIn = 1'b0;
  i2c_master #(
    .ReadCountWidth_g  (I2C_ReadCountWidth),
    .FIFOAddressWidth_g(I2C_FIFOAddressWidth),
    .DividerWidth_g    (I2C_DividerWidth)
  ) i2c_master_1 (
    .Reset_i           (Reset_s),
    .Clk_i             (Clk_i),
    .F100_400_n_i      (I2C_F100_400_n),
    .Divider800_i      (I2C_Divider800),
    .StartProcess_i    (I2C_StartProcess),
    .ReceiveSend_n_i   (I2C_ReceiveSend_n),
    .Busy_o            (I2C_Busy),
    .ReadCount_i       (I2C_ReadCount),
    .FIFOReadNext_i    (I2C_FIFOReadNext),
    .FIFOWrite_i       (I2C_FIFOWrite),
    .FIFOEmpty_o       (I2C_FIFOEmpty),
    .FIFOFull_o        (I2C_FIFOFull),
    .Data_i            (I2C_DataIn),
    .Data_o            (I2C_DataOut),
    .ErrAck_i          (I2C_ErrAck),
    .ErrBusColl_o      (I2C_ErrBusColl),
    .ErrFIFOFull_o     (I2C_ErrFIFOFull),
    .ErrGotNAck_o      (I2C_ErrGotNAck),
    .ErrCoreBusy_o     (I2C_ErrCoreBusy),
    .ErrFIFOEmpty_o    (I2C_ErrFIFOEmpty),
    .ErrCoreStopped_o  (I2C_ErrCoreStopped),
    .ErrDevNotPresent_o(I2C_ErrDevNotPresent),
    .ErrReadCountZero_o(I2C_ErrReadCountZero),
    .SDA_i             (I2CSDA_i),
    .SDA_o             (I2CSDA_o),
    .SCL_o             (I2CSCL_o),
    .ScanEnable_i      (I2C_ScanEnable),
    .ScanClk_i         (I2C_ScanClk),
    .ScanDataIn_i      (I2C_ScanDataIn),
    .ScanDataOut_o     (I2C_ScanDataOut)
  );
  wire I2C_ErrAckParam;
  reg  I2C_ErrAckParamOld;
  always @(negedge Reset_n_s or posedge Clk_i)
  begin
    if (!Reset_n_s)
    begin
      I2C_ErrAck <= 1'b0;
      I2C_ErrAckParamOld <= 1'b0;
    end
    else
    begin
      // edge detection
      if (I2C_ErrAckParam == 1'b1 && I2C_ErrAckParamOld == 1'b0)
      begin
        I2C_ErrAck <= 1'b1;
      end
      else
      begin
        I2C_ErrAck <= 1'b0;
      end
      I2C_ErrAckParamOld <= I2C_ErrAckParam;
    end
  end

`ifdef INCLUDE_ONEWIRE

  // 1-Wire
  parameter OneWire_ROMIDArraySize         = 8;
  parameter OneWire_ROMIDIndexSize         = 3;
  parameter OneWire_ROMIDByteIndexSize     = 3;
  parameter OneWire_SearchCommand          = 8'hF0;
  parameter OneWire_CondSearchCommand      = 8'hEC;
  parameter OneWire_MatchCommand           = 8'h55;
  parameter OneWire_ReadCommand            = 8'h33;
  parameter OneWire_SkipCommand            = 8'hCC;
  parameter OneWire_OverdriveSkipCommand   = 8'h3C;
  parameter OneWire_OverdriveMatchCommand  = 8'h69;
  parameter OneWire_ConditionalReadCommand = 8'h0F;
  parameter OneWire_ResumeCommand          = 8'hA5;
  parameter OneWire_TimerWidth             = 16;
  wire OneWire_OWReset;
  wire OneWire_DeactivateOverdriveMode;
  wire OneWire_SearchROM;
  wire OneWire_ReadROM;
  wire OneWire_MatchROM;
  wire OneWire_SkipROM;
  wire OneWire_CondSearchROM;
  wire OneWire_OverdriveSkipROM;
  wire OneWire_OverdriveMatchROM;
  wire OneWire_CondReadROM;
  wire OneWire_ResumeROM;
  wire OneWire_WriteByte;
  wire OneWire_ReadByte;
  wire OneWire_GetROMID;
  wire [7:0] OneWire_DataIn;
  wire [7:0] OneWire_DataOut;
  wire [(OneWire_ROMIDIndexSize-1):0] OneWire_ROMIDsInArray;
  wire OneWire_Noslaves;
  wire OneWire_ROMIDArrayToSmall;
  wire OneWire_PDROut;
  wire OneWire_Ready;
  wire [(OneWire_TimerWidth-1):0] OneWire_ResetLowTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ResetTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ResetWaitForDetectionDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_ResetPrecenceIntervalDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_WRSlotHighDataTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_RDSlotSampleTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_SlotLowDataTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_SlotDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_RDSlotInitTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODResetLowTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODResetTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODResetWaitForDetectionDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODResetPrecenceIntervalDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODWRSlotHighDataTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODRDSlotSampleTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODSlotLowDataTime;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODSlotDuration;
  wire [(OneWire_TimerWidth-1):0] OneWire_ODRDSlotInitTime;
  wire OneWire_ScanEnable;
  wire OneWire_ScanClk;
  wire OneWire_ScanDataIn;
  wire OneWire_ScanDataOut;
  assign OneWire_ScanEnable = 1'b0;
  assign OneWire_ScanClk    = 1'b0;
  assign OneWire_ScanDataIn = 1'b0;
  onewire_master #(
    .ROMIDArraySize        (OneWire_ROMIDArraySize),
    .ROMIDIndexSize        (OneWire_ROMIDIndexSize),
    .ROMIDByteIndexSize    (OneWire_ROMIDByteIndexSize),
    .SearchCommand         (OneWire_SearchCommand),
    .CondSearchCommand     (OneWire_CondSearchCommand),
    .MatchCommand          (OneWire_MatchCommand),
    .ReadCommand           (OneWire_ReadCommand),
    .SkipCommand           (OneWire_SkipCommand),
    .OverdriveSkipCommand  (OneWire_OverdriveSkipCommand),
    .OverdriveMatchCommand (OneWire_OverdriveMatchCommand),
    .ConditionalReadCommand(OneWire_ConditionalReadCommand),
    .ResumeCommand         (OneWire_ResumeCommand),
    .TimerWidth            (OneWire_TimerWidth)
  ) onewire_master_1 (
    .Clk                            (Clk_i),
    .Reset                          (Reset_s),
    .OWIn_i                         (OneWire_i),
    .OWOut_o                        (OneWire_o),
    .OWReset_i                      (OneWire_OWReset),
    .DeactivateOverdriveMode_i      (OneWire_DeactivateOverdriveMode),
    .SearchROM_i                    (OneWire_SearchROM),
    .ReadROM_i                      (OneWire_ReadROM),
    .MatchROM_i                     (OneWire_MatchROM),
    .SkipROM_i                      (OneWire_SkipROM),
    .CondSearchROM_i                (OneWire_CondSearchROM),
    .OverdriveSkipROM_i             (OneWire_OverdriveSkipROM),
    .OverdriveMatchROM_i            (OneWire_OverdriveMatchROM),
    .CondReadROM_i                  (OneWire_CondReadROM),
    .ResumeROM_i                    (OneWire_ResumeROM),
    .WriteByte_i                    (OneWire_WriteByte),
    .ReadByte_i                     (OneWire_ReadByte),
    .GetROMID_i                     (OneWire_GetROMID),
    .Data_i                         (OneWire_DataIn),
    .Data_o                         (OneWire_DataOut),
    .ROMIDsInArray_o                (OneWire_ROMIDsInArray),
    .Noslaves_o                     (OneWire_Noslaves),
    .ROMIDArrayToSmall_o            (OneWire_ROMIDArrayToSmall),
    .PDROut_o                       (OneWire_PDROut),
    .Ready_o                        (OneWire_Ready),
    .ResetLowTime                   (OneWire_ResetLowTime),
    .ResetTime                      (OneWire_ResetTime),
    .ResetWaitForDetectionDuration  (OneWire_ResetWaitForDetectionDuration),
    .ResetPrecenceIntervalDuration  (OneWire_ResetPrecenceIntervalDuration),
    .WRSlotHighDataTime             (OneWire_WRSlotHighDataTime),
    .RDSlotSampleTime               (OneWire_RDSlotSampleTime),
    .SlotLowDataTime                (OneWire_SlotLowDataTime),
    .SlotDuration                   (OneWire_SlotDuration),
    .RDSlotInitTime                 (OneWire_RDSlotInitTime),
    .ODResetLowTime                 (OneWire_ODResetLowTime),
    .ODResetTime                    (OneWire_ODResetTime),
    .ODResetWaitForDetectionDuration(OneWire_ODResetWaitForDetectionDuration),
    .ODResetPrecenceIntervalDuration(OneWire_ODResetPrecenceIntervalDuration),
    .ODWRSlotHighDataTime           (OneWire_ODWRSlotHighDataTime),
    .ODRDSlotSampleTime             (OneWire_ODRDSlotSampleTime),
    .ODSlotLowDataTime              (OneWire_ODSlotLowDataTime),
    .ODSlotDuration                 (OneWire_ODSlotDuration),
    .ODRDSlotInitTime               (OneWire_ODRDSlotInitTime),
    .ScanEnable_i                   (OneWire_ScanEnable),
    .ScanClk_i                      (OneWire_ScanClk),
    .ScanDataIn_i                   (OneWire_ScanDataIn),
    .ScanDataOut_o                  (OneWire_ScanDataOut)
  );
`endif   // INCLUDE_ONEWIRE

`ifdef INCLUDE_PWM
  // PWM
  parameter PWM_Resolution   = 12;
  parameter PWM_CounterWidth = 20;
  wire PWM_Polarity;
  wire [(PWM_Resolution-1):0] PWM_Value;
  wire PWM_NewValue;
  wire PWM_ScanEnable;
  wire PWM_ScanClk;
  wire PWM_ScanDataIn;
  wire PWM_ScanDataOut;
  assign PWM_ScanEnable = 1'b0;
  assign PWM_ScanClk    = 1'b0;
  assign PWM_ScanDataIn = 1'b0;
  pwm_master #(
    .Resolution_g  (PWM_Resolution),
    .CounterWidth_g(PWM_CounterWidth)
  ) pwm_master_1 (
    .Clk           (Clk_i),
    .Reset         (Reset_s),
    .Polarity_i    (PWM_Polarity),
    .Input_i       (PWMInput_i),
    .Value_o       (PWM_Value),
    .NewValue_o    (PWM_NewValue),
    .ScanEnable_i  (PWM_ScanEnable),
    .ScanClk_i     (PWM_ScanClk),
    .ScanDataIn_i  (PWM_ScanDataIn),
    .ScanDataOut_o (PWM_ScanDataOut)
  );
`endif   // INCLUDE_PWM

`ifdef INCLUDE_SENT
  // SENT
  parameter SENT_MaxDatNibble =  6;
  parameter SENT_CountWidth   = 16;
  wire SENT_Chipselect;
  wire [2:0] SENT_NumDatNibble;  // width is RoundUp(Ld(MaxDatNibble_g))
  wire [(SENT_CountWidth-1):0] SENT_MinSync;
  wire [(SENT_MaxDatNibble*4+3):0] SENT_OutWide;
  wire SENT_NewData;
  wire SENT_CrcOk;
  wire SENT_ScanEnable;
  wire SENT_ScanClk;
  wire SENT_ScanDataIn;
  wire SENT_ScanDataOut;
  assign SENT_ScanEnable = 1'b0;
  assign SENT_ScanClk    = 1'b0;
  assign SENT_ScanDataIn = 1'b0;
  sent_master #(
    .MaxDatNibble_g (SENT_MaxDatNibble),
    .CountWidth_g   (SENT_CountWidth)
  ) sent_master_1 (
    .Clk            (Clk_i),
    .Reset          (Reset_s),
    .Chipselect_i   (SENT_Chipselect),
    .NumDatNibble_i (SENT_NumDatNibble),
    .Input_i        (SENTInput_i),
    .MinSync_i      (SENT_MinSync),
    .Out_o          (SENT_OutWide),
    .NewData_o      (SENT_NewData),
    .CrcOk_o        (SENT_CrcOk),
    .ScanEnable_i   (SENT_ScanEnable),
    .ScanClk_i      (SENT_ScanClk),
    .ScanDataIn_i   (SENT_ScanDataIn),
    .ScanDataOut_o  (SENT_ScanDataOut)
  );
  (* keep *) wire [1:0] SENT_OutMUX;
  (* keep *) wire [7:0] SENT_OutByte;
  assign SENT_OutByte = ((SENT_OutMUX == 2'b00) ? (*keep*) SENT_OutWide[ 7: 0] : 
                        ((SENT_OutMUX == 2'b01) ?          SENT_OutWide[15: 8] :
                        ((SENT_OutMUX == 2'b10) ?          SENT_OutWide[23:16] :
                                        {4'b0000,          SENT_OutWide[27:24]})));  
  // The array-index form below doesn't work with Yosys, because its opt_clean
  // pass removes the $shr cell, because its output is unused.
  // (* keep *) assign SENT_Out = SENT_OutWide[((SENT_OutMUX << 3)+7) : (SENT_OutMUX << 3)];
`endif   // INCLUDE_SENT

`ifdef INCLUDE_SPC
  // SPC
  parameter SPC_MaxDatNibble =  6;
  parameter SPC_CountWidth   = 16;
  parameter SPC_TimeoutWidth = 16;
  parameter SPC_UseTimeout   =  1;
  wire SPC_Start;
  wire [2:0] SPC_NumDatNibble;  // width is RoundUp(Ld(MaxDatNibble_g))
  wire [(SPC_CountWidth-1):0]   SPC_LengthTrigger;
  wire [(SPC_TimeoutWidth-1):0] SPC_LengthTimeout;
  wire [(SPC_CountWidth-1):0]     SPC_MinSync;
  wire [(SPC_MaxDatNibble*4+3):0] SPC_OutWide;
  wire SPC_NewData;
  wire SPC_CrcOk;
  wire SPC_SPCReady;
  wire SPC_ScanEnable;
  wire SPC_ScanClk;
  wire SPC_ScanDataIn;
  wire SPC_ScanDataOut;
  assign SPC_ScanEnable = 1'b0;
  assign SPC_ScanClk    = 1'b0;
  assign SPC_ScanDataIn = 1'b0;
  spc_master #(
    .MaxDatNibble_g (SPC_MaxDatNibble),
    .CountWidth_g   (SPC_CountWidth),
    .TimeoutWidth_g (SPC_TimeoutWidth),
    .UseTimeout_g   (SPC_UseTimeout)
  ) spc_master_1 (
    .Clk            (Clk_i),
    .Reset          (Reset_s),
    .Input_i        (SPCInput_i),
    .Start_i        (SPC_Start),
    .NumDatNibble_i (SPC_NumDatNibble),
    .LengthTrigger_i(SPC_LengthTrigger),
    .LengthTimeout_i(SPC_LengthTimeout),
    .MinSync_i      (SPC_MinSync),
    .Out_o          (SPCTrigger_o),
    .DataOut_o      (SPC_OutWide),
    .NewData_o      (SPC_NewData),
    .CrcOk_o        (SPC_CrcOk),
    .SPCReady_o     (SPC_SPCReady),
    .ScanEnable_i   (SPC_ScanEnable),
    .ScanClk_i      (SPC_ScanClk),
    .ScanDataIn_i   (SPC_ScanDataIn),
    .ScanDataOut_o  (SPC_ScanDataOut)
  );
  (* keep *) wire [1:0] SPC_OutMUX;
  (* keep *) wire [7:0] SPC_OutByte;
  assign SPC_OutByte = ((SPC_OutMUX == 2'b00) ? (*keep*) SPC_OutWide[ 7: 0] : 
                       ((SPC_OutMUX == 2'b01) ?          SPC_OutWide[15: 8] :
                       ((SPC_OutMUX == 2'b10) ?          SPC_OutWide[23:16] :
                                      {4'b0000,          SPC_OutWide[27:24]})));  
`endif   // INCLUDE_SPC

  // Reconfigurable Module
  `ifdef ReconfModuleConstDrivers
    `include "const-drivers.inc.v";   // just drive constant values to undriven signals
  `else
    `ifndef ReconfModuleNone
      `include "reconflogic-instance.inc.v"         // auto-generated, use later
    `endif
  `endif

endmodule
