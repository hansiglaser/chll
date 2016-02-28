-- Manually adapted from ../../../../../reconfmodule/chll/out/chip-fpga_top-a.vhd

architecture fpga_top of chip is

  component Core
    generic (
      DBG_I2C_ADDR : integer := 42
    );
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

  signal Dbg_SDA_In_i : std_logic;
  signal Dbg_SDA_Out_o : std_logic;
  signal P1_DIn_i : std_logic_vector(7 downto 0);
  signal P1_DOut_o : std_logic_vector(7 downto 0);
  signal P1_En_o : std_logic_vector(7 downto 0);
  signal P2_DIn_i : std_logic_vector(7 downto 0);
  signal P2_DOut_o : std_logic_vector(7 downto 0);
  signal P2_En_o : std_logic_vector(7 downto 0);
  signal I2CSCL_o : std_logic;
  signal I2CSDA_i : std_logic;
  signal I2CSDA_o : std_logic;
  
  constant ClkDivWidth : integer := 10;
  constant StartValue  : integer range 0 to (2**ClkDivWidth-1) := 4; -- 4: 10MHz, 49: 1MHz, 499: 100kHz
  signal Clk_s : std_logic;
  signal Counter : unsigned(ClkDivWidth-1 downto 0);

begin

  ClkDividerProc: process (Clk_i, Reset_i)
  begin  -- process ClkDividerProc
    if Reset_i = '1' then
      Counter <= (others => '0');
      Clk_s   <= '0';
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge
      if Counter = 0 then
        Counter <= to_unsigned(StartValue,Counter'length);
        Clk_s   <= not Clk_s;
      else
        Counter <= Counter - 1;
      end if;
    end if;  
  end process ClkDividerProc;

  core_1: Core
    port map (
      Reset_n_i => "not"(Reset_i),
      Clk_i => Clk_s,
      Cpu_En_i => '1',
      LFXT_Clk_i => '0',
      Dbg_En_i => Dbg_En_i,
      Dbg_SCL_i => Dbg_SCL_i,
      Dbg_SDA_In_i => Dbg_SDA_In_i,
      Dbg_SDA_Out_o => Dbg_SDA_Out_o,
      P1_DIn_i => P1_DIn_i,
      P1_DOut_o => P1_DOut_o,
      P1_En_o => P1_En_o,
      P2_DIn_i => P2_DIn_i,
      P2_DOut_o => P2_DOut_o,
      P2_En_o => P2_En_o,
      UartRxD_i => UartRxD_i,
      UartTxD_o => UartTxD_o,
      MISO_i => MISO_i,
      MOSI_o => MOSI_o,
      SCK_o => SCK_o,
      Inputs_i => Inputs_i,
      Outputs_o => Outputs_o,
      SPIMISO_i => SPIMISO_i,
      SPIMOSI_o => SPIMOSI_o,
      SPISCK_o => SPISCK_o,
      I2CSCL_o => I2CSCL_o,
      I2CSDA_i => I2CSDA_i,
      I2CSDA_o => I2CSDA_o,
      AdcConvComplete_i => AdcConvComplete_i,
      AdcDoConvert_o => AdcDoConvert_o,
      AdcValue_i => AdcValue_i
    );

  Dbg_SDA_In_i <= To_X01(Dbg_SDA_b);

  OD_Dbg_SDA_b_Proc: process (Dbg_SDA_Out_o)
  begin
    if Dbg_SDA_Out_o = '1' then
      Dbg_SDA_b <= 'Z';
    else
      Dbg_SDA_b <= '0';
    end if;
  end process OD_Dbg_SDA_b_Proc;

  P1_DIn_i <= To_X01(P1_b);

  InOut_P1_b_Proc: process (P1_DOut_o,P1_En_o)
  begin
    for I in P1_DOut_o'range loop
      if P1_En_o(I) = '1' then
        P1_b(I) <= P1_DOut_o(I);
      else
        P1_b(I) <= 'Z';
      end if;
    end loop;
  end process InOut_P1_b_Proc;

  P2_DIn_i <= To_X01(P2_b);

  InOut_P2_b_Proc: process (P2_DOut_o,P2_En_o)
  begin
    for I in P2_DOut_o'range loop
      if P2_En_o(I) = '1' then
        P2_b(I) <= P2_DOut_o(I);
      else
        P2_b(I) <= 'Z';
      end if;
    end loop;
  end process InOut_P2_b_Proc;

  OD_I2CSCL_b_Proc: process (I2CSCL_o)
  begin
    if I2CSCL_o = '1' then
      I2CSCL_b <= 'Z';
    else
      I2CSCL_b <= '0';
    end if;
  end process OD_I2CSCL_b_Proc;

  I2CSDA_i <= To_X01(I2CSDA_b);

  OD_I2CSDA_b_Proc: process (I2CSDA_o)
  begin
    if I2CSDA_o = '1' then
      I2CSDA_b <= 'Z';
    else
      I2CSDA_b <= '0';
    end if;
  end process OD_I2CSDA_b_Proc;


end fpga_top;

