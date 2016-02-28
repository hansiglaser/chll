-------------------------------------------------------------------------------
--
-- SPI peripheral for the OpenMSP430
--
-- Author: Johann Glaser
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SimpleSPI is
  generic (
    BaseAddr     : integer := 16#0180#
  );
  port (
    Reset_n_i    : in  std_logic;
    Clk_i        : in  std_logic;
    -- OpenMSP430 Interface
    PerAddr_i    : in  std_logic_vector(13 downto 0);  -- in reality this is 14 downto 1
    PerDIn_i     : in  std_logic_vector(15 downto 0);
    PerDOut_o    : out std_logic_vector(15 downto 0);
    PerWr_i      : in  std_logic_vector( 1 downto 0);  -- byte select
    PerEn_i      : in  std_logic;
    Intr_o       : out std_logic;
    -- SPI Interface
    SCK_o        : out std_logic;
    MOSI_o       : out std_logic;
    MISO_i       : in  std_logic
  );
end SimpleSPI;

-------------------------------------------------------------------------------
-- Registers:
--
--   0x00 CSR Config and Status Register
--               15 (r)   Busy Bit
--            14:11       reserved
--               10 (rw)  Interrupt Enable
--             9: 6 (rw)  BRDE  (baud rate divider exponent)
--             5: 2 (rw)  BRDM  (baud rate divider mantissa)
--                1 (rw)  CPHA  (clock phase) (0: shift on falling edge of SCK (with CPOL=0))
--                0 (rw)  CPOL  (clock polarity) (0: SCK is low when idle)
--
--   0x02 DAT Data Read/Write
--            15: 8       reserved
--             7: 0 (rw)  Data
--
-- see http://sigalrm.blogspot.co.at/2011/01/introduction-to-arm-cortex-m3-part-2.html
-- see http://commons.wikimedia.org/wiki/File:SPI_timing_diagram.svg
-------------------------------------------------------------------------------

architecture rtl of SimpleSPI is
  -- addressing this module
  constant AddrWidth    : integer := 2;    -- number of bits considered for (full) address decoding
  constant MaskAddrRegs : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**AddrWidth-1,15));
  constant MaskAddrBase : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(2**15-1,15)) and (not MaskAddrRegs);
  constant BaseAddrVec  : std_logic_vector(14 downto 0) := std_logic_vector(to_unsigned(BaseAddr,15)) and MaskAddrBase;
  -- addressing individual registers
  constant AddrCSR : integer := 0;  -- Config and Status Register
  constant AddrDAT : integer := 2;  -- Data Read/Write
  -- ...
  signal Busy          : std_logic;
  signal IntEn         : std_logic;
  signal BRDE          : std_logic_vector(3 downto 0);
  signal BRDM          : std_logic_vector(3 downto 0);
  signal CPHA          : std_logic;
  signal CPOL          : std_logic;
  signal DataRead      : std_logic_vector(7 downto 0);
  signal DataWrite     : std_logic_vector(7 downto 0);

  signal Xfer          : unsigned(3 downto 0);   -- one bit wider than necessary for 8 bits
  signal XferPhase     : std_logic;

  signal Prescaler     : unsigned(15 downto 0);
  signal PrescalerPrev : unsigned(15 downto 0);
  signal PrescalerEdge : std_logic_vector(15 downto 0);
  signal PrescalerSel  : std_logic;
  signal BaudDivider   : unsigned(3 downto 0);
  signal SckToggle     : std_logic;

  signal SCK_s         : std_logic;

  
begin  -- rtl

  -- purpose: peripheral read via bus interface
  -- type   : combinational
  -- inputs : PerAddr_i,PerEn_i
  -- outputs: PerDOut_o
  PerRead: process (PerAddr_i,PerEn_i,PerWr_i,Busy,IntEn,BRDE,BRDM,CPHA,CPOL,DataRead)
  begin  -- process PerRead
    PerDOut_o <= (others => '0');
    if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "00" then
      case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
        when AddrCSR => PerDOut_o <= Busy & "0000" & IntEn & BRDE & BRDM & CPHA & CPOL;
        when AddrDAT => PerDOut_o <= "00000000" & DataRead;
        when others => null;
      end case;
    end if;
  end process PerRead;

  -- purpose: Peripheral write, shift config
  -- type   : sequential
  -- inputs : Clk_i, Reset_n_i
  -- outputs: 
  PerWrite: process (Clk_i, Reset_n_i)
  begin  -- process PerWrite
    if Reset_n_i = '0' then             -- asynchronous reset (active low)
      Intr_o        <= '0';
      Busy          <= '0';
      IntEn         <= '0';
      BRDE          <= (others => '0');
      BRDM          <= (others => '0');
      CPHA          <= '0';
      CPOL          <= '0';
      DataWrite     <= (others => '0');
      DataRead      <= (others => '0');
      Xfer          <= (others => '0');
      XferPhase     <= '0';

      Prescaler     <= (others => '0');
      PrescalerPrev <= (others => '0');
      BaudDivider   <= (others => '0');
      SckToggle     <= '0';
      SCK_s         <= '0';
      MOSI_o        <= '0';
    elsif Clk_i'event and Clk_i = '1' then  -- rising clock edge

      Intr_o    <= '0';

      -- Peripheral write via bus interface
      if PerEn_i = '1' and ((PerAddr_i & '0') and MaskAddrBase) = BaseAddrVec and PerWr_i = "11" and Busy = '0' then
        case to_integer(unsigned(PerAddr_i(AddrWidth-2 downto 0) & '0')) is
          when AddrCSR =>
            -- 15: don't write Busy
            -- 14:11: reserved, ignore
            IntEn   <= PerDIn_i(          10); 
            BRDE    <= PerDIn_i( 9 downto  6);
            BRDM    <= PerDIn_i( 5 downto  2); 
            CPHA    <= PerDIn_i(           1); 
            CPOL    <= PerDIn_i(           0); 
          when AddrDAT =>
            DataWrite <= PerDIn_i(7 downto 0);
            Busy      <= '1';
            Xfer      <= to_unsigned(9,Xfer'length);
            XferPhase <= '0';
          when others => null;
        end case;
      end if;

      -- baud rate generator
      if Xfer = 0 then
        -- reset baud rate generator
        Prescaler     <= (others => '0');
        PrescalerPrev <= (others => '1');    -- set to all '1', so that PrescalerSel is '1' when starting a transmission
        BaudDivider   <= (others => '0');
        SCK_s         <= '0';
      else
        PrescalerPrev <= PreScaler;
        Prescaler     <= Prescaler+1;         -- use integer overflow
        if PrescalerSel = '1' then
          if BaudDivider = 0 then
            BaudDivider <= unsigned(BRDM);
            if XferPhase = '0' then
              -- set MOSI, shift data
              if not (Xfer = 1) then
                MOSI_o    <= DataWrite(7);
                DataWrite <= DataWrite(6 downto 0) & '0';
                SCK_s     <= CPHA;
              else
                MOSI_o    <= '0';
                SCK_s     <= '0';
                Busy      <= '0';
                Intr_o    <= IntEn;
              end if;
              XferPhase <= '1';
              Xfer      <= Xfer - 1;
            else
              -- sample MISO
              DataRead  <= DataRead(6 downto 0) & MISO_i;    -- attention: this is an unregistered input :-( TODO
              XferPhase <= '0';
              SCK_s     <= not CPHA;
            end if;
          else
            BaudDivider <= BaudDivider-1;
          end if;
        end if;
      end if;
    end if;
  end process PerWrite;

  PrescalerEdge <= std_logic_vector(Prescaler) xor std_logic_vector(PrescalerPrev);   -- '1' for all bits which have toggled in the last clock periode
  PrescalerSel  <= PrescalerEdge(to_integer(unsigned(BRDE))); -- prescaled clock: select toggle indicator

  SCK_o <= SCK_s xor CPOL;

end rtl;
