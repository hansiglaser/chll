-------------------------------------------------------------------------------
-- Title      : VHDL model of the ADT7310 SPI Temperature Sensor
-- Project    : 
-------------------------------------------------------------------------------
-- File       : adt7310.vhd
-- Author     : Johann Glaser
-- Company    : 
-- Created    : 2012-09-25
-- Last update: 2013-05-20
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2012 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2012-09-25  1.0      hansi	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.math_real.all;

entity adt7310_model is
  port (
    -- SPI interface
    SCLK_i  : in  std_logic;
    DOUT_o  : out std_logic;
    DIN_i   : in  std_logic;
    CS_n_i  : in  std_logic;
    -- open drain outputs
    CT_n_o  : out std_logic;
    INT_n_o : out std_logic;
    -- simulation ports
    Temp_i  : in  real
  );
end adt7310_model;

architecture behavior of adt7310_model is
  -- registers
  type IntArray  is array (natural range <>) of integer;
  type BoolArray is array (natural range <>) of boolean;
  type WordArray is array (natural range <>) of std_logic_vector(15 downto 0); -- some are only 8 bit, but we simplify to use an array
  constant RegSize     : IntArray (0 to 7) := (8,8,16,8,16,8,16,16);
  constant RegWritable : BoolArray(0 to 7) := (false,true,false,false,true,true,true,true);
  signal Registers     : WordArray(0 to 7) := (
    conv_std_logic_vector(16#0080#,16),
    conv_std_logic_vector(16#0000#,16),
    conv_std_logic_vector(16#0000#,16),
    conv_std_logic_vector(16#00C1#,16),
    conv_std_logic_vector(16#4980#,16),
    conv_std_logic_vector(16#0005#,16),
    conv_std_logic_vector(16#2000#,16),
    conv_std_logic_vector(16#0500#,16));
  alias ConfigReg : std_logic_vector(15 downto 0) is Registers(1);
  alias OpMode : std_logic_vector(1 downto 0) is Registers(1)(6 downto 5);
  alias Resolution : std_logic is Registers(1)(7);
  alias TempReg : std_logic_vector(15 downto 0) is Registers(2);
  alias TCritReg : std_logic_vector(15 downto 0) is Registers(3);
  alias THystReg : std_logic_vector( 3 downto 0) is Registers(5)(3 downto 0);
  alias THighReg : std_logic_vector(15 downto 0) is Registers(6);
  alias TLowReg  : std_logic_vector(15 downto 0) is Registers(7);
  -- temperature threshold crossings
  signal TLowFlag  : std_logic := '0';
  signal THighFlag : std_logic := '0';
  signal TCritFlag : std_logic := '0';
  -- RegWriter process
  signal RegWrSPI : boolean := false;
  signal RegWrSPIAddr : std_logic_vector( 2 downto 0) := (others => '0');
  signal RegWrSPIData : std_logic_vector(15 downto 0) := (others => '0');
  signal RegWrOneShot : boolean := false;

begin  -- behavior

  -- CT and INT are not implemented, so just set them to undriven (open drain)
  CT_n_o  <= 'Z';
  INT_n_o <= 'Z';

  -- purpose: handle SPI interface
  -- type   : combinational
  -- inputs : CS_n_i,SCLK_i,DIN_i
  -- outputs: 
  SPI_proc: process (CS_n_i,SCLK_i,DIN_i,TempReg)
    type State_t is (ssWaitCommand,ssRead,ssWrite,ssIdle);
    variable State : State_t;
    -- SPI handling
    type Dir_t is (dirIn,dirOut,dirIdle);
    variable Dir      : Dir_t;
    variable BitCount : integer;        -- down-counting for each byte or word
    variable SPIIn    : std_logic_vector(15 downto 0) := (others => '0');
    variable SPIOut   : std_logic_vector(15 downto 0) := (others => '0');
    -- command handling
    variable Command  : std_logic_vector(7 downto 0);
    alias ContRead : std_logic                    is Command(2);
    alias Addr     : std_logic_vector(2 downto 0) is Command(5 downto 3);
    alias RWn      : std_logic                    is Command(6);
  begin  -- process SPI_proc
    if CS_n_i = '0' then
      -- set DOUT on falling edge
      if (Dir = dirOut) and falling_edge(SCLK_i) then
        BitCount := BitCount-1;
        DOUT_o <= SPIOut(BitCount);
      end if;
      -- sample DIN on rising edge
      if (Dir = dirIn) and rising_edge(SCLK_i) then
        BitCount := BitCount-1;
        SPIIn(BitCount) := DIN_i;
        if BitCount = 0 then
--          SPIInDone := true;
        end if;
      end if;
      
      if BitCount = 0 then
        case State is
          when ssWaitCommand =>
            -- store command
            Command := SPIIn(7 downto 0);
            -- handle command
            if RWn = '0' then
              -- write to sensor
              State := ssWrite;
              Dir := dirIn;
            else
              -- read from sensor
              State := ssRead;
              Dir := dirOut;
              SPIOut := Registers(conv_integer(unsigned(Addr)));
            end if;
            BitCount := RegSize(conv_integer(unsigned(Addr)));
          when ssRead =>
            State := ssIdle;
            Dir := dirIdle;
          when ssWrite =>
            -- use RegWriter
            RegWrSPIAddr <= Addr;
            RegWrSPIData <= SPIIn;
            RegWrSPI <= true;
            -- if config register was written, we have to do some commands
            if Addr = "001" then
              case SPIIn(6 downto 5) is
                when "00" =>            -- continouos conversion mode
                  -- TODO
                  report "Continuous Conversion Mode not yet implemented" severity failure;
                when "01" =>            -- one shot
                  -- prepare for read
                  State := ssRead;
                  Dir   := dirOut;
                  BitCount := 16;
                  Addr  := "010";       -- Temperature value, note the trick above!
                when "10" =>            -- 1 SPS mode
                  -- TODO
                  report "1 SPS Mode not yet implemented" severity failure;
                when "11" =>            -- shutdown mode
                  -- idle
                  State := ssIdle;
                  Dir := dirIdle;
                when others =>
                  -- idle
                  State := ssIdle;
                  Dir := dirIdle;
              end case;
            else
              -- idle
              State := ssIdle;
              Dir := dirIdle;
            end if;
          when ssIdle => null;          -- do nothing, just wait
          when others => null;
        end case;
      end if;
    else
      -- default values
      DOUT_o <= '1';
      -- reset state machine
      State    := ssWaitCommand;
      Dir      := dirIn;
      BitCount := 8;
      RegWrSPI <= false;
    end if;
  end process SPI_proc;

  Measure: process
  begin  -- process Measure
    wait until OpMode = "01";
    if OpMode = "01" then
      wait for 239 ms;
      RegWrOneShot <= true;
      wait for 1 ns;
      RegWrOneShot <= false;
    end if;
  end process Measure;

  RegWriter: process(RegWrSPI,RegWrOneShot)
  begin  -- process RegWriter
    if RegWrSPI then
      if RegWritable(conv_integer(unsigned(RegWrSPIAddr))) then
        Registers(conv_integer(unsigned(RegWrSPIAddr))) <= RegWrSPIData;
      end if;
    end if;
    if RegWrOneShot then
      -- save new value
      if Resolution = '0' then
        -- 13 bit mode: (2:0) = flags, (6:3) = decimal places, (15:7) = integer
        TempReg <= conv_std_logic_vector(integer(round(Temp_i * 16.0)),13) &
                   TCritFlag & THighFlag & TLowFlag;
      else
        -- 16 bit mode
        TempReg <= conv_std_logic_vector(integer(round(Temp_i * 16.0 * 8.0)),16);
      end if;
      -- switch to idle mode
      OpMode <= "11";
    end if;
  end process RegWriter;

end behavior;
