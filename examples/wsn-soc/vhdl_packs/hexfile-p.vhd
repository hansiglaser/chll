------------------------------------------------------------------------------
--
-- Intel Hex File Reader
--
-- This package provides the function ReadHexFile which reads in an Intel Hex
-- file from the file system and provides its contents as a variable.
--
-- Author: Johann Glaser
--
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

package HexFile is

  -- data types
  subtype Byte_t is std_logic_vector(7 downto 0);
  type ByteArray is array (natural range <>) of Byte_t;
  type HexRecord is record
                      Addr : integer;
                      Len  : integer;
                      Data : ByteArray(0 to 15);
                    end record;
  type HexRecordArray is array (natural range <>) of HexRecord;
  type PHexRecordArray is access HexRecordArray;

  -- Intel Hex file reader
  impure function ReadHexFile (
    constant Filename : string)
    return PHexRecordArray;

end HexFile;

package body HexFile is

  -- maximum line length in the Intel Hex file
  constant MaxLen : integer := 80;

  -- helper function to convert a hex number as string to an integer
  function Hex2Int (
    constant Hex : string)
    return integer is
    variable Result : integer;
    variable D      : integer;
  begin  -- Hex2Int
    Result := 0;
    for i in Hex'left to Hex'right loop
      if Hex(i) >= '0' and Hex(i) <= '9' then
        D := character'pos(Hex(i)) - character'pos('0');
      elsif Hex(i) >= 'A' and Hex(i) <= 'F' then
        D := character'pos(Hex(i)) - character'pos('A') + 10;
      elsif Hex(i) >= 'a' and Hex(i) <= 'f' then
        D := character'pos(Hex(i)) - character'pos('a') + 10;
      else
        report "Invalid Hex string """ & Hex & """" severity failure;
      end if;
      Result := Result*16 + D;
    end loop;  -- i
    return Result;
  end Hex2Int;

  -- Intel Hex file reader
  impure function ReadHexFile(
    constant Filename : string)
    return PHexRecordArray is
    file F : text open read_mode is Filename;
    variable L : line;                  -- line is access string
    variable Row : integer;
    variable St : string(1 to MaxLen);
    variable ChkSum : integer;
    variable Data : HexRecord;
    variable Result : PHexRecordArray;
    variable OldResult : PHexRecordArray;
  begin  -- ReadHexFile
    ---------------------------------------------------------------------------
    -- Intel HEX File
    --
    -- see e.g. http://de.wikipedia.org/wiki/Intel_HEX
    --
    -- ":lloooorr....cc"
    --   ll:   length of payload data
    --   oooo: offset (big endian)
    --   rr:   record type
    --   ....: payload data
    --   cc:   checksum
    --
    -- Record Types:
    --   00: data record (payload)
    --   01: end of file
    --   02: segment address (for following data)
    --   03: start address (CS:IP)
    --   04: extended linear address
    --   05: start address (EIP)

    Row := 0;
    Result := null;
    while not EndFile(F) loop
      
      readline(F,L);
      St := L.all;                      -- use as shortcut, but we can't rely on its length!
      Row := Row + 1;
      -- check leading ':'
      assert St(1) = ':'
        report Filename & ":" & integer'image(Row) & ": """ & L.all & """: Invalid syntax" severity failure;
      -- check checksum
      ChkSum := 0;
      for i in 0 to (L.all'length-1)/2-1 loop
        ChkSum := ChkSum + Hex2Int(St(2+2*i to 3+2*i));
      end loop;  -- i
      assert to_unsigned(ChkSum,32)(7 downto 0) = "0000000"
        report Filename & ":" & integer'image(Row) & ": """ & L.all & """: Invalid checksum" severity failure;

      -- interpret string
      if St(8) = '0' and St(9) = '0' then
        -- data record (payload)
        Data.Addr := Hex2Int(St(4 to 7));
        Data.Len  := Hex2Int(St(2 to 3));
        for i in 0 to Data.Len-1 loop
          Data.Data(i) := std_logic_vector(to_unsigned(Hex2Int(St(10+2*i to 11+2*i)),8));
        end loop;  -- i

        if Result = null then
          -- first record
          Result := new HexRecordArray(0 to 0);
        else
          -- 2nd and following records: new larger array
          OldResult := Result;
          Result := new HexRecordArray(0 to OldResult'length);
          for i in OldResult'range loop
            Result(i) := OldResult(i);
          end loop;  -- i
          Deallocate(OldResult);
        end if;
        Result(Result'high) := Data;
      elsif St(8) = '0' and St(9) = '1' then
        -- end of file
        exit;                             -- exit this loop
      elsif St(8) = '0' and St(9) = '3' then
        -- extended linear address
        -- ignored
      else
        report Filename & ":" & integer'image(Row) & ": """ & L.all & """: Invalid or not supported record type" severity failure;
      end if;
    end loop;
    return Result;
  end ReadHexFile;

end HexFile;
