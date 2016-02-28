------------------------------------------------------------------------------
--
-- Miscellaneous Utility Functions
--
-- Author: Johann Glaser
--
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package Utils is 

  function WidthFromMax (x : positive) return natural;

  type integer_vector is array (natural range <>) of integer; 

  function max (constant x : integer_vector) return integer;

  function max (constant l,r : integer) return integer;

  function min (constant x : integer_vector) return integer;

  function min (constant l,r : integer) return integer;

  function FillVec (
    constant Width : in positive;
    constant Value : in std_logic
  ) return std_logic_vector;

end Utils;


package body Utils is 

  function WidthFromMax (x : positive) return natural is
    variable temp, log : natural;
  begin
    temp := x;
    log  := 0;
    while (temp /= 0) loop
      temp := temp/2;
      log  := log + 1;
    end loop;
    return log;
  end WidthFromMax;

  function max (
    constant x : integer_vector
  ) return integer is
    variable temp : integer;
  begin
    temp := integer'low;
    for i in x'low to x'high loop
      if x(i) > temp then
        temp := x(i);
      end if;
    end loop;  -- i
    return temp;
  end max;

  function max (constant l,r : integer) return integer is
  begin  -- max
    if l > r then
      return l;
    else
      return r;
    end if;
  end max;

  function min (
    constant x : integer_vector
  ) return integer is
    variable temp : integer;
  begin
    temp := integer'high;
    for i in x'low to x'high loop
      if x(i) < temp then
        temp := x(i);
      end if;
    end loop;  -- i
    return temp;
  end min;

  function min (constant l,r : integer) return integer is
  begin  -- min
    if l < r then
      return l;
    else
      return r;
    end if;
  end min;

  function FillVec (
    constant Width : in positive;
    constant Value : in std_logic
  ) return std_logic_vector is
    variable Result : std_logic_vector(Width-1 downto 0);
  begin
    Result := (others => '0');
    return Result;
  end FillVec;
  
end Utils;
