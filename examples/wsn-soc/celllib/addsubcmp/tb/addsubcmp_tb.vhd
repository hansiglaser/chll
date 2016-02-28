-------------------------------------------------------------------------------
-- Title      : Testbench for design "AddSubCmp"
-- Project    : 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.tbfuncs.all;

-------------------------------------------------------------------------------

entity AddSubCmp_tb is

end AddSubCmp_tb;

-------------------------------------------------------------------------------

architecture behavior of AddSubCmp_tb is

  component AddSubCmp
    generic (
      Width : integer
    );
    port (
      AddOrSub_i : in  std_logic;
      A_i        : in  std_logic_vector(Width-1 downto 0);
      B_i        : in  std_logic_vector(Width-1 downto 0);
      D_o        : out std_logic_vector(Width-1 downto 0);
      Carry_i    : in  std_logic;
      Carry_o    : out std_logic;
      Zero_o     : out std_logic;
      Sign_o     : out std_logic;
      Overflow_o : out std_logic
    );
  end component;

  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  -- component generics
  constant Width : integer := 4;

  -- component ports
  signal AddOrSub_i : std_logic;
  signal A_i        : std_logic_vector(Width-1 downto 0);
  signal B_i        : std_logic_vector(Width-1 downto 0);
  signal D_o        : std_logic_vector(Width-1 downto 0);
  signal Carry_i    : std_logic;
  signal Carry_o    : std_logic;
  signal Zero_o     : std_logic;
  signal Sign_o     : std_logic;
  signal Overflow_o : std_logic;

  procedure CheckAddSubCmp (
    constant A          : in  integer;
    constant B          : in  integer;
    constant CarryIn    : in  std_logic;
    constant AddOrSub   : in  std_logic;
    constant SignedCalc : in  boolean;
    signal   A_i        : out std_logic_vector(Width-1 downto 0);
    signal   B_i        : out std_logic_vector(Width-1 downto 0);
    signal   Carry_i    : out std_logic;
    signal   AddOrSub_i : out std_logic;
    signal   D_o        : in  std_logic_vector(Width-1 downto 0);
    signal   Carry_o    : in  std_logic;
    signal   Zero_o     : in  std_logic;
    signal   Sign_o     : in  std_logic;
    signal   Overflow_o : in  std_logic
  ) is
    variable A_s      : std_logic_vector(Width+1 downto 0);
    variable B_s      : std_logic_vector(Width+1 downto 0);
    variable A_v      : integer;
    variable B_v      : integer;
    variable D        : integer;
    variable D_s      : std_logic_vector(Width+1 downto 0);
    variable Carry    : std_logic;
    variable Zero     : std_logic;
    variable Sign     : std_logic;
    variable Overflow : std_logic;
    variable Temp     : integer;
  begin  -- CheckAddSubCmp
    if not SignedCalc then
      -- unsigned
      assert A <   2**Width report "A is too large (" & integer'image(A) & " > " & integer'image(2**Width-1) severity failure;
      assert A >=  0        report "A is too small (" & integer'image(A) & " < " & integer'image(0         ) severity failure;
      assert B <   2**Width report "B is too large (" & integer'image(A) & " > " & integer'image(2**Width-1) severity failure;
      assert B >=  0        report "B is too small (" & integer'image(A) & " < " & integer'image(0         ) severity failure;
    else
      -- signed
      assert A <   2**(Width-1) report "A is too large (" & integer'image(A) & " > " & integer'image( 2**(Width-1)-1) severity failure;
      assert A >= -2**(Width-1) report "A is too small (" & integer'image(A) & " < " & integer'image(-2**(Width-1)  ) severity failure;
      assert B <   2**(Width-1) report "B is too large (" & integer'image(A) & " > " & integer'image( 2**(Width-1)-1) severity failure;
      assert B >= -2**(Width-1) report "B is too small (" & integer'image(A) & " < " & integer'image(-2**(Width-1)  ) severity failure;
    end if;

    -- set defaults
    Zero := '0';
    Carry := '0';

    -- Calculation
    A_v := A;
    B_v := B;
    if A_v < 0 then
      A_v := A_v + 2**Width;
    end if;
    if B_v < 0 then
      B_v := B_v + 2**Width;
    end if;
    if CarryIn = '1' then
      if AddOrSub = '0' then
        -- add
        A_v := A_v + 1;
      else
        -- sub
        A_v := A_v - 1;
      end if;
    end if;
    if AddOrSub = '1' then
      B_v := 2**Width - B_v;
    end if;

    D := A_v + B_v;

    A_s  := std_logic_vector(to_signed(A,Width+2));  -- use Width+2 to avoid warnings on a truncated vector by to_signed
    B_s  := std_logic_vector(to_signed(B,Width+2));
    D_s  := std_logic_vector(to_signed(D,Width+2));
    if D > 2**Width-1 then
      Carry := '1';
    end if;
    if unsigned(D_s(Width-1 downto 0)) = 0 then
      Zero := '1';
    end if;
    Sign := D_s(Width-1);
    if AddOrSub = '0' then
      Overflow := ((not A_s(Width-1)) and (not B_s(Width-1)) and      D_s(Width-1) ) or
                  (     A_s(Width-1)  and      B_s(Width-1)  and (not D_s(Width-1)));
    else
      Overflow := ((not A_s(Width-1)) and      B_s(Width-1)  and      D_s(Width-1) ) or
                  (     A_s(Width-1)  and (not B_s(Width-1)) and (not D_s(Width-1)));
    end if;
    -- set inputs
    A_i        <= A_s(Width-1 downto 0);
    B_i        <= B_s(Width-1 downto 0);
    Carry_i    <= CarryIn;
    AddOrSub_i <= AddOrSub;
    wait for CheckOutputDelay;
    -- check outputs
    assert D_o = D_s(Width-1 downto 0)
      report                            "Wrong Result " &   Vector2String(D_o)          & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & Vector2String(D_s)        severity error;
    assert Carry_o    = Carry    report "Wrong Carry " &    std_logic'image(Carry_o)    & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & std_logic'image(Carry)    severity error;
    assert Zero_o     = Zero     report "Wrong Zero " &     std_logic'image(Zero_o)     & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & std_logic'image(Zero)     severity error;
    assert Sign_o     = Sign     report "Wrong Sign " &     std_logic'image(Sign_o)     & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & std_logic'image(Sign)     severity error;
    assert Overflow_o = Overflow report "Wrong Overflow " & std_logic'image(Overflow_o) & " for A = " & integer'image(A) & ", B = " & integer'image(B) & ", should be " & std_logic'image(Overflow) severity error;
    wait for SetupNextInputDelay;
  end CheckAddSubCmp;

begin  -- behavior

  -- component instantiation
  DUT: AddSubCmp
    generic map (
      Width => Width)
    port map (
      AddOrSub_i => AddOrSub_i,
      A_i        => A_i,
      B_i        => B_i,
      D_o        => D_o,
      Carry_i    => Carry_i,
      Carry_o    => Carry_o,
      Zero_o     => Zero_o,
      Sign_o     => Sign_o,
      Overflow_o => Overflow_o);

  -- Check
  CheckProc: process
  begin
    wait for 10 ns;
    -- add, unsigned, CarryIn = '0'
    assert false report "Add, unsigned, CarryIn = 0" severity note;
    for A in 0 to 2**Width-1 loop
      for B in 0 to 2**Width-1 loop
        CheckAddSubCmp(A,B,'0','0',false, A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- add, unsigned, CarryIn = '1'
    assert false report "Add, unsigned, CarryIn = 1" severity note;
    for A in 0 to 2**Width-1 loop
      for B in 0 to 2**Width-1 loop
        CheckAddSubCmp(A,B,'1','0',false, A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- add, signed, CarryIn = '0'
    assert false report "Add, signed, CarryIn = 0" severity note;
    for A in -(2**(Width-1)) to 2**(Width-1)-1 loop
      for B in -(2**(Width-1)) to 2**(Width-1)-1 loop
        CheckAddSubCmp(A,B,'0','0',true,  A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- add, signed, CarryIn = '1'
    assert false report "Add, signed, CarryIn = 1" severity note;
    for A in -(2**(Width-1)) to 2**(Width-1)-1 loop
      for B in -(2**(Width-1)) to 2**(Width-1)-1 loop
        CheckAddSubCmp(A,B,'1','0',true,  A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- sub, unsigned, CarryIn = '0'
    assert false report "Sub, unsigned, CarryIn = 0" severity note;
    for A in 0 to 2**Width-1 loop
      for B in 0 to 2**Width-1 loop
        CheckAddSubCmp(A,B,'0','1',false, A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- sub, unsigned, CarryIn = '1'
    assert false report "Sub, unsigned, CarryIn = 1" severity note;
    for A in 0 to 2**Width-1 loop
      for B in 0 to 2**Width-1 loop
        CheckAddSubCmp(A,B,'1','1',false, A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- sub, signed, CarryIn = '0'
    assert false report "Sub, signed, CarryIn = 0" severity note;
    for A in -(2**(Width-1)) to 2**(Width-1)-1 loop
      for B in -(2**(Width-1)) to 2**(Width-1)-1 loop
        CheckAddSubCmp(A,B,'0','1',true,  A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    -- sub, signed, CarryIn = '1'
    assert false report "Sub, signed, CarryIn = 1" severity note;
    for A in -(2**(Width-1)) to 2**(Width-1)-1 loop
      for B in -(2**(Width-1)) to 2**(Width-1)-1 loop
        CheckAddSubCmp(A,B,'1','1',true,  A_i,B_i,Carry_i,AddOrSub_i,D_o,Carry_o,Zero_o,Sign_o,Overflow_o);
      end loop;  -- B
    end loop;  -- A

    ---------------------------------------------------------------------------
    -- Simulation is finished
    ---------------------------------------------------------------------------
    assert false
      report "### simulation is finished ###"
      severity failure ;

  end process CheckProc;

end behavior;

configuration AddSubCmp_tb_verilog_cfg of AddSubCmp_tb is
  for behavior
    for DUT : AddSubCmp
      use configuration work.AddSubCmpVerilog;
    end for;
  end for;
end AddSubCmp_tb_verilog_cfg;
