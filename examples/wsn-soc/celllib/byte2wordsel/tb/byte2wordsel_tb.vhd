library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library work;
use work.TbFuncs.all;

entity Byte2WordSel_tb is
end Byte2WordSel_tb;

architecture behavior of Byte2WordSel_tb is

  component Byte2WordSel
    port (
      H_i     : in  std_logic_vector(7 downto 0);
      L_i     : in  std_logic_vector(7 downto 0);
      Y_o     : out std_logic_vector(15 downto 0);
      Shift_i : in  std_logic_vector(3 downto 0);
      Mask_i  : in  std_logic_vector(3 downto 0)
    );
  end component;

  constant TestCases           : natural := 100;
  constant CheckOutputDelay    : time := 20 ns;
  constant SetupNextInputDelay : time := 20 ns;

  signal H_i        : std_logic_vector(7 downto 0);
  signal L_i        : std_logic_vector(7 downto 0);
  signal Y_o        : std_logic_vector(15 downto 0);
  signal CfgShift_i : std_logic_vector(3 downto 0);
  signal CfgMask_i  : std_logic_vector(3 downto 0);

begin

  DUT: Byte2WordSel
    port map (
      H_i     => H_i,
      L_i     => L_i,
      Y_o     => Y_o,
      Shift_i => CfgShift_i,
      Mask_i  => CfgMask_i
    );

  StimulusProc: process 
    variable S1 : positive;
    variable S2 : positive;
    variable R  : real;
    variable Y  : std_logic_vector(15 downto 0);
    variable M  : unsigned(15 downto 0);    -- mask, can't be given directly
  begin
    H_i        <= "00000000";
    L_i        <= "00000000";
    CfgShift_i <= "0000";
    CfgMask_i  <= "0000";
    M          := (others => '1'); 

    wait for SetupNextInputDelay;
    
    for i in 1 to TestCases loop
      Uniform(S1,S2,R);
      H_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(256))),8));
      Uniform(S1,S2,R);
      L_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(256))),8));
      Uniform(S1,S2,R);
      CfgShift_i <= std_logic_vector(to_unsigned(integer(trunc(R * real(16))),4));
      Uniform(S1,S2,R);
      CfgMask_i  <= std_logic_vector(to_unsigned(integer(trunc(R * real(16))),4));

      wait for CheckOutputDelay;

      -- reference
      Y := H_i & L_i;
      Y := std_logic_vector(unsigned(Y) srl to_integer(unsigned(CfgShift_i)));
      Y := Y and std_logic_vector(M srl (15-to_integer(unsigned(CfgMask_i))));

      assert Y_o = Y
        report "Wrong Result Y_o = " & Vector2String(Y_o) &
          " for H_i = " & Vector2String(H_i) &
             ", L_i = " & Vector2String(L_i) &
             ", Shift_i = " & Vector2String(CfgShift_i) &
             ", Mask_i = " & Vector2String(CfgMask_i) &
          ", should be " & Vector2String(Y) severity error;

      wait for SetupNextInputDelay;
    end loop;

    -- End of simulation
    report "### Simulation Finished ###" severity failure;
    wait;
  end process StimulusProc;

end behavior;
