

architecture rtl of FIFOSyncCmp is

begin
  -- generate output signals
  FIFOEmpty_o <= '1' when (PointerA_i = PointerB_i) and (MSBPointerA_i = MSBPointerB_i) else
                 '0';
  FIFOFull_o  <= '1' when (PointerA_i = PointerB_i) and (MSBPointerA_i /= MSBPointerB_i) else
                 '0';

end rtl;

