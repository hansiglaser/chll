configuration Core_tb_behavior_cfg of Core_tb is
  for behavior
    for DUT : Core
      use entity work.Core(verilog);
    end for;
  end for;
end Core_tb_behavior_cfg;

