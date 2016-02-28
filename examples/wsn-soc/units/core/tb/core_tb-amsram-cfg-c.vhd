configuration Core_tb_amsram_cfg of Core_tb is
  for behavior
    for DUT : Core
      use configuration work.CoreAMSRAM;
--      use entity work.Core(verilog);
--      for verilog
--        for DMem_0 : DMem
--          use configuration work.DMem_TimingChecksOFF_cfg;
--        end for;
--      end for;
    end for;
  end for;
end Core_tb_amsram_cfg;
