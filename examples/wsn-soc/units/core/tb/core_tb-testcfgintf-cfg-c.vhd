configuration Core_tb_TestCfgIntf_cfg of Core_tb is
  for behavior
    for DUT : Core
      use configuration work.CoreTestCfgIntf;
    end for;
  end for;
end Core_tb_TestCfgIntf_cfg;
