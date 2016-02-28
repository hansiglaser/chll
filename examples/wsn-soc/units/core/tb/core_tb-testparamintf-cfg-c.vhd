configuration Core_tb_TestParamIntf_cfg of Core_tb is
  for behavior
    for DUT : Core
      use configuration work.CoreTestParamIntf;
    end for;
  end for;
end Core_tb_TestParamIntf_cfg;
