config CoreTestCfgIntf;
  design work.Core;
  default liblist work;
  instance Core.MyReconfigLogic_0 use work.myreconfiglogictestcfgintf_cfg;
endconfig
