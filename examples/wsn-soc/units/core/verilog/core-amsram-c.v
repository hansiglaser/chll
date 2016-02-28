config CoreAMSRAM;
  design work.Core;
  default liblist work;
  instance Core.DMem_0 use work.dmem_timingchecksoff_cfg;
endconfig
