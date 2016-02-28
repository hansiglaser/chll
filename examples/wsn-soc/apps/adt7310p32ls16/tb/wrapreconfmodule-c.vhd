------------------------------------------------------------------------------
-- Special configuration which disconnects the ParamOutReg modules, so that
-- we can drive the values with VHDL'2008 external names in the Reconf.Module
-- wrapper <app>-wrapreconfmodule.vhd.
------------------------------------------------------------------------------

configuration WrapReconfModule_cfg of ADT7310_tb is
  for behavior
    for DUT : ADT7310
      for WrapReconfModule
        for MyReconfigLogic_0 : MyReconfigLogic
          for struct
            for all : ParamOutReg
              use entity work.ParamOutReg(rtl)
                port map (
                  Reset_n_i     => '0',
                  Clk_i         => '0',
                  Enable_i      => '0',
                  ParamWrData_i => (others => '0'),
                  Param_o       => open
                );
            end for;
          end for;
        end for;
      end for;
    end for;
  end for;
end WrapReconfModule_cfg;
