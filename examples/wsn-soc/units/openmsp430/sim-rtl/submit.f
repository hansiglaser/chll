//=============================================================================
// Copyright (C) 2001 Authors
//
// This source file may be used and distributed without restriction provided
// that this copyright statement is not removed from the file and that any
// derivative work contains the original copyright notice and the associated
// disclaimer.
//
// This source file is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
//
// This source is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this source; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//-----------------------------------------------------------------------------
// 
// File Name: submit.f
// 
// Author(s):
//             - Olivier Girard,    olgirard@gmail.com
//
//-----------------------------------------------------------------------------
// $Rev: 154 $
// $LastChangedBy: olivier.girard $
// $LastChangedDate: 2012-10-15 22:44:20 +0200 (Mon, 15. Okt 2012) $
//=============================================================================

//=============================================================================
// Testbench related
//=============================================================================

+incdir+../tb/
../tb/tb_openMSP430.v
../tb/ram.v
../tb/io_cell.v
../tb/msp_debug.v


//=============================================================================
// CPU
//=============================================================================

+incdir+../verilog/
-f core.f


//=============================================================================
// Peripherals
//=============================================================================

../verilog/omsp_gpio.v
../verilog/omsp_timerA.v
//../verilog/omsp_uart.v
//../verilog/template_periph_8b.v
//../verilog/template_periph_16b.v
