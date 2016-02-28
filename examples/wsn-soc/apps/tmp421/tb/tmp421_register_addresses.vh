/***************************************************************************
 *  TMP421 Simulation Model                                                *
 *  Johann Glaser <Johann.Glaser@gmx.at>                                   *
 *  based on Martin Schmölzer's AaDT7410 Simulation Model                   *
 *                                                                         *
 *  AaDT7410 Simulation Model                                               *
 *  Copyright (C) 2013  Martin Schmölzer                                   *
 *  <martin.schmoelzer@student.tuwien.ac.at>                               *
 *                                                                         *
 *  This program is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by   *
 *  the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                    *
 *                                                                         *
 *  This program is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *  GNU General Public License for more details.                           *
 *                                                                         *
 *  You should have received a copy of the GNU General Public License      *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *
 ***************************************************************************/

/*
 * Register addresses as described in the TMP421 data sheet, march 2008,
 * page 12.
 */
localparam TMP421_ADDR_LOCAL_TEMP_MSB    = 8'h00;
localparam TMP421_ADDR_LOCAL_TEMP_LSB    = 8'h10;

localparam TMP421_ADDR_REMOTE_TEMP_MSB   = 8'h01;
localparam TMP421_ADDR_REMOTE_TEMP_LSB   = 8'h11;

localparam TMP421_ADDR_STATUS            = 8'h08;

localparam TMP421_ADDR_CONFIG_1          = 8'h09;
localparam TMP421_ADDR_CONFIG_2          = 8'h0A;

// TODO: more registers

localparam TMP421_ADDR_ID          = 8'hFF;

// vim: ff=unix:fenc=utf-8:ft=verilog:tw=0:ts=2:ss=2:sw=2:ai:et:
