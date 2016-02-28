/***************************************************************************
 *  ADT7410 Simulation Model                                               *
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
 * Register addresses as described in the ADT7410 data sheet, revision A,
 * page 13.
 */
localparam ADT7410_ADDR_TEMP_MSB    = 8'h00;
localparam ADT7410_ADDR_TEMP_LSB    = 8'h01;

localparam ADT7410_ADDR_STATUS      = 8'h02;

localparam ADT7410_ADDR_CONFIG      = 8'h03;

localparam ADT7410_ADDR_THIGH_MSB   = 8'h04;
localparam ADT7410_ADDR_THIGH_LSB   = 8'h05;

localparam ADT7410_ADDR_TLOW_MSB    = 8'h06;
localparam ADT7410_ADDR_TLOW_LSB    = 8'h07;

localparam ADT7410_ADDR_TCRIT_MSB   = 8'h08;
localparam ADT7410_ADDR_TCRIT_LSB   = 8'h09;

localparam ADT7410_ADDR_THYST       = 8'h0A;

localparam ADT7410_ADDR_ID          = 8'h0B;

/*
 * Reset command as described in the ADT7410 data sheet, revision A, page 13.
 */
localparam ADT7410_CMD_RESET        = 8'h2F;

// vim: ff=unix:fenc=utf-8:ft=verilog:tw=0:ts=2:ss=2:sw=2:ai:et:
