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

/**
 * ADT7410 simulation model
 *
 * Warning: simulation model is INCOMPLETE. Currently, there is only very
 * basic functioniality (Reading and writing ADT7410 registers).
 *
 * 16-bit writes are not supported.
 *
 * The temperature value register always reads the value 0x1234.
 *
 * INT/CT pins have no functionality.
 */
module adt7410_model (
  /** I2C Serial Clock Line */
  input  wire        scl_i,

  /**< I2C Slave Data Line */
  inout  wire        sda_io,

  /** I2C bus address selection */
  input  wire  [1:0] i2c_addr_i,

  /** Overtemperature/Undertemperatur Indicator */
  output wire        int_o,

  /** Critical Overtemperature Indicator */
  output wire        ct_o,

  /** Sensor value */
  input  wire [15:0] temp_i
);

/**
 * ADT7410 simulation model instance name
 *
 * Included with text output, can be used to identify the ADT7410 instance.
 */
parameter   INSTANCE_NAME = "ADT7410";

`include "adt7410_register_addresses.vh"

/***************************************************************************
 * Static I2C Address Part                                                 *
 *                                                                         *
 * The ADT7410 has a 7-bit I2C address where the two least significant     *
 * bits (1:0) are configurable. Bits 6:2 are hard-wired to a constant      *
 * value. See the ADT7410 data sheet, page 17 for further information.     *
 ***************************************************************************/
localparam ADT7410_I2C_ADDRESS_MSB = 5'b10010;

/***************************************************************************
 * Address Pointer                                                         *
 *                                                                         *
 * Selects a register for the next read or write transaction.              *
 ***************************************************************************/
reg   [7:0] addr_ptr;

/***************************************************************************
 * Temperature Register                                                    *
 *                                                                         *
 * If the ADT7410's ADC is configured to convert the temperature to a      *
 * 13-bit value, the three least significant bits are flag bits:           *
 *   Bit 0 - T_LOW flag (temperature is lower than the value in the T_LOW  *
 *           setpoint register                                             *
 *   Bit 1 - T_HIGH flag (temperature is higher than the value in the      *
 *           T_HIGH setpoint register.                                     *
 *   Bit 2 - T_CRIT flag (temperature is higher than the value in the      *
 *           T_CRIT setpoint register.                                     *
 *                                                                         *
 * If the ADT is configured to convert the temperature to a 16-bit value,  *
 * the three least significant bits are used as data bits.                 *
 ***************************************************************************/
reg  [15:0] temp_reg;

/***************************************************************************
 * Status Register                                                         *
 *                                                                         *
 * Read-only, reflects the status of the over- and undertemperature        *
 * interrupts as well as the status of the temperature conversion process. *
 ***************************************************************************/
reg   [7:0] status_reg;

/***************************************************************************
 * Configuration Register                                                  *
 *                                                                         *
 * Bit 1:0 - Fault Queue (number of over- or undertemperature faults that  *
 *           can occur before the INT and CT pins are set.                 *
 * Bit 2   - CT pin polarity ('0' - active low, '1' - active high)         *
 * Bit 3   - INT pin polarity ('0' - active low, '1' - active high)        *
 * Bit 4   - INT/CT mode ('0' - interrupt mode, '1' - comparator mode)     *
 * Bit 6:5 - Operation Mode:                                               *
 *             '00' - continuous conversion (default mode)                 *
 *             '01' - one shot mode                                        *
 *             '10' - 1 SPS mode                                           *
 *             '11' - shutdown mode                                        *
 * Bit 7   - ADC resolution:                                               *
 *             '0' - 13-bit resolution (sign bit + 12 data bits)           *
 *             '1' - 16-bit resolution (sign bit + 15 data bits)           *
 ***************************************************************************/
reg   [7:0] config_reg;

/***************************************************************************
 * T_HIGH, T_LOW and T_CRIT Setpoint Registers                             *
 ***************************************************************************/
reg  [15:0] t_high_setpoint_reg;
reg  [15:0] t_low_setpoint_reg;
reg  [15:0] t_crit_setpoint_reg;

/***************************************************************************
 * T_HYST Setpoint Register                                                *
 ***************************************************************************/
reg   [7:0] t_hyst_setpoint_reg;

/***************************************************************************
 * ID Register                                                             *
 *                                                                         *
 * Read-only, constant value 8'hC4.                                        *
 ***************************************************************************/
localparam ADT7410_ID = 8'hC4;

reg         sda_en; /* SDA output enable */
reg         rw_n;

reg   [7:0] temp;

reg         ret, ack, stop;
integer     i;

/* ADT7410 simulation model state */
reg         state;
localparam  STATE_ADDR = 1'b0;
localparam  STATE_DATA = 1'b1;

assign sda_io = sda_en ? 1'b0 : 1'bZ;

/**
 * Wait for I2C start condition
 *
 * A start condition is defined as a falling edge on the SDA line while SCL
 * is at high level.
 */
task i2c_wait_for_start_condition;
reg           done;
begin
  done = 1'b0;

  while (done != 1'b1) begin
    @(negedge sda_io);

    if (scl_i == 1'b1)
      done = 1'b1;
  end

  $display("%g - %s: start condition detected", $time, INSTANCE_NAME);
end
endtask

/**
 * Wait for I2C stop condition
 *
 * A stop condition is defined as a rising edge on the SDA line while SCL
 * is at high level.
 */
task i2c_wait_for_stop_condition;
reg           done;
begin
  done = 1'b0;

  while (done != 1'b1) begin
    @(posedge sda_io);

    if (scl_i == 1'b1)
      done = 1'b1;
  end

  $display("%g - %s: stop condition detected", $time, INSTANCE_NAME);
end
endtask

/**
 * Transmit a single bit over the I2C bus, checking for arbitration failures.
 *
 * @return 1'b0 on success.
 * @return 1'b1 if arbitration was lost (another device holds SDA low while we
 *  attempt to transmit a high value).
 */
task i2c_transmit_bit;
input         bit;
output        ret;
begin
  /* Pull down or release SDA at falling edge of SCL */
  @(negedge scl_i);
  //$display("%g - %s: i2c_transmit_bit: after negedge scl_i", $time, INSTANCE_NAME);
  case (bit)
    1'b0: sda_en = 1'b1;
    1'b1: sda_en = 1'b0;
    default:
      begin
        $display("%g - %s: i2c_transmit_bit: fatal error - invalid data bit", $time, INSTANCE_NAME);
        $stop;
      end
  endcase

  /* Compare actual SDA value at rising edge of SCL */
  @(posedge scl_i);
  //$display("%g - %s: i2c_transmit_bit: after posedge scl_i", $time, INSTANCE_NAME);
  if (sda_io != bit) begin
    ret = 1'b1;
    $display("%g - %s: i2c_transmit_bit: arbitration lost", $time, INSTANCE_NAME);
  end
  else
    ret = 1'b0;
end
endtask

/**
 * Receive a single bit from the I2C bus.
 */
task i2c_receive_bit;
output        bit;
output        stop;   // 1'b1: there was a stop condition instead of a bit
begin
  stop = 1'b0;

  @(negedge scl_i, posedge sda_io) sda_en = 1'b0;
  if (scl_i != 1'b0) begin
    /* Stop condition received */
    //$display("%g - %s: Stop condition received: SCL = %b, SDA = %b", $time, INSTANCE_NAME, scl_i, sda_io);
    stop = 1'b1;
  end else begin
    /* negedge scl_i --> data bit */
    @(posedge scl_i) bit = sda_io;
    //$display("%g - %s: received bit %b", $time, INSTANCE_NAME, bit);
  end
end
endtask

/**
 * Receive address and !R/W bit from I2C bus, generate ACK/NACK for address
 */
task i2c_receive_address;
output        rw_n;
reg     [6:0] slave_address;
integer       i;
reg 	      stop;  
begin
  sda_en = 1'b0;

  /* Address is transmitted MSB first */
  for (i = 6; i >= 0; i = i - 1)
    i2c_receive_bit(slave_address[i],stop);

  $display("%g - %s: Address 0x%02h", $time, INSTANCE_NAME, slave_address);

  /* !R/W bit is transmitted last */
  i2c_receive_bit(rw_n,stop);

  /* Address ACK/NACK */
  if (slave_address == {ADT7410_I2C_ADDRESS_MSB, i2c_addr_i})
  begin
    //$display("%g - %s: We are accessed, sending ACK", $time, INSTANCE_NAME);
    i2c_transmit_bit(1'b0, ret);
  end else begin
    i2c_transmit_bit(1'b1, ret);
  end
end
endtask

/**
 * Receive a single data byte from I2C bus ("slave receive mode")
 */
task i2c_slave_receive;
output  [7:0] data;
output        ret;
output 	      stop;
integer       i;
begin
  stop = 1'b0;

  /* Receive data bits */
  for (i = 7; (i >= 0) && (stop == 1'b0); i = i - 1) begin
    i2c_receive_bit(data[i],stop);
    //$display("%g - received data bit %i", $time, i);
  end

  /* Generate ACK bit */
  if (stop == 1'b0)
    i2c_transmit_bit(1'b0, ret);
end
endtask

/**
 * Transmit a single data byte over I2C bus ("slave transmit mode")
 */
task i2c_slave_transmit;
input   [7:0] data;
output        ack;
reg           ret;
integer       i;
reg 	      stop;
begin
  /* Transmit data bits */
  for (i = 7; i >= 0; i = i - 1) begin
    i2c_transmit_bit(data[i], ret);
    if (ret != 1'b0)
      $stop;
  end

  /* Listen for ACK/NACK bit */
  i2c_receive_bit(ack, stop);
end
endtask

/**
 * Determine whether an address belongs to the most significant byte of a
 * 16-bit register.
 */
function adt7410_reg_is_16bit;
input    [7:0] addr;
begin
  case (addr)
    ADT7410_ADDR_TEMP_MSB:  adt7410_reg_is_16bit = 1'b1;
    ADT7410_ADDR_THIGH_MSB: adt7410_reg_is_16bit = 1'b1;
    ADT7410_ADDR_TLOW_MSB:  adt7410_reg_is_16bit = 1'b1;
    ADT7410_ADDR_TCRIT_MSB: adt7410_reg_is_16bit = 1'b1;
    default:                adt7410_reg_is_16bit = 1'b0;
  endcase
end
endfunction

/**
 * Read from an 8-bit register.
 */
task adt7410_read_register_byte;
input    [7:0] addr;
output   [7:0] reg_value;
begin
  case (addr)
      ADT7410_ADDR_TEMP_MSB:  reg_value = temp_reg[15:8];
      ADT7410_ADDR_TEMP_LSB:  reg_value = temp_reg[ 7:0];

      ADT7410_ADDR_STATUS:    reg_value = status_reg;

      ADT7410_ADDR_CONFIG:    reg_value = config_reg;

      ADT7410_ADDR_THIGH_MSB: reg_value = t_high_setpoint_reg[15:8];
      ADT7410_ADDR_THIGH_LSB: reg_value = t_high_setpoint_reg[ 7:0];

      ADT7410_ADDR_TLOW_MSB:  reg_value = t_low_setpoint_reg[15:8];
      ADT7410_ADDR_TLOW_LSB:  reg_value = t_low_setpoint_reg[ 7:0];

      ADT7410_ADDR_TCRIT_MSB: reg_value = t_crit_setpoint_reg[15:8];
      ADT7410_ADDR_TCRIT_LSB: reg_value = t_crit_setpoint_reg[ 7:0];

      ADT7410_ADDR_THYST:     reg_value = t_hyst_setpoint_reg;

      ADT7410_ADDR_ID:        reg_value = ADT7410_ID;

      default:                reg_value = 8'h00;
  endcase
end
endtask

/**
 * Write to an 8-bit register.
 */
task adt7410_write_register_byte;
input   [7:0] addr;
input   [7:0] data;
begin
  case(addr)
    ADT7410_ADDR_TEMP_MSB:  temp_reg[15:8] <= data;
    ADT7410_ADDR_TEMP_LSB:  temp_reg[ 7:0] <= data;

    ADT7410_ADDR_STATUS:    status_reg <= data;

    ADT7410_ADDR_CONFIG:    config_reg <= data;

    ADT7410_ADDR_THIGH_MSB: t_high_setpoint_reg[15:8] <= data;
    ADT7410_ADDR_THIGH_LSB: t_high_setpoint_reg[ 7:0] <= data;

    ADT7410_ADDR_TLOW_MSB:  t_low_setpoint_reg[15:8] <= data;
    ADT7410_ADDR_TLOW_LSB:  t_low_setpoint_reg[ 7:0] <= data;

    ADT7410_ADDR_TCRIT_MSB: t_crit_setpoint_reg[15:8] <= data;
    ADT7410_ADDR_TCRIT_LSB: t_crit_setpoint_reg[ 7:0] <= data;

    ADT7410_ADDR_THYST:     t_hyst_setpoint_reg <= data;

    ADT7410_ADDR_ID:        /* do nothing */ ;

    default:                /* do nothing */ ;
  endcase
end
endtask

always @(temp_i)
begin
  temp_reg = temp_i;
end

initial begin
  state               = STATE_ADDR;

  /* Default values (see ADT7410 data sheet, page 13) */
  addr_ptr            = 0/*8'h0b*/;
  temp_reg            = /*0*/16'h1234;
  status_reg          = 0;
  config_reg          = 0;
  t_high_setpoint_reg = 16'h2000;
  t_low_setpoint_reg  = 16'h0500;
  t_crit_setpoint_reg = 16'h4980;
  t_hyst_setpoint_reg = 8'h05;

  temp     = 0;

  sda_en   = 1'b0;
  rw_n     = 1'b0;

  while (1) begin
    /* Wait for start condition */
    i2c_wait_for_start_condition();

    i2c_receive_address(rw_n);

    $display("%g - %s: after i2c_receive_address, state = ", $time, INSTANCE_NAME, state);

    if (rw_n == 1'b0) begin
      /* write access --> first byte is address pointer */
      $display("%g - %s: write access, first byte is address pointer", $time, INSTANCE_NAME);
      i2c_slave_receive(addr_ptr, ret, stop);
      $display("%g - %s: received address pointer %d", $time, INSTANCE_NAME, addr_ptr);
      /* following bytes are data bytes, receive until a stop condition */
      stop = 1'b0;
      while (stop == 1'b0) begin      
        i2c_slave_receive(temp, ret, stop);
        $display("%g - %s: stop = %b", $time, INSTANCE_NAME, stop);
	if (stop == 1'b0) begin
          $display("%g - %s: writing value %d to address %d", $time, INSTANCE_NAME, temp, addr_ptr);
          adt7410_write_register_byte(addr_ptr, temp);
          addr_ptr = addr_ptr + 1;
	end
      end
      $display("%g - %s: finished write access, new address pointer = %d", $time, INSTANCE_NAME, addr_ptr);
    end
    else if (rw_n == 1'b1) begin
      /* write access --> first byte is address pointer */
      $display("%g - %s: read access, current address pointer is %d", $time, INSTANCE_NAME, addr_ptr);
      ack = 1'b0;
      while (ack == 1'b0) begin
        adt7410_read_register_byte(addr_ptr, temp);
        $display("%g - %s: reading value %d at address %d", $time, INSTANCE_NAME, temp, addr_ptr);
        i2c_slave_transmit(temp, ack);
        $display("%g - %s: ack = %b", $time, INSTANCE_NAME, ack);
        addr_ptr = addr_ptr + 1;
      end
      $display("%g - %s: finished read access, new address pointer = %d", $time, INSTANCE_NAME, addr_ptr);
    end
     
  end
end

endmodule

// vim: ff=unix:fenc=utf-8:ft=verilog:tw=0:ts=2:ss=2:sw=2:ai:et:
