------------------------------------------------------------------------------
--  This file is used with the GRLIB VHDL IP LIBRARY from Cobham Gaisler
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 
-----------------------------------------------------------------------------
-- Entity: 	ldb_led12
-- File:	ldb_led12.vhd
-- Author:	Luc De Busser  luc.debusser@gmail.com
-- Description:	12 bit LED output port for A-C4E6 fpga board ( EP4CE10 version!!! )
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library gaisler;
use gaisler.misc.all;
--pragma translate_off
use std.textio.all;

--pragma translate_on

entity ldb_led12 is
  generic (
    pindex   : integer := 0;
    paddr    : integer := 0;
    pmask    : integer := 16#fff#;
    imask    : integer := 16#0000#;
    nbits    : integer := 12;
    pirq     : integer := 0;
   doutresv : integer := 0
  );
  port (
    rst    : in  std_ulogic;
    clk    : in  std_ulogic;
    apbi   : in  apb_slv_in_type;
    apbo   : out apb_slv_out_type;
    leds12 : out std_logic_vector(11 downto 0)
  );
end;

architecture rtl of ldb_led12 is

constant REVISION : integer := 1;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);


constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+10, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));


signal arst     : std_ulogic;
signal s_leds12 : std_logic_vector(nbits-1 downto 0);
begin

  
reg_access : process(rst, apbi)
  variable readdata: std_logic_vector(31 downto 0);
  begin


-- write registers
    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => s_leds12 <= apbi.pwdata(nbits-1 downto 0);
      when others => null;
      end case;
    end if;

-- read registers
    if (apbi.psel(pindex) and apbi.penable and (not apbi.pwrite)) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => readdata := "00000000000000000000" & s_leds12;
      when others => null;
      end case;
      apbo.prdata <= readdata; 	-- drive apb read bus
    end if;

-- reset operation
    if rst = '0' then
		s_leds12 <= "111111111111";
    end if;
    


  end process;


  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;

  leds12 <= s_leds12;

end;

