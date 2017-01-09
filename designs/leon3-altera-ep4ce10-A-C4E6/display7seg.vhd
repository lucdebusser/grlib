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
-- Entity: 	display7seg
-- File:	display7seg.vhd
-- Author:	Luc De Busser  luc.debusser@gmail.com
-- Description:	
-- A-C4E6 board has 8 multiplexed 7-segment displays
-- Interface to LEON3 is through APB : 8 registers, one for each digit
-- Address 0x80000500 : right most digit, value 0 .. 9
-- Address 0x80000504 : ...
-- Address 0x80000508 : ...
-- Address 0x8000050c : ...
-- Address 0x80000500 : ...
-- Address 0x80000504 : ...
-- Address 0x80000508 : ...
-- Address 0x8000050c : left most digit, value 0 .. 9
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library gaisler;
use gaisler.misc.all;

 
entity display7seg is
  generic (
    pindex   : integer := 0;
    paddr    : integer := 0;
    pmask    : integer := 16#fff#;
    imask    : integer := 16#0000#;
    pirq     : integer := 0;
   doutresv : integer := 0
  );
    port ( 
		rst    : in  std_ulogic;
		clk    : in  std_ulogic;
		apbi   : in  apb_slv_in_type;
		apbo   : out apb_slv_out_type;
        led       : out std_logic;
		  clk_50_MHz : in std_logic;
		  beeper_out : out std_logic;
		  segment_out : out std_logic_vector(7 downto 0);
		  col_out : out std_logic_vector(7 downto 0)
		  );
end display7seg;


architecture hardwired of display7seg is
subtype segments_t is std_logic_vector(7 downto 0);
type bcd_to_7seg_table_type is array (0 to 9) of std_logic_vector(7 downto 0) ;
type segment_data_type is array (0 to 7) of unsigned(7 downto 0);
signal led_value : std_logic;
signal tick_counter : unsigned(24 downto 0);
signal tick_counter_7seg : unsigned(24 downto 0);
signal tick_counter_incrementer : unsigned(24 downto 0);
signal clk_in : std_logic;
signal beeper : std_logic;
signal segment : std_logic_vector(7 downto 0) ;
signal col : std_logic_vector(7 downto 0) := "00000001";
signal col_nr : integer range 0 to 7;
signal segment_data : segment_data_type := ( "00000000", "00000001", "00000010", "00000011",
                                              "00000100", "00000101", "00000110", "00000111");

constant REVISION : integer := 1;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);

constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+11, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));


function bcd_to_7segment( bcd : integer range 0 to 9 ) return segments_t is
variable rv : segments_t := "00000000";
begin
	case bcd is
		when 0 => rv := "00111111";
		when 1 => rv := "00000110";
		when 2 => rv := "01011011";
		when 3 => rv := "01001111";
		when 4 => rv := "01100110";
		when 5 => rv := "01101101";
		when 6 => rv := "01111101";
		when 7 => rv := "00000111";
		when 8 => rv := "01111111";
		when 9 => rv := "01101111";
		when others => rv := "00000000";
	end case;
	return rv;
end function bcd_to_7segment;

begin

	clk_in <= clk_50_MHz;
	beeper <= '0';
	segment <= "00000111";
	
blink_counter:	
process ( clk_in )
begin
	if clk_in'event and clk_in='1' then
		if tick_counter="1011111010111100001000000" then
			tick_counter <= "0000000000000000000000000";
			led_value <= led_value xor '1';
		else
			tick_counter <= tick_counter + 1;
		end if;
	end if;
end process blink_counter;

led_7seg_multiplexer:	
process ( clk_in )
begin
	if clk_in'event and clk_in='1' then
		if tick_counter_7seg="0000000011110100001001000" then -- 400 Hz
			tick_counter_7seg <= "0000000000000000000000000";
			if col="10000000" then
				col <= "00000001";
				col_nr <= 0;
			else
				col <= col(6 downto 0) & '0';
				col_nr <= col_nr + 1;
			end if;
		else
			tick_counter_7seg <= tick_counter_7seg + 1;
		end if;
	end if;
end process led_7seg_multiplexer;

reg_access : process( rst, apbi )
  variable readdata: std_logic_vector(31 downto 0);
  begin


-- write registers
    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => segment_data(0) <= unsigned(apbi.pwdata(7 downto 0));
      when "00001" => segment_data(1) <= unsigned(apbi.pwdata(7 downto 0));
      when "00010" => segment_data(2) <= unsigned(apbi.pwdata(7 downto 0));
      when "00011" => segment_data(3) <= unsigned(apbi.pwdata(7 downto 0));
      when "00100" => segment_data(4) <= unsigned(apbi.pwdata(7 downto 0));
      when "00101" => segment_data(5) <= unsigned(apbi.pwdata(7 downto 0));
      when "00110" => segment_data(6) <= unsigned(apbi.pwdata(7 downto 0));
      when "00111" => segment_data(7) <= unsigned(apbi.pwdata(7 downto 0));
      when others => null;
      end case;
    end if;

-- read registers
    if (apbi.psel(pindex) and apbi.penable and (not apbi.pwrite)) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(0));
      when "00001" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(1));
      when "00010" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(2));
      when "00011" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(3));
      when "00100" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(4));
      when "00101" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(5));
      when "00110" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(6));
      when "00111" => readdata := "000000000000000000000000" & std_logic_vector(segment_data(7));
      when others => null;
      end case;
      apbo.prdata <= readdata; 	-- drive apb read bus
    end if;

-- reset operation
    if rst = '0' then
		segment_data(0) <= "00000000";
    end if;


  end process;


  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;


led <= led_value;
beeper_out <= beeper;
col_out <= not col;
segment_out <= not bcd_to_7segment(to_integer(segment_data(col_nr)));
		
end hardwired;
