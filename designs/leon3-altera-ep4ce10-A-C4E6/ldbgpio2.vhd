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
-- Entity: 	ldbgpio2
-- File:	ldbgpio2.vhd
-- Author:	Luc De Busser  luc.debusser@gmail.com
-- Description:	12 bit LED output port for A-C4E6 fpga board ( EP4CE10 version!!! )
------------------------------------------------------------------------------

fout

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

entity ldbgpio2 is
  generic (
    pindex   : integer := 0;
    paddr    : integer := 0;
    pmask    : integer := 16#fff#;
    imask    : integer := 16#0000#;
    nbits    : integer := 8;			-- GPIO bits
    pirq     : integer := 0;
   doutresv : integer := 0
  );
  port (
    rst    : in  std_ulogic;
    clk    : in  std_ulogic;
    apbi   : in  apb_slv_in_type;
    apbo   : out apb_slv_out_type;
    gpioi  : in  gpio_in_type;
    gpioo  : out gpio_out_type
  );
end;

architecture rtl of ldbgpio2 is

constant REVISION : integer := 3;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);


constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+10, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));




type registers is record
  din1  	:  std_logic_vector(nbits-1 downto 0);
  din2  	:  std_logic_vector(nbits-1 downto 0);
  dout   	:  std_logic_vector(nbits-1 downto 0);
end record;

constant nbitszero : std_logic_vector(nbits-1 downto 0) := (others => '0');

constant RESET_ALL : boolean := GRLIB_CONFIG_ARRAY(grlib_sync_reset_enable_all) = 1;
constant RES : registers := (
  din1 => nbitszero, din2 => nbitszero,  -- Sync. regs, not reset
  dout => DOUT_RESVAL(nbits-1 downto 0 ) );


signal r, rin, rout : registers;
signal arst     : std_ulogic;

begin

  
  comb : process(rst, r, apbi, gpioi)
  variable readdata, tmp2, dout, dir, pval, din : std_logic_vector(31 downto 0);
  variable v : registers;
  variable xirq : std_logic_vector(NAHBIRQ-1 downto 0);
  begin

    dout(nbits-1 downto 0) := v.dout(nbits-1 downto 0);


-- write registers

    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => null;
      when "00001" => rout.dout <= apbi.pwdata(nbits-1 downto 0);
      when others => null;
      end case;
    end if;


-- reset operation

    if (not RESET_ALL) and (rst = '0') then
    end if;
    
    rin <= v;

    apbo.prdata <= readdata; 	-- drive apb read bus
    apbo.pirq <= xirq;

      
    gpioo.oen <= "11111111111111111111111111111111";
    gpioo.val <= pval;

-- non filtered input
    gpioo.sig_out <= din;

  end process;


  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;

-- registers

  regs : process(clk, arst)
  begin
    if rising_edge(clk) then
      r <= rin;
      if RESET_ALL and rst = '0' then
        r <= RES;
        -- Sync. registers din1 and din2 not reset
        r.din1 <= rin.din1;
        r.din2 <= rin.din2;
      end if;
    end if;

	gpioo.dout(0) <= rout.dout(0);
	gpioo.dout(1) <= rout.dout(1);
	gpioo.dout(2) <= rout.dout(2);
	gpioo.dout(3) <= rout.dout(3);
	gpioo.dout(4) <= rout.dout(4);
	gpioo.dout(5) <= rout.dout(5);
	gpioo.dout(6) <= rout.dout(6);
	gpioo.dout(7) <= rout.dout(7);
	gpioo.dout(8) <= rout.dout(8);
	gpioo.dout(9) <= rout.dout(9);
	gpioo.dout(10) <= rout.dout(10);
	gpioo.dout(11) <= rout.dout(11);

  end process;

-- boot message

-- pragma translate_off
    bootmsg : report_version
    generic map ("ldbgpio2" & tost(pindex) &
	": " &  tost(nbits) & "-bit GPIO Unit rev " & tost(REVISION));
-- pragma translate_on

end;

