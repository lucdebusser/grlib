------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2003 - 2008, Gaisler Research
--  Copyright (C) 2008 - 2014, Aeroflex Gaisler
--  Copyright (C) 2015 - 2016, Cobham Gaisler
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
-- Entity: 	ldbgpio
-- File:	ldbgpio.vhd
-- Author:	Jiri Gaisler - Gaisler Research
-- Description:	Scalable general-purpose I/O port
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

entity ldbgpio is
  generic (
    pindex   : integer := 0;
    paddr    : integer := 0;
    pmask    : integer := 16#fff#;
    imask    : integer := 16#0000#;
    nbits    : integer := 8;			-- GPIO bits
    oepol    : integer := 0;                    -- Output enable polarity
    syncrst  : integer := 0;                    -- Only synchronous reset
    bypass   : integer := 16#0000#;
    scantest : integer := 0;
    bpdir    : integer := 16#0000#;
    pirq     : integer := 0;
    irqgen   : integer := 0;
    iflagreg : integer range 0 to 1 := 0;
    bpmode   : integer range 0 to 1 := 0;
    inpen    : integer range 0 to 1 := 0;
    doutresv : integer := 0;
    dirresv  : integer := 0;
    bpresv   : integer := 0;
    inpresv  : integer := 0;
    pulse    : integer := 0
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

architecture rtl of ldbgpio is

constant REVISION : integer := 3;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);

constant BPMASK : std_logic_vector(31 downto 0) := conv_std_logic_vector(bypass, 32);
constant BPDIRM  : std_logic_vector(31 downto 0) := conv_std_logic_vector(bpdir, 32);

constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);
constant DIR_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(dirresv, 32);
constant BP_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(bpresv, 32);
constant INPEN_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(inpresv, 32);

constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+10, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));

-- Prevent tools from issuing index errors for unused code
function calc_nirqmux return integer is
begin
  if irqgen = 0 then return 1; end if;
  return irqgen;
end;

constant NIRQMUX : integer := calc_nirqmux;

subtype irqmap_type is std_logic_vector(log2x(NIRQMUX)-1 downto 0);

type irqmap_array_type is array (natural range <>) of irqmap_type;

type registers is record
  din1  	:  std_logic_vector(nbits-1 downto 0);
  din2  	:  std_logic_vector(nbits-1 downto 0);
  dout   	:  std_logic_vector(nbits-1 downto 0);
  imask  	:  std_logic_vector(nbits-1 downto 0);
  level  	:  std_logic_vector(nbits-1 downto 0);
  edge   	:  std_logic_vector(nbits-1 downto 0);
  ilat   	:  std_logic_vector(nbits-1 downto 0);
  dir    	:  std_logic_vector(nbits-1 downto 0);
  bypass        :  std_logic_vector(nbits-1 downto 0);
  irqmap        :  irqmap_array_type(nbits-1 downto 0);
  iflag         :  std_logic_vector(nbits-1 downto 0);
  inpen         :  std_logic_vector(nbits-1 downto 0);
  pulse         :  std_logic_vector(nbits-1 downto 0);
end record;

constant nbitszero : std_logic_vector(nbits-1 downto 0) := (others => '0');
constant irqmapzero : irqmap_array_type(nbits-1 downto 0) := (others => (others => '0'));

constant RESET_ALL : boolean := GRLIB_CONFIG_ARRAY(grlib_sync_reset_enable_all) = 1;
constant RES : registers := (
  din1 => nbitszero, din2 => nbitszero,  -- Sync. regs, not reset
  dout => DOUT_RESVAL(nbits-1 downto 0), imask => nbitszero, level => nbitszero, edge => nbitszero,
  ilat => nbitszero, dir => DIR_RESVAL(nbits-1 downto 0), bypass => BP_RESVAL(nbits-1 downto 0), irqmap => irqmapzero,
  iflag => nbitszero, inpen => INPEN_RESVAL(nbits-1 downto 0),
  pulse => nbitszero);


signal r, rin, rout : registers;
signal arst     : std_ulogic;

begin

  arst <= apbi.testrst when (scantest = 1) and (apbi.testen = '1') else rst;
  
  
  comb : process(rst, r, apbi, gpioi)
  variable readdata, tmp2, dout, dir, pval, din : std_logic_vector(31 downto 0);
  variable v : registers;
  variable xirq : std_logic_vector(NAHBIRQ-1 downto 0);
  begin

    if (syncrst = 1) and (rst = '0') then
      dir(nbits-1 downto 0) := DIR_RESVAL(nbits-1 downto 0);
    end if;
    dout(nbits-1 downto 0) := v.dout(nbits-1 downto 0);


-- write registers

    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(6 downto 2) is
      when "00000" => null;
      when "00001" => rout.dout <= apbi.pwdata(nbits-1 downto 0);
      when "00010" => v.dir := apbi.pwdata(nbits-1 downto 0);
      when others => v.dir := apbi.pwdata(nbits-1 downto 0);
      end case;
    end if;


-- reset operation

    if (not RESET_ALL) and (rst = '0') then
      v.imask := RES.imask; v.bypass := RES.bypass;
      v.dir := RES.dir; v.dout := RES.dout;
      v.irqmap := RES.irqmap;
      if iflagreg /= 0 then
        v.iflag := RES.iflag;
      end if;
      if inpen /= 0 then
        v.inpen := RES.inpen;
      end if;
      if pulse /= 0 then
        v.pulse := RES.pulse;
      end if;
    end if;
    
    rin <= v;

    apbo.prdata <= readdata; 	-- drive apb read bus
    apbo.pirq <= xirq;

    if (scantest = 1) and (apbi.testen = '1') then
      dir := (others => apbi.testoen);
      if oepol = 0 then dir := not dir; end if;
    elsif (syncrst = 1 ) and (rst = '0') then
      dir := (others => '0');
    end if;
      
    gpioo.oen <= dir;
    if oepol = 0 then gpioo.oen <= not dir; end if;
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
    if (syncrst = 0 ) and (arst = '0') then
      r.dir <= DIR_RESVAL(nbits-1 downto 0);
      r.dout <= DOUT_RESVAL(nbits-1 downto 0);
      if bypass /= 0 then
        r.bypass <= BP_RESVAL(nbits-1 downto 0);
      end if;
      if inpen /= 0 then
        r.inpen <= INPEN_RESVAL(nbits-1 downto 0);
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
  end process;

-- boot message

-- pragma translate_off
    bootmsg : report_version
    generic map ("ldbgpio" & tost(pindex) &
	": " &  tost(nbits) & "-bit GPIO Unit rev " & tost(REVISION));
-- pragma translate_on

end;

