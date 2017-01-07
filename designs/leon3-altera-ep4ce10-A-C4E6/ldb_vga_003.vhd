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
use work.font_rom.all;
use work.ram_2port;

entity ldb_vga is
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
		v_sync_out : out std_logic;
		h_sync_out : out std_logic;
		r_out : out std_logic;
		g_out : out std_logic;
		b_out : out std_logic
		  );
end ldb_vga;


architecture hardwired of ldb_vga is
subtype segments_t is std_logic_vector(7 downto 0);
type bcd_to_7seg_table_type is array (0 to 9) of std_logic_vector(7 downto 0) ;
type segment_data_type is array (0 to 7) of unsigned(7 downto 0);

signal clk_in : std_logic;
signal clk_25_Mhz : std_logic;
signal h_sync : std_logic;
signal v_sync : std_logic;
--signal red : std_logic;
--signal green : std_logic;
--signal blue : std_logic;
signal o_h_sync : std_logic;
signal o_v_sync : std_logic;
signal o_red : std_logic;
signal o_green : std_logic;
signal o_blue : std_logic;
signal vga_dot_clock : std_logic;
--signal x : unsigned(9 downto 0);
--signal y : unsigned(9 downto 0);
--signal xc : unsigned(2 downto 0);
--signal yc : unsigned(3 downto 0);
--signal dots_line : std_logic_vector(7 downto 0);

--type vga_data_type is array (0 to 2399) of unsigned(7 downto 0);
type vga_data_type is array (0 to 100) of unsigned(7 downto 0);
signal vga_data : vga_data_type;
attribute ramstyle : string;
attribute ramstyle of vga_data : signal is "M512";

signal tick_counter : integer := 0;
signal rooster_zichtbaar : std_logic;
signal rooster_zichtbaar_echt : std_logic;
constant REVISION : integer := 1;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);

type char_data_type is array (0 to 15) of std_logic_vector(0 to 7);
constant Message1 : String(1 to 4) := "Luc ";
constant Message2 : String(1 to 3) := "De ";
constant Message3 : String(1 to 6) := "Busser";
constant Message4 : String(1 to 13) := "FPGA VGA test";
constant Message5 : String(1 to 14) := "DJENNAH SAELEN";

--function copy_string_to_vga_data( str : string; p : integer; dp : integer ) return integer is
--variable pp : integer;
--begin
--	pp := p;
--	for i in str'range loop
--		vga_data(pp) <= to_unsigned(character'pos(str(i)), 8);
--		pp := pp + dp;
--	end loop;
--	return pp;
--end function copy_string_to_vga_data;

constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+11, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));

signal vram_addr_a : std_logic_vector(11 downto 0);
signal vram_din_a : std_logic_vector(7 downto 0);
signal vram_wr_clock : std_logic;
signal vram_rd_clock : std_logic;
signal vram_we : std_logic;
signal vram_addr_b : std_logic_vector(11 downto 0);
signal vram_dout_b : std_logic_vector(7 downto 0);

signal aram_addr_a : std_logic_vector(11 downto 0);
signal aram_din_a : std_logic_vector(7 downto 0);
signal aram_wr_clock : std_logic;
signal aram_rd_clock : std_logic;
signal aram_we : std_logic;
signal aram_addr_b : std_logic_vector(11 downto 0);
signal aram_dout_b : std_logic_vector(7 downto 0);

signal vga_enable : std_logic := '0';

signal vram_write_request : std_logic := '0';
signal vram_read_request : std_logic := '0';
shared variable next_vram_wr_clock: std_logic := '0';
shared variable next_vram_rd_clock: std_logic := '0';

begin

vga_ram: ram_2port port map( vram_din_a, vram_addr_b, vram_rd_clock, vram_addr_a, vram_wr_clock, vram_we, vram_dout_b );
attr_ram: ram_2port port map( aram_din_a, vram_addr_b, vram_rd_clock, aram_addr_a, vram_wr_clock, vram_we, aram_dout_b );

	clk_in <= clk;

reg_access : process( rst, apbi )
  variable readdata: std_logic_vector(31 downto 0);
  variable vga_ram_write_address: std_logic_vector(11 downto 0);
  variable vga_ram_write_data: std_logic_vector(7 downto 0);
  variable vga_ram_attr_data: std_logic_vector(7 downto 0);
  
  begin


-- write registers
    if (apbi.psel(pindex) and apbi.penable) = '1' then
      if (apbi.pwrite = '1') then
			case apbi.paddr(6 downto 2) is
			when "00000" => 
				vga_ram_write_address := apbi.pwdata(11 downto 0);
			when "00001" =>
				vga_ram_write_data := apbi.pwdata(7 downto 0);
				vram_addr_a <= vga_ram_write_address;
				vram_din_a <= vga_ram_write_data;
			when "00010" =>
				vga_ram_attr_data := apbi.pwdata(7 downto 0);
				aram_addr_a <= vga_ram_write_address;
				aram_din_a <= vga_ram_attr_data;
			when "00011" =>
				vga_enable <= apbi.pwdata(0);
			when others => null;
			end case;
		end if;
	end if;

-- read registers
    if (apbi.psel(pindex) and apbi.penable and (not apbi.pwrite)) = '1' then
      case apbi.paddr(6 downto 2) is
		when "00000" => readdata := "00000000000000000000" & vga_ram_write_address;
		when "00001" => readdata := "000000000000000000000000" & vga_ram_write_data;
		when others => null;
      end case;
      apbo.prdata <= readdata; 	-- drive apb read bus
    end if;

-- reset operation
    if rst = '0' then
    end if;


  end process;


generate_vga_dot_clock:	
process ( clk_in )
begin
	if clk_in='1' then
		vga_dot_clock <= vga_dot_clock xor '1';
	end if;
end process generate_vga_dot_clock;

generate_vram_signals:	
process ( clk_in )
variable wr_state : std_logic := '0';
variable rd_state : std_logic := '0';
begin
	if clk_in='0' then
		if ( wr_state = '0' ) then
			vram_we <= '1';
			vram_wr_clock <= '1';
		else
			vram_wr_clock <= '0';
		end if;

--		if ( rd_state = '0' ) then
--			vram_rd_clock <= '1';
--		else
--			vram_rd_clock <= '0';
--		end if;
			
		wr_state := not wr_state;
		rd_state := not rd_state;
		
	end if;
end process generate_vram_signals;


blink_counter:	
process ( clk_in )
begin
	if clk_in'event and clk_in='1' then
		if tick_counter=25000000 then
		--if tick_counter="0000011001011011100110101" then
			tick_counter <= 0;
			rooster_zichtbaar <= rooster_zichtbaar xor '1';
		else
			tick_counter <= tick_counter + 1;
		end if;
	end if;
end process blink_counter;

generate_vga_signals:	
process ( vga_dot_clock )
variable x : integer := 0;
variable y : integer := 0;
variable xx : integer := 0;
variable yy : integer := 0;
variable xc : unsigned(2 downto 0);
variable xcc : unsigned(2 downto 0);
variable ycc : unsigned(3 downto 0);
variable yc : unsigned(3 downto 0);
variable ascii_code : unsigned(7 downto 0);
variable ascii_code_12 : unsigned(11 downto 0);
variable dots_line : std_logic_vector(7 downto 0);
variable color : std_logic_vector(2 downto 0);
variable tmp32 : integer;
variable tmp12 : unsigned(11 downto 0);
variable positie : integer;
variable attrib : std_logic_vector(7 downto 0) := "00010100";
variable red : std_logic;
variable green : std_logic;
variable blue : std_logic;
variable xchar : integer ;

begin

	if (vga_dot_clock='1') then
		vram_read_request <= '1';
		

		--positie := copy_string_to_vga_data( Message1, 118, 80 );
		--positie := copy_string_to_vga_data( Message2, positie - 2, 78 );
		--positie := copy_string_to_vga_data( Message3, positie + 4, 80 );
		--positie := copy_string_to_vga_data( Message5, 1, 84+79 );
		--positie := copy_string_to_vga_data( Message4, 29*79+33, 1 );

		if ( (x<632) and (y<480) ) then
			xcc := to_unsigned(x, 3);
			ycc := to_unsigned(y, 4);
			if ( xcc = "000" )then
				xx := x;
				yy := y;
			end if;
		    case xcc is
				when "000" =>
					ascii_code := unsigned( vram_dout_b );
					--ascii_code_12 := unsigned( vram_addr_b and "000001111111" );
					--ascii_code := ascii_code_12( 7 downto 0 );
					attrib := aram_dout_b(7 downto 0);
				when "001" =>
					tmp32 := y / 16;
					tmp32 := tmp32 * 79;
				when "010" =>
					xchar := x/8;
					tmp32 := tmp32 + xchar;
				when "011" =>
					if ( xchar /= 78 ) or ( ycc = 15 ) then
						tmp32 := tmp32 + 1;
					else
						tmp32 := tmp32 - 78;
					end if;
					vram_addr_b <= std_logic_vector(to_unsigned(tmp32, 12));
				when "100" =>
					--if ( tmp32 < 0 ) then
						--tmp32 := tmp32 + 3270;
					--elsif ( tmp32 > 3269 ) then
						--tmp32 := tmp32 - 3270;
					--end if;
					vram_addr_b <= std_logic_vector(to_unsigned(tmp32, 12));
					--vram_addr_b <= std_logic_vector(to_unsigned(48+tmp32,12));
				when "101" =>
					vram_rd_clock <= '1';
					aram_rd_clock <= '0';
				when "111" =>
					vram_rd_clock <= '0';
					aram_rd_clock <= '1';
				when others => null;
			end case;
		end if;
		
		--ascii_code := vga_data( tmp32 );

		-- 
		if ( (x>631) or (y>479) ) then
			red := '0';
			blue := '0';
			green := '0';
		else
			dots_line := char_data(16*to_integer(ascii_code) + to_integer(unsigned(yc)));
			if ( xc = 7 ) and ( rooster_zichtbaar_echt = '1' ) then
				red := '0';
				blue := '0';
				green := '0';	
			elsif ( yc = 15 ) and ( rooster_zichtbaar_echt = '1' )  then
				red := '0';
				blue := '0';
				green := '0';		
			elsif ( dots_line(7-to_integer(unsigned(xc))) = '1' ) then
				-- fore ground color
				red := attrib( 2 );
				blue := attrib( 1 );
				green := attrib( 0 );
			else
				-- back ground color
				red := attrib( 5 );
				blue := attrib( 4 );
				green := attrib( 3 );
			end if;
		end if;
		
		o_red <= red;
		o_green <= green;
		o_blue <= blue;
		o_v_sync <= v_sync;
		o_h_sync <= h_sync;


		x := x + 1;
		xc := xc + 1;
		
		if ( x = 651 ) then
			h_sync <= '0' ;
		elsif ( x = 746 ) then
			h_sync <= '1' ;
		elsif ( x = 794) then
			h_sync <= '1' ; 
			x := 0;
			xc := "000";
			y := y + 1;
			yc := yc + 1;
			if ( y = 490 ) then
				v_sync <= '0' ;
			elsif ( y = 492 ) then
				v_sync <= '1' ;
			elsif ( y = 525 ) then
				v_sync <= '1' ;
				y := 0;
				yc := "0000";
				--rooster_zichtbaar_echt <= rooster_zichtbaar;
				rooster_zichtbaar_echt <= '0';
			end if;
		end if;
	end if;
end process generate_vga_signals;


	  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;	


	v_sync_out <= o_v_sync;
	h_sync_out <= o_h_sync;
	r_out <= o_red;
	g_out <= o_green;
	b_out <= o_blue;

		
end hardwired;
