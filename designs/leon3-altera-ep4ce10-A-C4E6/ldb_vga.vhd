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
use work.ram_2port;
use work.pll;
use work.from;

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

signal o_vram_clock : std_logic;
signal o_vram_addr : std_logic_vector(11 downto 0) ;

signal enable_double_width : std_logic := '0';

--signal x : unsigned(9 downto 0);
--signal y : unsigned(9 downto 0);
--signal xc : unsigned(2 downto 0);
--signal yc : unsigned(3 downto 0);
--signal dots_line : std_logic_vector(7 downto 0);



signal tick_counter : integer := 0;
signal rooster_zichtbaar : std_logic;
signal rooster_zichtbaar_echt : std_logic;
constant REVISION : integer := 1;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);

constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+11, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));

signal rst_gnd : std_logic := '0';
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

signal rom_address : std_logic_vector(10 downto 0);
signal rom_data : std_logic_vector(7 downto 0);
signal rom_rd_clock : std_logic;

begin

vga_ram: ram_2port port map( vram_din_a, vram_addr_b, vram_rd_clock, vram_addr_a, vram_wr_clock, vram_we, vram_dout_b );
attr_ram: ram_2port port map( aram_din_a, aram_addr_b, aram_rd_clock, aram_addr_a, aram_wr_clock, aram_we, aram_dout_b );
pll_vga_dot_clock : pll port map ( rst_gnd, clk_in, vga_dot_clock );
font_rom : from port map(address=>rom_address,clock=>rom_rd_clock,  q=>rom_data);
 
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


--generate_vga_dot_clock:	
--process ( clk_in )
--begin
--	if clk_in='1' then
--		vga_dot_clock <= vga_dot_clock xor '1';
--	end if;
--end process generate_vga_dot_clock;

generate_vram_signals:	
process ( clk_in )
variable wr_state : std_logic := '0';
variable rd_state : std_logic := '0';
begin
	if rising_edge(clk_in) then
			if ( wr_state = '0' ) then
				vram_we <= '1';
				vram_wr_clock <= '1';
				aram_we <= '1';
				aram_wr_clock <= '1';
			else
				vram_wr_clock <= '0';
				aram_wr_clock <= '0';
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

variable xx : std_logic_vector(11 downto 0) ;
variable yy : std_logic_vector(11 downto 0) ;
variable xcc : std_logic_vector(2 downto 0) ;
variable ycc : std_logic_vector(3 downto 0);
variable index : std_logic_vector(11 downto 0) ;
variable ascii_code : std_logic_vector(7 downto 0);
variable dots_line : std_logic_vector(7 downto 0);
variable color : std_logic_vector(2 downto 0);
variable tmp14 : std_logic_vector(13 downto 0);
variable tmp6 : std_logic_vector(5 downto 0);
variable positie : integer;
variable attrib : std_logic_vector(7 downto 0) ;
variable red : std_logic;
variable green : std_logic;
variable blue : std_logic;
variable xchar : std_logic_vector(7 downto 0);
variable vram_addr : std_logic_vector(11 downto 0);
variable vram_clock : std_logic;
variable x : std_logic_vector(11 downto 0) ;
variable y : std_logic_vector(11 downto 0) ;
variable tmp12 : std_logic_vector(11 downto 0);
variable blink : std_logic;
variable dot : std_logic;


--constant characters_per_line : std_logic_vector(7 downto 0) := std_logic_vector( to_unsigned(79,8) );
constant characters_per_line : std_logic_vector(7 downto 0) := std_logic_vector( to_unsigned(80,8) );

--constant horizontal_visible_area : std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(632,12) );
--constant horizontal_front_porch :  std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(19,12) );
--constant horizontal_sync_pulse :   std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(95,12) );
--constant horizontal_back_porch :   std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(48,12) );

constant horizontal_visible_area : std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(640,12) );
constant horizontal_front_porch :  std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(16,12) );
constant horizontal_sync_pulse :   std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(96,12) );
constant horizontal_back_porch :   std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(48,12) );

constant vertical_visible_area :   std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(480,12) );
constant vertical_front_porch :    std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(10,12) );
constant vertical_sync_pulse :    std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(2,12) );
constant vertical_back_porch :    std_logic_vector(11 downto 0) := std_logic_vector( to_unsigned(33,12) );

begin

	if rising_edge(vga_dot_clock) then

		if ( (x<horizontal_visible_area) and (y<vertical_visible_area) ) then
	
			xcc := x(2 downto 0);
			if ( enable_double_width = '0' ) then
				ycc := y(3 downto 0);
			else
				ycc := y(4 downto 1);
			end if;
			
		    case xcc is
				when "000" =>				
					--dots_line :=char_data( to_integer(unsigned(index)) );
					dots_line := rom_data;
					attrib := aram_dout_b(7 downto 0);		
					if ( enable_double_width = '0' )	then	
						tmp6 := y(9 downto 4); --  yy/16
					else
						tmp6 := y(10 downto 5); --  yy/32
					end if;
					tmp14 := tmp6 * characters_per_line;
					xchar := x(10 downto 3); -- xx/8
					if ( (xchar = (characters_per_line-1)) and (ycc /= X"F") ) then
						vram_addr := tmp14(11 downto 0) + xchar - (characters_per_line-1);
					else
						vram_addr := tmp14(11 downto 0) + xchar + 1;					
					end if;
				when "001" =>
					vram_clock := '1';
				when "010" =>
					vram_clock := '0';
				when "011" =>
					vram_clock := '1';
				when "100" =>
					vram_clock := '0';
				when "101" =>
					ascii_code := vram_dout_b;
					index := (ascii_code & "0000") + ycc;
					rom_address <= index(10 downto 0);
				when "110" =>
					rom_rd_clock <= '1';
				when "111" =>
					rom_rd_clock <= '0';
				when others => null;
			end case;

			blink := attrib( 7 ) and rooster_zichtbaar;
			dot := dots_line(7-to_integer(unsigned(xcc))) xor blink;
			if dot = '1' then
				-- fore ground color
				red := attrib( 2 );
				green := attrib( 1 );
				blue := attrib( 0 );
			else
				-- back ground color
				red := attrib( 5 );
				green := attrib( 4 );
				blue := attrib( 3 );
			end if;
			
		else
			red := '0';
			blue := '0';
			green := '0';
		end if;

		o_vram_addr <= vram_addr;
		o_vram_clock <= vram_clock;

		o_red <= red;
		o_green <= green;
		o_blue <= blue;
		o_v_sync <= v_sync;
		o_h_sync <= h_sync;

		x := x + 1;
		case x is
			when horizontal_visible_area + horizontal_front_porch =>
				h_sync <= '0' ;
			when horizontal_visible_area + horizontal_front_porch + horizontal_sync_pulse => -- 746
				h_sync <= '1' ;
			when horizontal_visible_area + horizontal_front_porch + horizontal_sync_pulse + horizontal_back_porch => -- 794
				h_sync <= '1' ;
				x := X"000";
				y := y + 1;
				case y is
					when vertical_visible_area + vertical_front_porch => -- 490
						v_sync <='0';
					when vertical_visible_area + vertical_front_porch + vertical_sync_pulse => -- 492
						v_sync <='1';
					when vertical_visible_area + vertical_front_porch + vertical_sync_pulse + vertical_back_porch => -- 525
						v_sync <='1';
						y := X"000";
					when others => null;
				end case;
			when others => null;
		end case;
		
	end if;
	
end process generate_vga_signals;


	  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;	


	v_sync_out <= o_v_sync;
	h_sync_out <= o_h_sync;
	r_out <= o_red;
	g_out <= o_green;
	b_out <= o_blue;

	vram_addr_b <= o_vram_addr;
	aram_addr_b <= o_vram_addr;
	
	vram_rd_clock <= o_vram_clock;
	aram_rd_clock <= o_vram_clock;
		
end hardwired;
