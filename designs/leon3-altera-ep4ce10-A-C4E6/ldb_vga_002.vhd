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
signal red : std_logic;
signal green : std_logic;
signal blue : std_logic;
signal o_h_sync : std_logic;
signal o_v_sync : std_logic;
signal o_red : std_logic;
signal o_green : std_logic;
signal o_blue : std_logic;
signal vga_dot_clock : std_logic;
signal tick_counter : unsigned(24 downto 0);
signal x : unsigned(9 downto 0);
signal y : unsigned(9 downto 0);
signal xc : unsigned(2 downto 0);
signal yc : unsigned(3 downto 0);
signal dots_line : std_logic_vector(7 downto 0);

constant REVISION : integer := 1;
constant PIMASK : std_logic_vector(31 downto 0) := '0' & conv_std_logic_vector(imask, 31);
constant DOUT_RESVAL : std_logic_vector(31 downto 0) := conv_std_logic_vector(doutresv, 32);

type char_data_type is array (0 to 15) of std_logic_vector(0 to 7);
constant char_data : char_data_type := ( 
	"00011111",
	"00100001",
	"01000001",
	"01000001",
	"00111111",
	"00100001",
	"01000001",
	"01000001",
	"00100001",
	"00011111",
	"00000000",
	"00000000",
	"00000000",
	"00000000",
	"00000000",
	"00000000" );

constant pconfig : apb_config_type := (
  0 => ahb_device_reg ( VENDOR_GAISLER+100, GAISLER_GPIO+11, 0, REVISION, pirq),
  1 => apb_iobar(paddr, pmask));

begin

	clk_in <= clk;

reg_access : process( rst, apbi )
  variable readdata: std_logic_vector(31 downto 0);
  begin


-- write registers
    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(6 downto 2) is
      when others => null;
      end case;
    end if;

-- read registers
    if (apbi.psel(pindex) and apbi.penable and (not apbi.pwrite)) = '1' then
      case apbi.paddr(6 downto 2) is
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

generate_vga_signals:	
process ( vga_dot_clock )
begin
	if vga_dot_clock='1' then
		o_red <= red;
		o_green <= green;
		o_blue <= blue;
		o_v_sync <= v_sync;
		o_h_sync <= h_sync;
		x <= x + 1;
		xc <= xc + 1;
		if ( x = "1010001011" ) then
			h_sync <= '0' ;
		elsif ( x = "1011101010" ) then
			h_sync <= '1' ;
		elsif ( x = "1100011010" ) then
			h_sync <= '1' ; 
			x <= "0000000000";
			xc <= "000";
			y <= y + 1;
			yc <= yc + 1;
			if ( y = "0111101010" ) then
				v_sync <= '0' ;
			elsif ( y = "0111101100" ) then
				v_sync <= '1' ;
			elsif ( y = "1000001101" ) then
				v_sync <= '1' ;
				y <= "0000000000";
				yc <= "0000";
			end if;
		end if;
		if ( (x>"1001111011") or (y>"0111100000") ) then
			red <= '0';
			blue <= '0';
			green <= '0';
		elsif ( (x>"0100111000") and (x<"0101000010")  and (y>"0011101011") and (y<"0011110101") ) then
			red <= '1';
			blue <= '0';
			green <= '0';
		else 
			red <= '0';
			blue <= '0';
--			green <= '1';
			dots_line <= char_data(to_integer(unsigned(yc)));
			green <= dots_line(7-to_integer(unsigned(xc)));
		end if;
	end if;
end process generate_vga_signals;

test_clock:	
process ( vga_dot_clock )
begin
	if vga_dot_clock='1' then
		if tick_counter="0000000011110100001001000" then -- 400 Hz
			tick_counter <= "0000000000000000000000000";
			--v_sync <= v_sync xor '1';
		else
			tick_counter <= tick_counter + 1;
		end if;
	end if;
end process test_clock;

  apbo.pindex <= pindex;
  apbo.pconfig <= pconfig;

	v_sync_out <= o_v_sync;
	h_sync_out <= o_h_sync;
	r_out <= o_red;
	g_out <= o_green;
	b_out <= o_blue;

		
end hardwired;
