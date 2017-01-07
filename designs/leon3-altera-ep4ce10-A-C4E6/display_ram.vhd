library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity display_ram is
   port(
      clk: in std_logic;
      addr_a: in std_logic_vector(11 downto 0);
      din_a: in std_logic_vector(7 downto 0);
      we : in std_logic;
      addr_b: in std_logic_vector(11 downto 0);
      dout_b: out std_logic_vector(7 downto 0)
   );
end display_ram;

architecture arch of display_ram is
   constant ADDR_WIDTH: integer:=12;
   constant DATA_WIDTH: integer:=8;
   signal addr_reg: std_logic_vector(11 downto 0);
   type ram_type is array (0 to 2047) of std_logic_vector(7 downto 0);
   -- RAM definition
   signal RAM: ram_type;
	attribute ramstyle : string;
	attribute ramstyle of RAM : signal is "Auto";
begin
   -- addr register to infer block RAM
   process (clk)

	variable init_done : std_logic := '0';
	
   begin
      if (clk'event and clk = '1') then
			if ( init_done = '0' ) then
				RAM(0) <= "01000001";
				RAM(1) <= "01000010";
				RAM(2) <= "01000011";
				init_done := '1';
			end if;
			if ( we = '1' ) then
				addr_reg <= addr_a;
				RAM(to_integer(unsigned(addr_reg))) <= din_a ;
			else
				addr_reg <= addr_b;
				dout_b <= RAM(to_integer(unsigned(addr_reg)));
			end if;
      end if;
   end process;
end arch;
