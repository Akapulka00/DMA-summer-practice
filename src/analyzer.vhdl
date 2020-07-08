library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 

entity analyzer is
    port (
      clk : in std_logic;
      rst : in std_logic;

      -- interface with buffer --
      data_in : in std_logic_vector(31 downto 0); -- bus with buffer --
      serv_info : in std_logic_vector(0 to 1); -- first bit flag end package, second bit EOF/EEP
      num_byte : in std_logic_vector(1 downto 0); -- [0-3] number byte, which is an end of package in word --
      valid_in : in std_logic; -- report from buffer about valid of data --
      ready : out std_logic; -- i ready of read word from buffer --

      --interface with writer
      data_out : out std_logic_vector(31 downto 0); -- bus with writer could transfer data or descriptor --
      ready_wrt : in std_logic;  -- writer ready for write data/desc --
      valid_data : out std_logic; -- report to writer about valid of data --
      valid_desc : out std_logic -- report to writer about valid of desc --
    );
end analyzer ;

architecture arch of analyzer is
begin
  
  main : process( clk, rst )
    variable desc : std_logic_vector(63 downto 0); -- local register for descriptor --
    variable cnt : std_logic_vector(62 downto 0); -- local register for counter --
    variable flag_desc : std_logic; -- flag '0' - descriptor is not ready, '1' - descriptor is ready 

    type State is (pack_end, first_word_desc, second_word_desc);
    variable my_state : State;
  
    -- local mapping of signals -- 
    variable map_ready : std_logic;
    variable map_data : std_logic_vector(31 downto 0);
    variable map_valid_desc : std_logic;
    variable map_valid_data : std_logic;

  begin
    clk_rst : if rst = '1' then
      ready <= '1'; 
      map_ready := '1';
      valid_data <= '0';
      map_valid_data := '0'; 
      valid_desc <= '0';  
      map_valid_desc := '0'; 
      data_out <= std_logic_vector(to_unsigned(0, 32));
      map_data := std_logic_vector(to_unsigned(0, 32));
      cnt := std_logic_vector(to_unsigned(0, 63));
      desc := std_logic_vector(to_unsigned(0, 64));
      my_state := State'(pack_end);
      flag_desc := '0';
    elsif rising_edge(clk)  then

      valid_data_handler : if map_valid_data = '1' and ready_wrt = '1' then
        valid_data <= '0';
        map_valid_data := '0';
      end if valid_data_handler;
      
      valid_desc_handler : if map_valid_desc = '1' and ready_wrt = '1' then 
        valid_desc <= '0';
        map_valid_desc := '0';
      end if valid_desc_handler;
      
      data_handler : if map_ready = '1' and valid_in = '1' then 
        ready <= '0';
        map_ready := '0';

        map_data := data_in;

        if serv_info(0) = '1' then
          case num_byte is
            when "00" => 
              map_data(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
              my_state := State'(first_word_desc);
            when "01" => 
              cnt := std_logic_vector(signed(cnt) + 1);
              map_data(23 downto 0) := std_logic_vector(to_unsigned(0, 24));
            when "10" => 
              cnt := std_logic_vector(signed(cnt) + 2);
              map_data(15 downto 0) := std_logic_vector(to_unsigned(0, 16));
            when "11" => 
              cnt := std_logic_vector(signed(cnt) + 3);
              map_data(7 downto 0) := std_logic_vector(to_unsigned(0, 8));
            when others => 
              cnt := std_logic_vector(signed(cnt) + 3);
              map_data(7 downto 0) := std_logic_vector(to_unsigned(0, 8));
          end case;

          desc(63) := serv_info(1);
          desc(62 downto 0) := cnt;
          
          flag_desc := '1';
          cnt := std_logic_vector(to_unsigned(0,63)); 
        else 
          cnt := std_logic_vector(signed(cnt) + 4);
        end if;
      end if data_handler;
  
      manager_of_send : if map_ready = '0' and map_valid_data = '0' and map_valid_desc = '0' then
        if flag_desc = '0' then 
          valid_data <= '1';
          map_valid_data := '1';
          ready <= '1';
          map_ready := '1';
          data_out <= map_data;
        else 
          case my_state is
            when State'(pack_end) =>
              data_out <= map_data;
              valid_data <= '1';
              map_valid_data := '1';
              my_state := State'(first_word_desc);

            when State'(first_word_desc) =>
              data_out <= desc(63 downto 32);
              valid_desc <= '1';
              map_valid_desc := '1';  
              my_state := State'(second_word_desc);

            when State'(second_word_desc) =>
              data_out <= desc(31 downto 0);
              valid_desc <= '1';
              map_valid_desc := '1';  
              ready <= '1';
              map_ready := '1';
              flag_desc := '0';
              my_state := State'(pack_end);

            when others =>
              my_state := State'(pack_end);
          end case;
        end if;
      end if manager_of_send;

    end if clk_rst;  
  end process main;
  
end architecture ; -- arch