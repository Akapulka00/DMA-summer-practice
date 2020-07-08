library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inputBlock is
  port 
  (
    clk : in std_logic;
    rst : in std_logic;

    -- interface with network controller --
    data_in : in std_logic_vector(31 downto 0); -- bus with network controller(data or spec symbols 0 - EOF, 1 - EEF, 2 - FILL) --
    flags : in std_logic_vector(3 downto 0); -- 4 bit that help make interpretation part(1 byte) of data word, 0 - data 1 - spec symbols(EOF, EEF, FILL) --
    valid_data : in std_logic; -- report from network controller about valid of data --
    ready : out std_logic; -- i ready of read word from network controller --

    --interface with buffer --
    data_out : out std_logic_vector(31 downto 0); -- bus with buffer for only data,  if word of data is have  end of package,  bus could have trash after data, this noted in serv_info and num_byte signal --
    serv_info : out std_logic_vector(0 to 1); -- first bit flag end package, second bit EOF/EEP --
    num_byte : out std_logic_vector(1 downto 0); -- [0-3] number byte, which is an end of package in word, word could have trash after this byte --
    valid : out std_logic; -- report to buffer about valid of data --
    ready_buf : in std_logic -- buffer ready for write data --
  ) ;
end inputBlock ;

architecture arch of inputBlock is
begin

  main: process(clk, rst)
    variable buf : std_logic_vector(63 downto 0); -- local buffer for collect compressed(without free field) data, that need, because for one compressed word of data could need 2 usual word --
    variable buf_index : integer; -- index last byte of data in local buffer -- 
    
    -- local mapping of signals -- 
    variable map_serv_info : std_logic_vector(0 to 1); 
    variable map_num_byte : integer;
    variable map_valid : std_logic;
    variable map_ready : std_logic;

  begin
    clk_rst : if rst = '1' then
      ready <= '1';
      map_ready := '1';
      valid <= '0';
      map_valid := '0';
      serv_info <= std_logic_vector(to_unsigned(0, 2));
      map_serv_info := std_logic_vector(to_unsigned(0, 2));
      num_byte <= std_logic_vector(to_unsigned(0, 2));
      map_num_byte := 0;
      data_out <= std_logic_vector(to_unsigned(0, 32));
      buf := std_logic_vector(to_unsigned(0, 64));
      buf_index := 0;
    elsif rising_edge(clk) then

      valid_handler : if map_valid = '1' and ready_buf = '1' then
        map_valid := '0';
        valid <= '0';
        serv_info <= "00";
        num_byte <= "00";
      end if valid_handler;

      data_handler : if valid_data = '1' and map_ready = '1' then
        map_ready := '0';
        ready <= '0';

        case flags is

          when "0000" =>
            buf(63 - buf_index*8 downto 32 - buf_index*8) := data_in(31 downto 0);
            buf_index := buf_index + 4;

          when "0001" =>
            buf(63 - buf_index*8 downto 40 - buf_index*8) := data_in(31 downto 8);
            buf_index :=  buf_index + 3;
            if data_in(7 downto 0) /= "00000010" then 
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(0);
              map_num_byte := buf_index;
            end if;

          when "0010" =>
            buf(63 - buf_index*8 downto 48 - buf_index*8) := data_in(31 downto 16);
            buf(47 - buf_index*8 downto 40 - buf_index*8) := data_in(7 downto 0);
            buf_index := buf_index + 3;
            if data_in(15 downto 8) /= "00000010" then 
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(8);
              map_num_byte := buf_index - 1;
            end if;

          when "0011" =>
            buf(63 - buf_index*8 downto 48 - buf_index*8) := data_in(31 downto 16);
            buf_index := buf_index + 2;
            if data_in(15 downto 8) /= "00000010" then 
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(8);
              map_num_byte := buf_index;
            else
              if data_in(7 downto 0) /= "00000010" then 
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(0);
                map_num_byte := buf_index;
              end if;
            end if;

          when "0100" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(31 downto 24);
            buf(55 - buf_index*8 downto 40 - buf_index*8) := data_in(15 downto 0);
            buf_index := buf_index + 3;
            if data_in(23 downto 16) /= "00000010" then 
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(16);
              map_num_byte := buf_index - 2;
            end if;

          when "0101" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(31 downto 24);
            buf(55 - buf_index*8 downto 48 - buf_index*8) := data_in(15 downto 8);
            buf_index := buf_index + 2;
            if data_in(23 downto 16) /= "00000010" then 
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(16);
              map_num_byte := buf_index - 1;
            else 
              if data_in(7 downto 0) /= "00000010" then 
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(0);
                map_num_byte := buf_index;
              end if;
            end if;

          when "0110" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(31 downto 24);
            buf(55 - buf_index*8 downto 48 - buf_index*8) := data_in(7 downto 0);
            buf_index := buf_index + 2;         
            if data_in(23 downto 16) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(16);
              map_num_byte := buf_index - 1;
            else 
              if data_in(15 downto 8) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(8);
                map_num_byte := buf_index - 1;
              end if;
            end if;

          when "0111" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(31 downto 24);
            buf_index := buf_index + 1;                  
            if data_in(23 downto 16) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(16);
              map_num_byte := buf_index;
            else 
              if data_in(15 downto 8) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(8);
                map_num_byte := buf_index;
              else
                if data_in(7 downto 0) /= "00000010" then
                  map_serv_info(0) := '1';
                  map_serv_info(1) := data_in(0);
                  map_num_byte := buf_index;
                end if;
              end if;
            end if;

          when "1000" =>
            buf(63 - buf_index*8 downto 40 - buf_index*8) := data_in(23 downto 0);
            buf_index := buf_index + 3;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 3;
            end if;

          when "1001" =>
            buf(63 - buf_index*8 downto 48 - buf_index*8) := data_in(23 downto 8);
            buf_index := buf_index + 2;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 2;
            else 
              if data_in(7 downto 0) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(0);
                map_num_byte := buf_index;
              end if;
            end if;

          when "1010" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(23 downto 16);
            buf(55 - buf_index*8 downto 48 - buf_index*8) := data_in(7 downto 0);
            buf_index := buf_index + 2;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 2;
            else 
              if data_in(15 downto 8) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(8);
                map_num_byte := buf_index - 1;
              end if;
            end if;

          when "1011" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(23 downto 16);
            buf_index := buf_index + 1;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 1;
            else 
              if data_in(15 downto 8) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(8);
                map_num_byte := buf_index;
              else
                if data_in(7 downto 0) /= "00000010" then
                  map_serv_info(0) := '1';
                  map_serv_info(1) := data_in(0);
                  map_num_byte := buf_index;
                end if;
              end if;
            end if;

          when "1100" =>
            buf(63 - buf_index*8 downto 48 - buf_index*8) := data_in(15 downto 0);
            buf_index := buf_index + 2;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 2;
            else 
              if data_in(23 downto 16) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(16);
                map_num_byte := buf_index - 2;
              end if;
            end if;

          when "1101" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(15 downto 8);
            buf_index :=  buf_index + 1;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 1;
            else 
              if data_in(23 downto 16) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(16);
                map_num_byte := buf_index - 1;
              else 
                if data_in(7 downto 0) /= "00000010" then
                  map_serv_info(0) := '1';
                  map_serv_info(1) := data_in(0);
                  map_num_byte :=  buf_index;
                end if;
              end if;
            end if;

          when "1110" =>
            buf(63 - buf_index*8 downto 56 - buf_index*8) := data_in(7 downto 0);
            buf_index := buf_index + 1;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index - 1;
            else 
              if data_in(23 downto 16) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(16);
                map_num_byte := buf_index - 1;
              else 
                if data_in(15 downto 8) /= "00000010" then
                  map_serv_info(0) := '1';
                  map_serv_info(1) := data_in(8);
                  map_num_byte := buf_index - 1;
                end if;
              end if;
            end if;

          when "1111" =>
            buf_index := buf_index;
            if data_in(31 downto 24) /= "00000010" then
              map_serv_info(0) := '1';
              map_serv_info(1) := data_in(24);
              map_num_byte := buf_index;
            else 
              if data_in(23 downto 16) /= "00000010" then
                map_serv_info(0) := '1';
                map_serv_info(1) := data_in(16);
                map_num_byte := buf_index;
              else 
                if data_in(15 downto 8) /= "00000010" then
                  map_serv_info(0) := '1';
                  map_serv_info(1) := data_in(8);
                  map_num_byte := buf_index;
                else
                  if data_in(7 downto 0) /= "00000010" then
                    map_serv_info(0) := '1';
                    map_serv_info(1) := data_in(0);
                    map_num_byte := buf_index;
                  end if;
                end if;
              end if;
            end if;

          when others =>
        end case;
      end if data_handler;

      manager_of_send : if map_valid = '0' and (buf_index >= 4 or map_serv_info(0) = '1') then
        data_out <= buf(63 downto 32);
        buf(63 downto 32) := buf(31 downto 0);
        buf(31 downto 0) := std_logic_vector(to_unsigned(0, 32));

        if map_serv_info(0) = '1' then 
          if map_num_byte >= 4 then
            serv_info <= "00";
            num_byte <= "00";
            map_num_byte := map_num_byte - 4;
          else
            serv_info <= map_serv_info;
            num_byte <= std_logic_vector(to_unsigned(map_num_byte, 2));
            map_serv_info := "00";
            map_num_byte := 0;
          end if;
        end if;    

        if buf_index > 4 then
          buf_index := buf_index - 4;
        else
          buf_index := 0;
        end if;

        map_valid := '1';
        valid <= '1';
      end if manager_of_send;

      ready_handler : if buf_index < 4 and map_serv_info(0) /= '1' then
        ready <= '1';
        map_ready := '1';
      end if ready_handler;

    end if clk_rst;
  end process main;

end architecture ; -- arch