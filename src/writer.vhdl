library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use ieee.std_logic_arith;

entity writer is
  port (
    clk : in std_logic;
    rst : in std_logic;

    --interface with memory --
    address : out std_logic_vector(19 downto 0);--address of the cell to be recorded --
    data : out std_logic_vector(63 downto 0); --data for record in memory -- 
    valid : out std_logic; --sign of recording command --
    mem_ready : in std_logic; --sign of ready memory for recording --

    --interface with analyzer --
    word : in std_logic_vector(31 downto 0);--bus with analyzer --
    ready : out std_logic;
    valid_data : in std_logic;-- transmission word data --
    valid_desc : in std_logic;-- transmission word desc --

    --interface with registers block --
    write_desc : out std_logic;
    write_data : out std_logic;

    INTFF : in std_logic; -- interrupt first field ended --
    INTSF : in std_logic; -- interrupt second field ended --

    CAFF_REG : in std_logic_vector(31 downto 0); -- current address first field --   
    CASF_REG : in std_logic_vector(31 downto 0); -- current address second field --
    SZSF_REG : in std_logic_vector(31 downto 0) -- when = 0 stopped --
  );
end writer;

architecture arch of writer is

begin
  
  main : process( clk, rst )
    variable part_data : std_logic;
    variable end_pack : std_logic;
    variable desc_out : std_logic;
    variable flag_data : std_logic;
    variable map_ready : std_logic;
    variable tmp_ready : std_logic;
    variable flag_ready : std_logic;
    variable map_valid : std_logic;
    variable map_data : std_logic_vector(63 downto 0);
    variable lost_data : std_logic;
    variable tmp_data : std_logic_vector(31 downto 0);
    variable f_desc : std_logic;
    variable map_write_desc : std_logic;
    variable map_write_data : std_logic;
  begin
    clk_rst : if( rst = '1' ) then
      write_desc <= '0';
      f_desc := '0';
      flag_ready := '1';
      map_write_desc := '0';
      write_data <= '0';
      map_write_data := '0';
      ready <= '0';
      map_ready := '0';
      tmp_ready := '1';
      valid <= '0';
      map_valid := '0';
      flag_data := '0';
      lost_data := '0';
      part_data := '1';
      desc_out := '0';
      end_pack := '0';
      address <= std_logic_vector(to_unsigned(0, 20));
      data <= std_logic_vector(to_unsigned(0, 64));
      map_data := std_logic_vector(to_unsigned(0, 64));
      tmp_data := std_logic_vector(to_unsigned(0, 32));
    elsif( rising_edge(clk) ) then

      unlocking_write_desc : if map_write_desc = '1' then
        write_desc <= '0';
        map_write_desc := '1';
      end if unlocking_write_desc;

      unlocking_write_data : if map_write_data = '1' then
        write_data <= '0';
        map_write_data := '0';
      end if unlocking_write_data;



      stopped_without_set_SZSF : if SZSF_REG /= std_logic_vector(to_unsigned(0, 32)) then
        stopped_with_int : if INTFF /= '1' and INTSF /= '1' then

          read : if map_ready = '1' and  (valid_desc = '1' or valid_data = '1') then
                      
            end_pack := '0';
            map_ready := '0';

            spec_do_with_desc : if valid_desc = '1' then
 
              if part_data = '0' and f_desc = '0' then -- if first half of data taken and now first half of desc
                lost_data := '1'; 
                tmp_data := map_data(63 downto 32);
              end if;

              -- if second half of desc, that pack is end
              if f_desc = '1' then 
                end_pack := '1';
              else
                -- else reset part_data and preparation second half of desc
                part_data := '1';
                f_desc := '1';
              end if; 
            end if spec_do_with_desc;
            
            if part_data = '1' then
              part_data := '0';
              map_data(63 downto 32) := word; 
              map_ready := '1';
              flag_data := '1';
            else 
              part_data := '1';
              map_data(31 downto 0) := word;
            end if;
            ready <= map_ready;
          end if read;
          
          return_value_for_ready_after_stopped : if flag_ready = '1'  then
            ready <= tmp_ready;
            map_ready := tmp_ready;
            flag_ready := '0';
          end if return_value_for_ready_after_stopped;

          unlocking_valid : if map_valid = '1' and mem_ready = '1' then 
            valid <= '0';
            map_valid := '0';
            if desc_out = '1' then
              end_pack := '0';
              desc_out := '0';
              write_desc <= '1';
              map_write_desc := '1';
              ready <= '1';
              map_ready := '1';
            end if;
          end if unlocking_valid;


          if flag_data = '1' and map_valid = '0' and (part_data = '1' or lost_data = '1') then 
            if lost_data = '1' then
              lost_data := '0';
              valid <= '1';
              map_valid := '1';
              data <= tmp_data & std_logic_vector(to_unsigned(0, 32));
              address <= CAFF_REG(19 downto 0);
              write_data <= '1';
              map_write_data := '1';
            elsif part_data = '1' then 
              data <= map_data;
              valid <= '1';
              map_valid := '1';
              flag_data := '0';
              if end_pack = '1' then
                desc_out := '1';
                f_desc := '0';
                address <= CASF_REG(19 downto 0);
              else
                write_data <= '1';
                map_write_data := '1';
                ready <= '1';
                map_ready := '1';
                address <= CAFF_REG(19 downto 0);
              end if;
            end if;
          end if;
        else
          if flag_ready = '0' then
            tmp_ready := map_ready;
          end if;
          flag_ready := '1';
          ready <= '0';
          map_ready := '0';   
        end if stopped_with_int;
      else
        if flag_ready = '0' then
          tmp_ready := map_ready;
        end if;
        flag_ready := '1';
        ready <= '0';
        map_ready := '0';
      end if stopped_without_set_SZSF;
    end if clk_rst;
   end process ; -- main
  
end architecture ; -- arch