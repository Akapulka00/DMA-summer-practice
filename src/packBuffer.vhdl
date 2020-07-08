library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity packBuffer is
  port 
  (
    clk : in std_Logic;
    rst : in std_logic;

    --interface with analyzer --
    data_out : out std_logic_vector(31 downto 0);
    serv_info_out : out std_logic_vector(0 to 1);
    num_byte_out : out std_logic_vector(1 downto 0);
    ready_anl : in std_logic;
    valid : out std_logic;

    --interface with input block --
    data_in : in std_logic_vector(31 downto 0);
    serv_info_in : in std_logic_vector(0 to 1);
    num_byte_in : in std_logic_vector(1 downto 0);
    ready : out std_logic;
    valid_inb : in std_logic
  ) ;
end packBuffer ;

architecture arch of packBuffer is

constant SIZEBUFF: integer := 64;
constant IMAXBUFF: integer := SIZEBUFF - 1;

type Queue_data is array (0 to SIZEBUFF - 1) of std_logic_vector(31 downto 0);
type Queue_serv_info is array (0 to SIZEBUFF - 1) of std_logic_vector(0 to 1);
type Queue_byte is array (0 to SIZEBUFF - 1) of std_logic_vector(1 downto 0);

signal buff_data : Queue_data;
signal buff_serv_info : Queue_serv_info;
signal buff_byte : Queue_byte;

subtype IndexBuff is natural range 0 to IMAXBUFF;

signal head : IndexBuff;
signal tail : IndexBuff;

type BuffState is (empty, full, work);
--empty - the buff is empty, read - unallowed, write - allowed --
--full - the buff is full, read - allowed, write - unallowed --
--work - operation r/w allowed --

signal state : BuffState;

signal map_valid_out : std_logic;

begin

  stateManager : process(clk, rst)
  begin
    clk_rst : if rst = '1' then 
      state <= BuffState'(empty);
    elsif rising_edge(clk) then

      -- operation only read or write --
      border_event_handler : if not ((state /= BuffState'(full) and valid_inb = '1') and (state /= BuffState'(empty) and (map_valid_out = '0' or (map_valid_out = '1' and ready_anl = '1' )))) then 

        -- mb change state in process read from buffer --
        control_dequeue : if state /= BuffState'(empty) and (map_valid_out = '0' or (map_valid_out = '1' and ready_anl = '1' )) then 
          case state is 
            when BuffState'(full) =>
              state <= BuffState'(work);
            when BuffState'(work) =>
              if head = IMAXBUFF then
                if tail = 0 then
                  state <= BuffState'(empty); 
                end if;
              else 
                if head + 1 = tail then 
                  state <= BuffState'(empty);
                end if;
              end if;
            when BuffState'(empty) =>
              report "The error haven't name!!! But that state full when only read";
            when others =>
              report "The error haven't name!!! Section only read";
          end case;
        end if control_dequeue;

        -- mb change state in process write to buffer -- 
        control_enqueue : if state /= BuffState'(full) and valid_inb = '1' then
          case state is 
            when BuffState'(empty) =>
              state <= BuffState'(work);
            when BuffState'(work) =>
              if tail = IMAXBUFF then
                if head = 0 then
                  state <= BuffState'(full); 
                end if;
              else 
                if tail + 1 = head then 
                  state <= BuffState'(full);
                end if;
              end if;
            when BuffState'(full) =>
              report "The error haven't name!!! But that state full when only write";
            when others =>
              report "The error haven't name!!! Section only write";
          end case;
        end if control_enqueue;

      end if border_event_handler;

    end if clk_rst;
  end process stateManager;

  enqueue : process(clk, rst)
    variable map_ready : std_logic;
  begin
    clk_rst : if rst = '1' then
      tail <= 0;
      ready <= '1';
      map_ready := '1';
      buff_data <= (others => "00000000000000000000000000000000");
      buff_serv_info <= (others => "00");
      buff_byte <= (others => "00");
    elsif rising_edge(clk) then

      if state /= BuffState'(full)  then

        if valid_inb = '1' and map_ready = '1' then
          map_ready := '0';
          buff_data(tail) <= data_in;
          buff_serv_info(tail) <= serv_info_in;
          buff_byte(tail) <= num_byte_in;
          if tail = IMAXBUFF then
            tail <= 0;
          else
            tail <= tail + 1;
          end if;
        end if;

        if tail = IMAXBUFF then
          if head /= 0 then
            map_ready := '1'; 
          end if;
        else 
          if tail + 1 /= head then 
            map_ready := '1'; 
          end if;
        end if;

        ready <= map_ready;
      end if;
    end if clk_rst;

  end process enqueue; 
  
  dequeue : process(clk, rst)
  variable map_valid : std_logic:='0';
  begin
    clk_rst : if rst = '1' then
      head <= 0;
      valid <= '0';
      map_valid := '0';
      map_valid_out <= '0';
      num_byte_out <= "00";
      serv_info_out <= "00";
      data_out <= "00000000000000000000000000000000";
    elsif rising_edge(clk) then

      if map_valid = '1' and ready_anl = '1' then 
        map_valid := '0';
      end if; 

      if state /= BuffState'(empty) then
        if map_valid = '0' then 
          data_out <= buff_data(head);
          serv_info_out <= buff_serv_info(head);
          num_byte_out <= buff_byte(head);
          map_valid := '1';
          if head = IMAXBUFF then
            head <= 0;
          else
            head <= head + 1;
          end if;
        end if;
      end if;

      map_valid_out <= map_valid;
      valid <= map_valid;

    end if clk_rst;
  end process dequeue;
            
end architecture ; -- arch