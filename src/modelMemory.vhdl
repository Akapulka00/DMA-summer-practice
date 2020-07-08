library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity modelMemory is
  port
  (
    clk : in std_logic;
    rst : in std_logic;

    -- interface with writer
    ready : out std_logic;
    comWrite : in std_logic;
    data : in std_logic_vector(63 downto 0);
    address : in std_logic_vector(19 downto 0);

    -- sub interface for testing 
    memory : out std_logic_vector(0 to 64 * 20 - 1); -- for module of test
    settingValid : in std_logic; -- setting on bus valid
    ready_for_set : out std_logic; -- now test is not run
    setMaxInterval : in integer; -- it is setting of max interval for wait data. When wait is large memory 
    setMinInterval : in integer; -- it is setting of min delay before start work 
    setNumWord : in integer; -- it is setting of num word in test
    error : out std_logic; -- 1 = error, 0 = normal work
    start : in std_logic; -- start of testing
    reset_error : in std_logic; -- rst state of error
    end_test : out std_logic -- flag end of test
  );
end modelMemory ;

architecture main of modelMemory is

  signal watchDog : integer;
  signal delay : integer;
  signal numWord : integer;
  signal cntWord : integer;

begin
  
  setupManager : process( clk, rst )
  variable v_ready_for_set : std_logic;
  begin
    if( rst = '1' ) then
      watchDog <= 0;  
      delay <= 0;
      numWord <= 0;
      ready_for_set <= '1';
      v_ready_for_set := '1';
    elsif( rising_edge(clk) ) then

      if (cntWord = numWord or start = '1') then
        ready_for_set <= '1';
        v_ready_for_set := '1';
      end if;

      if (v_ready_for_set = '1' and settingValid = '1') then 
        watchDog <= setMaxInterval;
        delay <= setMinInterval;
        numWord <= setNumWord;
        ready_for_set <= '0';
        v_ready_for_set := '0';
      end if;

    end if ;
  end process ; -- setupManager
  
  memoryManager : process( clk, rst )
  variable v_ready : std_logic;
  variable v_cntWord : integer;
  variable v_cntWait : integer;
  variable v_error : std_logic;
  variable map_end_test : std_logic;
  begin
    if( rst = '1' ) then
      memory <= std_logic_vector(to_unsigned(0, 64*20));
      cntWord <= 0;
      v_cntWord := 0;
      error <= '0';
      v_error := '0';
      ready <= '0';
      v_ready := '0';
      v_cntWait := 0;
      end_test <= '0';
      map_end_test := '0';
    elsif( rising_edge(clk) ) then
      if reset_error = '1' then 
        error <= '0';
        v_error := '0';
        v_cntWait := 0;
        
        ready <= '0';
        v_ready := '0';
        end_test <= '1';
        map_end_test := '1';
      end if;
      if start = '1' then
        cntWord <= 0;
        v_cntWord := 0;
        v_cntWait := 0;
        ready <= '1';
        v_ready := '1';
        error <= '0';
        v_error := '0';     
        end_test <= '0';
        map_end_test := '0';
      elsif v_error = '0' and map_end_test = '0' then
        if v_cntWord /= numWord then
          v_cntWait := v_cntWait + 1;
          if v_ready = '1' and comWrite = '1' then 
            ready <= '0';
            v_ready := '0';
            memory(8 * to_integer(unsigned(address)) to (8 * to_integer(unsigned(address) + 8))  - 1) <= data; 
            cntWord <= cntWord + 1;
            v_cntWord := v_cntWord + 1;
            v_cntWait := 0;
          end if;

          if v_cntWait >= delay then 
            ready <= '1';
            v_ready := '1';
          end if;

          if v_cntWait > watchDog then
            error <= '1';
            v_error := '1';
          end if;
        else 
          ready <= '0';
          v_ready := '0';
          end_test <= '1';
          map_end_test := '1';
        end if; 
      end if ;
    end if ;
  end process ; -- memoryManager
  
end architecture ; -- main