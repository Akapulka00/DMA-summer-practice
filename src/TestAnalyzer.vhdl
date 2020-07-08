library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
--test environment
entity TestAnalyzer is
  generic 
  ( 
    TICK: time := 200 fs
  );
end TestAnalyzer;

architecture main of TestAnalyzer is 
  signal testCNT: integer:=0;--counter test
  signal testReset: std_logic:='0';--test signal is modeling of rst
  signal testClock: std_logic:='1';--test signal is modeling of clock
  signal testsCompleted: boolean:=false;--flag test completion
  signal testAssert: boolean:=true;--signal that says the test passed
  signal testAllOk: boolean:=true;--signal that says what all test passed or we have problem
  signal testFirstFailedTest: integer:=-1;--number test, which first failed;
  signal testStageTest: integer:=0;--stage of one test
  signal test_request_word_for_assert: std_logic;--signal for assert
  signal test_word_for_asset_valid: std_logic;--signal says, what word from bus for assert operation valid;
  signal test_bus_word_assert_operation: std_logic_vector(31 downto 0);
  signal test_serv_info_assert_operation: std_logic_vector(0 to 1);
  signal test_nByte_assert_operation: std_logic_vector(1 downto 0);
  --model bus between buf and block analyze
  signal testWordToAnalyzer: std_logic_vector(31 downto 0);
  signal testServInfoToAnalyzer: std_logic_vector(0 to 1);
  signal testNByteToAnalyzer: std_logic_vector(1 downto 0);
  signal testReadyFromAnalyzer: std_logic;
  signal testValidBufToAnalyzer: std_logic;

  --model bus between block analyze and block writer;
  signal testWordFromAnalyzer: std_logic_vector(31 downto 0);
  signal testReadyMemToAnalyzer: std_logic;
  signal testValidDescripterFromAnalyzer: std_logic;
  signal testValidWordFromAnalyzer: std_logic;
  

  component analyzer is
    port 
    (
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
  end component analyzer;
  
begin
  myAnalyzer:analyzer port map (
    rst => testReset,
    clk => testClock,
    data_in => testWordToAnalyzer,
    serv_info => testServInfoToAnalyzer,
    num_byte => testNByteToAnalyzer,
    ready => testReadyFromAnalyzer,
    valid_in => testValidBufToAnalyzer,
    data_out => testWordFromAnalyzer,
    ready_wrt => testReadyMemToAnalyzer,
    valid_data => testValidWordFromAnalyzer,
    valid_desc => testValidDescripterFromAnalyzer
  );

  clock:process(testClock)
  begin 
    if testsCompleted = false then 
      if testClock = '1' then
        testClock <= '0' after TICK;
      else
        testClock <= '1' after TICK;
      end if;
    end if;
  end process clock;

  checkResultTest:process(testAssert)
  begin
    if testAssert = false then
      if testFirstFailedTest = -1 then
        testFirstFailedTest <= testCNT;
      end if;
      testAllOk <= false;
    end if;
  end process checkResultTest;

  process(testStageTest)
  begin
    case testCNT is
      when 0 =>
        --test: rst
        case testStageTest is
          when 0 => 
            testStageTest <= testStageTest+1 after 2*Tick;
            testReset <= '0';
          when 1 =>
            testStageTest <= testStageTest+1 after 2*TICK;
            testReset <= '1', '0' after 2*TICK;
          when 2 =>
            if testReadyFromAnalyzer = '1' and testWordFromAnalyzer = "00000000000000000000000000000000" and testValidWordFromAnalyzer = '0' and testValidDescripterFromAnalyzer = '0' then
              testAssert <= true;
            else
              testAssert <= false; 
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when others =>
            testStageTest <=0;
            testCNT <= 1;
        end case;
      when 1 => 
        --test: package for 8 word, word on down front
        case testStageTest is
          when 0 =>
            testStageTest <= testStageTest+1 after 2*TICK;
            testReset <= '0';
          when 1 =>   
            testStageTest <= testStageTest+1 after 2*TICK;
            testReset <= '1', '0' after 2*TICK;
          when 2 =>
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000000000000000000000001111";
            testStageTest <= testStageTest+1 after 6*TICK;--
            testServInfoToAnalyzer <= "00";
            testNByteToAnalyzer <= "00";
          when 3 =>
            if testWordFromAnalyzer = "00000000000000000000000000001111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 4 => 
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00000000000000000000000011110000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 5 => 
            if testWordFromAnalyzer = "00000000000000000000000011110000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 6 =>
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00000000000000000000111100000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 7 =>
            if testWordFromAnalyzer = "00000000000000000000111100000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 8 => 
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00000000000000001111000000000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 9 =>
            if testWordFromAnalyzer = "00000000000000001111000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 10 =>
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00000000000011110000000000000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 11 => 
            if testWordFromAnalyzer = "00000000000011110000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 12 => 
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00000000111100000000000000000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 13 =>
            if testWordFromAnalyzer = "00000000111100000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 14 => 
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer<="00001111000000000000000000000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 15 =>
            if testWordFromAnalyzer = "00001111000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 16 =>
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testWordToAnalyzer <="11110000000000000000000000000000";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "11";
            testNByteToAnalyzer <= "11";
          when 17 =>
            if testWordFromAnalyzer = "11110000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 18 =>
            if testWordFromAnalyzer = "10000000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 19 =>
            if testWordFromAnalyzer = "00000000000000000000000000011111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when others =>
            --go to next test
            testStageTest <=0;
            testCNT <= 2;
        end case;
      when 2 =>
        --test: package for 4 word, word on rising front
        case testStageTest is
          when 0 =>
            testStageTest <= testStageTest+1 after 2*TICK;
            testReset <= '0';
          when 1 =>   
            testStageTest <= testStageTest+1 after 2*TICK;
            testReset <= '1', '0' after TICK;
          when 2 =>
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000000000000000000011111111";
            testStageTest <= testStageTest+1 after 6*TICK;
            testServInfoToAnalyzer <= "00";
            testNByteToAnalyzer <= "00";
          when 3 =>
            if testWordFromAnalyzer = "00000000000000000000000011111111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 4 => 
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000000000001111111100000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 5 => 
            if testWordFromAnalyzer = "00000000000000001111111100000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 6 => 
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000111111110000000000000000";
            testStageTest <= testStageTest+1 after 6*TICK;
          when 7 => 
            if testWordFromAnalyzer = "00000000111111110000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK; 
          when 8 => 
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="11111111000000000000000000000000";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "10";
            testNByteToAnalyzer <= "11";
          when 9 => 
            if testWordFromAnalyzer = "11111111000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;            
          when 10 =>
            if testWordFromAnalyzer = "00000000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 19 =>
            if testWordFromAnalyzer = "00000000000000000000000000001111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when others =>
            testStageTest <=0;
            testCNT <= 3;
        end case;
      when 3 => 
        --test two package 
        case testStageTest is
          when 0 =>
            testStageTest <= testStageTest+1 after 1*TICK;
            testReset <= '0';
          when 1 =>   
            testStageTest <= testStageTest+1 after 1*TICK;
            testReset <= '1', '0' after TICK;
          when 2 =>
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000000000001111111111111111";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "00";
            testNByteToAnalyzer <= "00";
          when 3 =>
            if testWordFromAnalyzer = "00000000000000001111111111111111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 4 => 
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="11111111111111110000000000000000";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "11", "00" after 4*TICK;
            testNByteToAnalyzer <= "11", "00" after 4*TICK;
          when 5 => 
            if testWordFromAnalyzer = "11111111111111110000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 6 =>
            if testWordFromAnalyzer = "10000000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 7 =>
            if testWordFromAnalyzer = "00000000000000000000000000000111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 8 =>
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="11111111111111110000000000000000";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "00";
            testNByteToAnalyzer <= "00";
          when 9 =>
            if testWordFromAnalyzer = "11111111111111110000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 10 => 
            testReadyMemToAnalyzer <= '0', '1' after 4*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="00000000000000001111111111111111";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "10";
            testNByteToAnalyzer <= "11";
          when 11 => 
            if testWordFromAnalyzer = "00000000000000001111111100000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 12 =>
            if testWordFromAnalyzer = "00000000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 13 =>
            if testWordFromAnalyzer = "00000000000000000000000000000111" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when others =>
            testStageTest <= 0;
            testCNT <= 4;
        end case;
      when 4 =>
        --test don't full package
        case testStageTest is
          when 0 =>
            testStageTest <= testStageTest+1 after 1*TICK;
            testReset <= '0';
          when 1 =>   
            testStageTest <= testStageTest+1 after 1*TICK;
            testReset <= '1', '0' after TICK;
          when 2 =>
            testReadyMemToAnalyzer <= '0', '1' after 2*TICK;
            testValidBufToAnalyzer <= '1', '0' after 4*TICK;
            testWordToAnalyzer<="11111111111111111111111111111111";
            testStageTest <= testStageTest+1 after 4*TICK;
            testServInfoToAnalyzer <= "10";
            testNByteToAnalyzer <= "11";
          when 3 =>
            if testWordFromAnalyzer = "11111111111111111111111100000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 4 =>
            if testWordFromAnalyzer = "00000000000000000000000000000000" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when 5 =>
            if testWordFromAnalyzer = "00000000000000000000000000000011" then
              testAssert <= true;
            else 
              testAssert <= false;
            end if;
            testStageTest <= testStageTest+1 after 2*TICK;
          when others =>
            testStageTest <= 0;
            testCNT <= 5;
        end case;
      when 5 =>
        --test different time interval
        --one proc
        case testStageTest is
          when others =>
            testStageTest <= 0;
            testCNT <= 6 after 2 * TICK;
            testsCompleted <= true;
        end case;
      when others => 
        testsCompleted <= true;
    end case;
  end process;
end architecture main;