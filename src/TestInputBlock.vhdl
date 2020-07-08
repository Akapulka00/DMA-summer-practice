library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 

entity TestInputBlock is
  generic 
  ( 
    TICK: time := 200 fs
  );
end TestInputBlock;

architecture main of TestInputBlock is 
  signal test_rst: std_logic := '0';--test signal is modeling of test_rst
  signal test_clk: std_logic := '1';

  --interface specific signal
  signal TE_INT_testCnt: integer := 0;--counter test
  signal TE_INT_stageTest: integer := 0;--stage of one test
  signal TE_INT_firstFailedTest: integer := -1;--number test, which first failed;
  signal TE_INT_firstStageTest: integer := -1;
  signal TE_BOOL_testsCompleted: boolean := false;--flag test completion
  signal TE_BOOL_testAssert: boolean := true;--signal that says the test passed
  signal TEST_BUFS:std_logic := '0';
  signal TE_BOOL_AllOk: boolean := true;--signal that says what all test passed or we have problem
  
  --interface for control of generate package 
  signal TE_INT_NC_setInterval: integer := 4;
  signal TE_INT_NC_numWord: integer := 5;
  signal TE_SL_NC_ready: std_logic;
  signal TE_SL_NC_generatePackage: std_logic;
 
  --interface for assert operation
  signal TE_SL_NC_requestAssertBuf:std_logic;
  signal TE_SL_NC_validBuf:std_logic;
  signal TE_SLV_20W_NC_buf:std_logic_vector(0 to 32*20 - 1);
  signal TE_SLV_20W_IB_buf:std_logic_vector(0 to 32*20 - 1);

  --model bus between network controller and input block;
  signal TE_SLV_WR_NCIB_word:std_logic_vector(31 downto 0);
  signal TE_SLB_4B_NCIB_flag:std_logic_vector(3 downto 0);
  signal TE_SL_NCIB_valid:std_logic; 
  signal TE_SL_NCIB_ready:std_logic;
  
  --model bus between input block and buffer;
  signal TE_SLV_WR_IBBUF_word:std_logic_vector(31 downto 0);
  signal TE_SLV_2B_IBBUF_servInfo:std_logic_vector(0 to 1);
  signal TE_SLV_2B_IBBUF_nByte:std_logic_vector(1 downto 0);
  signal TE_SL_IBBUF_valid:std_logic;
  signal TE_SL_IBBUF_ready:std_logic;
  signal SizeAssertBuf:integer;

  component modelNetworkController is
    port 
    (
      rst: in std_logic;
      clk: in std_logic;
      
      --interface with test modules

      --subinterface for control of generate package
      NC_INT_IN_TE_setInterval: in integer;--port network controller connected to test environment, he is need for set interval transmission data_in;
      NC_INT_IN_TE_numWord: in integer;--port network controller connected to test environment, he is need for set quantity data_in of generate package; 
      NC_SL_IN_TE_generatePackage: in std_logic;--port network controller connected to test environment, he transmission the request of start generation package;
      NC_SL_OUT_TE_ready: out std_logic;--port  network controller connected to test environment, he is need for transmission of readiness network controller for generate package; 

      --subinterface for request data for operation assert
      NC_SL_IN_TE_requestAssertBuf: in std_logic;--port network controller connected to test environment, he request data_in for assert to correct proccesing data_in on input block;
      NC_SL_OUT_TE_validBuf: out std_logic;--port network controller connected to test environment, he transmission the assert, that data_in on the bus correct;
      NC_SLV_20W_OUT_TE_buf: out std_logic_vector(0 to 32*20 - 1); 
      --interface with input block
      NC_SLV_WR_OUT_IB_word: out std_logic_vector(31 downto 0);--bus between the network controller and input block(data or spec symbols 0-EOF, 1 - EEF,2 - FILL);
      NC_SLV_4BR_OUT_IB_flag: out std_logic_vector(3 downto 0);--4 bit that help make interpretation part(1 byte) of data data_in, 0-data 1-spec symbols(EOF,EEF,FILL); 
      NC_SL_OUT_IB_valid: out std_logic;--data_in on bus valid
      NC_SL_IN_IB_ready: in std_logic--input block ready for reception data_in;
    );
  end component modelNetworkController; 

  component inputBlock is
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
      data_out : out std_logic_vector(31 downto 0);-- bus with buffer for only data,  if word of data is have  end of package,  bus could have trash after data, this noted in serv_info and num_byte signal --
      serv_info : out std_logic_vector(0 to 1);-- first bit flag end package, second bit EOF/EEP --
      num_byte : out std_logic_vector(1 downto 0);-- [0-3] number byte, which is an end of package in word, word could have trash after this byte --
      valid : out std_logic; -- report to buffer about valid of data --
      ready_buf : in std_logic -- buffer ready for write data --
    );
  end component inputBlock;

  begin
    myNetworkController:modelNetworkController port map 
    (
      rst => test_rst,
      clk => test_clk,

      --subinterface for control of generate package
      NC_INT_IN_TE_setInterval => TE_INT_NC_setInterval,
      NC_INT_IN_TE_numWord => TE_INT_NC_numWord,
      NC_SL_IN_TE_generatePackage => TE_SL_NC_generatePackage,
      NC_SL_OUT_TE_ready => TE_SL_NC_ready, 
  
      --subinterface for request data for operation assert 
      NC_SL_IN_TE_requestAssertBuf => TE_SL_NC_requestAssertBuf,
      NC_SL_OUT_TE_validBuf => TE_SL_NC_validBuf,
      NC_SLV_20W_OUT_TE_buf => TE_SLV_20W_NC_buf,
  
      NC_SLV_WR_OUT_IB_word => TE_SLV_WR_NCIB_word,
      NC_SLV_4BR_OUT_IB_flag => TE_SLB_4B_NCIB_flag,
      NC_SL_OUT_IB_valid => TE_SL_NCIB_valid,
      NC_SL_IN_IB_ready => TE_SL_NCIB_ready
    );

    myInputBlock:inputBlock port map 
    (
      rst => test_rst,
      clk => test_clk,
      data_in => TE_SLV_WR_NCIB_word,
      flags => TE_SLB_4B_NCIB_flag,
      valid_data => TE_SL_NCIB_valid,
      ready => TE_SL_NCIB_ready,
      data_out => TE_SLV_WR_IBBUF_word,
      serv_info => TE_SLV_2B_IBBUF_servInfo,
      num_byte => TE_SLV_2B_IBBUF_nByte,
      valid => TE_SL_IBBUF_valid,
      ready_buf => TE_SL_IBBUF_ready
    );

    clk:process(test_clk)
    begin 
      if TE_BOOL_testsCompleted = false then 
        if test_clk = '1' then
          test_clk <= '0' after TICK;
        else
          test_clk <= '1' after TICK;
        end if;
      end if;
    end process clk;

    checkResultTest:process(TE_BOOL_testAssert)
    begin
      if TE_BOOL_testAssert = false then
        if TE_INT_firstFailedTest = -1 then
          TE_INT_firstFailedTest <= TE_INT_testCnt;
          TE_INT_firstStageTest <= TE_INT_stageTest;
        end if;
        TE_BOOL_AllOk <= false;
      end if;
    end process checkResultTest;

    process(TE_INT_stageTest)
    begin 
    TE_SL_IBBUF_ready <= '1';
      case TE_INT_testCnt is
        when 0 =>
          --test: test_rst
          case TE_INT_stageTest is
            when 0 => 
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
              test_rst <= '0';
            when 1 => 
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
              test_rst <= '1', '0' after 2*TICK;
            when 2 => 
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 60*TICK;
              TE_INT_NC_setInterval <= 2; 
              TE_INT_NC_numWord <= 25;           
              TE_SL_NC_generatePackage <= '1','0' after 6*TICK;
            when 3 =>
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
              TE_SL_NC_requestAssertBuf <= '1';
            when others =>
              TE_INT_stageTest <= 0 after 2*TICK;
              TEST_BUFS <= '1';
              TE_INT_testCnt <= TE_INT_testCnt + 1 after 2*TICK;
          end case;
        when 1 =>
          --test: test_rst
          case TE_INT_stageTest is
            when 0 => 
              TEST_BUFS <= '0';
              TE_SL_NC_requestAssertBuf <= '0';
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
              test_rst <= '0';
            when 1 => 
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
            when 2 => 
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 120*TICK;
              TE_INT_NC_setInterval <= 12; 
              TE_INT_NC_numWord <= 9;           
              TE_SL_NC_generatePackage <= '1','0' after 6*TICK;
            when 3 =>
              TE_INT_stageTest <= TE_INT_stageTest + 1 after 2*TICK;
              TE_SL_NC_requestAssertBuf <= '1';
            when others =>
              TE_INT_stageTest <= 0 after 2*TICK;
              TEST_BUFS <= '1';
              TE_INT_testCnt <= TE_INT_testCnt + 1 after 2*TICK;
          end case;
        when others=>
          TE_BOOL_testsCompleted <= true;
      end case;
    end process;

    collectorDataOutIB:process(test_rst, test_clk)
    variable iterator:integer:=0;
    begin
      if test_rst = '1' then 
        TE_SLV_20W_IB_buf <= std_logic_vector(to_unsigned(0, 32*20));
        iterator := 0;
      elsif test_clk = '1' and test_clk'event then
        if TE_SL_IBBUF_valid = '1' and TE_SL_IBBUF_ready = '1' then
          TE_SLV_20W_IB_buf(iterator to iterator + 31) <= TE_SLV_WR_IBBUF_word;
          iterator := iterator + 32;
        end if;
      end if;
    end process;
    
    checkBuff:process(test_rst, TEST_BUFS)
    variable flag:boolean:=true;
    begin
      if test_rst = '1' then
        flag := true;
      else 
        flag := true;
        if TEST_BUFS = '1' then 
          cmpbufs : for i in 0 to 20*32-1 loop
            if TE_SLV_20W_NC_buf(i) /= TE_SLV_20W_IB_buf(i) then 
              flag := false;
            end if;
          end loop;
          if flag = false then
            TE_BOOL_testAssert <= false;
          else
            TE_BOOL_testAssert <= true;
          end if;
        end if;
      end if;
    end process;
end architecture main;
    