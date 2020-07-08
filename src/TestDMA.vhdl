library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith;
use ieee.math_real.uniform;
use ieee.math_real.floor;

entity TestDMA is
  generic 
  (
    TICK : time := 200 fs
  ) ;
end TestDMA ;

architecture arch of TestDMA is

  signal test_clk : std_logic := '1';
  signal test_rst : std_logic := '0';
  
  constant PULSE : time := 2*TICK;

  signal AllOk : boolean := true;
  signal numFirstFailedTest : integer := -1;
  signal testsCompleted : boolean := false;
  
  signal test_cnt : integer := 0;
  signal test_pulse_cnt : integer := 0;
  
  signal test_num_pack : integer := 0;
  signal test_num_word : integer := 0;

  signal test_min_interval_record : integer := 0;
  signal test_max_interval_record : integer := 0;
  signal test_end : std_logic := '0';

  signal test_min_interval_received_word : integer := 0;
  signal test_max_interval_received_word : integer := 0;
  signal test_min_interval_received_pack : integer := 0;
  signal test_transfer_end : std_logic := '0';
  signal test_enable_error : std_logic_vector(8 downto 0) := std_logic_vector(to_unsigned(0, 9));
  -- 1 - error, 0 - warning, 
  constant ERR : std_logic := '1';
  constant WAR : std_logic := '0';
  
  signal memory_flag_error : std_logic_vector(8 downto 0) := std_logic_vector(to_unsigned(0, 9));
  signal TE_flag_error : std_logic_vector(8 downto 0) := std_logic_vector(to_unsigned(0, 9));
  signal user_mode_flag_error : std_logic_vector(8 downto 0) := std_logic_vector(to_unsigned(0, 9));
  signal assert_data_flag_error : std_logic_vector(8 downto 0) := std_logic_vector(to_unsigned(0, 9));
  -- 1 - record, 0 - skip
  constant REC : std_logic := '1';
  constant SKIP : std_logic := '0';

  -- set bit enable/flag
  -- 8 - invalid_desk 
  constant INV_DESK : integer := 8;
  -- 7 - missed_data;
  constant INV_WORD : integer := 7;
  -- 6 - missed_pack;
  constant MIS_PACK : integer := 6;
  -- 5 - missed_index;
  constant MIS_INDEX : integer := 5;
  -- 4 - sets_not_match; 
  constant SETS_NM : integer := 4;
  -- 3 - big_delay_output;
  constant BD_OUT : integer := 3; 
  -- 2 - big_delay_input;
  constant BD_IN : integer := 2;
  -- 1 - end_field_word;
  constant EF_WORD : integer := 1;
  -- 0 - end_field_desc;
  constant EF_DESC : integer := 0;

  type TestWarning is (invalid_desk, missed_data, missed_pack, missed_index, sets_not_match, big_delay_output, big_delay_input, end_field_word, end_field_desc, none);

  type TestError is (invalid_desk, missed_data, missed_pack, missed_index, sets_not_match, big_delay_output, big_delay_input, end_field_word, end_field_desc, none);
  
  type RecordOfWarning is record 
    time : integer;
    test : integer;
    value : TestWarning;
  end record RecordOfWarning;
  
  type RecordOfError is record 
    time : integer;
    test : integer;
    value : TestError;
  end record RecordOfError;
  
  constant MAXWARNING : integer := 64;
  
  type WarningArray is array (0 to MAXWARNING - 1) of RecordOfWarning;
  
  signal BuffWarning : WarningArray := (others => (0, 0, TestWarning'(none)));
  signal pWarning : integer := 0;

  procedure ReportWarning
  (
    variable war : in TestWarning; 
    signal pWar : inout integer; 
    signal buffWar : out WarningArray
  ) is
  begin
    buffWar(pWar) <= (test_pulse_cnt, test_cnt, war);
    pWar <= (pWar + 1) mod MAXWARNING;
  end procedure ReportWarning;

  constant MAXERROR : integer := 64;

  type ErrorArray is array (0 to MAXERROR - 1) of RecordOfError;

  signal BuffError : ErrorArray := (others => (0,0, TestError'(none)));
  signal pError : integer := 0;

  procedure ReportError
  (
    variable err : in TestError;
    signal pErr : inout integer;
    signal buffErr : out ErrorArray
  ) is
  begin
    buffErr(pErr) <= (test_pulse_cnt, test_cnt, err);
    pErr <= (pErr + 1) mod MAXERROR;
  end procedure ReportError ;

  procedure RecordProblems 
  (
    signal option : in std_logic_vector(8 downto 0);
    signal flags : in std_logic_vector(8 downto 0);
    signal pWar : inout integer; 
    signal buffWar : out WarningArray;
    signal pErr : inout integer;
    signal buffErr : out ErrorArray
  ) is 
  begin

    if flags(INV_DESK) = REC then
      if option(INV_DESK) = ERR then
        ReportError(TestError'(invalid_desk), pErr, buffErr);
      else
        ReportWarning(TestWarning'(invalid_desk), pWar, buffWar);
      end if;
    end if;

    if flags(INV_WORD) = REC then
      if option(INV_WORD) = ERR then
        ReportError(TestError'(missed_data), pErr, buffErr);
      else
        ReportWarning(TestWarning'(missed_data), pWar, buffWar);
      end if;
    end if;

    if flags(MIS_PACK) = REC then
      if option(MIS_PACK) = ERR then
        ReportError(TestError'(missed_pack), pErr, buffErr);
      else
        ReportWarning(TestWarning'(missed_pack), pWar, buffWar);
      end if;
    end if;

    if flags(MIS_INDEX) = REC then
      if option(MIS_INDEX) = ERR then
        ReportError(TestError'(missed_index), pErr, buffErr);
      else
        ReportWarning(TestWarning'(missed_index), pWar, buffWar);
      end if;
    end if;

    if flags(SETS_NM) = REC then
      if option(SETS_NM) = ERR then
        ReportError(TestError'(sets_not_match), pErr, buffErr);
      else
        ReportWarning(TestWarning'(sets_not_match), pWar, buffWar);
      end if;
    end if;

    if flags(BD_OUT) = REC then
      if option(BD_OUT) = ERR then
        ReportError(TestError'(big_delay_output), pErr, buffErr);
      else
        ReportWarning(TestWarning'(big_delay_output), pWar, buffWar);
      end if;
    end if;    

    if flags(BD_IN) = REC then
      if option(BD_IN) = ERR then
        ReportError(TestError'(big_delay_input), pErr, buffErr);
      else
        ReportWarning(TestWarning'(big_delay_input), pWar, buffWar);
      end if;
    end if;    

    if flags(EF_WORD) = REC then
      if option(EF_WORD) = ERR then
        ReportError(TestError'(end_field_word), pErr, buffErr);
      else
        ReportWarning(TestWarning'(end_field_word), pWar, buffWar);
      end if;
    end if;   

    if flags(EF_DESC) = REC then
      if option(EF_DESC) = ERR then
        ReportError(TestError'(end_field_desc), pErr, buffErr);
      else
        ReportWarning(TestWarning'(end_field_desc), pWar, buffWar);
      end if;
    end if;
    
  end procedure RecordProblems;

  signal rand_num : integer := 0;
  signal rand_std_lv0 : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(0, 8));
  signal rand_std_lv1 : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(0, 8)); 
  signal rand_std_lv2 : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(0, 8));
  signal rand_std_lv3 : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(0, 8));
  signal rand_flags : std_logic_vector(0 to 3) := std_logic_vector(to_unsigned(0,4));
  signal rand_std_l : std_logic := '0';
  subtype TestSuite is std_logic_vector(0 to 5);
  signal test_includedTest : TestSuite := "111111";
  type StateTestEnvironment is (off, set, run, error);

  signal net_controller_state : StateTestEnvironment := StateTestEnvironment'(off);

  signal memory_state : StateTestEnvironment := StateTestEnvironment'(off);

  type TestStage is (start, set, run, assertData, userMode, finish);

  signal test_stage : TestStage := TestStage'(set);

  type TestResult is (successful, failed);

  constant MEMORYSIZE : integer := 1024;-- byte 
  
  subtype word64 is std_logic_vector(63 downto 0);
  
  type Memory is array (0 to MEMORYSIZE / 8 - 1) of word64; 

  signal memoryOfResult : Memory := (others => std_logic_vector(to_unsigned(0, 64)));
  signal correctOfMemory : Memory := (others => std_logic_vector(to_unsigned(0, 64)));
     
  impure function CheckedResultTest return boolean is
    variable result : boolean := true;  
  begin
    for i in correctOfMemory'range loop
      if correctOfMemory(i) /= memoryOfResult(i) then
        result := false;
      end if;
    end loop;
    return result;
  end function CheckedResultTest; 

  procedure Finish 
  (
    signal test_index : inout integer;
    signal set_enable_test : in TestSuite;
    signal stop_test : out boolean;
    signal now_stage : out TestStage
  ) is
    variable iterator : integer := 0;
  begin
    iterator := test_index + 1;
    searchNextIncludedTest : while set_enable_test'length > iterator and set_enable_test(iterator) = '0' loop
      iterator := iterator + 1;
    end loop; -- searchNextIncludedTest
    
    if iterator = set_enable_test'length then
      stop_test <= true;
    end if;
    test_index <= iterator;
    now_stage <= TestStage'(set) after PULSE;
  end procedure Finish;

  procedure Reset
  (
    signal rst : out std_logic
  ) is
  begin 
    rst <= '1', '0' after PULSE;
    wait for PULSE;
  end procedure Reset;
  
  signal user_mode_flag : std_logic := '0';

  procedure UnlockUserMode 
  (
    signal key : out std_logic
  ) is
  begin
    key <= '1';
  end procedure UnlockUserMode;


  procedure AssertData 
  (
    signal next_stage : out TestStage;
    variable test_result : out TestResult;
    signal flag_error : out std_logic_vector(8 downto 0)
  ) is 
  begin
    if CheckedResultTest then
      test_result := TestResult'(successful);
    else
      test_result := TestResult'(failed);
      flag_error(SETS_NM) <= REC;
    end if;
    flag_error <= std_logic_vector(to_unsigned(0, 9)) after 2*PULSE;
    next_stage <= TestStage'(finish) after PULSE;
  end procedure AssertData;

  procedure SetError
  (
    signal vectorErrors : out std_logic_vector(8 downto 0);
    constant indexError : in integer
  ) is
  begin
    vectorErrors(indexError) <= ERR;
  end procedure SetError;

  procedure TestSet
  (
    signal pack : out integer;
    signal word : out integer;
    constant numPack : in integer := 3;
    constant numWord : in integer := 7
  ) is
  begin
    pack <= numPack;
    word <= numWord;
  end procedure TestSet;

  procedure SetIntervalNetController 
  (
    signal minIntervalWord : out integer;
    signal maxIntervalWord : out integer;
    signal minIntervalPack : out integer;
    constant minIW : in integer := 0;
    constant maxIW : in integer := 3;
    constant minIP : in integer := 0
  ) is
  begin
    minIntervalWord <= minIW;
    maxIntervalWord <= maxIW;
    minIntervalPack <= minIP;
  end procedure SetIntervalNetController;

  procedure SetIntervalMemory
  (
    signal minInterval : out integer;
    signal maxInterval : out integer;
    constant min : in integer := 0;
    constant max : in integer := 3
  ) is
  begin
    minInterval <= min;
    maxInterval <= max;
  end procedure SetIntervalMemory;

  signal bus_input : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  signal bus_flags : std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(0, 4));
  signal bus_address : std_logic_vector(19 downto 0) := std_logic_vector(to_unsigned(0, 20)); 
  signal bus_output : std_logic_vector(63 downto 0) := std_logic_vector(to_unsigned(0, 64));
  signal bus_address_reg : std_logic_vector(2 downto 0) := std_logic_vector(to_unsigned(0, 3));
  signal bus_data_in_reg : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  signal bus_data_out_reg : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  signal wire_ready : std_logic := '0';
  signal wire_valid : std_logic := '0';
  signal wire_write : std_logic := '0';
  signal wire_memory : std_logic := '0';
  signal wire_IO_reg : std_logic := '0';
  signal wire_interrupt_first : std_logic := '0';
  signal wire_interrupt_second : std_logic := '0';
  signal wire_interrupt_pack : std_logic := '0';
  signal DMA_interupt_vector : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  -- set bit of interupt vector
  -- 31 - 8 reserv
  signal test_bus_data : std_logic_vector(63 downto 0) := std_logic_vector(to_unsigned(0, 64));
  signal double_test_bus_data : std_logic_vector(63 downto 0) := std_logic_vector(to_unsigned(0, 64));
  signal test_bus_address : std_logic_vector(19 downto 0) := std_logic_vector(to_unsigned(0, 20));
  signal double_test_bus_address : std_logic_vector(19 downto 0) := std_logic_vector(to_unsigned(0, 20));
  signal test_address : std_logic_vector(2 downto 0) := std_logic_vector(to_unsigned(0,3));
  signal test_data : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  signal test_write : std_logic := '0';
  signal double_test_write : std_logic := '0';
  signal test_wire_write : std_logic := '0';
  constant GIE  : integer := 7;-- 7 - GIE[group interrupt enable] 1 - enable all interrupt, 0 - disable all interrupt 
  constant FFIE : integer := 6; -- 6 - FFIE[field first end interrupt enable] 1 - enable, 0 - disable
  constant FSIE : integer := 5;-- 5 - FSIE[field second end interrupt enable] 1 - enable, 0 - disable
  constant RPIE : integer := 4;-- 4 - RPIE[reception pack interrupt enable] 1 - enable, 0 - disable
  --3 reserv
  constant FFIF : integer := 2;-- 2 - FFIF[field first interrupt flag] 1 - interrupt call, when the current address is behind the first field, 0 - rst by software
  constant FSIF : integer := 1;-- 1 - FSIF[field second interrupt flag] 1 - interrupt call, when the current address is behind the second field, 0 - rst by software
  constant RPIF : integer := 0;-- 0 - RPIF[reception pack interrupt flag] 1 - interrupt call, when DMA wrote the whole packet in, 0 - rst by hardware, when record in all field possible

  constant pSAFF : std_logic_vector(2 downto 0) := "001";
  constant pSZFF : std_logic_vector(2 downto 0) := "010";
  constant pCAFF : std_logic_vector(2 downto 0) := "011";
  constant pSASF : std_logic_vector(2 downto 0) := "101";
  constant pSZSF : std_logic_vector(2 downto 0) := "110";
  constant pCASF : std_logic_vector(2 downto 0) := "111";
  constant pFREG : std_logic_vector(2 downto 0) := "000";

  signal indexWord : integer := 0; 
  signal indexDesc : integer := 0;

  
  procedure SetDMA 
  (
    signal indexW : out integer;
    signal indexD : out integer;
    signal address : out std_logic_vector(2 downto 0);
    signal bus_data_in_reg : out std_logic_vector(31 downto 0);
    signal data_out : in std_logic_vector(31 downto 0);
    signal write : out std_logic;
    signal inside_bus_data_in_reg : out std_logic_vector(31 downto 0);
    signal inside_address : out std_logic_vector(2 downto 0);
    signal inside_write : out std_logic;
    constant FREG : in std_logic_vector(7 downto 0);
    constant SAFF : in integer := 0;
    constant SZFF : in integer := 0;
    constant CAFF : in integer := 20;
    constant SASF : in integer := 20;
    constant SZSF : in integer := 20;
    constant CASF : in integer := 20;
    constant delay_set_DMA : in integer := 0
  ) is
    variable delay : time := PULSE * delay_set_DMA;
  begin
    indexW <= CAFF;
    indexD <= CASF;
    
    inside_write <= '1', '0' after 7*PULSE;
    inside_bus_data_in_reg <= std_logic_vector(to_unsigned(0,24)) & FREG, std_logic_vector(to_unsigned(SAFF, 32)) after 1*PULSE, std_logic_vector(to_unsigned(SZFF, 32)) after 2*PULSE, std_logic_vector(to_unsigned(CAFF, 32)) after 3*PULSE, std_logic_vector(to_unsigned(SASF, 32)) after 4*PULSE, std_logic_vector(to_unsigned(CASF, 32)) after 5*PULSE, std_logic_vector(to_unsigned(SZSF, 32)) after 6*PULSE;
    inside_address <= pFREG, pSAFF after 1*PULSE, pSZFF after 2*PULSE, pCAFF after 3*PULSE, pSASF after 4*PULSE, pCASF after 5*PULSE, pSZSF after 6*PULSE;

    write <= '1' after delay, '0' after 7*PULSE + delay;
    address <= pFREG after delay, pSAFF after 1*PULSE + delay, pSZFF after 2*PULSE + delay, pCAFF after 3*PULSE + delay, pSASF after 4*PULSE + delay, pCASF after 5*PULSE + delay, pSZSF after 6*PULSE + delay;
    bus_data_in_reg <= std_logic_vector(to_unsigned(0,24)) & FREG after delay, std_logic_vector(to_unsigned(SAFF, 32)) after 1*PULSE + delay, std_logic_vector(to_unsigned(SZFF, 32)) after 2*PULSE + delay, std_logic_vector(to_unsigned(CAFF, 32)) after 3*PULSE + delay, std_logic_vector(to_unsigned(SASF, 32)) after 4*PULSE + delay, std_logic_vector(to_unsigned(CASF, 32)) after 5*PULSE + delay, std_logic_vector(to_unsigned(SZSF, 32)) after 6*PULSE + delay; 
  end procedure SetDMA;

  procedure EndSet 
  ( 
    signal now_stage : out TestStage
  ) is
  begin
    now_stage <= TestStage'(start) after 7 * PULSE;
  end EndSet;

  constant EOP : std_logic_vector(7 downto 0) := X"00";
  constant EEP : std_logic_vector(7 downto 0) := X"01";
  constant FILL : std_logic_vector(7 downto 0) := X"02";

  procedure StartTest
  (
    signal now_stage : out TestStage;
    signal mem_state : out StateTestEnvironment;
    signal net_state : out StateTestEnvironment

  ) is
  begin
    now_stage <= TestStage'(run) after 2*PULSE;
    mem_state <= StateTestEnvironment'(set), StateTestEnvironment'(run) after 2*PULSE;
    net_state <= StateTestEnvironment'(set), StateTestEnvironment'(run) after 2*PULSE;
  end procedure StartTest;

  procedure ResetConfigTest
  (
    signal errorVector : out std_logic_vector(8 downto 0);
    signal interuptVector : out std_logic_vector(31 downto 0)
  ) is
  begin
    errorVector <= std_logic_vector(to_unsigned(0,9));
    interuptVector <= std_logic_vector(to_unsigned(0,32));
  end procedure ResetConfigTest;

  procedure RunTest
  (
    signal next_stage : out TestStage;
    signal finish_test : in std_logic;
    signal index_error : in integer;
    signal key_user_mode : inout std_logic; 
    signal memory_state : out StateTestEnvironment;
    signal net_controller_state : out StateTestEnvironment
  ) is
  begin
    wait until (index_error'event or finish_test'event);
    if key_user_mode = '1' then
      next_stage <= TestStage(userMode) after PULSE;
    else 
      next_stage <= TestStage(assertData) after PULSE;
    end if;
    net_controller_state <= StateTestEnvironment'(off);
    memory_state <= StateTestEnvironment'(off);
    key_user_mode <= '0';
  end procedure RunTest;

  component DMA is
    port 
    (
      clk : in std_logic;
      rst : in std_logic;
  
      --interface with network control 
      input : in std_logic_vector(31 downto 0);
      flags : in std_logic_vector(3 downto 0);
      valid : in std_logic;
      ready : out std_logic;
  
      --interface with memory
      data_address : out std_logic_vector(19 downto 0);
      output : out std_logic_vector(63 downto 0);
      write : out std_logic;
      mem_ready : in std_logic;
  
      --service interface 
      address_reg : in std_logic_vector(2 downto 0);
      data_reg_in : in std_logic_vector(31 downto 0);
      data_reg_out : out std_logic_vector(31 downto 0);
      IO_flag : in std_logic;
  
      --interrupt interface 
      INT_FF : out std_logic;
      INT_SF : out std_logic;
      INT_RP : out std_logic
    );
  end component DMA;

begin
  
  myDMA : DMA port map
  (
    clk => test_clk,
    rst => test_rst, 
    input => bus_input,
    flags => bus_flags,
    valid => wire_valid,
    ready => wire_ready,
    data_address => bus_address,
    output => bus_output,
    write => wire_write,
    mem_ready => wire_memory, 
    address_reg => bus_address_reg,
    data_reg_in => bus_data_in_reg,
    data_reg_out => bus_data_out_reg,
    IO_flag => wire_IO_reg, 
    INT_FF => wire_interrupt_first,
    INT_SF => wire_interrupt_second,
    INT_RP => wire_interrupt_pack
  );

  clock : process( test_clk )
  begin
    if test_pulse_cnt = 0 then
      report "------------------------------------ Test of DMA is started -------------------------------------" severity note;
    end if;
    
    if testsCompleted = false then
      if test_clk = '1' then
        test_clk <= '0' after TICK;
        test_pulse_cnt <= test_pulse_cnt + 1;
      else 
        test_clk <= '1' after TICK;
      end if;
    else
      report "------------------------------------ Test of DMA is finished ------------------------------------" severity note;
      assert AllOk /= true report "---------------------------- All tests of testing DMA is successful  ----------------------------" severity note;
      assert AllOk /= false report "-------------- Not all tests of testing DMA is successful, first failed test = " & integer'image(numFirstFailedTest) & " ---------------" severity note;
    end if;
  end process ; -- clock

  
  modelMemory : process( test_clk, test_rst )
  variable SAFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SZFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable CAFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SASF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SZSF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable CASF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  begin
    if( test_rst = '1' ) then
      memoryOfResult <= (others => std_logic_vector(to_unsigned(0, 64)));
      SAFF := std_logic_vector(to_unsigned(0, 32));
      SZFF := std_logic_vector(to_unsigned(0, 32));
      CAFF := std_logic_vector(to_unsigned(0, 32));
      SASF := std_logic_vector(to_unsigned(0, 32));
      SZSF := std_logic_vector(to_unsigned(0, 32));
      CASF := std_logic_vector(to_unsigned(0, 32));
      memory_flag_error <= std_logic_vector(to_unsigned(0, 9));
    elsif( rising_edge(test_clk) ) then
      memory_flag_error <= std_logic_vector(to_unsigned(0, 9));
      if wire_IO_reg = '1' then
        case bus_address_reg is
          when "001" =>
            SAFF := bus_data_in_reg;
          when "010" =>
            SZFF := bus_data_in_reg;
          when "011" =>
            CAFF := bus_data_in_reg;
          when "101" =>
            SASF := bus_data_in_reg;
          when "110" => 
            SZSF := bus_data_in_reg;
          when "111" => 
            CASF := bus_data_in_reg;
          when others =>
        end case;
      end if;
      if wire_memory = '1' and wire_write = '1' then

        memoryOfResult(to_integer(unsigned(bus_address))/8) <= bus_output;  
        if to_integer(unsigned(bus_address)) /= to_integer(unsigned(CAFF)) and to_integer(unsigned(bus_address)) /= to_integer(unsigned(CASF)) then
          memory_flag_error(MIS_INDEX) <= REC;
        end if;

        if to_integer(unsigned(bus_address)) = to_integer(unsigned(CAFF)) then
          if bus_output /= correctOfMemory(to_integer(unsigned(CAFF))/8) then
            memory_flag_error(INV_WORD) <= REC;  
          end if;
          CAFF := std_logic_vector(to_unsigned(to_integer(unsigned(CAFF)) + 8, 32));
        end if;

        if to_integer(unsigned(bus_address)) = to_integer(unsigned(CASF)) then
          if bus_output /= correctOfMemory(to_integer(unsigned(CASF))/8) then
            memory_flag_error(INV_DESK) <= REC;  
          end if;
          CASF := std_logic_vector(to_unsigned(to_integer(unsigned(CASF)) + 8, 32));
        end if;

        if to_integer(unsigned(CASF)) - to_integer(unsigned(SASF)) + 1 > to_integer(unsigned(SZSF)) then
          memory_flag_error(EF_DESC) <= REC;
        end if;

        if to_integer(unsigned(CAFF)) - to_integer(unsigned(SAFF)) + 1 > to_integer(unsigned(SZFF)) then
          memory_flag_error(EF_WORD) <= REC;
        end if;

      end if ;
    end if ;
  end process ; -- modelMemory
  
  collectorData : process( test_clk, test_rst )
  variable SAFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SZFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable CAFF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SASF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable SZSF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  variable CASF : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(0, 32));
  begin
    if( test_rst = '1' ) then
      correctOfMemory <= (others => std_logic_vector(to_unsigned(0, 64)));
      SAFF := std_logic_vector(to_unsigned(0, 32));
      SZFF := std_logic_vector(to_unsigned(0, 32));
      CAFF := std_logic_vector(to_unsigned(0, 32));
      SASF := std_logic_vector(to_unsigned(0, 32));
      SZSF := std_logic_vector(to_unsigned(0, 32));
      CASF := std_logic_vector(to_unsigned(0, 32));
    elsif( rising_edge(test_clk) ) then
      if test_write = '1' then
        case test_address is
          when "001" =>
            SAFF := test_data;
          when "010" =>
            SZFF := test_data;
          when "011" =>
            CAFF := test_data;
          when "101" =>
            SASF := test_data;
          when "110" => 
            SZSF := test_data;
          when "111" => 
            CASF := test_data;
          when others =>
        end case;
      end if;
      if test_wire_write = '1' then 
        correctOfMemory(to_integer(unsigned(test_bus_address))/8) <= test_bus_data;
      end if ;
      if double_test_write = '1' then
        correctOfMemory(to_integer(unsigned(double_test_bus_address))/8) <= double_test_bus_data;
      end if;
    end if ;
  end process ; -- collectorData
  
  random5 : process( test_clk )
  variable x_out : real := 0.0;
  variable seed_1 : positive := 5;
  variable seed_2 : positive := 7; 
  variable rand_int : integer;
  begin
    if( rising_edge(test_clk) ) then
      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_num <= rand_int;

      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_std_lv0 <= std_logic_vector(to_unsigned(rand_int, 8));
      
      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_std_lv1 <= std_logic_vector(to_unsigned(rand_int, 8));
      
      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_std_lv2 <= std_logic_vector(to_unsigned(rand_int, 8));
      
      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_std_lv3 <= std_logic_vector(to_unsigned(rand_int, 8));

      UNIFORM(seed_1, seed_2, x_out);
      rand_int := integer(x_out * 1000.0);
      rand_flags <= std_logic_vector(to_unsigned(rand_int, 4));

      UNIFORM(seed_1, seed_2, x_out);
      if x_out > 0.5 then
        rand_std_l <= '1';
      else 
        rand_std_l <= '0';        
      end if ;
    end if ;
  end process ; -- random5
  
  testEnvironment : process( test_clk, test_rst )
  --section of memory 
  variable memory_delay : integer := 0;
  variable memory_watch_dog : integer := 0;
  variable memory_cnt_pulse : integer := 0;
  variable memory_cnt_word64 : integer := 0;
  variable map_test_end : std_logic := '0';

  --section of net controller 
  variable net_delay : integer := 0;
  variable net_watch_dog : integer := 0;
  variable net_delay_pack : integer := 0;
  variable net_cnt_pulse : integer := 0;
  variable net_num_pack : integer := 0;
  variable net_num_word64 : integer := 0;
  variable num_in_pack : integer := 0;
  variable map_test_transfer_end : std_logic := '0';
  variable f_type_pack : std_logic := '0';
  variable index_word : integer := 0;
  variable index_desc : integer := 0;
  variable lsat_index_desc : integer := 0;
  variable isConfigPack : std_logic := '0';
  variable tmp_flags : std_logic_vector(0 to 3) := std_logic_vector(to_unsigned(0, 4));
  variable net_num_byte : integer := 0;
  variable tmp : std_logic_vector(63 downto 0) := std_logic_vector(to_unsigned(0, 64));
  variable net_iterator_word64 : integer := 7;
  variable net_local_end_pack : std_logic := '0';
  variable net_pack_end : std_logic := '0';
  variable map_valid : std_logic := '0';
  variable repeat_transfer_data : std_logic := '0';
  begin
    if( test_rst = '1' ) then
      memory_delay := 0;
      memory_watch_dog := 0;
      memory_cnt_pulse := 0;
      memory_cnt_word64 := 0;
      test_end <= '0';
      map_test_end := '0';
      wire_memory <= '0';
      lsat_index_desc := 0; 
      net_num_byte := 0;
      tmp := std_logic_vector(to_unsigned(0, 64));
      net_iterator_word64 := 7;
      net_delay := 0;
      net_watch_dog := 0;
      net_cnt_pulse := 0; 
      net_delay_pack := 0;
      test_transfer_end <= '0';
      map_test_transfer_end := '0';
      wire_valid <= '0';
      map_valid := '0';
      
      num_in_pack := 0;
      TE_flag_error <= std_logic_vector(to_unsigned(0, 9));
      f_type_pack := '0';
      index_word := 0;
      index_desc := 0;
      isConfigPack := '0';
    elsif( rising_edge(test_clk) ) then
      TE_flag_error <= std_logic_vector(to_unsigned(0, 9));
      case memory_state is
        when StateTestEnvironment'(off) =>
          
          wire_memory <= '0';
          test_end <= '0';
          map_test_end := '0';
          memory_delay := 0;
          memory_watch_dog := 0;
          memory_cnt_pulse := 0;
          memory_cnt_word64 := 0;
          lsat_index_desc := 0;
        when StateTestEnvironment'(set) =>
          memory_delay := test_min_interval_record;
          memory_watch_dog := test_max_interval_record;
          memory_cnt_pulse := 0;
          memory_cnt_word64 := test_num_pack + test_num_word;
          test_end <= '0';
          map_test_end := '0';
          lsat_index_desc := indexDesc + test_num_pack*8;
        when StateTestEnvironment'(run) =>
            
          if wire_write = '1' and wire_memory = '1' then
            wire_memory <= '0';
            memory_cnt_pulse := 0;
            memory_cnt_word64 := memory_cnt_word64 - 1;
            if memory_cnt_word64 = 0 then
              test_end <= '1';
              map_test_end := '1';
              if lsat_index_desc /= index_desc  then
                TE_flag_error(MIS_PACK) <= REC;
              end if;
            end if;
          end if;
          
          if map_test_end = '0' then
            memory_cnt_pulse := memory_cnt_pulse + 1;
          
            if memory_cnt_pulse >= memory_delay then
              wire_memory <= '1';
            end if;

            if memory_cnt_pulse > memory_watch_dog then
              TE_flag_error(BD_OUT) <= REC;
            end if;
          end if;

        when StateTestEnvironment'(error) =>

        when others =>
      end case;

      if test_wire_write = '1' then
        test_wire_write <= '0';
        double_test_write <= '0';
      end if ;

      case net_controller_state is
        when StateTestEnvironment'(off) =>
          net_delay := 0;
          net_watch_dog := 0;
          net_cnt_pulse := 0; 
          net_delay_pack := 0;
          test_transfer_end <= '0';
          map_test_transfer_end := '0';
          wire_valid <= '0';
          map_valid := '0';
          net_num_pack := 0;
          f_type_pack := '0';
          num_in_pack := 0;
          net_num_word64 := 0;
          isConfigPack := '0';
          tmp := std_logic_vector(to_unsigned(0, 64));
          net_iterator_word64 := 7;
          net_num_byte := 0;
          net_pack_end := '0';
        when StateTestEnvironment'(set) => 
          net_pack_end := '0';
          net_delay := test_min_interval_received_word;
          net_watch_dog := test_max_interval_received_word;
          net_delay_pack := test_min_interval_received_pack;
          test_transfer_end <= '0';
          map_test_transfer_end := '0';
          net_cnt_pulse := 0;
          net_num_pack := test_num_pack;
          net_num_word64 := test_num_word;
          net_num_byte := net_num_word64 * 8;
          index_word := indexWord;
          index_desc := indexDesc;
          tmp := std_logic_vector(to_unsigned(0, 64));
          net_iterator_word64 := 7;
          isConfigPack := '1';
          if net_num_pack > 1 then
            num_in_pack := rand_num/7 mod net_num_byte;
          else
            num_in_pack := net_num_byte;
          end if;
          f_type_pack := rand_std_l;
          test_wire_write <= '1'; 
          test_bus_address <= std_logic_vector(to_unsigned(index_desc, 20));
          test_bus_data <= f_type_pack & std_logic_vector(to_unsigned(num_in_pack, 63));
          index_desc := index_desc + 8;
        when StateTestEnvironment'(run) =>
        
          if  isConfigPack = '0' and  map_test_transfer_end = '0'  then 
            isConfigPack := '1';
            if net_num_pack > 1 then
              num_in_pack := rand_num*8 mod net_num_byte;
            else
              num_in_pack := net_num_byte;
            end if;
            f_type_pack := rand_std_l;
            test_wire_write <= '1'; 
            test_bus_address <= std_logic_vector(to_unsigned(index_desc, 20));
            test_bus_data <= f_type_pack & std_logic_vector(to_unsigned(num_in_pack, 63));
            index_desc := index_desc + 8;
            net_cnt_pulse := 0;
          end if ;

          if wire_ready = '1' and map_valid = '1' then
            wire_valid <= '0';
            map_valid := '0';
            net_cnt_pulse := 0;
          end if ;

          if map_test_transfer_end = '0' then
            net_cnt_pulse := net_cnt_pulse + 1;
            repeat_transfer_data := '0';
            if (num_in_pack > 0 or net_pack_end = '0') then
              if net_cnt_pulse >= net_delay  and map_valid = '0' then
                wire_valid <= '1';
                map_valid := '1';
                tmp_flags := rand_flags;
                for i in rand_flags'range loop
                  if rand_flags(i) = '1' then
                    if num_in_pack > 0 or net_pack_end = '1'  then
                      bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= FILL;
                    else
                      if f_type_pack = '0' then
                        bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= EOP;
                      else 
                        bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= EEP;
                      end if ;
                      net_pack_end := '1';
                    end if ;
                  else
                    if num_in_pack > 0 and net_pack_end = '0' then
                      case i is
                        when 0 =>
                          bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= rand_std_lv0;
                          tmp(63 - 8*(7 - net_iterator_word64) downto 63 - 8*( 7 - net_iterator_word64 + 1) + 1) := rand_std_lv0;
                        when 1 =>
                          bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= rand_std_lv1;
                          tmp(63 - 8*(7 - net_iterator_word64) downto 63 - 8 * ( 7 - net_iterator_word64 + 1) + 1) := rand_std_lv1;
                        when 2 =>
                          bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= rand_std_lv2;
                          tmp(63 - 8*(7 - net_iterator_word64) downto 63 - 8 * ( 7 - net_iterator_word64 + 1) + 1) := rand_std_lv2;
                        when 3 =>
                          bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= rand_std_lv3;
                          tmp(63 - 8*(7 - net_iterator_word64) downto 63 - 8*( 7 - net_iterator_word64 + 1) + 1) := rand_std_lv3;
                        when others =>
                          bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= rand_std_lv0;
                          tmp(63 - 8*(7 - net_iterator_word64) downto 63 - 8 * ( 7 - net_iterator_word64 + 1) + 1) := rand_std_lv0;
                      end case;
                      net_iterator_word64 := net_iterator_word64 - 1;
                      net_num_byte := net_num_byte - 1;
                      num_in_pack := num_in_pack - 1;

                      if net_iterator_word64 = -1 or num_in_pack = 0 then
                        if repeat_transfer_data = '0' then
                          repeat_transfer_data := '1';
                          test_bus_data <= tmp;
                          test_bus_address <= std_logic_vector(to_unsigned(index_word, 20)); 
                        else
                          double_test_bus_data <= tmp;
                          double_test_bus_address <= std_logic_vector(to_unsigned(index_word, 20)); 
                          double_test_write <= '1';
                        end if;
                        if num_in_pack = 0 then
                          net_num_byte := net_num_byte - net_iterator_word64 - 1;
                        end if;
                        net_iterator_word64 := 7;
                        net_num_word64 := net_num_word64 - 1;
                        test_wire_write <= '1';
                        tmp := std_logic_vector(to_unsigned(0, 64));
                        index_word := index_word + 8;
                      end if; 
                    else
                      tmp_flags(i) := '1';
                      bus_input(31 - 8*i downto 31 - 8*(i+1) + 1) <= FILL;
                    end if ;
                  end if ;
                end loop;
                bus_flags <= tmp_flags;
              end if ;

              if net_cnt_pulse > net_watch_dog then
                TE_flag_error(BD_IN) <= REC;
              end if ;
            else 

              if net_cnt_pulse >= net_delay_pack then
                isConfigPack := '0';
                net_num_pack := net_num_pack - 1; 
                net_pack_end := '0';
              end if ;
            end if ; 
          end if;
          
          if net_num_pack = 0 then
            test_transfer_end <= '1';
            map_test_transfer_end := '1';
          end if ;

        when StateTestEnvironment'(error) =>
        when others =>
      end case;
    end if ;
  end process ; -- testEnvironment

  record_error: process(test_clk)
  begin
    if rising_edge(test_clk) then
      RecordProblems(test_enable_error, memory_flag_error, pWarning, BuffWarning, pError, BuffError);
      RecordProblems(test_enable_error, TE_flag_error, pWarning, BuffWarning, pError, BuffError);
      RecordProblems(test_enable_error, user_mode_flag_error, pWarning, BuffWarning, pError, BuffError);
      
      RecordProblems(test_enable_error, assert_data_flag_error, pWarning, BuffWarning, pError, BuffError);
    end if ;
  end process record_error;

  main : process
  variable index_test : integer := 0;
  variable result : TestResult := TestResult'(successful);
  variable cnt_call : integer := 0;
  variable test_run : std_logic := '0';
  begin
    index_test := test_cnt;
    
    if test_run = '0' then
      report "Test #" & integer'image(index_test) & " is started" severity note; 
      test_run := '1';
    end if;

    if test_stage = TestStage'(set) then
      ResetConfigTest(test_enable_error, DMA_interupt_vector);
    end if ;
    
    case test_cnt is
      when 0 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetError(test_enable_error, INV_DESK);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              7, 20
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              0, 4, 5
            );
            TestSet(test_num_pack, test_num_word, 3, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000",
              9, 728, 9, 729, 24, 729, 40
            );
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when 1 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              0, 8
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              0, 4, 0
            );
            TestSet(test_num_pack, test_num_word, 20, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000",
              0, 720, 0, 721, 24, 721
            );
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
          
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when 2 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetError(test_enable_error, INV_DESK);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              0, 8
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              0, 4, 0
            );
            TestSet(test_num_pack, test_num_word, 1, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000",
              0, 720, 0, 721, 24, 721
            );
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
          
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when 3 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetError(test_enable_error, INV_DESK);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              0, 8
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              0, 4, 0
            );
            TestSet(test_num_pack, test_num_word, 3, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000",
              25, 720, 25, 0, 24, 0 
            );
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
          
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when 4 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetError(test_enable_error, INV_DESK);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              0, 8
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              0, 4, 0
            );
            TestSet(test_num_pack, test_num_word, 23, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000", 
              185, 720, 185, 0, 184, 0
            );
            UnlockUserMode(user_mode_flag);
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
            AssertData(test_stage, result, assert_data_flag_error);      
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when 5 =>
        case test_stage is
          when TestStage'(set) =>
            Reset(test_rst);
            SetError(test_enable_error, INV_DESK);
            SetIntervalMemory
            (
              test_min_interval_record,
              test_max_interval_record,
              0, 8
            );
            SetIntervalNetController
            (
              test_min_interval_received_word,
              test_max_interval_received_word,
              test_min_interval_received_pack,
              2, 2, 0
            );
            TestSet(test_num_pack, test_num_word, 3, 90);
            SetDMA
            (
              indexWord,
              indexDesc,
              bus_address_reg,
              bus_data_in_reg,
              bus_data_out_reg,
              wire_IO_reg,
              test_data,
              test_address, 
              test_write,
              "00000000",
              5, 720, 9, 729, 24, 729
            );
            EndSet(test_stage);
          when TestStage'(start) =>
            StartTest(test_stage, memory_state, net_controller_state);
          when TestStage'(run) =>
            RunTest(test_stage, test_end, pError, user_mode_flag, memory_state, net_controller_state);
          when TestStage'(assertData) =>
            AssertData(test_stage, result, assert_data_flag_error);
          when TestStage'(userMode) =>
          
          when TestStage'(finish) =>
            Finish(test_cnt, test_includedTest, testsCompleted, test_stage);
          when others =>
            report "Undefind test stage - " & integer'image(index_test) severity error;
        end case;
      when others =>
        report "Test #" & integer'image(index_test) & " not found" severity error;
    end case;
    
    if result = TestResult'(failed) then
      if numFirstFailedTest = -1 then
        numFirstFailedTest <= index_test;
      end if;
      AllOk <= false;
    end if;

    if test_stage = TestStage'(finish) then
      report "Test #" & integer'image(index_test) & " finished, test - " & TestResult'image(result) severity note;
      test_run := '0'; 
    end if;

    wait until test_stage'event;
  end process ; -- main
  
end architecture ; -- arch