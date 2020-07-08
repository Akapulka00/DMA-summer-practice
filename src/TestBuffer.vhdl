library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ModuleTestsOfBuffer is
  generic
  (
    TICK: time := 200 fs
  );
end ModuleTestsOfBuffer ;

architecture arch of ModuleTestsOfBuffer is
  --service signal for tests
  signal testsCnt: integer := 0;--counter of test
  signal tickCnt: integer := 0;--counter of takt
  signal testReset: std_logic := '0';--model of rst;
  signal testClk: std_logic := '1';--model of clk;
  signal testsCompleted: boolean:=false;
  signal AllOk:boolean := true; -- flag of passed all test(true - tests success/false - tests fail);

  signal numFirstFailedTest: integer := -1;

  signal numPack:integer := 5;
  signal minIntervalSrc:integer := 3; 
  signal maxIntervalSrc:integer := 15;
  signal minIntervalDst:integer := 3;
  signal maxIntervalDst:integer := 15;
  type StateTestModels is (off, run, error);
  signal stateSrc:StateTestModels := StateTestModels'(off);
  signal stateDst:StateTestModels := StateTestModels'(off);
  
  signal iteratorShopSrc: integer:=0;
  signal int: std_logic := '0';

  signal runSrc: std_logic := '0';
  signal runDst: std_logic := '0';
  signal errRstSrc : std_logic := '0';
  signal errRstDst : std_logic := '0';

  type TypeError is (buffNotReadyMuchTimes, buffNotRespondingMuchTimes, none);
  
  type MyError is record
    time:integer;
    value:TypeError;
  end record MyError;

  constant MAXERRORS: integer:=64;
  type BuffErrors is array (0 to MAXERRORS - 1) of MyError;
  signal stackErrors: BuffErrors;
  signal iLastError: integer := 0;
  signal iErrorS: TypeError := TypeError'(none);
  signal iErrorD: TypeError := TypeError'(none);
  type TestResult is(successful, failed);

  type TestStage is (start, set, run, assertData, userMode, finish);
  signal stageOfTesting: TestStage := TestStage'(set);
  signal nextTest: std_logic := '0'; 

  
  signal includedTest: std_logic_vector(0 to 31):="11111100000000000000000000000000";
  --                                               01234567890123456789012345678901
  -- zero test is base of test that it running with ignored list includes of tests
  
  type TestPack is record
    data: std_logic_vector(31 downto 0);
    servInfo: std_logic_vector(0 to 1);
    nByte: std_logic_vector(1 downto 0);
  end record TestPack;
  type SetOfTestPack is array (integer range <> ) of TestPack; 

  constant MAXNUMCHECKEDPACK: integer:= 128;
  signal keeperPack: SetOfTestPack(0 to MAXNUMCHECKEDPACK-1);
  signal storagePack: SetOfTestPack(0 to MAXNUMCHECKEDPACK-1);
  
  --model of bus between  buffer and analyzer;
  signal dataFromBuff: std_logic_vector(31 downto 0);
  signal servInfoFromBuff: std_logic_vector(0 to 1);
  signal nByteFromBuff: std_logic_vector(1 downto 0);
  signal readyFromBuff: std_logic;
  signal validFromAnalyzer: std_logic;

  --model of bus between buffer and inputBlock;
  signal dataToBuff: std_logic_vector(31 downto 0);
  signal servInfoToBuff: std_logic_vector(0 to 1);
  signal nByteToBuff: std_logic_vector(1 downto 0);
  signal readyFromInputBlock: std_logic;
  signal validFromBuff: std_logic;

  impure function assertEqual(nChecked: in integer) return boolean is
    variable result: boolean := true;
  begin 
    for i in 0 to nChecked - 1 loop
      if storagePack(i) /= keeperPack(i) then 
        result := false;
      end if; 
    end loop;
    return result;
  end function assertEqual;
    
  component packBuffer is 
  port
  (
    rst: in std_logic;
    clk: in std_Logic;

    --interface with analyzer;
    data_out: out std_logic_vector(31 downto 0);
    serv_info_out: out std_logic_vector(0 to 1);
    num_byte_out: out std_logic_vector(1 downto 0);
    ready_anl: in std_logic;
    valid: out std_logic;

    --interface with input block;
    data_in: in std_logic_vector(31 downto 0);
    serv_info_in: in std_logic_vector(0 to 1);
    num_byte_in: in std_logic_vector(1 downto 0);
    ready: out std_logic;
    valid_inb: in std_logic
  );
  end component packBuffer;

begin
  testBuffer:packBuffer port map (
    rst => testReset,
    clk => testClk,

    data_out => dataFromBuff,
    serv_info_out => servInfoFromBuff,
    num_byte_out => nByteFromBuff,
    ready_anl => readyFromBuff,
    valid => validFromAnalyzer,

    data_in => dataToBuff,
    serv_info_in => servInfoToBuff,
    num_byte_in => nByteToBuff,
    ready => readyFromInputBlock,
    valid_inb => validFromBuff
  );

  clock:process(testClk)
  begin
    if tickCnt = 0 then 
      testReset <= '1', '0' after TICK;
    end if;
    if testsCompleted = false then
      if testClk = '1' then 
        testClk <= '0' after TICK;
        tickCnt <= tickCnt + 1;
      else
        testClk <= '1' after TICK;
      end if;
    else
      report "------------------------------------ Test of buffer is finished ------------------------------------" severity note;
      assert AllOk /= true report "---------------------------- All tests of testing buffer is successful  ----------------------------" severity note;
      assert AllOk /= false report "-------------- Not all tests of testing buffer is successful, first failed test = " & integer'image(numFirstFailedTest) & " ---------------" severity note;
    end if;
  end process clock;
  
  source : process(testReset, testClk)
    constant SIZESHOP: integer := 16;
    constant PACKSHOP: SetOfTestPack(0 to SIZESHOP-1) := 
    (
      0 => ("00000000000000000000000000000000", "00", "00"),
      1 => ("00000001000000010000000100000001", "00", "01"),
      2 => ("00000010000000100000001000000010", "00", "10"),
      3 => ("00000011000000110000001100000011", "00", "11"),
      4 => ("00000100000001000000010000000100", "01", "00"),
      5 => ("00000101000001010000010100000101", "01", "01"),
      6 => ("00000110000001100000011000000110", "01", "10"),
      7 => ("00000111000001110000011100000111", "01", "11"),
      8 => ("00001000000010000000100000001000", "10", "00"),
      9 => ("00001001000010010000100100001001", "10", "01"),
     10 => ("00001010000010100000101000001010", "10", "10"),
     11 => ("00001011000010110000101100001011", "10", "11"),
     12 => ("00001100000011000000110000001100", "11", "00"),
     13 => ("00001101000011010000110100001101", "11", "01"),
     14 => ("00001110000011100000111000001110", "11", "10"),
     15 => ("00001111000011110000111100001111", "11", "11")
    );
    variable cntPack: integer;
    variable watchDog: integer;
    variable delay: integer;
    variable cntWait: integer;
    variable dataOnBus: std_logic;
    variable indexKeeper: integer;
  begin
    if testReset = '1' then
      iteratorShopSrc <= 0;
      dataOnBus := '0';
      cntPack := numPack mod MAXNUMCHECKEDPACK;
      watchDog := maxIntervalSrc;
      delay := minIntervalSrc;  
      cntWait := 0;
      indexKeeper := 0;
      validFromBuff <= '0';
      stateSrc <= StateTestModels'(off);
      for i in 0 to MAXNUMCHECKEDPACK-1 loop
        keeperPack(i) <= ("00000000000000000000000000000000", "00", "00");
      end loop;
    else 
      if testClk = '1' and testClk'event then
        if runSrc = '1' then
          cntPack := numPack mod MAXNUMCHECKEDPACK;
          watchDog := maxIntervalSrc;
          delay := minIntervalSrc;  
          dataOnBus := '0';
          cntWait := 0;
          validFromBuff <= '0';
          indexKeeper := 0;
          stateSrc <= StateTestModels'(run);
          for i in 0 to MAXNUMCHECKEDPACK-1 loop
            keeperPack(i) <= ("00000000000000000000000000000000", "00", "00");
          end loop;
        end if;
        if errRstSrc = '1' and stateSrc = StateTestModels'(error) then
          stateSrc <= StateTestModels'(off);
          validFromBuff <= '0';
        end if;
        if stateSrc = StateTestModels'(run) then 
          cntWait := cntWait + 1;
         
          if cntPack /= 0 then 
            if validFromBuff = '1' and readyFromInputBlock = '1' then 
              validFromBuff <= '0';
              dataOnBus := '0';
              cntWait := 0;
            end if;

            if cntWait >= delay and dataOnBus = '0' then 
              dataToBuff <= PACKSHOP(iteratorShopSrc).data;
              servInfoToBuff <= PACKSHOP(iteratorShopSrc).servInfo;
              nByteToBuff <= PACKSHOP(iteratorShopSrc).nByte;
              validFromBuff <= '1';
              dataOnBus := '1';
              keeperPack(indexKeeper) <= PACKSHOP(iteratorShopSrc);
              indexKeeper := indexKeeper + 1;
              cntPack := cntPack - 1;
              iteratorShopSrc <= (iteratorShopSrc + 1) mod SIZESHOP;
            end if;

            if cntWait > watchDog then
              iErrorS <= TypeError'(buffNotReadyMuchTimes), TypeError'(none) after TICK;
              stateSrc <= StateTestModels'(error);
            end if;
          else
            stateSrc <= StateTestModels'(off);
            validFromBuff <= '0';
          end if;
        end if;
      end if;
    end if;
  end process ; -- source

  destination : process(testReset, testClk)
    variable cntPack: integer;
    variable watchDog: integer;
    variable delay: integer;
    variable cntWait: integer;
    variable indexStorage: integer;
    variable iReady: std_logic;
  begin
    if testReset = '1' then
      cntPack := numPack mod MAXNUMCHECKEDPACK;
      watchDog := maxIntervalDst;
      delay := minIntervalDst;  
      cntWait := 0;
      iReady := '0';
      indexStorage := 0;
      readyFromBuff <= '0';
      stateDst <= StateTestModels'(off);
      for i in 0 to MAXNUMCHECKEDPACK-1 loop
        storagePack(i) <= ("00000000000000000000000000000000", "00", "00");
      end loop;
    else 
      if testClk = '1' and testClk'event then
        
        if errRstDst = '1' and stateDst = StateTestModels'(error) then
          stateDst <= StateTestModels'(off);
        end if;
        if runDst = '1' then
          cntPack := numPack mod MAXNUMCHECKEDPACK;
          watchDog := maxIntervalDst;
          delay := minIntervalDst;  
          iReady := '0';
          cntWait := 0;
          stateDst <= StateTestModels'(run);
          readyFromBuff <= '0';
          indexStorage := 0;
          for i in 0 to MAXNUMCHECKEDPACK-1 loop
            storagePack(i) <= ("00000000000000000000000000000000", "00", "00");
          end loop;
        end if;
        if stateDst = StateTestModels'(run) then 
          cntWait := cntWait + 1;
          if cntPack /= 0 then 

            if iReady = '1' and validFromAnalyzer = '1' then 
              iReady := '0';
              readyFromBuff <= '0';
              storagePack(indexStorage).nByte <= nByteFromBuff;
              storagePack(indexStorage).servInfo <= servInfoFromBuff;
              storagePack(indexStorage).data <= dataFromBuff;
              cntPack := cntPack - 1;
              indexStorage := indexStorage + 1;
              cntWait := 0;
            end if;
            if cntWait >= delay then 
              iReady := '1';
              readyFromBuff <= '1';
            end if;
            if cntWait > watchDog then
              iErrorD <= TypeError'(buffNotRespondingMuchTimes), TypeError'(none) after TICK;
              stateDst <= StateTestModels'(error);
            end if;
          else 
            readyFromBuff <= '0';
            iReady := '0';
            stateDst <= StateTestModels'(off);
          end if;
        end if;
      end if ;
    end if;
  end process ; -- destination

  mycontrol:process(testReset, testClk)
    variable indexTest:integer := 0;
    variable result:TestResult := TestResult'(successful);
    variable cntCall: integer := 0;
    variable fStart: std_logic := '0';
    variable modelsIsStart: std_logic := '0';
    
  begin
    if testReset = '1' then
      errRstSrc <= '0';
      errRstDst <= '0';
    else 
      if testClk = '1' and testClk'event then
        assert  fStart /= '0' report "------------------------------------ Test of buffer is started -------------------------------------" severity note;
        fStart := '1';
        indexTest := testsCnt;
        assert stageOfTesting /= TestStage'(set) report "Test #" & integer'image(indexTest) & " is started" severity note;
        case testsCnt is
          when 0 =>
            case stageOfTesting is
              when TestStage'(set) =>
                stageOfTesting <= TestStage'(start);
              when TestStage'(start) =>
                runSrc <= '1', '0' after 4*TICK;
                runDst <= '1', '0' after 4*TICK;
                stageOfTesting <= TestStage'(run);-------
              when TestStage'(run) =>
              
                if stateSrc = StateTestModels'(error) then 
                  stageOfTesting <= TestStage'(userMode); 
                end if;
                
                if stateDst = StateTestModels'(error) then
                  stageOfTesting <= TestStage'(userMode); 
                end if;

                if stateSrc = StateTestModels'(run) and stateDst = StateTestModels'(run) then
                  modelsIsStart := '1';
                end if;

                if stateSrc = StateTestModels'(off) and stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                  modelsIsStart := '0';
                  stageOfTesting <= TestStage'(assertData);
                end if;

              when TestStage'(assertData) =>
                if assertEqual(MAXNUMCHECKEDPACK) then
                  result := TestResult'(successful);
                else
                  result := TestResult'(failed);
                end if;
                stageOfTesting <= TestStage'(finish);
              when TestStage'(userMode) =>
                cntCall := cntCall + 1;
                if cntCall < 4 then 
                  stageOfTesting <= TestStage'(start);
                else 
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(failed);
                end if;
              when TestStage'(finish) => 
                stageOfTesting <= TestStage'(set);
                nextTest <= '1',  '0' after 1*TICK;
              when others =>
                report "Undefind test stage of test - " & integer'image(indexTest) severity error;
            end case; 
          when 1 =>
            case stageOfTesting is
              when TestStage'(set) =>
                numPack <= 10;
                minIntervalSrc <= 5; 
                maxIntervalSrc <= 7;
                minIntervalDst <= 5;
                maxIntervalDst <= 7;
                
                stageOfTesting <= TestStage'(start);
              when TestStage'(start) =>
                runSrc <= '1', '0' after 4*TICK;
                runDst <= '1', '0' after 4*TICK;
                stageOfTesting <= TestStage'(run);
              when TestStage'(run) =>
              
                if stateSrc = StateTestModels'(error) then 
                  stageOfTesting <= TestStage'(userMode); 
                end if;
                
                if stateDst = StateTestModels'(error) then
                  stageOfTesting <= TestStage'(userMode); 
                end if;

                if stateSrc = StateTestModels'(run) and stateDst = StateTestModels'(run) then
                  modelsIsStart := '1';
                end if;

                if stateSrc = StateTestModels'(off) and stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                  modelsIsStart := '0';
                  stageOfTesting <= TestStage'(assertData);
                end if;

              when TestStage'(assertData) =>
                if assertEqual(MAXNUMCHECKEDPACK) then
                  result := TestResult'(successful);
                else
                  result := TestResult'(failed);
                end if;
                stageOfTesting <= TestStage'(finish);
              when TestStage'(userMode) =>
                cntCall := cntCall + 1;
                if cntCall < 4 then 
                  stageOfTesting <= TestStage'(start);
                else 
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(failed);
                end if;
              when TestStage'(finish) => 
                stageOfTesting <= TestStage'(set);
                nextTest <= '1',  '0' after 1*TICK;
              when others =>
                report "Undefind test stage of test - " & integer'image(indexTest) severity error;
            end case; 
        when 2 =>
          case stageOfTesting is
            when TestStage'(set) =>
              numPack <= 10;
              minIntervalSrc <= 1; 
              maxIntervalSrc <= 3;
              minIntervalDst <= 1;
              maxIntervalDst <= 3;
              
              stageOfTesting <= TestStage'(start);
            when TestStage'(start) =>
              runSrc <= '1', '0' after 4*TICK;
              runDst <= '1', '0' after 4*TICK;
              stageOfTesting <= TestStage'(run);
            when TestStage'(run) =>
            
              if stateSrc = StateTestModels'(error) then 
                stageOfTesting <= TestStage'(userMode); 
              end if;
              
              if stateDst = StateTestModels'(error) then
                stageOfTesting <= TestStage'(userMode); 
              end if;

              if stateSrc = StateTestModels'(run) and stateDst = StateTestModels'(run) then
                modelsIsStart := '1';
              end if;

              if stateSrc = StateTestModels'(off) and stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                modelsIsStart := '0';
                stageOfTesting <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>
              if assertEqual(MAXNUMCHECKEDPACK) then
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              stageOfTesting <= TestStage'(finish);
            when TestStage'(userMode) =>
              cntCall := cntCall + 1;
              if cntCall < 4 then 
                stageOfTesting <= TestStage'(start);
              else 
                stageOfTesting <= TestStage'(finish);
                result := TestResult'(failed);
              end if;
            when TestStage'(finish) => 
              stageOfTesting <= TestStage'(set);
              nextTest <= '1',  '0' after 1*TICK;
            when others =>
              report "Undefind test stage of test - " & integer'image(indexTest) severity error;
          end case;
          when 3 =>
            case stageOfTesting is
              when TestStage'(set) =>
                numPack <= 126;
                minIntervalSrc <= 0; 
                maxIntervalSrc <= 3;
                minIntervalDst <= 0;
                maxIntervalDst <= 3;
                stageOfTesting <= TestStage'(start);
              when TestStage'(start) =>
                runSrc <= '1', '0' after 4*TICK;
                runDst <= '1', '0' after 4*TICK;
                stageOfTesting <= TestStage'(run);
              when TestStage'(run) =>
              
                if stateSrc = StateTestModels'(error) then 
                  stageOfTesting <= TestStage'(userMode); 
                end if;
                
                if stateDst = StateTestModels'(error) then
                  stageOfTesting <= TestStage'(userMode); 
                end if;

                if stateSrc = StateTestModels'(run) and stateDst = StateTestModels'(run) then
                  modelsIsStart := '1';
                end if;

                if stateSrc = StateTestModels'(off) and stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                  modelsIsStart := '0';
                  stageOfTesting <= TestStage'(assertData);
                end if;

              when TestStage'(assertData) =>
                if assertEqual(MAXNUMCHECKEDPACK) then
                  result := TestResult'(successful);
                else
                  result := TestResult'(failed);
                end if;
                stageOfTesting <= TestStage'(finish);
              when TestStage'(userMode) =>
                cntCall := cntCall + 1;
                if cntCall < 4 then 
                  stageOfTesting <= TestStage'(start);
                else 
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(failed);
                end if;
              when TestStage'(finish) => 
                stageOfTesting <= TestStage'(set);
                nextTest <= '1',  '0' after 1*TICK;
              when others =>
                report "Undefind test stage of test - " & integer'image(indexTest) severity error;
            end case; 
          when 4 =>
            case stageOfTesting is
              when TestStage'(set) =>
                numPack <= 70;
                minIntervalSrc <= 0; 
                maxIntervalSrc <= 2;
                stageOfTesting <= TestStage'(start);
              when TestStage'(start) =>
                runSrc <= '1', '0' after 4*TICK;
                stageOfTesting <= TestStage'(run);
              when TestStage'(run) =>
              
                if stateSrc = StateTestModels'(error) then 
                  stageOfTesting <= TestStage'(userMode); 
                end if;
                
                if stateDst = StateTestModels'(error) then
                  stageOfTesting <= TestStage'(userMode); 
                end if;

                if stateSrc = StateTestModels'(run) and stateDst = StateTestModels'(run) then
                  modelsIsStart := '1';
                end if;

                if stateSrc = StateTestModels'(off) and stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                  modelsIsStart := '0';
                  stageOfTesting <= TestStage'(assertData);
                end if;

              when TestStage'(assertData) =>
                if assertEqual(MAXNUMCHECKEDPACK) then
                  result := TestResult'(successful);
                else
                  result := TestResult'(failed);
                end if;
                stageOfTesting <= TestStage'(finish);
              when TestStage'(userMode) =>
                if stackErrors(iLastError - 1).value = TypeError'(buffNotReadyMuchTimes) then  
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(successful);
                  errRstSrc <= '1', '0' after 4*TICK;  
                else 
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(failed);
                end if;
              when TestStage'(finish) => 
                stageOfTesting <= TestStage'(set);
                nextTest <= '1',  '0' after 1*TICK;
              when others =>
                report "Undefind test stage of test - " & integer'image(indexTest) severity error;
            end case;
          when 5 =>
            case stageOfTesting is
              when TestStage'(set) =>
                numPack <= 70;
                minIntervalDst <= 0; 
                maxIntervalDst <= 2;
                stageOfTesting <= TestStage'(start);
              when TestStage'(start) =>
                runDst <= '1', '0' after 4*TICK;
                stageOfTesting <= TestStage'(run);
              when TestStage'(run) =>

                if stateDst = StateTestModels'(error) then
                  stageOfTesting <= TestStage'(userMode); 
                end if;

                if stateDst = StateTestModels'(run) then
                  modelsIsStart := '1';
                end if;

                if stateDst = StateTestModels'(off) and modelsIsStart = '1' then
                  modelsIsStart := '0';
                  stageOfTesting <= TestStage'(assertData);
                end if;

              when TestStage'(assertData) =>
                if assertEqual(64) then
                  result := TestResult'(successful);
                else
                  result := TestResult'(failed);
                end if;
                stageOfTesting <= TestStage'(finish);
              when TestStage'(userMode) =>
                if stackErrors(iLastError - 1).value = TypeError'(buffNotRespondingMuchTimes) then  
                  stageOfTesting <= TestStage'(assertData);
                  result := TestResult'(successful);
                else 
                  stageOfTesting <= TestStage'(finish);
                  result := TestResult'(failed);
                end if;
              when TestStage'(finish) => 
                stageOfTesting <= TestStage'(set);
                nextTest <= '1',  '0' after 1*TICK;
              when others =>
                report "Undefind test stage of test - " & integer'image(indexTest) severity error;
            end case;
          when others =>
            report "Test #" & integer'image(indexTest) & " not found" severity error;
        end case;
        if result = TestResult'(failed) then 
          if numFirstFailedTest = -1 then
            numFirstFailedTest <= indexTest;
          end if;
          AllOk <= false;
        end if;
        assert stageOfTesting /= TestStage'(finish) report "Test #" & integer'image(indexTest) & " finished, test - " & TestResult'image(result) severity note;
      end if;
    end if;
  end process mycontrol;

  testIterator : process( nextTest )
  variable iterator:integer:=0;
  begin
    if nextTest = '1' and nextTest'event then
      iterator := testsCnt + 1;
      searchNextIncludedTest : while includedTest'length > iterator and includedTest(iterator) = '0' loop
        iterator := iterator + 1;
      end loop ; -- searchNextIncludedTest
      if iterator = includedTest'length then 
        testsCompleted <= true; 
      end if;
      testsCnt <= iterator;
    end if;
  end process; 

  recordError: process(iErrorS, iErrorD)
    begin 
      if iErrorS /= TypeError'(none) or iErrorD /= TypeError'(none) then
        assert iLastError + 1 < MAXERRORS  report "Stack of error full. Tick = " & integer'image(tickCnt) severity error;  
        stackErrors(iLastError).time <= tickCnt;
        if iErrorS /= TypeError'(none) then
          stackErrors(iLastError).value <= iErrorS;
        else
          if iErrorD /= TypeError'(none) then 
            stackErrors(iLastError).value <= iErrorD;
          end if;
        end if;
        iLastError <= (iLastError + 1) mod MAXERRORS;
      end if;
    end process recordError;

end architecture ; -- arch