library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith;

entity TestWriter is
  generic
  (
    TICK : time := 200 fs 
  );
end TestWriter ;

architecture main of TestWriter is

  signal test_clk : std_logic := '1';
  signal test_rst : std_logic := '0';
  signal test_cnt : integer := 0;
  signal test_pulse_cnt : integer := 0; 
  signal testsCompleted : boolean := false;
  signal AllOk : boolean := true;
  signal numFirstFailedTest: integer := -1;
  signal test_num_pack : integer := 3;
  signal test_num_word : integer := 3;
  signal test_min_interval_word : integer := 3;
  signal test_max_interval_word : integer := 15;
  signal test_min_interval_pack : integer := 4;
  signal test_max_interval_pack : integer := 14;
  signal test_source_start : std_logic := '0';

  type TypeError is (writerDontRespond,none,writerDontSendPack);

  type MyError is record
    time : integer;
    value : TypeError;
  end record MyError;

  type StateTestModels is (off, run, error);
  signal stateSrc : StateTestModels := StateTestModels'(off);
  constant MAXERRORS : integer := 64;
  type BuffErrors is array (0 to MAXERRORS - 1) of MyError;
  signal stackErrors : BuffErrors;
  signal error_pLast : integer := 0;
  signal error_source : TypeError := TypeError'(none);
  signal error_clr_source : std_logic := '0';
  type TestStage is (start, set, run, assertData, userMode, finish);
  signal test_stage : TestStage := TestStage'(set);

  type TestResult is (successful, failed);
  constant MEMORYNUMWORD : integer := 40;
  type MapMemory is array ( 0 to MEMORYNUMWORD - 1) of std_logic_vector(31 downto 0); 
  signal map_memory : MapMemory;

  signal test_next : std_logic := '0';
  signal test_includedTest: std_logic_vector(0 to 31) := "11111100000000000000000000000000";
  --                                                      01234567890123456789012345678901
  -- zero test is base of test that it running with ignored list includes of tests


  signal bus_SAFF : std_logic_vector(31 downto 0);
  constant pSAFF : std_logic_vector(2 downto 0) := "001";
  signal bus_SZFF : std_logic_vector(31 downto 0);
  constant pSZFF : std_logic_vector(2 downto 0) := "010";
  signal bus_CAFF : std_logic_vector(31 downto 0);
  constant pCAFF : std_logic_vector(2 downto 0) := "011";
  signal bus_SASF : std_logic_vector(31 downto 0);
  constant pSASF : std_logic_vector(2 downto 0) := "101";
  signal bus_SZSF : std_logic_vector(31 downto 0);
  constant pSZSF : std_logic_vector(2 downto 0) := "110";
  signal bus_CASF : std_logic_vector(31 downto 0);
  constant pCASF : std_logic_vector(2 downto 0) := "111";
  signal bus_FREG : std_logic_vector(31 downto 0);
  constant pFREG1 : std_logic_vector(2 downto 0) := "000";
  constant pFREG2 : std_logic_vector(2 downto 0) := "100";

  signal wire_INTFF : std_logic;
  signal wire_INTSF : std_logic;
  signal wire_INTRP : std_logic;
  
  signal wire_mem_ready : std_logic;
  signal wire_writer_ready : std_logic;
  signal wire_valid_data : std_logic;
  signal wire_valid_word : std_logic;
  signal wire_valid_desc : std_logic;
  signal wire_inc_wordA : std_logic;
  signal wire_inc_descA : std_logic;
  signal wire_io_reg : std_logic;
  
  signal bus_address : std_logic_vector(19 downto 0);
  signal bus_data : std_logic_vector(63 downto 0);
  signal bus_word : std_logic_vector(31 downto 0);
  signal bus_case_reg : std_logic_vector(2 downto 0);
  signal bus_value_in_reg : std_logic_vector(31 downto 0);
  signal bus_value_out_reg : std_logic_vector(31 downto 0);

  signal wire_flag_ended_test : std_logic;
  signal wire_clr_mem_error : std_logic := '0';
  signal wire_mem_error : std_logic;
  signal wire_start_mem : std_logic := '0';
  signal test_collector : std_logic_vector(0 to 64 * 20 - 1);
  signal wire_set_mem : std_logic := '0';
  signal wire_mem_set_ready : std_logic;
  signal bus_max_interval : integer := 0;
  signal bus_min_interval : integer := 0;
  signal bus_num_dataWord : integer := 0; 
  
  -- set bit of FREG
  -- 31 - 8 reserv
  constant GIE  : integer := 7;-- 7 - GIE[group interrupt enable] 1 - enable all interrupt, 0 - disable all interrupt 
  constant FFIE : integer := 6; -- 6 - FFIE[field first end interrupt enable] 1 - enable, 0 - disable
  constant FSIE : integer := 5;-- 5 - FSIE[field second end interrupt enable] 1 - enable, 0 - disable
  constant RPIE : integer := 4;-- 4 - RPIE[reception pack interrupt enable] 1 - enable, 0 - disable
  --3 reserv
  constant FFIF : integer := 2;-- 2 - FFIF[field first interrupt flag] 1 - interrupt call, when the current address is behind the first field, 0 - test_rst by software
  constant FSIF : integer := 1;-- 1 - FSIF[field second interrupt flag] 1 - interrupt call, when the current address is behind the second field, 0 - test_rst by software
  constant RPIF : integer := 0;-- 0 - RPIF[reception pack interrupt flag] 1 - interrupt call, when DMA wrote the whole packet in, 0 - test_rst by hardware, when record in all field possible
  -- 24 - 0 reserv
  constant MAXCHECKEDCELL : integer := 40;
  impure function assertEqual(numCheck: in integer) return boolean is
    variable result: boolean := true;
  begin 
    for i in 0 to numCheck - 1 loop
      if map_memory(i) /= test_collector(32*(i) to 32*(i+1) -1) then 
        result := false;
      end if; 
    end loop;
    return result;
  end function assertEqual;

  component writer is
  port 
  (
    clk: in std_logic;
    rst: in std_logic;

    --interface with memory
    address: out std_logic_vector(19 downto 0);--address of the cell to be recorded
    data: out std_logic_vector(63 downto 0); --data for record in memory  
    valid: out std_logic; --sign of recording command
    mem_ready: in std_logic; --sign of ready memory for recording

    --interface with analyzer
    word: in std_logic_vector(31 downto 0);--bus with analyzer
    ready: out std_logic;
    valid_data: in std_logic;-- transmission word data
    valid_desc: in std_logic;-- transmission word desc

    --interface with registers block
    write_desc : out std_logic;
    write_data : out std_logic;

    INTFF : in std_logic; -- interrupt first field ended
    INTSF : in std_logic; -- interrupt second field ended

    CAFF_REG : in std_logic_vector(31 downto 0); -- current address first field    
    CASF_REG : in std_logic_vector(31 downto 0); -- current address second field
    SZSF_REG : in std_logic_vector(31 downto 0) -- when = 0 stopped;
  );
  end component writer;

  component registersBlock is 
  port 
  (
    clk : in std_logic;
    rst : in std_logic;
    
    -- service interface for setting regester
    address : in std_logic_vector(2 downto 0);
    data_in : in std_logic_vector(31 downto 0);
    data_out : out std_logic_vector(31 downto 0);
    write_read : in std_logic;

    -- reg
    SAFF_REG : out std_logic_vector(31 downto 0); -- start address first field; address == 001
    SZFF_REG : out std_logic_vector(31 downto 0); -- size first field;  address == 010
    CAFF_REG : out std_logic_vector(31 downto 0); -- current address first field; address == 011
    SASF_REG : out std_logic_vector(31 downto 0); -- start address second field; address == 101
    SZSF_REG : out std_logic_vector(31 downto 0); -- size second field, record will started by configure that register(SZSF_REG /= 0) ;  address == 110
    CASF_REG : out std_logic_vector(31 downto 0); -- current address second field; address == 111
    FREG_REG : out std_logic_vector(31 downto 0); -- flag reg  address == 000 and 100

    -- sub interface with writer
    write_desc : in std_logic;
    write_data : in std_logic;

    -- interface interrupt
    INTFF : out std_logic; -- interrupt first field ended
    INTSF : out std_logic; -- interrupt second field ended
    INTRP : out std_logic -- interrupt reception pack

  );
  end component registersBlock;

  component modelMemory is
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
    reset_error : in std_logic; -- test_rst state of error
    end_test : out std_logic -- flag end of test
  );
  end component modelMemory;

begin
  myWriter : writer port map
  (
    clk => test_clk,
    rst => test_rst,
    address => bus_address,
    data => bus_data,  
    valid => wire_valid_data,
    mem_ready => wire_mem_ready,
    word => bus_word,
    ready => wire_writer_ready,
    valid_data => wire_valid_word,
    valid_desc => wire_valid_desc,
    write_desc => wire_inc_descA,
    write_data => wire_inc_wordA,
    INTFF => wire_INTFF,
    INTSF => wire_INTSF,
    CAFF_REG => bus_CAFF,
    CASF_REG => bus_CASF,
    SZSF_REG => bus_SZSF
  );

  myRegistersBlock : registersBlock port map
  (
    clk => test_clk,
    rst => test_rst,
    address => bus_case_reg,
    data_in => bus_value_in_reg,
    data_out => bus_value_out_reg,
    write_read => wire_io_reg,
    SAFF_REG => bus_SAFF,
    SZFF_REG => bus_SZFF,
    CAFF_REG => bus_CAFF,
    SASF_REG => bus_SASF,
    SZSF_REG => bus_SZSF,
    CASF_REG => bus_CASF,
    FREG_REG => bus_FREG,
    write_desc => wire_inc_descA,
    write_data => wire_inc_wordA,   
    INTFF => wire_INTFF,
    INTSF => wire_INTSF,
    INTRP => wire_INTRP
  );

  myModelMemory : modelMemory port map
  (
    clk => test_clk, 
    rst => test_rst, 
    ready => wire_mem_ready, 
    comWrite => wire_valid_data,
    data => bus_data,
    address => bus_address, 
    memory => test_collector, 
    settingValid => wire_set_mem, 
    ready_for_set => wire_mem_set_ready, 
    setMaxInterval => bus_max_interval, 
    setMinInterval => bus_min_interval, 
    setNumWord => bus_num_dataWord,
    error => wire_mem_error, 
    start => wire_start_mem, 
    reset_error => wire_clr_mem_error,
    end_test => wire_flag_ended_test
  );

  clock : process( test_clk )
  begin
    if test_pulse_cnt = 0 then
      test_rst <= '1', '0' after TICK;
    end if;
    if testsCompleted = false then
      if test_clk = '1' then
        test_clk <= '0' after TICK;
        test_pulse_cnt <= test_pulse_cnt + 1;
      else 
        test_clk <= '1' after TICK;
      end if;
    else 
    report "------------------------------------ Test of writer is finished ------------------------------------" severity note;
    assert AllOk /= true report "---------------------------- All tests of testing writer is successful  ----------------------------" severity note;
    assert AllOk /= false report "-------------- Not all tests of testing writer is successful, first failed test = " & integer'image(numFirstFailedTest) & " ---------------" severity note;
    end if;
  end process ; -- clock

  source : process( test_clk, test_rst )
  variable map_valid_word : std_logic;
  variable map_valid_desc : std_logic;
  variable cnt_pack : integer;
  variable cnt_word : integer;
  variable watchDog : integer;
  variable delay : integer; 
  variable watchDog_pack : integer;
  variable delay_pack : integer;
  variable desc_p_word : std_logic;
  variable desc_out : std_logic;
  variable cnt_wait : integer;
  variable word_index : integer; 
  variable p_word_mem : integer;
  variable p_desc_mem : integer;
  variable f_pack_end : std_logic;
  variable desc : std_logic_vector(63 downto 0);
  constant SIZESHOP : integer := 256;
  type Words is array (0 to SIZESHOP - 1) of std_logic_vector(31 downto 0); 
  constant WORDSHOP : Words :=
  (
    0   => "10100101101001011010010110100101",
    1   => "00000000000000000000000000001111",
    2   => "00000000000000000000000011110000",
    3   => "00000000000000000000000011111111",
    4   => "00000000000000000000111100000000",
    5   => "00000000000000000000111100001111",
    6   => "00000000000000000000111111110000",
    7   => "00000000000000000000111111111111",
    8   => "00000000000000001111000000000000",
    9   => "00000000000000001111000000001111",
    10  => "00000000000000001111000011110000",
    11  => "00000000000000001111000011111111",
    12  => "00000000000000001111111100000000",
    13  => "00000000000000001111111100001111",
    14  => "00000000000000001111111111110000",
    15  => "00000000000000001111111111111111",
    16  => "00000000000011110000000000000000",
    17  => "00000000000011110000000000001111",
    18  => "00000000000011110000000011110000",
    19  => "00000000000011110000000011111111",
    20  => "00000000000011110000111100000000",
    21  => "00000000000011110000111100001111",
    22  => "00000000000011110000111111110000",
    23  => "00000000000011110000111111111111",
    24  => "00000000000011111111000000000000",
    25  => "00000000000011111111000000001111",
    26  => "00000000000011111111000011110000",
    27  => "00000000000011111111000011111111",
    28  => "00000000000011111111111100000000",
    29  => "00000000000011111111111100001111",
    30  => "00000000000011111111111111110000",
    31  => "00000000000011111111111111111111",
    32  => "00000000111100000000000000000000",
    33  => "00000000111100000000000000001111",
    34  => "00000000111100000000000011110000",
    35  => "00000000111100000000000011111111",
    36  => "00000000111100000000111100000000",
    37  => "00000000111100000000111100001111",
    38  => "00000000111100000000111111110000",
    39  => "00000000111100000000111111111111",
    40  => "00000000111100001111000000000000",
    41  => "00000000111100001111000000001111",
    42  => "00000000111100001111000011110000",
    43  => "00000000111100001111000011111111",
    44  => "00000000111100001111111100000000",
    45  => "00000000111100001111111100001111",
    46  => "00000000111100001111111111110000",
    47  => "00000000111100001111111111111111",
    48  => "00000000111111110000000000000000",
    49  => "00000000111111110000000000001111",
    50  => "00000000111111110000000011110000",
    51  => "00000000111111110000000011111111",
    52  => "00000000111111110000111100000000",
    53  => "00000000111111110000111100001111",
    54  => "00000000111111110000111111110000",
    55  => "00000000111111110000111111111111",
    56  => "00000000111111111111000000000000",
    57  => "00000000111111111111000000001111",
    58  => "00000000111111111111000011110000",
    59  => "00000000111111111111000011111111",
    60  => "00000000111111111111111100000000",
    61  => "00000000111111111111111100001111",
    62  => "00000000111111111111111111110000",
    63  => "00000000111111111111111111111111",
    64  => "00001111000000000000000000000000",
    65  => "00001111000000000000000000001111",
    66  => "00001111000000000000000011110000",
    67  => "00001111000000000000000011111111",
    68  => "00001111000000000000111100000000",
    69  => "00001111000000000000111100001111",
    70  => "00001111000000000000111111110000",
    71  => "00001111000000000000111111111111",
    72  => "00001111000000001111000000000000",
    73  => "00001111000000001111000000001111",
    74  => "00001111000000001111000011110000",
    75  => "00001111000000001111000011111111",
    76  => "00001111000000001111111100000000",
    77  => "00001111000000001111111100001111",
    78  => "00001111000000001111111111110000",
    79  => "00001111000000001111111111111111",
    80  => "00001111000011110000000000000000",
    81  => "00001111000011110000000000001111",
    82  => "00001111000011110000000011110000",
    83  => "00001111000011110000000011111111",
    84  => "00001111000011110000111100000000",
    85  => "00001111000011110000111100001111",
    86  => "00001111000011110000111111110000",
    87  => "00001111000011110000111111111111",
    88  => "00001111000011111111000000000000",
    89  => "00001111000011111111000000001111",
    90  => "00001111000011111111000011110000",
    91  => "00001111000011111111000011111111",
    92  => "00001111000011111111111100000000",
    93  => "00001111000011111111111100001111",
    94  => "00001111000011111111111111110000",
    95  => "00001111000011111111111111111111",
    96  => "00001111111100000000000000000000",
    97  => "00001111111100000000000000001111",
    98  => "00001111111100000000000011110000",
    99  => "00001111111100000000000011111111",
    100 => "00001111111100000000111100000000",
    101 => "00001111111100000000111100001111",
    102 => "00001111111100000000111111110000",
    103 => "00001111111100000000111111111111",
    104 => "00001111111100001111000000000000",
    105 => "00001111111100001111000000001111",
    106 => "00001111111100001111000011110000",
    107 => "00001111111100001111000011111111",
    108 => "00001111111100001111111100000000",
    109 => "00001111111100001111111100001111",
    110 => "00001111111100001111111111110000",
    111 => "00001111111100001111111111111111",
    112 => "00001111111111110000000000000000",
    113 => "00001111111111110000000000001111",
    114 => "00001111111111110000000011110000",
    115 => "00001111111111110000000011111111",
    116 => "00001111111111110000111100000000",
    117 => "00001111111111110000111100001111",
    118 => "00001111111111110000111111110000",
    119 => "00001111111111110000111111111111",
    120 => "00001111111111111111000000000000",
    121 => "00001111111111111111000000001111",
    122 => "00001111111111111111000011110000",
    123 => "00001111111111111111000011111111",
    124 => "00001111111111111111111100000000",
    125 => "00001111111111111111111100001111",
    126 => "00001111111111111111111111110000",
    127 => "00001111111111111111111111111111",
    128 => "11110000000000000000000000000000",
    129 => "11110000000000000000000000001111",
    130 => "11110000000000000000000011110000",
    131 => "11110000000000000000000011111111",
    132 => "11110000000000000000111100000000",
    133 => "11110000000000000000111100001111",
    134 => "11110000000000000000111111110000",
    135 => "11110000000000000000111111111111",
    136 => "11110000000000001111000000000000",
    137 => "11110000000000001111000000001111",
    138 => "11110000000000001111000011110000",
    139 => "11110000000000001111000011111111",
    140 => "11110000000000001111111100000000",
    141 => "11110000000000001111111100001111",
    142 => "11110000000000001111111111110000",
    143 => "11110000000000001111111111111111",
    144 => "11110000000011110000000000000000",
    145 => "11110000000011110000000000001111",
    146 => "11110000000011110000000011110000",
    147 => "11110000000011110000000011111111",
    148 => "11110000000011110000111100000000",
    149 => "11110000000011110000111100001111",
    150 => "11110000000011110000111111110000",
    151 => "11110000000011110000111111111111",
    152 => "11110000000011111111000000000000",
    153 => "11110000000011111111000000001111",
    154 => "11110000000011111111000011110000",
    155 => "11110000000011111111000011111111",
    156 => "11110000000011111111111100000000",
    157 => "11110000000011111111111100001111",
    158 => "11110000000011111111111111110000",
    159 => "11110000000011111111111111111111",
    160 => "11110000111100000000000000000000",
    161 => "11110000111100000000000000001111",
    162 => "11110000111100000000000011110000",
    163 => "11110000111100000000000011111111",
    164 => "11110000111100000000111100000000",
    165 => "11110000111100000000111100001111",
    166 => "11110000111100000000111111110000",
    167 => "11110000111100000000111111111111",
    168 => "11110000111100001111000000000000",
    169 => "11110000111100001111000000001111",
    170 => "11110000111100001111000011110000",
    171 => "11110000111100001111000011111111",
    172 => "11110000111100001111111100000000",
    173 => "11110000111100001111111100001111",
    174 => "11110000111100001111111111110000",
    175 => "11110000111100001111111111111111",
    176 => "11110000111111110000000000000000",
    177 => "11110000111111110000000000001111",
    178 => "11110000111111110000000011110000",
    179 => "11110000111111110000000011111111",
    180 => "11110000111111110000111100000000",
    181 => "11110000111111110000111100001111",
    182 => "11110000111111110000111111110000",
    183 => "11110000111111110000111111111111",
    184 => "11110000111111111111000000000000",
    185 => "11110000111111111111000000001111",
    186 => "11110000111111111111000011110000",
    187 => "11110000111111111111000011111111",
    188 => "11110000111111111111111100000000",
    189 => "11110000111111111111111100001111",
    190 => "11110000111111111111111111110000",
    191 => "11110000111111111111111111111111",
    192 => "11111111000000000000000000000000",
    193 => "11111111000000000000000000001111",
    194 => "11111111000000000000000011110000",
    195 => "11111111000000000000000011111111",
    196 => "11111111000000000000111100000000",
    197 => "11111111000000000000111100001111",
    198 => "11111111000000000000111111110000",
    199 => "11111111000000000000111111111111",
    200 => "11111111000000001111000000000000",
    201 => "11111111000000001111000000001111",
    202 => "11111111000000001111000011110000",
    203 => "11111111000000001111000011111111",
    204 => "11111111000000001111111100000000",
    205 => "11111111000000001111111100001111",
    206 => "11111111000000001111111111110000",
    207 => "11111111000000001111111111111111",
    208 => "11111111000011110000000000000000",
    209 => "11111111000011110000000000001111",
    210 => "11111111000011110000000011110000",
    211 => "11111111000011110000000011111111",
    212 => "11111111000011110000111100000000",
    213 => "11111111000011110000111100001111",
    214 => "11111111000011110000111111110000",
    215 => "11111111000011110000111111111111",
    216 => "11111111000011111111000000000000",
    217 => "11111111000011111111000000001111",
    218 => "11111111000011111111000011110000",
    219 => "11111111000011111111000011111111",
    220 => "11111111000011111111111100000000",
    221 => "11111111000011111111111100001111",
    222 => "11111111000011111111111111110000",
    223 => "11111111000011111111111111111111",
    224 => "11111111111100000000000000000000",
    225 => "11111111111100000000000000001111",
    226 => "11111111111100000000000011110000",
    227 => "11111111111100000000000011111111",
    228 => "11111111111100000000111100000000",
    229 => "11111111111100000000111100001111",
    230 => "11111111111100000000111111110000",
    231 => "11111111111100000000111111111111",
    232 => "11111111111100001111000000000000",
    233 => "11111111111100001111000000001111",
    234 => "11111111111100001111000011110000",
    235 => "11111111111100001111000011111111",
    236 => "11111111111100001111111100000000",
    237 => "11111111111100001111111100001111",
    238 => "11111111111100001111111111110000",
    239 => "11111111111100001111111111111111",
    240 => "11111111111111110000000000000000",
    241 => "11111111111111110000000000001111",
    242 => "11111111111111110000000011110000",
    243 => "11111111111111110000000011111111",
    244 => "11111111111111110000111100000000",
    245 => "11111111111111110000111100001111",
    246 => "11111111111111110000111111110000",
    247 => "11111111111111110000111111111111",
    248 => "11111111111111111111000000000000",
    249 => "11111111111111111111000000001111",
    250 => "11111111111111111111000011110000",
    251 => "11111111111111111111000011111111",
    252 => "11111111111111111111111100000000",
    253 => "11111111111111111111111100001111",
    254 => "11111111111111111111111111110000",
    255 => "11111111111111111111111111111111"
  ); 
  begin
    if( test_rst = '1' ) then
      cnt_pack := test_num_pack;
      cnt_word := test_num_word;
      watchDog := test_max_interval_word;
      delay := test_min_interval_word;
      watchDog_pack := test_max_interval_pack;
      delay_pack := test_min_interval_pack;
      desc_p_word := '1'; 
      cnt_wait := 0;
      word_index := 0;
      desc_out := '0';
      desc := std_logic_vector(to_unsigned(0, 64));
      bus_word <= std_logic_vector(to_unsigned(0, 32));
      wire_valid_word <= '0';
      map_valid_word := '0';
      wire_valid_desc <= '0';
      map_valid_desc := '0';
      p_word_mem := to_integer(unsigned(bus_CAFF) srl 2);
      p_desc_mem := to_integer(unsigned(bus_CASF) srl 2);
      stateSrc <= StateTestModels'(off);
      f_pack_end := '0';
      for i in 0 to MEMORYNUMWORD - 1 loop
        map_memory(i) <= std_logic_vector(to_unsigned(0, 32));
      end loop;

    elsif( rising_edge(test_clk) ) then
      if test_source_start = '1' then
        cnt_pack := test_num_pack;
        cnt_word := test_num_word;
        watchDog := test_max_interval_word;
        delay := test_min_interval_word;
        watchDog_pack := test_max_interval_pack;
        delay_pack := test_min_interval_pack;
        desc_out := '0';
        f_pack_end := '0';
        p_word_mem := to_integer(unsigned(bus_CAFF) srl 2);
        p_desc_mem := to_integer(unsigned(bus_CASF) srl 2);
        desc_p_word := '1'; 
        cnt_wait := 0;
        desc := "1" & std_logic_vector(to_unsigned(cnt_word, 63));
        bus_word <= std_logic_vector(to_unsigned(0, 32));
        wire_valid_word <= '0';
        map_valid_word := '0';
        wire_valid_desc <= '0';
        map_valid_desc := '0';
        stateSrc <= StateTestModels'(run);
        
        for i in 0 to MEMORYNUMWORD - 1 loop
          map_memory(i) <= std_logic_vector(to_unsigned(0, 32));
        end loop;
      end if;
      if error_clr_source = '1' and stateSrc = StateTestModels'(error) then
        stateSrc <= StateTestModels'(off);
        wire_valid_word <= '0';
        map_valid_word := '0';
        wire_valid_desc <= '0';
        map_valid_desc := '0';
      end if ;
      if stateSrc = StateTestModels'(run) then
        cnt_wait := cnt_wait + 1;

        if cnt_pack /= 0 then

          if (map_valid_word = '1' or map_valid_desc = '1') and wire_writer_ready = '1' then
            wire_valid_word <= '0';
            map_valid_word := '0';
            wire_valid_desc <= '0';
            map_valid_desc := '0';
            cnt_wait := 0;
            if desc_out = '1' then 
              f_pack_end := '1';
              cnt_pack := cnt_pack - 1;
              desc_out := '0';
            end if;
          end if;

          if f_pack_end = '1' then
            if cnt_wait >= delay_pack and map_valid_word = '0' and map_valid_desc = '0' then
              cnt_wait := 0;
              cnt_word := test_num_word;
              f_pack_end := '0';  
            end if;

            if cnt_wait > watchDog_pack then 
              error_source <= TypeError'(writerDontRespond), TypeError'(none) after TICK;
              stateSrc <= StateTestModels'(error);
            end if;
          else
            if cnt_word /= 0 then
              
              if cnt_wait >= delay and map_valid_word = '0' and map_valid_desc = '0' then
                bus_word <= WORDSHOP(word_index);
                wire_valid_word <= '1';
                map_valid_word := '1';
                map_memory(p_word_mem) <= WORDSHOP(word_index); 
                word_index := ((word_index + 1) mod SIZESHOP);
                p_word_mem := p_word_mem + 1;
                cnt_word := cnt_word - 1;
              end if;

              if cnt_wait > watchDog then 
                error_source <= TypeError'(writerDontRespond), TypeError'(none) after TICK;
                stateSrc <= StateTestModels'(error);
              end if;
            else 
              if cnt_wait >= delay and map_valid_word = '0' and map_valid_desc = '0' then
                if desc_p_word = '1' then
                  if p_word_mem mod 2 = 1 then 
                    p_word_mem := p_word_mem + 1;
                  end if;
                  desc_p_word := '0';
                  bus_word <= desc(63 downto 32);
                  map_valid_desc := '1';
                  wire_valid_desc <= '1';
                  map_memory(p_desc_mem) <= desc(63 downto 32);
                  p_desc_mem := p_desc_mem + 1;
                else 
                  desc_p_word := '1';
                  bus_word <= desc(31 downto 0);
                  map_valid_desc := '1';
                  wire_valid_desc <= '1';
                  map_memory(p_desc_mem) <= desc(31 downto 0);
                  p_desc_mem := p_desc_mem + 1;
                  desc_out := '1';
                end if;

                if cnt_wait > watchDog then 
                  error_source <= TypeError'(writerDontRespond), TypeError'(none) after TICK;
                  stateSrc <= StateTestModels'(error);
                end if;
              end if;
            end if;
          end if;
        else 
          stateSrc <= StateTestModels'(off);
          map_valid_word := '0';
          wire_valid_word <= '0';
          map_valid_desc := '0';
          wire_valid_desc <= '0';
        end if;
      end if;
    end if ;
  end process ; -- source
  
  testIterator : process( test_next )
  variable iterator:integer:=0;
  begin
    if test_next = '1' and test_next'event then
      iterator := test_cnt + 1;
      searchNextIncludedTest : while test_includedTest'length > iterator and test_includedTest(iterator) = '0' loop
        iterator := iterator + 1;
      end loop ; -- searchNextIncludedTest
      if iterator = test_includedTest'length then 
        testsCompleted <= true; 
      end if;
      test_cnt <= iterator;
    end if;
  end process; 
  
  recordError: process(error_source, wire_mem_error)
  begin 
    if error_source /= TypeError'(none) or wire_mem_error /= '1' then
      assert error_pLast + 1 < MAXERRORS  report "Stack of error full. Tick = " & integer'image(test_pulse_cnt) severity error;  
      stackErrors(error_pLast).time <= test_pulse_cnt;
      if error_source /= TypeError'(none) then
        stackErrors(error_pLast).value <= error_source;
      else
        if wire_mem_error /= '1' then 
          stackErrors(error_pLast).value <= TypeError'(writerDontSendPack);
        end if;
      end if;
      error_pLast <= (error_pLast + 1) mod MAXERRORS;
    end if;
  end process recordError;
      
  myControl : process( test_clk, test_rst )
  variable index_test : integer := 0;
  variable result : TestResult := TestResult'(successful);
  variable cnt_call : integer := 0;
  variable test_run : std_logic := '0';
  variable f_start : std_logic := '0';
  variable model_start : std_logic := '0';
  begin
    if( test_rst = '1' ) then
      error_clr_source <= '0';
      wire_clr_mem_error <= '0';
    elsif( rising_edge(test_clk) ) then
      assert  f_start /= '0' report "------------------------------------ Test of writer is started -------------------------------------" severity note;
      f_start := '1';
      index_test := test_cnt;
      if test_run = '0' then
        report "Test #" & integer'image(index_test) & " is started" severity note;
        test_run := '1';        
      end if;
      case test_cnt is
        when 0 =>
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  bus_max_interval <= 55;
                  bus_min_interval <= 4;
                  wire_set_mem <= '1';
                  bus_case_reg <= pSAFF;
                  test_num_pack <= 2;
                  test_num_word <= 4;
                  
                  bus_num_dataWord <= 6;
                  test_max_interval_word <= 40;
                  test_min_interval_word <= 3;
                  test_max_interval_pack <= 39;
                  test_min_interval_pack <= 3;

                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 6 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(32, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run);
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;

              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;

              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>

              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              test_stage <= TestStage'(finish);
              result := TestResult'(failed);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
            when others =>
              report "Undefind test stage - " & integer'image(index_test) severity error;
          
          end case;
        when 1 => 
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  bus_max_interval <= 55;
                  bus_min_interval <= 0;
                  wire_set_mem <= '1';
                  bus_case_reg <= pSAFF;
                  test_num_pack <= 2;
                  test_num_word <= 4;
                  
                  bus_num_dataWord <= 6;
                  test_max_interval_word <= 40;
                  test_min_interval_word <= 0;
                  test_max_interval_pack <= 39;
                  test_min_interval_pack <= 0;

                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(64, 32));
                when 6 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(32, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run);
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;

              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;

              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>

              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              test_stage <= TestStage'(finish);
              result := TestResult'(failed);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
            when others =>
              report "Undefind test stage - " & integer'image(index_test) severity error;
          end case;
        when 2 => 
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  wire_set_mem <= '1';
                  
                  test_num_pack <= 3;
                  test_max_interval_pack <= 4;
                  test_min_interval_pack <= 0;
                  
                  test_num_word <= 6;
                  test_max_interval_word <= 2;
                  test_min_interval_word <= 0;
                
                  bus_num_dataWord <= 12;
                  bus_max_interval <= 4;
                  bus_min_interval <= 0;

                  bus_case_reg <= pSAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(72, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 6 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run);
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;

              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;

              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>

              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              test_stage <= TestStage'(finish);
              result := TestResult'(failed);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
            when others =>
              report "Undefind test stage - " & integer'image(index_test) severity error;
          end case;
        when 3 => 
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  wire_set_mem <= '1';
                  
                  test_num_pack <= 3;
                  test_max_interval_pack <= 4;
                  test_min_interval_pack <= 0;
                  
                  test_num_word <= 5;
                  test_max_interval_word <= 2;
                  test_min_interval_word <= 0;
                
                  bus_num_dataWord <= 12;
                  bus_max_interval <= 10;
                  bus_min_interval <= 0;

                  bus_case_reg <= pSAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(72, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 6 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run);
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;

              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;

              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>

              report "Assert failed";
              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              report "User mode failed";
              test_stage <= TestStage'(finish);
              result := TestResult'(failed);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
            when others =>
              report "Undefind test stage - " & integer'image(index_test) severity error;
          end case;
        when 4 => 
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  wire_set_mem <= '1';
                  
                  test_num_pack <= 3;
                  test_max_interval_pack <= 7;
                  test_min_interval_pack <= 0;
                  
                  test_num_word <= 6;
                  test_max_interval_word <= 5;
                  test_min_interval_word <= 0;
                
                  bus_num_dataWord <= 12;
                  bus_max_interval <= 4;
                  bus_min_interval <= 0;

                  bus_case_reg <= pSAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(72, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(32,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 6 => 
                  bus_case_reg <= pFREG1;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                  bus_value_in_reg(FFIE) <= '1';
                  bus_value_in_reg(FSIE) <= '1';
                  bus_value_in_reg(GIE) <= '1';
                  
                when 7 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run);
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;

              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;

              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;

            when TestStage'(assertData) =>

              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              if wire_INTFF = '1' then
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if ;
              test_stage <= TestStage'(finish);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
              wire_clr_mem_error <= '1', '0' after 4 * TICK;
            when others =>
              report "Undefind test stage - " & integer'image(index_test) severity error;
          end case;
        when 5 => 
          case test_stage is
            when TestStage'(set) =>
              cnt_call := cnt_call + 1;
              wire_io_reg <= '1';
              case cnt_call is
                when 1 =>
                  wire_set_mem <= '1';
                  
                  test_num_pack <= 3;
                  test_max_interval_pack <= 7;
                  test_min_interval_pack <= 0;
                  
                  test_num_word <= 6;
                  test_max_interval_word <= 5;
                  test_min_interval_word <= 0;
                
                  bus_num_dataWord <= 12;
                  bus_max_interval <= 4;
                  bus_min_interval <= 0;
  
                  bus_case_reg <= pSAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24, 32));  
                when 2 =>
                  bus_case_reg <= pSZFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(72, 32));
                when 3 => 
                  bus_case_reg <= pCAFF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(24,32));
                  wire_set_mem <= '0';
                when 4 =>
                  bus_case_reg <= pSASF;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 5 => 
                  bus_case_reg <= pCASF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                when 6 => 
                  bus_case_reg <= pFREG1;
                  bus_value_in_reg <= std_logic_vector(to_unsigned(0, 32));
                  bus_value_in_reg(FFIE) <= '1';
                  bus_value_in_reg(FSIE) <= '1';
                  bus_value_in_reg(GIE) <= '1';
                  
                when 7 =>
                  bus_case_reg <= pSZSF; 
                  bus_value_in_reg <= std_logic_vector(to_unsigned(16, 32));
                when others =>
                  test_stage <= TestStage'(start);
                  cnt_call := 0;               
              end case;
              
            when TestStage'(start) =>
              wire_io_reg <= '0';
              test_source_start <= '1', '0' after 4*TICK;
              wire_start_mem <= '1', '0' after 4*TICK; 
              test_stage <= TestStage'(run) after 4*TICK;
            when TestStage'(run) =>  
              if stateSrc = StateTestModels'(error) then 
                test_stage <= TestStage'(userMode);
              end if;
              if wire_mem_error = '1' then 
                test_stage <= TestStage'(userMode);
              end if;
  
              if stateSrc = StateTestModels'(run) and wire_flag_ended_test = '0' then
                model_start := '1';
              end if;
  
              if stateSrc = StateTestModels'(off) and wire_flag_ended_test = '1' and model_start = '1' then
                model_start := '0';
                test_stage <= TestStage'(assertData);
              end if;
  
            when TestStage'(assertData) =>
  
              if assertEqual(MAXCHECKEDCELL) then 
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if;
              test_stage <= TestStage'(finish);
            when TestStage'(userMode) =>
              if wire_INTSF = '1' then
                result := TestResult'(successful);
              else
                result := TestResult'(failed);
              end if ;
              test_stage <= TestStage'(finish);
            when TestStage'(finish)=>
              test_stage <= TestStage'(set);
              test_next <= '1', '0' after 2*TICK;
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
    end if ;
  end process ; -- myControl

end architecture ; -- main
