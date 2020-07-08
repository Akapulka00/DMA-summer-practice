library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use ieee.math_real.uniform;
use ieee.math_real.floor;

entity modelNetworkController is
  port 
  (
    rst: in std_logic;
    clk: in std_logic;
    
    --interface with test modules
    --subinterface for control of generate package
    NC_INT_IN_TE_setInterval: in integer;--port network controller connected to test environment, he is need for set interval transmission word;
    NC_INT_IN_TE_numWord: in integer;--port network controller connected to test environment, he is need for set quantity word of generate package; 
    NC_SL_IN_TE_generatePackage: in std_logic;--port network controller connected to test environment, he transmission the request of start generation package;
    NC_SL_OUT_TE_ready: out std_logic;--port  network controller connected to test environment, he is need for transmission of readiness network controller for generate package; 

    --subinterface for request data for operation assert 
    NC_SL_IN_TE_requestAssertBuf: in std_logic;--port network controller connected to test environment, he request word for assert to correct proccesing word on input block;
    NC_SL_OUT_TE_validBuf: out std_logic;--port network controller connected to test environment, he transmission the assert, that word on the bus correct;
    NC_SLV_20W_OUT_TE_buf: out std_logic_vector(0 to 32*20 - 1); 

    --interface with input block
    NC_SLV_WR_OUT_IB_word: out std_logic_vector(31 downto 0);--bus between the network controller and input block(data or spec symbols 0-EOF, 1 - EEF,2 - FILL);
    NC_SLV_4BR_OUT_IB_flag: out std_logic_vector(3 downto 0);--4 bit that help make interpretation part(1 byte) of data word, 0-data 1-spec symbols(EOF,EEF,FILL); 
    NC_SL_OUT_IB_valid: out std_logic;--word on bus valid
    NC_SL_IN_IB_ready: in std_logic--input block ready for reception word;
  );
  
end modelNetworkController ;

architecture main of modelNetworkController is
    signal NC_INT_sizeInterval:integer; 
    signal NC_INT_wordNum:integer;
    signal NC_INT_wordCnt:integer;
    signal NC_INT_substate:integer;
    signal NC_INT_state:integer;
    signal NC_INT_indexWord:integer;
    signal NC_INT_indexEndWord:integer;
    signal NC_INT_preIndexWord:integer;
    signal NC_INT_preIndexEndWord:integer;
    signal NC_SL_valid:std_logic;
    signal NC_SL_ready:std_logic;

    --data for assert
    signal NC_INT_packageIterator: integer;
    type BYTE_T is array (integer range <>) of std_logic_vector(7 downto 0);
    signal NC_BYTE_T_80_buf: BYTE_T(0 to 20*4 - 1);

    --end data for assert;
    signal data : std_logic_vector(31 downto 0);
    signal flags : std_logic_vector(3 downto 0);

    constant TICK: time := 200 fs;
    type FLAG_VALUE is array (integer range <>) of integer; 
    type WORD is record
        data:std_logic_vector(31 downto 0);
        flag:std_logic_vector(3 downto 0);
    end record WORD;
    constant SIZE_ARR_WORD: integer := 79;
    constant SIZE_ARR_END_WORD: integer := 64;
    type WORDS is array (integer range <>) of WORD;
    constant NC_ARR_WORD_partOfBodyPackage:WORDS(0 to SIZE_ARR_WORD):=
    (
         0 => ("00000010000000101100000000000010", "1101"), 
         1 => ("00000010000000100000001010000011", "1110"),
         2 => ("00000010000000100001000100000010", "1101"),
         3 => ("11000011111101100000001000000010", "0011"),
         4 => ("00000010000100010110111110100001", "1000"),
         5 => ("00000110000000100000001000000010", "0111"),
         6 => ("01000100000000100000001011001001", "0110"),
         7 => ("00000010000000100000001011100010", "1110"),
         8 => ("00000010011100010010001000000010", "1001"),
         9 => ("00001111000000101010000011100011", "0100"),
        10 => ("00000010000100001100100011000010", "1000"),
        11 => ("00101010000000100000001000000010", "0111"),
        12 => ("00111111000000100000001011011101", "0110"),
        13 => ("00000010100010100010101000000010", "1001"),
        14 => ("01000000000000100111001110111001", "0100"),
        15 => ("00000010000000100111100100000010", "1101"),
        16 => ("11011100000000100000001001111010", "0110"),
        17 => ("00000010011100111101000100000010", "1001"),
        18 => ("00000000000000100000001000000010", "0111"),
        19 => ("00000010000000101111000000000010", "1101"),
        20 => ("00000010001001010000001001110011", "1010"),
        21 => ("01101010001010010000001000000010", "0011"),
        22 => ("11010010000000100000001000000010", "0111"),
        23 => ("00000010000000101000010100000010", "1101"),
        24 => ("00111000000000100010011011001101", "0100"),
        25 => ("01111111001001010000001000001101", "0010"),
        26 => ("11110100010001110011000100000010", "0001"),
        27 => ("00000010000000101000011010111011", "1100"),
        28 => ("01111110100101110000001010100110", "0010"),
        29 => ("00000010011000000100111000000010", "1001"),
        30 => ("00000010000000101110001100000010", "1101"),
        31 => ("00000010000000100000001000000010", "1111"),
        32 => ("00000010111100100000001000000010", "1011"),
        33 => ("01010100000000100000001000000010", "0111"),
        34 => ("00000010011101110001111101010000", "1000"),
        35 => ("00000010000000101110001100000010", "1101"),
        36 => ("01010111000000100000001000000010", "0111"),
        37 => ("00000111000010101101100100000010", "0001"),
        38 => ("00000010010011110000001001000100", "1010"),
        39 => ("00000010000000100000001000000010", "1111"),
        40 => ("10111111000000100000001011101000", "0110"),
        41 => ("10111010010100110001011100010011", "0000"),
        42 => ("00111001111000000000001000101011", "0010"),
        43 => ("00000010101000110000001000000010", "1011"),
        44 => ("00000010100100100000001011110101", "1010"),
        45 => ("01011110000000100110010011010011", "0100"),
        46 => ("00111111000000100000001000000010", "0111"),
        47 => ("10011011110011111010011100000010", "0001"),
        48 => ("00000010000000101001000010101100", "1100"),
        49 => ("00000010101000010000001000000010", "1011"),
        50 => ("00000010010101111101101110010011", "0000"),
        51 => ("11001010000000100000001000000010", "0111"),
        52 => ("10101010010100010000001000000010", "0011"),
        53 => ("00000010010011000000001000000010", "1011"),
        54 => ("00000010101010100000001001110010", "1010"),
        55 => ("11000001001011110010101100101110", "0000"),
        56 => ("00101010111000000000001000000010", "0011"),
        57 => ("00001101000000010000001000000010", "0011"),
        58 => ("00000010000000100000001001101101", "1110"),
        59 => ("00000010001001011110011100000010", "1001"),
        60 => ("00000010000000100000001011001110", "1110"),
        61 => ("11100110000100100010110000000010", "0001"),
        62 => ("00000010110110010001101100100111", "1000"),
        63 => ("11111111000000101001011000000010", "0101"),
        64 => ("01110111010000000000001000111000", "0010"),
        65 => ("00000010000000100111001100000010", "1101"),
        66 => ("11010100000000101010100010010111", "0100"),
        67 => ("00000010000000101001101010010101", "1100"),
        68 => ("01011011000000100010010001100001", "0100"),
        69 => ("01010000101110100000101110111010", "0000"),
        70 => ("00000010000000100000001000000010", "1111"),
        71 => ("10101110011000111110100110110001", "0000"),
        72 => ("00101100010111100010010100111101", "0000"),
        73 => ("00000010000100100010101000000010", "1001"),
        74 => ("00000010011110000110110001000001", "1000"),
        75 => ("00000010000000100010111100000010", "1101"),
        76 => ("11010011111000010000100000000010", "0001"),
        77 => ("01100001000000100000001000000010", "0111"),
        78 => ("10001101000000100000001011100000", "0110"),
        79 => ("01001010110100100000001000000000", "0010") 
    );
    constant NC_ARR_WORD_endPackage:WORDS(0 to SIZE_ARR_END_WORD):=
    (
         0 => ("10101101101101101011011111110011","0000"),
         1 => ("10110000100111001110001100000000","0001"),
         2 => ("00001111010101101101011100000001","0001"),
         3 => ("11001110000001000000000001111000","0010"),
         4 => ("11101000110111110000000110101010","0010"),
         5 => ("01011011100011000000000000000010","0011"),
         6 => ("11110011111010110000000100000010","0011"),
         7 => ("00001001010110000000001000000000","0011"),
         8 => ("01111000101110000000001000000001","0011"),
         9 => ("10101101000000001011011111110011","0100"),
        10 => ("01111100000000011101001010000101","0100"),
        11 => ("00000111000000000010101000000010","0101"),
        12 => ("10100011000000011101001000000010","0101"),
        13 => ("10101010000000100100101000000000","0101"),
        14 => ("10000110000000100011000100000001","0101"),
        15 => ("01000100000000000000001001001011","0110"),
        16 => ("01011010000000010000001010000001","0110"),
        17 => ("01011001000000100000000010011101","0110"),
        18 => ("01000011000000100000000101100011","0110"),
        19 => ("10101111000000000000001000000010","0111"),
        20 => ("11110101000000010000001000000010","0111"),
        21 => ("11001000000000100000000000000010","0111"),
        22 => ("00000001000000100000000100000010","0111"),
        23 => ("10100110000000100000001000000000","0111"),
        24 => ("01100001000000100000001000000001","0111"),
        25 => ("00000000000000000100000100111000","1000"),
        26 => ("00000001000000011001110100100111","1000"),
        27 => ("00000000001110100010100000000010","1001"),
        28 => ("00000001000011001001001100000010","1001"),
        29 => ("00000010111100101010000000000000","1001"),
        30 => ("00000010111110000101011100000001","1001"),
        31 => ("00000000101111010000001001010010","1010"),
        32 => ("00000001011001100000001010110000","1010"),
        33 => ("00000010010001010000000001000000","1010"),
        34 => ("00000010010111010000000101110100","1010"),
        35 => ("00000000111101110000001000000010","1011"),
        36 => ("00000001001001010000001000000010","1011"),
        37 => ("00000010001101100000000000000010","1011"),
        38 => ("00000010011101100000000100000010","1011"),
        39 => ("00000010111000100000001000000000","1011"),
        40 => ("00000010111010000000001000000001","1011"),
        41 => ("00000000000000100101101101111010","1100"),
        42 => ("00000001000000100101111011010011","1100"),
        43 => ("00000010000000000101101110101100","1100"),
        44 => ("00000010000000011111011011011011","1100"),
        45 => ("00000000000000100110110000000010","1101"),
        46 => ("00000001000000101000110000000010","1101"),
        47 => ("00000010000000001001001100000010","1101"),
        48 => ("00000010000000011110100000000010","1101"),
        49 => ("00000010000000100100011000000000","1101"),
        50 => ("00000010000000100001111000000001","1101"),
        51 => ("00000000000000100000001000110110","1110"),
        52 => ("00000001000000100000001010010110","1110"),
        53 => ("00000010000000000000001010010111","1110"),
        54 => ("00000010000000010000001011001100","1110"),
        55 => ("00000010000000100000000010110111","1110"),
        56 => ("00000010000000100000000111100011","1110"),
        57 => ("00000000000000100000001000000010","1111"),
        58 => ("00000001000000100000001000000010","1111"),
        59 => ("00000010000000000000001000000010","1111"),
        60 => ("00000010000000010000001000000010","1111"),
        61 => ("00000010000000100000000000000010","1111"),
        62 => ("00000010000000100000000100000010","1111"),
        63 => ("00000010000000100000001000000000","1111"),
        64 => ("00000010000000100000001000000001","1111")
    );
    constant NC_ARR_FV_CFTI:FLAG_VALUE(0 to 15):= --convert flag to integer
    (
         0 => 4,
         1 => 3,
         2 => 3,
         3 => 2,
         4 => 3,
         5 => 2,
         6 => 2,
         7 => 1,
         8 => 3,
         9 => 2,
        10 => 2,
        11 => 1,
        12 => 2,
        13 => 1,
        14 => 1,
        15 => 0
    );

begin

    main:process(clk,rst)
    begin
        if rst='1' then 
            NC_SL_OUT_IB_valid <= '0'; 
            NC_INT_substate <= 0;
            NC_INT_state <= 1;
            NC_INT_wordCnt <= 0;
            NC_SL_valid <= '0';
            
            data <= std_logic_vector(to_unsigned(0,32));
            flags <= std_logic_vector(to_unsigned(0,4));
        else
            if clk='1' and clk'event then
                if NC_SL_ready = '1' and NC_SL_IN_TE_generatePackage = '1' and not(NC_INT_substate = 2 or NC_INT_substate = 3) then
                    NC_INT_substate <= 2;
                end if;
                
                if NC_SL_IN_IB_ready = '1' and NC_SL_valid = '1' then 
                    NC_SL_valid <= '0'; 
                    NC_SL_OUT_IB_valid <= '0';
                end if;

                if NC_SL_ready = '1' and NC_INT_state = 1 and NC_SL_IN_IB_ready = '1' and (NC_INT_substate = 2 or NC_INT_substate = 3) then
                    NC_SL_valid <= '1';
                    NC_SL_OUT_IB_valid <= '1';
                    NC_INT_state <= 0, 1 after TICK*NC_INT_sizeInterval;
                    if NC_INT_substate = 2 then  
                        NC_SLV_WR_OUT_IB_word <= NC_ARR_WORD_partOfBodyPackage(NC_INT_indexWord).data;
                        data <= NC_ARR_WORD_partOfBodyPackage(NC_INT_indexWord).data;
                        NC_SLV_4BR_OUT_IB_flag <= NC_ARR_WORD_partOfBodyPackage(NC_INT_indexWord).flag;
                        flags <= NC_ARR_WORD_partOfBodyPackage(NC_INT_indexWord).flag;
                        NC_INT_wordCnt <= NC_INT_wordCnt + 1;
                        if NC_INT_wordCnt+1 = NC_INT_wordNum - 1 then
                            NC_INT_substate <= 3 after TICK*NC_INT_sizeInterval;
                        end if;
                    else 
                        if NC_INT_substate = 3 then 
                            NC_SLV_WR_OUT_IB_word <= NC_ARR_WORD_endPackage(NC_INT_indexEndWord).data;
                            data <= NC_ARR_WORD_endPackage(NC_INT_indexEndWord).data;
                            NC_SLV_4BR_OUT_IB_flag <= NC_ARR_WORD_endPackage(NC_INT_indexEndWord).flag;
                            flags <= NC_ARR_WORD_endPackage(NC_INT_indexEndWord).flag;
                            NC_INT_substate <= 1 after TICK*NC_INT_sizeInterval;
                            NC_INT_wordCnt <= 0;
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process main;

    setPackage:process(rst, NC_INT_IN_TE_setInterval, NC_INT_IN_TE_numWord)
    begin
        if rst = '1' then
            NC_INT_sizeInterval <= 0;
            NC_INT_wordNum <= 0;
            NC_SL_ready <= '0';
            NC_SL_OUT_TE_ready <= '0';
        else
            if NC_INT_substate = 1 or NC_INT_substate = 0 then
                NC_INT_sizeInterval <= NC_INT_IN_TE_setInterval;
                NC_INT_wordNum <= NC_INT_IN_TE_numWord;
                if NC_INT_wordNum /= 0 and NC_INT_sizeInterval /= 0 then
                    NC_SL_OUT_TE_ready <= '1';
                    NC_SL_ready <= '1';
                else 
                    NC_SL_OUT_TE_ready <= '0';
                    NC_SL_ready <= '0';
                end if;
            end if;
        end if;
    end process setPackage;
    
    generateNumberWord:process(rst, NC_INT_state) is
        variable seed1:positive := 1;
        variable seed2:positive := 1;
        variable x:real;
    begin
        if rst = '1' then 
            seed1 := 1;
            seed2 := 1;
            uniform(seed1, seed2, x);
            NC_INT_preIndexWord <= 0;
            NC_INT_preIndexEndWord <= 0; 
            NC_INT_indexWord <= integer(floor(x * real(SIZE_ARR_WORD)));
            NC_INT_indexEndWord <= integer(floor(x * real(SIZE_ARR_END_WORD)));
        else 
            if NC_INT_state = 0 then
                uniform(seed1, seed2, x);
                NC_INT_preIndexWord <= NC_INT_indexWord;
                NC_INT_preIndexEndWord <= NC_INT_indexEndWord; 
                NC_INT_indexWord <= integer(floor(x * real(SIZE_ARR_WORD)));
                NC_INT_indexEndWord <= integer(floor(x * real(SIZE_ARR_END_WORD)));
            end if;
        end if;
    end process generateNumberWord;

    dataComposition:process(rst,  clk)
        variable flag_end:std_logic := '0'; 
        variable iteratorPackage: integer := 0;
    begin 
        if rst = '1' then 
            flag_end := '0';
            iteratorPackage := 0;
            NC_INT_packageIterator <= 0;
            NC_BYTE_T_80_buf <= (others => "00000000");
        elsif rising_edge(clk) then 
            if NC_SL_valid = '1' and NC_SL_IN_IB_ready = '1' then     
                flag_end := '0'; 
                iteratorPackage := NC_INT_packageIterator;
                processing_flag : for i in 0 to 3 loop
                    if flags(3-i) = '0'then
                        NC_BYTE_T_80_buf(iteratorPackage) <= data(31 - 8 * i downto 24 - 8 * i);
                        iteratorPackage := iteratorPackage + 1;
                    else 
                        if data(31 - 8 * i downto 24 - 8 * i) /= "00000010" then
                            flag_end := '1'; 
                        end if;
                    end if;
                end loop ; -- processing_flag
                if flag_end = '1' then
                    iteratorPackage:= iteratorPackage + (4 - iteratorPackage mod 4);
                end if;
                NC_INT_packageIterator <= iteratorPackage;
            end if;
        end if;
    end process dataComposition;
  
    returnDataForAssert:process(rst,NC_SL_IN_TE_requestAssertBuf)    
    begin
        if rst = '1' then 
            NC_SL_OUT_TE_validBuf <= '0';
        else
            if NC_SL_IN_TE_requestAssertBuf = '1' then
                confert_byte_array_to_slv : for i in 0 to 80-1 loop
                    NC_SLV_20W_OUT_TE_buf(i*8 to ((i+1)*8)-1) <= NC_BYTE_T_80_buf(i);
                end loop; -- confert_byte_array_to_slv
                NC_SL_OUT_TE_validBuf <= '1', '0' after TICK*2;
            end if;
        end if;
    end process returnDataForAssert;

end architecture ; -- arch