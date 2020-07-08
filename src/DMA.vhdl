library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity DMA is
  port (

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
    
  ) ;
end DMA ;
 
architecture main of DMA is
  
  signal SAFF : std_logic_vector(31 downto 0);
  signal SZFF : std_logic_vector(31 downto 0);
  signal CAFF : std_logic_vector(31 downto 0);
  signal SASF : std_logic_vector(31 downto 0);
  signal SZSF : std_logic_vector(31 downto 0);
  signal CASF : std_logic_vector(31 downto 0);
  signal FREG : std_logic_vector(31 downto 0);

  signal bus_in_to_buf_data : std_logic_vector(31 downto 0);
  signal bus_in_to_buf_serv_info : std_logic_vector(0 to 1);
  signal bus_in_to_buf_num_byte : std_logic_vector(1 downto 0);
  signal bus_in_to_buf_valid : std_logic;
  signal bus_in_to_buf_ready : std_logic;
  signal bus_buf_to_analyzer_data : std_logic_vector(31 downto 0);
  signal bus_buf_to_analyzer_serv_info : std_logic_vector(0 to 1);
  signal bus_buf_to_analyzer_num_byte : std_logic_vector(1 downto 0);
  signal bus_buf_to_analyzer_valid : std_logic;
  signal bus_buf_to_analyzer_ready : std_logic;

  signal inc_desc : std_logic; 
  signal inc_word : std_logic; 
  
  signal map_INT_FF : std_logic;
  signal map_INT_SF : std_logic;
  
  signal bus_analyzer_to_writer_data : std_logic_vector(31 downto 0);
  signal bus_analyzer_to_writer_ready : std_logic;
  signal bus_analyzer_to_writer_valid_data : std_logic;
  signal bus_analyzer_to_writer_valid_desc : std_logic;

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
      data_out : out std_logic_vector(31 downto 0); -- bus with buffer for only data,  if word of data is have  end of package,  bus could have trash after data, this noted in serv_info and num_byte signal --
      serv_info : out std_logic_vector(0 to 1); -- first bit flag end package, second bit EOF/EEP --
      num_byte : out std_logic_vector(1 downto 0); -- [0-3] number byte, which is an end of package in word, word could have trash after this byte --
      valid : out std_logic; -- report to buffer about valid of data --
      ready_buf : in std_logic -- buffer ready for write data --
    );
  end component inputBlock;
    
  component packBuffer is 
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
    );
  end component packBuffer;

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

  component writer is
    port 
    (
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

begin

  myInputBlock : inputBlock port map
  (
    clk => clk,
    rst => rst,
    data_in => input,
    flags => flags,
    valid_data => valid,
    ready => ready,
    data_out => bus_in_to_buf_data,
    serv_info => bus_in_to_buf_serv_info,
    num_byte => bus_in_to_buf_num_byte,
    valid => bus_in_to_buf_valid,
    ready_buf => bus_in_to_buf_ready 
  );

  myPackBuffer : packBuffer port map
  (
    clk => clk,
    rst => rst,
    data_out => bus_buf_to_analyzer_data,
    serv_info_out => bus_buf_to_analyzer_serv_info,
    num_byte_out => bus_buf_to_analyzer_num_byte,
    ready_anl => bus_buf_to_analyzer_ready,
    valid => bus_buf_to_analyzer_valid,
    data_in => bus_in_to_buf_data,
    serv_info_in => bus_in_to_buf_serv_info,
    num_byte_in => bus_in_to_buf_num_byte,
    ready => bus_in_to_buf_ready,
    valid_inb => bus_in_to_buf_valid
  );

  myAnalyzer : analyzer port map
  (
    clk => clk,
    rst => rst,
    data_in => bus_buf_to_analyzer_data,
    serv_info => bus_buf_to_analyzer_serv_info,
    num_byte => bus_buf_to_analyzer_num_byte,
    ready => bus_buf_to_analyzer_ready,
    valid_in => bus_buf_to_analyzer_valid,
    data_out => bus_analyzer_to_writer_data,
    ready_wrt => bus_analyzer_to_writer_ready,
    valid_data => bus_analyzer_to_writer_valid_data,
    valid_desc => bus_analyzer_to_writer_valid_desc
  );
 
  myWriter : writer port map
  (
    clk => clk,
    rst => rst,
    address => data_address,
    data => output,
    valid => write,
    mem_ready => mem_ready,
    word => bus_analyzer_to_writer_data,
    ready => bus_analyzer_to_writer_ready,
    valid_data => bus_analyzer_to_writer_valid_data,
    valid_desc => bus_analyzer_to_writer_valid_desc,
    write_desc => inc_desc, 
    write_data => inc_word, 
    INTFF => map_INT_FF, 
    INTSF => map_INT_SF, 
    CAFF_REG => CAFF, 
    CASF_REG => CASF, 
    SZSF_REG => SZSF
  );
 
  myRegistersBlock : registersBlock port map
  (
    clk => clk,
    rst => rst,
    address => address_reg,
    data_in => data_reg_in,
    data_out => data_reg_out,
    write_read => IO_flag,
    SAFF_REG => SAFF, 
    SZFF_REG => SZFF, 
    CAFF_REG => CAFF, 
    SASF_REG => SASF, 
    SZSF_REG => SZSF, 
    CASF_REG => CASF, 
    FREG_REG => FREG, 
    write_desc => inc_desc, 
    write_data => inc_word, 
    INTFF => map_INT_FF, 
    INTSF => map_INT_SF, 
    INTRP => INT_RP
  );

  INT_FF <= map_INT_FF;
  INT_SF <= map_INT_SF;

end architecture ; -- main