library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 

entity registersBlock is
  port (
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
end registersBlock;


architecture main of registersBlock is
begin

  regManager : process( clk, rst )
    variable SAFF : std_logic_vector(31 downto 0);
    variable SZFF : std_logic_vector(31 downto 0);
    variable CAFF : std_logic_vector(31 downto 0);
    variable SASF : std_logic_vector(31 downto 0);
    variable SZSF : std_logic_vector(31 downto 0);
    variable CASF : std_logic_vector(31 downto 0);
    variable FREG : std_logic_vector(31 downto 0);
    -- set bit of FREG
    -- 31 - 8 reserv
    constant GIE  : integer := 7;-- 7 - GIE[group interrupt enable] 1 - enable all interrupt, 0 - disable all interrupt 
    constant FFIE : integer := 6; -- 6 - FFIE[field first end interrupt enable] 1 - enable, 0 - disable
    constant FSIE : integer := 5;-- 5 - FSIE[field second end interrupt enable] 1 - enable, 0 - disable
    constant RPIE : integer := 4;-- 4 - RPIE[reception pack interrupt enable] 1 - enable, 0 - disable
    --3 reserv
    constant FFIF : integer := 2;-- 2 - FFIF[field first interrupt flag] 1 - interrupt call, when the current address is behind the first field, 0 - rst by software
    constant FSIF : integer := 1;-- 1 - FSIF[field second interrupt flag] 1 - interrupt call, when the current address is behind the second field, 0 - rst by software
    constant RPIF : integer := 0;-- 0 - RPIF[reception pack interrupt flag] 1 - interrupt call, when DMA wrote the whole packet in, 0 - rst by hardware, when record in all field possible
  begin
    if( rst = '1' ) then
      data_out <= std_logic_vector(to_unsigned(0, 32));
      
      SAFF_REG <= std_logic_vector(to_unsigned(0,32));
      SAFF := std_logic_vector(to_unsigned(0,32));
      SZFF_REG <= std_logic_vector(to_unsigned(0,32));
      SZFF := std_logic_vector(to_unsigned(0,32));
      CAFF_REG <= std_logic_vector(to_unsigned(0,32));
      CAFF := std_logic_vector(to_unsigned(0,32));
      SASF_REG <= std_logic_vector(to_unsigned(0,32));
      SASF := std_logic_vector(to_unsigned(0,32));
      SZSF_REG <= std_logic_vector(to_unsigned(0,32));
      SZSF := std_logic_vector(to_unsigned(0,32));
      CASF_REG <= std_logic_vector(to_unsigned(0,32));
      CASF := std_logic_vector(to_unsigned(0,32));
      FREG_REG <= std_logic_vector(to_unsigned(0,32));
      FREG := std_logic_vector(to_unsigned(0,32));
      INTFF <= '0';
      INTSF <= '0';
      INTRP <= '0';
    elsif( rising_edge(clk) ) then
      if write_read = '1' then
        case address is
          when "000" =>
            FREG_REG <= data_in;
            FREG := data_in;
          when "001" =>
            SAFF_REG <= data_in;
            SAFF := data_in;
          when "010" =>
            SZFF_REG <= data_in;
            SZFF := data_in;
          when "011" =>
            CAFF_REG <= data_in;
            CAFF := data_in;
          when "100" =>
            FREG_REG <= data_in;
            FREG := data_in;
          when "101" =>
            SASF_REG <= data_in;
            SASF := data_in;
          when "110" => 
            SZSF_REG <= data_in;
            SZSF := data_in;
          when "111" => 
            CASF_REG <= data_in;
            CASF := data_in;
          when others =>
            FREG_REG <= data_in;
            FREG := data_in;
        end case;
      else
        case address is
          when "000" =>
            data_out <= FREG;
          when "001" =>
            data_out <= SAFF;
          when "010" => 
            data_out <= SZFF;
          when "011" =>
            data_out <= CAFF; 
          when "100" =>
            data_out <= FREG;
          when "101" => 
            data_out <= SASF;
          when "110" =>
            data_out <= SZSF;
          when "111" =>
            data_out <= CASF;
          when others =>
            data_out <= FREG;
        end case;
      end if;
      
      if write_desc = '1' then
        CASF_REG <= std_logic_vector(unsigned(CASF) + 8);
        CASF := std_logic_vector(unsigned(CASF) + 8);
        if FREG(GIE) = '1' then
          if FREG(RPIE) = '1' then
            FREG_REG(RPIF) <= '1';
            FREG(RPIF) := '1';
          end if;
        end if ;
      end if;

      INTRP <= FREG(RPIF);

      if write_data = '1' then
        CAFF_REG <= std_logic_vector(unsigned(CAFF) + 8);
        CAFF := std_logic_vector(unsigned(CAFF) + 8);
      end if ;
      
      if FREG(GIE) = '1' then
        
        if FREG(FFIE) = '1' then
          if unsigned(CAFF) - unsigned(SAFF) >= unsigned(SZFF) then
            INTFF <= '1';
            FREG_REG(FFIF) <= '1';
            FREG(FFIF) := '1';
          end if;
        end if;

        if FREG(FSIE) = '1' then
          if unsigned(CASF) - unsigned(SASF) >= unsigned(SZSF) then
            INTSF <= '1';
            FREG_REG(FSIF) <= '1';
            FREG(FSIF) := '1';
          end if;
        end if;

      end if;

      if unsigned(CAFF) - unsigned(SAFF) < unsigned(SZFF) then
        INTFF <= '0';
      end if;

      if unsigned(CASF) - unsigned(SASF) < unsigned(SZSF) then
        INTSF <= '0';
      end if;

    end if ;
  end process ; -- regManager
  
end main ; -- main