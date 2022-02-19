			
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;


entity separator is
    Port ( numar : in  STD_LOGIC_VECTOR (11 downto 0);
           unitati : out  STD_LOGIC_VECTOR (3 downto 0);
           zeci : out  STD_LOGIC_VECTOR (3 downto 0);
           sute : out  STD_LOGIC_VECTOR (3 downto 0);
           mii : out  STD_LOGIC_VECTOR (3 downto 0)
          );
end separator;

architecture SEPARARE of separator is

begin

bcd1: process(numar)
  variable temp : STD_LOGIC_VECTOR (11 downto 0);
  variable bcd : UNSIGNED (15 downto 0) := (others => '0');
  
  begin
    bcd := (others => '0');
    temp(11 downto 0) := numar;
    for i in 0 to 11 loop
    
      if bcd(3 downto 0) > 4 then 
        bcd(3 downto 0) := bcd(3 downto 0) + 3;
      end if;
      
      if bcd(7 downto 4) > 4 then 
        bcd(7 downto 4) := bcd(7 downto 4) + 3;
      end if;
    
      if bcd(11 downto 8) > 4 then  
        bcd(11 downto 8) := bcd(11 downto 8) + 3;
      end if;
    
      bcd := bcd(14 downto 0) & temp(11);
      temp := temp(10 downto 0) & '0';
    
    end loop;

    unitati <= STD_LOGIC_VECTOR(bcd(3 downto 0));
    zeci <= STD_LOGIC_VECTOR(bcd(7 downto 4));
    sute <= STD_LOGIC_VECTOR(bcd(11 downto 8));
    mii <= STD_LOGIC_VECTOR(bcd(15 downto 12));
  
  end process bcd1;            
  
end SEPARARE;
