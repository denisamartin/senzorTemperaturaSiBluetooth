
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;

entity displ7seg is
    Port ( Clk  : in  STD_LOGIC;
           Rst  : in  STD_LOGIC;
           Data : in  STD_LOGIC_VECTOR (31 downto 0);
           An   : out STD_LOGIC_VECTOR (7 downto 0);
           Seg  : out STD_LOGIC_VECTOR (7 downto 0));
end displ7seg;

architecture Behavioral of displ7seg is

constant CNT_100HZ : integer := 2**20;
signal number        : integer range 0 to CNT_100HZ - 1 := 0;
signal numberVect        : STD_LOGIC_VECTOR (19 downto 0) := (others => '0');
signal Hex         : STD_LOGIC_VECTOR (3 downto 0) := (others => '0');
signal selectareLed      : STD_LOGIC_VECTOR (2 downto 0) := (others => '0');

begin

divclk: process (Clk)
    begin
    if (Clk'event and Clk = '1') then
        if (Rst = '1') then
            number<= 0;
        elsif (number= CNT_100HZ - 1) then
            number<= 0;
        else
            number<= number+ 1;
        end if;
    end if;
    end process;

    numberVect <= CONV_STD_LOGIC_VECTOR (Number, 20);
    selectareLed <= numberVect (19 downto 17);

    An <= "11111110" when selectareLed = "000" else
          "11111101" when selectareLed = "001" else
          "11111011" when selectareLed = "010" else
          "11110111" when selectareLed = "011" else
          "11101111" when selectareLed = "100" else
          "11011111" when selectareLed = "101" else
          "10111111" when selectareLed = "110" else
          "01111111" when selectareLed = "111" else
          "11111111";


    Hex <= Data (3  downto  0) when selectareLed = "000" else
           Data (7  downto  4) when selectareLed = "001" else
           Data (11 downto  8) when selectareLed = "010" else
           Data (15 downto 12) when selectareLed = "011" else
           Data (19 downto 16) when selectareLed = "100" else
           Data (23 downto 20) when selectareLed = "101" else
           Data (27 downto 24) when selectareLed = "110" else
           Data (31 downto 28) when selectareLed = "111" else
           X"0";


    Seg <= "11111001" when Hex = "0001" else            -- 1
           "10100100" when Hex = "0010" else            -- 2
           "10110000" when Hex = "0011" else            -- 3
           "10011001" when Hex = "0100" else            -- 4
           "10010010" when Hex = "0101" else            -- 5
           "10000010" when Hex = "0110" else            -- 6
           "11111000" when Hex = "0111" else            -- 7
           "10000000" when Hex = "1000" else            -- 8
           "10010000" when Hex = "1001" else            -- 9
           "10001000" when Hex = "1010" else            -- A
           "10000011" when Hex = "1011" else            -- b
           "11000110" when Hex = "1100" else            -- C
           "10100001" when Hex = "1101" else            -- d
           "10000110" when Hex = "1110" else            -- E
           "10001110" when Hex = "1111" else            -- F
           "11000000";                                  -- 0

end Behavioral;