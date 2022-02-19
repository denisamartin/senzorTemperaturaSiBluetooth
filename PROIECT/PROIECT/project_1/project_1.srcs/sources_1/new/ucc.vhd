

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity UCC is
Port 
(
signal clk:in std_logic;
signal rst:in std_logic;
signal buttonStart:in std_logic;
signal dateDeIntrare:in std_logic_vector(15 downto 0);
signal activ:in std_logic;
signal done:in std_logic;
signal dateleDeIesire:out std_logic_vector(7 downto 0);
signal start:out std_logic
 );
end UCC;

architecture Behavioral of UCC is

type stari is (inceput,incData,oct1,trans1,oct2,trans2);
signal curentS:stari:=incData;
signal oc2:std_logic_vector(7 downto 0);
signal urmatoareaS:stari:=incData;
signal oc1:std_logic_vector(7 downto 0);

begin

process(clk,rst)
begin

if rst='1' then
curentS<=inceput;
elsif clk'event and clk='1' then 
curentS<=urmatoareaS;
end if;

end process;

process(curentS,activ,done,dateDeIntrare,oc1,oc2)
begin

case curentS is
when inceput=>  if buttonStart='1' then
                    urmatoareaS<=incData;
                else
                    urmatoareaS<=inceput;
                end if;
when incData=>urmatoareaS<=oct1;
                     oc1<=dateDeIntrare(15 downto 8);
                     oc2<=dateDeIntrare(7 downto 0);
when oct1=>urmatoareaS<=trans1;
             dateleDeIesire<=oc1;
when trans1=>if activ='0' and done='1' then
                        urmatoareaS<=oct2;
                  else
                        urmatoareaS<=trans1;
                  end if;
when oct2=>urmatoareaS<=trans2;
             dateleDeIesire<=oc2;
when trans2=>if activ='0' and done='1' then
                        urmatoareaS<=inceput;
                  else
                        urmatoareaS<=trans2;
                  end if;
end case;

end process;

process(curentS)
begin
case curentS is
when inceput=>start<='0';
when incData=>start<='0';
when trans1=>start<='0';
when trans2=>start<='0';
when oct1=>start<='1';
when oct2=>start<='1';
end case;
end process;
end Behavioral;
