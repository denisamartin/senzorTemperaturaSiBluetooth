
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity uartRx is
generic (
    clkPerBit : integer := 868
    );
  port (
  clk      : in  std_logic;
    RxSerialInput : in  std_logic;
    RxDvOutput     : out std_logic;
    RxByteOutput   : out std_logic_vector(7 downto 0)
    );
end uartRx;

architecture Behavioral of uartRx is
 type t_SM_Main is (idle, startBitRXs, dataBitsRxS,
                     rxStopB, cleanUp);
  signal smMainR : t_SM_Main := idle;
 
  signal rxDataR_R : std_logic := '0';
  signal rxDataR   : std_logic := '0';
   
  signal clkCountR : integer range 0 to clkPerBit-1 := 0;
  signal indexBitR : integer range 0 to 7 := 0;
  signal byteRxRR   : std_logic_vector(7 downto 0) := (others => '0');
  signal rxDvR     : std_logic := '0';
   
begin
 

  myProcS : process (clk)
  begin
    if rising_edge(clk) then
      rxDataR_R <= RxSerialInput;
      rxDataR   <= rxDataR_R;
    end if;
  end process myProcS;
   

  uartRxProcess : process (Clk)
  begin
    if rising_edge(Clk) then
         
      case smMainR is
 
        when idle =>
          clkCountR <= 0;
          rxDvR     <= '0';
          indexBitR <= 0;
 
          if rxDataR = '0' then
            smMainR <= startBitRXs;
          else
            smMainR <= idle;
          end if;
 

        when startBitRXs =>
          if clkCountR = (clkPerBit-1)/2 then
            if rxDataR = '0' then
              clkCountR <= 0;
              smMainR   <= dataBitsRxS;
            else
              smMainR   <= idle;
            end if;
          else
            clkCountR <= clkCountR + 1;
            smMainR   <= startBitRXs;
          end if;


        when dataBitsRxS =>
          if clkCountR < clkPerBit-1 then
            clkCountR <= clkCountR + 1;
            smMainR   <= dataBitsRxS;
          else
            clkCountR            <= 0;
            byteRxRR(indexBitR) <= rxDataR;

            if indexBitR < 7 then
              indexBitR <= indexBitR + 1;
              smMainR   <= dataBitsRxS;
            else
              indexBitR <= 0;
              smMainR   <= rxStopB;
            end if;
          end if;

        when rxStopB =>
          if clkCountR < clkPerBit-1 then
            clkCountR <= clkCountR + 1;
            smMainR   <= rxStopB;
          else
            rxDvR     <= '1';
            clkCountR <= 0;
            smMainR   <= cleanUp;
          end if;
 

        when cleanUp =>
          smMainR <= idle;
          rxDvR   <= '0';

        when others =>
          smMainR <= idle;
 
      end case;
    end if;
  end process uartRxProcess;
  RxByteOutput <= byteRxRR;
  RxDvOutput   <= rxDvR;

end Behavioral;
