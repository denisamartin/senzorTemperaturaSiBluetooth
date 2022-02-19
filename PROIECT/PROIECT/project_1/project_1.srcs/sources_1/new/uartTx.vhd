

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity uartTx is
 generic (
    clkPerBit : integer := 868
    );
  port (
    clk      : in  std_logic;
    txDvInput     : in  std_logic;
    tcByteInput   : in  std_logic_vector(7 downto 0);
    txActivatOut : out std_logic;
    txSerialOut : out std_logic;
    txTerminatOut   : out std_logic
    );
end uartTx;

architecture Behavioral of uartTx is

  signal indexBitR : integer range 0 to 7 := 0;
  type t_SM_Main is (idle, txStartBit, txDataBit,
                     stopBitTx, cleanUp);
  signal smMainR : t_SM_Main := idle;
  signal clkCountR : integer range 0 to clkPerBit-1 := 0;
  signal txDoneR   : std_logic := '0';
  signal txDataR   : std_logic_vector(7 downto 0) := (others => '0');

  
begin

uartTxprocess : process (Clk)
  begin
    if rising_edge(Clk) then
         
      case smMainR is
 
        when idle =>
          txActivatOut <= '0';
          txSerialOut <= '1';
          txDoneR   <= '0';
          clkCountR <= 0;
          indexBitR <= 0;
 
          if txDvInput = '1' then
            txDataR <= tcByteInput;
            smMainR <= txStartBit;
          else
            smMainR <= idle;
          end if;
 
           

        when txStartBit =>
          txActivatOut <= '1';
          txSerialOut <= '0';
 

          if clkCountR < clkPerBit-1 then
            clkCountR <= clkCountR + 1;
            smMainR   <= txStartBit;
          else
            clkCountR <= 0;
            smMainR   <= txDataBit;
          end if;
 

        when txDataBit =>
          txSerialOut <= txDataR(indexBitR);
           
          if clkCountR < clkPerBit-1 then
            clkCountR <= clkCountR + 1;
            smMainR   <= txDataBit;
          else
            clkCountR <= 0;

            if indexBitR < 7 then
              indexBitR <= indexBitR + 1;
              smMainR   <= txDataBit;
            else
              indexBitR <= 0;
              smMainR   <= stopBitTx;
            end if;
          end if;
 

        when stopBitTx =>
          txSerialOut <= '1';

          if clkCountR < clkPerBit-1 then
            clkCountR <= clkCountR + 1;
            smMainR   <= stopBitTx;
          else
            txDoneR   <= '1';
            clkCountR <= 0;
            smMainR   <= cleanUp;
          end if;
        when cleanUp =>
          txActivatOut <= '0';
          txDoneR   <= '1';
          smMainR   <= idle;
        when others =>
          smMainR <= idle;
 
      end case;
    end if;
  end process uartTxprocess;
 
  txTerminatOut <= txDoneR;

end Behavioral;
