

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;


ENTITY I2C IS
  GENERIC(
    clkInput : INTEGER := 50_000_000;
    clkBus   : INTEGER := 400_000);
  PORT(
    clk       : IN     STD_LOGIC;
    reset     : IN     STD_LOGIC;
    ena       : IN     STD_LOGIC;
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0);
    rw        : IN     STD_LOGIC;
    dataWrite  : IN     STD_LOGIC_VECTOR(7 DOWNTO 0);
    busy      : OUT    STD_LOGIC;
    dataRead   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0);
    error : BUFFER STD_LOGIC;
    semnalRead: OUT STD_LOGIC;
    sda       : INOUT  STD_LOGIC;
    scl       : INOUT  STD_LOGIC);
END I2C;

architecture Behavioral of I2C is
 CONSTANT divider  :  INTEGER := (clkInput/clkBus)/4;
  TYPE machine IS(ready, start, command, ack1, wr, rd, ack2, mstr_ack, stop);

  SIGNAL dataClk      : STD_LOGIC;
  SIGNAL dataClkPrev : STD_LOGIC;
  SIGNAL state         : machine;
  SIGNAL clkScl       : STD_LOGIC;
  SIGNAL enScl       : STD_LOGIC := '0';
  SIGNAL enSclN     : STD_LOGIC;
   SIGNAL intSda       : STD_LOGIC := '1';
  SIGNAL addrRW       : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL RxData       : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL bitCount       : INTEGER RANGE 0 TO 7 := 7;
  SIGNAL TxData       : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL stretch       : STD_LOGIC := '0';
BEGIN

  PROCESS(clk, reset)
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4;
  BEGIN
    IF(reset = '1') THEN
      stretch <= '0';
      count := 0;
    ELSIF(clk'EVENT AND clk = '1') THEN
      dataClkPrev <= dataClk;
      IF(count = divider*4-1) THEN
        count := 0;
      ELSIF(stretch = '0') THEN
        count := count + 1;
      END IF;
      CASE count IS
        WHEN 0 TO divider-1 =>
          clkScl <= '0';
          dataClk <= '0';
        WHEN divider TO divider*2-1 =>
          clkScl <= '0';
          dataClk <= '1';
        WHEN divider*2 TO divider*3-1 =>
          clkScl <= '1';
          IF(scl = '0') THEN
            stretch <= '1';
          ELSE
            stretch <= '0';
          END IF;
          dataClk <= '1';
        WHEN OTHERS =>
          clkScl <= '1';
          dataClk <= '0';
      END CASE;
    END IF;
  END PROCESS;
    
  PROCESS(clk)
  begin
  if clk'event and clk='1' then
        if state=mstr_ack then
            semnalRead<='1';
        else
            semnalRead<='0';
        end if;
  end if;
  
  end process;

  PROCESS(clk, reset)
  BEGIN
    IF(reset = '1') THEN
      state <= ready;
      enScl <= '0';
      intSda <= '1';
      busy <= '1';
      error <= '0';
      dataRead <= "00000000";
        bitCount <= 7;
    ELSIF(clk'EVENT AND clk = '1') THEN
      IF(dataClk = '1' AND dataClkPrev = '0') THEN
        CASE state IS
          WHEN ready =>
            IF(ena = '1') THEN
              busy <= '1';
              TxData <= dataWrite;
              addrRW <= addr & rw;
              state <= start;
            ELSE
              busy <= '0';
              state <= ready;
            END IF;
          WHEN start =>
            busy <= '1';
            intSda <= addrRW(bitCount);
            state <= command;
          WHEN command =>
            IF(bitCount = 0) THEN
              bitCount <= 7;
              intSda <= '1';
              state <= ack1;
            ELSE
              bitCount <= bitCount - 1;
              intSda <= addrRW(bitCount-1);
              state <= command;
            END IF;
          WHEN ack1 =>
            IF(addrRW(0) = '0') THEN
              intSda <= TxData(bitCount);
              state <= wr;
            ELSE
              intSda <= '1';
              state <= rd;
            END IF;
          WHEN wr =>
            busy <= '1';
            IF(bitCount = 0) THEN
              bitCount <= 7;
              intSda <= '1';
              state <= ack2;
            ELSE
              bitCount <= bitCount - 1;
              intSda <= TxData(bitCount-1);
              state <= wr;
            END IF;
          WHEN rd =>
            busy <= '1';
            IF(bitCount = 0) THEN
              IF(ena = '1' AND addrRW = addr & rw) THEN
                intSda <= '0';
              ELSE
                intSda <= '1';
              END IF;
              dataRead <= RxData;
              bitCount <= 7;
              state <= mstr_ack;
            ELSE
              bitCount <= bitCount - 1;
              state <= rd;
            END IF;
          WHEN ack2 =>
            IF(ena = '1') THEN
              addrRW <= addr & rw;
              busy <= '0';
              TxData <= dataWrite;
              IF(addrRW = addr & rw) THEN
                intSda <= dataWrite(bitCount);
                state <= wr;
              ELSE
                state <= start;
              END IF;
            ELSE
              state <= stop;
            END IF;
          WHEN mstr_ack =>
            IF(ena = '1') THEN
              busy <= '0';
              addrRW <= addr & rw;
              TxData <= dataWrite;
              IF(addrRW = addr & rw) THEN
                intSda <= '1';
                state <= rd;
              ELSE
                state <= start;
              END IF;    
            ELSE
              state <= stop;
            END IF;
          WHEN stop =>
            busy <= '0';
            state <= ready;
        END CASE;    
      ELSIF(dataClk = '0' AND dataClkPrev = '1') THEN
        CASE state IS
          WHEN start =>                  
            IF(enScl = '0') THEN
              error <= '0';
              enScl <= '1';

            END IF;
          WHEN ack1 =>
            IF(sda /= '0' OR error = '1') THEN
              error <= '1';
            END IF;
          WHEN rd =>
            RxData(bitCount) <= sda;
          WHEN ack2 =>
            IF(sda /= '0' OR error = '1') THEN
              error <= '1';
            END IF;
          WHEN stop =>
            enScl <= '0';
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS;  

  WITH state SELECT
    enSclN <= dataClkPrev WHEN start,
                 NOT dataClkPrev WHEN stop,
                 intSda WHEN OTHERS;
      
  sda <= '0' WHEN enSclN = '0' ELSE 'Z';
  scl <= '0' WHEN (enScl = '1' AND clkScl = '0') ELSE 'Z';


end Behavioral;
