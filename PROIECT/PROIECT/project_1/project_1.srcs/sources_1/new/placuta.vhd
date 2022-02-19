

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;


entity placuta is
Port
 (
 signal clk:in std_logic;
 signal btn:in std_logic_vector(4 downto 0);
 signal sw:in std_logic_vector(15 downto 0);
 signal TMP_INT:in std_logic;
 signal TMP_CT:in std_logic;
 signal cat:out std_logic_vector(7 downto 0);
 signal an:out std_logic_vector(7 downto 0);
 signal led:out std_logic_vector(15 downto 0);
 signal TMP_SCL:inout std_logic;
 signal TMP_SDA:inout std_logic;
 signal RX:in std_logic;
 signal TX:out std_logic
  );
end placuta;

architecture Behavioral of placuta is

signal TSR:std_logic_vector(23 downto 0):=(others=>'0');
signal buttonReset:std_logic;
signal semnalRead:std_logic;
signal buttonStart:std_logic;
signal counter:INTEGER:=0;
signal ena:std_logic:='0';
signal unitati : STD_LOGIC_VECTOR (3 downto 0);
signal zeci : STD_LOGIC_VECTOR (3 downto 0);
signal sute: STD_LOGIC_VECTOR (3 downto 0);
signal mii : STD_LOGIC_VECTOR (3 downto 0);
signal date:std_logic_vector(12 downto 0);
signal afisor:std_logic_vector(31 downto 0);
signal intVal : integer;
signal dateN : integer;
signal final : std_logic_vector(12 downto 0);
signal final_1 : std_logic_vector(15 downto 0);
signal start:std_logic;
signal activ:std_logic;
signal mesaj:std_logic_vector(7 downto 0);
signal rx8:std_logic_vector(7 downto 0);
signal numarator:integer:=0;
signal done:std_logic;
begin

sep: entity WORK.separator port map (
            numar => final(11 downto 0),
           unitati => unitati,
           zeci  => zeci,
           sute => sute,
           mii  => mii
);



buton_start:entity WORK.mpg port map
(
btn=>btn(0),
clk=>clk,
en=>buttonStart
);

buton_reset:entity WORK.mpg port map
(
btn=>btn(1),
clk=>clk,
en=>buttonReset
);

senzor:entity WORK.senzorTemperatura port map
(
TMP_SCL=>TMP_SCL,
		TMP_SDA=>TMP_SDA,
		TempOut =>date,
		RdyOut =>led(15),
		EROAREOo =>led(1),
		clkInput=>clk,
		SrstInput=>buttonReset
);

dateN <=  625 * to_integer(unsigned (date)) /10000;
final<= date(12) & std_logic_vector(to_unsigned(dateN, 12));

afisor<="0000000000000000000"& date(12) &sute & zeci &unitati;
final_1 <= "000" & date(12) &sute & zeci &unitati;

ssd:entity WORK.displ7seg port map
    (
    Clk=>Clk,
           Rst=>buttonReset,
           Data=>afisor,
           An=>an,
           Seg=>cat
    );
    
--data de la tel la placuta
bl_rx:entity WORK.uartRx
   generic map
    (
    clkPerBit => 10416
    )
    port map
          (
        Clk=>clk,
        RxSerialInput=>RX,
        RxDvOutput=>led(0),
        RxByteOutput=>rx8
          );
          
--data spre telefon     

bl_tx:entity WORK.uartTx
    generic map
    (
    clkPerBit => 10416
    )
    port map
          (
        Clk=>Clk,
        txDvInput=>start,
        tcByteInput=>mesaj,--x"41",
        txActivatOut=>activ,
        txSerialOut=>TX,
        txTerminatOut=>done
          );
          
    unitate_cc:entity WORK.UCC
    port map
    (
    clk=>clk,
    rst=>buttonReset,
    buttonStart=>buttonStart,
    dateDeIntrare=> final_1,
    activ=>activ,
    done=>done,
    dateleDeIesire=>mesaj,
    start=>start
    );      
    
end Behavioral;
