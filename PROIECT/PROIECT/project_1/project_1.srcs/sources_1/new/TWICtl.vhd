
package TWIUtils is
  type busState_type is (busUnknown, busBusy, busFree);
  type error_type is (errArb, errNAck);
end TWIUtils;

package body TWIUtils is 
end TWIUtils;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.math_real.all;
use work.TWIUtils.ALL;


entity TWICtl is

	generic (
		fregvClock : natural := 50;
		aSlaveUnblock : boolean := false
	);
	port (
		imsg : in STD_LOGIC;
		stbIi : in STD_LOGIC;
		A_I : in  STD_LOGIC_VECTOR (7 downto 0);
		D_I : in  STD_LOGIC_VECTOR (7 downto 0);
		D_O : out  STD_LOGIC_VECTOR (7 downto 0);
		doneOutput : out  STD_LOGIC;
        EROAREOo : out  STD_LOGIC;
		errTp : out error_type;
		CLK : in std_logic;
		SRST : in std_logic;
		SDA : inout std_logic;
		SCL : inout std_logic
	);
end TWICtl;

architecture Behavioral of TWICtl is
	attribute fsm_encoding: string;
	constant FSCL : natural := 400_000;
	constant TIMEOUT : natural := 10;
	constant cicTS : natural :=
		natural(ceil(real(fregvClock*1_000_000/FSCL)));
	constant TIMEOUT_CYCLES : natural :=
		natural(ceil(real(fregvClock*TIMEOUT*1_000)));
   type state_type is (idle, startSs, readss, writeSs, stError, stop,
		stSAck, MackSttt, stMNAckStop, MNAckStartSs, stopError);
   signal state, myState : state_type;
	attribute fsm_encoding of state: signal is "gray";
	signal dSda, ddSda, dScl : std_logic;
	signal busState : busState_type := busUnknown;
	signal startFi, stopFi : std_logic;
	signal sdaSync, sclSync : std_logic_vector(2 downto 0);
    signal busFreeCnt, sclCnt : natural range cicTS downto 0 := cicTS;
   	signal errTypeR, errType : error_type;
	signal timeOutCnt : natural range TIMEOUT_CYCLES downto 0 := TIMEOUT_CYCLES;
	signal waitSlaveW, lostArb : std_logic;
	signal dataByte, loadByte, adresaCurenta : std_logic_vector(7 downto 0);
	signal stareSub : std_logic_vector(1 downto 0) := "00";
	signal latchData, latchAddr, iDone, iErr, iSda, iScl, shiftBit, dataBitOut, rwBit, adrDataNN : std_logic;
	signal resetIni : std_logic := '0';
	signal sdaR, rScl : std_logic := '1';
	signal bitCount : natural range 0 to 7 := 7;
begin


mySync: process(CLK)
   begin
      if Rising_Edge(CLK) then
			sdaSync(0) <= SDA;
			sdaSync(1) <= sdaSync(0);
			sdaSync(2) <= sdaSync(1);
			sclSync(0) <= SCL;
			sclSync(1) <= sclSync(0);
			sclSync(2) <= sclSync(1);
      end if;
   end process;
	
	dSda <= sdaSync(1);
    dScl <= sclSync(1);
	ddSda <= sdaSync(2);
	stopFi <= dScl and dSda and not ddSda;
	startFi <= dScl and not dSda and ddSda;


TWISTATE: process(CLK)
   begin
      if Rising_Edge(CLK) then
			if (resetIni = '1') then
				busState <= busUnknown;
         elsif (startFi = '1') then
            busState <= busBusy;
			elsif (busFreeCnt = 0) then
            busState <= busFree;
         end if;
      end if;
   end process;

TBUF_CNT: process(CLK)
   begin
      if Rising_Edge(CLK) then
         if (dSCL = '0' or dSDA = '0' or resetIni = '1') then
            busFreeCnt <= cicTS;
         elsif (dSCL = '1' and dSDA = '1') then
            busFreeCnt <= busFreeCnt - 1;
         end if;
      end if;
   end process;
	
   lostArb <=     '1' when (dSCL = '1' and dSDA = '0' and sdaR = '1') else
                  '0';
   waitSlaveW <=   '1' when (dSCL = '0' and rScl = '1') else
                  '0';




   RST_PROC: process (CLK)
   begin
      if Rising_Edge(CLK) then
         if (state = idle and SRST = '0') then
            resetIni <= '0';
         elsif (SRST = '1') then
            resetIni <= '1';
         end if;
      end if;
   end process;
	

sclcount: process (CLK)
	begin
		if Rising_Edge(CLK) then
			if (sclCnt = 0 or state = idle) then
				sclCnt <= cicTS/4;
			elsif (waitSlaveW = '0') then
				sclCnt <= sclCnt - 1;
			end if;
		end if;
	end process;


UnblockTimeout: if aSlaveUnblock generate
timeo_c: process (CLK)
	begin
		if Rising_Edge(CLK) then
			if (state /= idle or busState = busFree or ((ddSda xor dSda) = '1')) then
				timeOutCnt <= TIMEOUT_CYCLES;
			else
				timeOutCnt <= timeOutCnt - 1;
			end if;
		end if;
	end process;
end generate;


shRegDataByte: process (CLK)
	begin
		if Rising_Edge(CLK) then
			if ((latchData = '1' or latchAddr = '1') and sclCnt = 0) then
				dataByte <= loadByte;
				bitCount <= 7;
				if (latchData = '1') then
					adrDataNN <= '0';
				else
					adrDataNN <= '1';
				end if;
			elsif (shiftBit = '1' and sclCnt = 0) then
				dataByte <= dataByte(dataByte'high-1 downto 0) & dSDA;
				bitCount <= bitCount - 1;
			end if;
		end if;
	end process;

	loadByte <= A_I when latchAddr = '1' else
					D_I;
	dataBitOut <= dataByte(dataByte'high);
	
	D_O <= dataByte;

adresaCurenta_REG: process (CLK)
	begin
		if Rising_Edge(CLK) then
			if (latchAddr = '1') then
				adresaCurenta <= A_I;
			end if;
		end if;
	end process;
	
	rwBit <= adresaCurenta(0);

stareSub_CNT: process (CLK)
   begin
      if Rising_Edge(CLK) then
			if (state = idle) then
				stareSub <= "00";
			elsif (sclCnt = 0) then
				stareSub <= stareSub + 1;
			end if;
		end if;
	end process;
	
SYNC_PROC: process (CLK)
   begin
      if Rising_Edge(CLK) then
         state <= myState;
			
			sdaR <= iSda;
         rScl <= iScl;
			if (resetIni = '1') then
				doneOutput <= '0';
				EROAREOo <= '0';
				errTypeR <= errType;
			else
				doneOutput <= iDone;
				EROAREOo <= iErr;
				errTypeR <= errType;
			end if;
      end if;
   end process;

OUTPUT_DECODE: process (myState, stareSub, state, errTypeR, dataByte(0),
	sclCnt, bitCount, sdaR, rScl, dataBitOut, lostArb, dSda, adrDataNN)
   begin
		iSda <= sdaR;
		iDone <= '0';
		iScl <= rScl;
		iErr <= '0';
		errType <= errTypeR;
		latchAddr <= '0';
		shiftBit <= '0';
		latchData <= '0';
		
		if (state = startSs) then
			case (stareSub) is
				when "00" =>
					iSda <= '1';
				when "01" =>
					iSda <= '1';
					iScl <= '1';
				when "10" =>
					iSda <= '0';
					iScl <= '1';
				when "11" =>
					iSda <= '0';
					iScl <= '0';
				when others =>
			end case;
		end if;
		
		if (state = stop or state = stopError) then
			case (stareSub) is
				when "00" =>
					iSda <= '0';
				when "01" =>
					iSda <= '0';
					iScl <= '1';
				when "10" =>
					iSda <= '1';
					iScl <= '1';
				when "11" =>
					iScl <= '0';
				when others =>					
			end case;
		end if;
		
		if (state = readss or state = stSAck) then
			case (stareSub) is
				when "00" =>
					iSda <= '1';
				when "01" =>
					iScl <= '1';
				when "10" =>
					iScl <= '1';
				when "11" =>
					iScl <= '0';
				when others =>					
			end case;
		end if;

		if (state = MackSttt) then
			case (stareSub) is
				when "00" =>
					iSda <= '0';
				when "01" =>
					iScl <= '1';
				when "10" =>
					iScl <= '1';
				when "11" =>
					iScl <= '0';
				when others =>					
			end case;
		end if;

				if (state = writeSs) then
			case (stareSub) is
				when "00" =>
					iSda <= dataBitOut;
				when "01" =>
					iScl <= '1';
				when "10" =>
					iScl <= '1';
				when "11" =>
					iScl <= '0';
				when others =>
			end case;
		end if;

		
		if (state = stMNAckStop or state = MNAckStartSs) then
			case (stareSub) is
				when "00" =>
					iSda <= '1';
				when "01" =>
					iScl <= '1';
				when "10" =>
					iScl <= '1';
				when "11" =>
					iScl <= '0';
				when others =>					
			end case;
		end if;
		
		if (state = stSAck and sclCnt = 0 and stareSub = "01") then
			if (dSda = '1') then
				iErr <= '1';
				iDone <= '1';
				errType <= errNAck;
			elsif (adrDataNN = '0') then
				iDone <= '1';
			end if;
		end if;
		
		if (state = readss and stareSub = "01" and sclCnt = 0 and bitCount = 0) then
			iDone <= '1';
		end if;
		
		if (state = writeSs and lostArb = '1') then
			errType <= errArb;
			iDone <= '1';
			iErr <= '1';
		end if;
		
		if ((state = writeSs and sclCnt = 0 and stareSub = "11") or
			((state = stSAck or state = readss) and stareSub = "01")) then
			shiftBit <= '1';
		end if;
		
		if (state = startSs) then
			latchAddr <= '1';
		end if;
		
		if (state = stSAck and stareSub = "11") then
			latchData <= '1';
		end if;
		
	end process;
	
NEXT_STATE_DECODE: process (state, busState, waitSlaveW, lostArb, stbIi, imsg,
SRST, stareSub, bitCount, resetIni, dataByte, A_I, adresaCurenta, rwBit, sclCnt, adrDataNN)
   begin
      
      myState <= state;
   
      case (state) is
         when idle =>
            if (stbIi = '1' and busState = busFree and SRST = '0') then
               myState <= startSs;
				elsif (aSlaveUnblock and timeOutCnt = 0) then
					myState <= stop;
            end if;
				
         when startSs =>
            if (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11") then
						myState <= writeSs;
					end if;
				end if;
			
			when writeSs =>
				if (lostArb = '1') then
					myState <= idle;
				elsif (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11" and bitCount = 0) then
						myState <= stSAck;
					end if;
				end if;
			
			when stSAck =>
				if (sclCnt = 0) then
					if (resetIni = '1' or (stareSub = "11" and dataByte(0) = '1')) then
						myState <= stop;
					elsif (stareSub = "11") then
						if (adrDataNN = '1') then
							if (rwBit = '1') then
								myState <= readss;
							else
								myState <= writeSs;
							end if;
						elsif (stbIi = '1') then
							if (imsg = '1' or adresaCurenta /= A_I) then
								myState <= startSs;
							else
								if (rwBit = '1') then
									myState <= readss;
								else
									myState <= writeSs;
								end if;
							end if;
						else
							myState <= stop;
						end if;
					end if;
				end if;
				
         when stop =>
				if (stareSub = "10" and sclCnt = 0 and lostArb = '0') then
					myState <= idle;
				end if;

			
			when readss =>
				if (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11" and bitCount = 7) then
						if (stbIi = '1') then
							if (imsg = '1' or adresaCurenta /= A_I) then
								myState <= MNAckStartSs;
							else
								myState <= MackSttt;
							end if;
						else
							myState <= stMNAckStop;
						end if;
					end if;
				end if;
			
			when MackSttt =>
				if (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11") then
						myState <= readss;
					end if;
				end if;
			
			when MNAckStartSs =>
				if (lostArb = '1') then
					myState <= idle;
				elsif (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11") then
						myState <= startSs;
					end if;
				end if;
			
			when stMNAckStop =>
				if (lostArb = '1') then
					myState <= idle;
				elsif (sclCnt = 0) then
					if (resetIni = '1') then
						myState <= stop;
					elsif (stareSub = "11") then
						myState <= stop;
					end if;
				end if;
				
         when others =>
            myState <= idle;
      end case;      
   end process;

   SDA <= 'Z' when sdaR = '1' else
          '0';
   SCL <= 'Z' when rSCL = '1' else
          '0';
			 
end Behavioral;
