


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.math_real.all;
use work.TWIUtils.ALL;


entity senzorTemperatura is
Generic (fregvClock : natural := 100);
	Port (
		TMP_SCL : inout STD_LOGIC;
		TMP_SDA : inout STD_LOGIC;
		TempOut : out STD_LOGIC_VECTOR(12 downto 0);
		RdyOut : out STD_LOGIC;
		EROAREOo : out STD_LOGIC;
		clkInput : in STD_LOGIC;
		SrstInput : in STD_LOGIC
	);
end senzorTemperatura;

architecture Behavioral of senzorTemperatura is


	component TWICtl
   generic 
   (
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
end component;
	constant IWR : std_logic := '0';
	constant IRD : std_logic := '1';

    constant ADT7420ID : std_logic_vector(7 downto 0)       := x"CB";
	constant ADT7420RID : std_logic_vector(7 downto 0)      := x"0B";
	constant ADT7420RRESET : std_logic_vector(7 downto 0)   := x"2F";
    constant ADT7420ADDRess : std_logic_vector(7 downto 1)     := "1001011";
	constant ADT7420RdTEMP : std_logic_vector(7 downto 0)    := x"00";
    constant RETRY_COUNT : NATURAL := 10;
	constant DELAY : NATURAL := 1;
	constant CYCLESDelay : NATURAL := natural(ceil(real(DELAY*1000*fregvClock)));

   type state_type is (
                        idle,
                        IniRegSt,
                        stInitData,
                        stRetry,
                        readssTempR,
                        readssTempD1,
                        readssTempD2,
                        stError
                        ); 
   signal state, myState : state_type;

	
   constant NO_OF_INIT_VECTORS : natural := 3;
	constant DATA_WIDTH : integer := 1 + 8 + 8;
	constant ADDR_WIDTH : natural := natural(ceil(log(real(NO_OF_INIT_VECTORS), 2.0)));
	
	type TempSensInitMap_type is array (0 to NO_OF_INIT_VECTORS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
	signal TempSensInitMap: TempSensInitMap_type := (
		IRD & x"0B" & x"CB",
		IWR & x"2F" & x"00",
		IRD & x"0B" & x"CB"
		);	
	signal initEn : std_logic;
	signal wordini: std_logic_vector (DATA_WIDTH-1 downto 0);
	signal twiMsg, twiStb, DoneTWI, errTwi : std_logic;
	signal iniA : natural range 0 to NO_OF_INIT_VECTORS := 0;
	signal twiDi, dotwi, twiAddr : std_logic_vector(7 downto 0);
	signal waitCnt : natural range 0 to CYCLESDelay := CYCLESDelay;
	signal waitCntEn : std_logic;
	signal retryCnt : natural range 0 to RETRY_COUNT := RETRY_COUNT;
	signal tempReg : std_logic_vector(15 downto 0) := (others => '0');
	signal retryCntEn : std_logic;
   signal fReady : boolean := false;
begin


TempOut <= tempReg(15 downto 3);

EROAREOo <= '1' when state = stError else
			'0';
RdyOut <= '1' when fReady else
			'0';

ctlCtl : TWICtl
	generic map (
		aSlaveUnblock => true,
		fregvClock => 100
	)
	port map (
		imsg => twiMsg,
		stbIi => twiStb,
		A_I => twiAddr,
		D_I => twiDi,
		D_O => dotwi,
		doneOutput => DoneTWI,
      EROAREOo => errTwi,
		errTp => open,
		CLK => clkInput,
		SRST => SrstInput,
		SDA => TMP_SDA,
		SCL => TMP_SCL
	);

	wordini <= TempSensInitMap(iniA);
	iniA_CNT: process (clkInput)
	begin
		if Rising_Edge(clkInput) then
			if (state = idle or iniA = NO_OF_INIT_VECTORS) then
				iniA <= 0;
			elsif (initEn = '1') then
				iniA <= iniA + 1;
			end if;
		end if;
	end process;
	

	Wait_CNT: process (clkInput)
	begin
		if Rising_Edge(clkInput) then
			if (waitCntEn = '0') then
				waitCnt <= CYCLESDelay;
			else
				waitCnt <= waitCnt - 1;
			end if;
		end if;
	end process;
	
	Retry_CNT: process (clkInput)
	begin
		if Rising_Edge(clkInput) then
			if (state = idle) then
				retryCnt <= RETRY_COUNT;
			elsif (retryCntEn = '1') then
				retryCnt <= retryCnt - 1;
			end if;
		end if;
	end process;

	TemperatureReg: process (clkInput)
	variable temp : std_logic_vector(7 downto 0);
	begin
		if Rising_Edge(clkInput) then
			if (state = readssTempD1 and DoneTWI = '1' and errTwi = '0') then
				temp := dotwi;
			end if;
			if (state = readssTempD2 and DoneTWI = '1' and errTwi = '0') then
				tempReg <= temp & dotwi;
			end if;
		end if;
	end process;
	

	ReadyFlag: process (clkInput)
	begin
		if Rising_Edge(clkInput) then
			if (state = idle or state = stError) then
				fReady <= false;
			elsif (state = readssTempD2 and DoneTWI = '1' and errTwi = '0') then
				fReady <= true;
			end if;
		end if;
	end process;	
	

  synicppp: process (clkInput)
   begin
      if (clkInput'event and clkInput = '1') then
         if (SrstInput = '1') then
            state <= idle;
         else
            state <= myState;
         end if;        
      end if;
   end process;
 
   OUTPUT_DECODE: process (state, wordini, DoneTWI, errTwi, dotwi, retryCnt, waitCnt, iniA)
   begin
		twiStb <= '0';
		waitCntEn <= '0';
		twiMsg <= '0';
		twiDi <= "--------";
		retryCntEn <= '0';
		twiAddr <= ADT7420ADDRess & '0';
		initEn <= '0';

		
		case (state) is
         when idle =>
			
         when IniRegSt =>
                twiStb <= '1';
				twiMsg <= '1';
				twiAddr(0) <= IWR;
				twiDi <= wordini(15 downto 8);
			when stInitData =>
            twiStb <= '1';
				twiAddr(0) <= wordini(wordini'high);
				twiDi <= wordini(7 downto 0);
				if (DoneTWI = '1' and
					(errTwi = '0' or (wordini(16) = IWR and wordini(15 downto 8) = ADT7420RRESET)) and
					(wordini(wordini'high) = IWR or dotwi = wordini(7 downto 0))) then
					initEn <= '1';
				end if;
			when stRetry=>
				if (retryCnt /= 0) then				
					waitCntEn <= '1';
					if (waitCnt = 0) then
						retryCntEn <= '1';
					end if;
				end if;
			
			when readssTempR =>
			    twiMsg <= '1';
			    twiDi <= ADT7420RdTEMP;
				twiStb <= '1';
				twiAddr(0) <= IWR;
			when readssTempD1 =>
				twiStb <= '1';
				twiAddr(0) <= IRD;
			when readssTempD2 =>
            twiStb <= '1';
				twiAddr(0) <= IRD;
				
			when stError =>
				null;
		end case;
			
   end process;
 
   NEXT_STATE_DECODE: process (state, DoneTWI, errTwi, wordini, dotwi, retryCnt, waitCnt)
   begin
      myState <= state;

      case (state) is
         when idle =>
            myState <= IniRegSt;
				
         when IniRegSt =>
            if (DoneTWI = '1') then
					if (errTwi = '1') then
						myState <= stRetry;
					else
						myState <= stInitData;
					end if;
				end if;
				
			when stInitData =>
            if (DoneTWI = '1') then
					if (errTwi = '1') then
						myState <= stRetry;
					else
						if (wordini(wordini'high) = IRD and dotwi /= wordini(7 downto 0)) then
							myState <= stRetry;
						elsif (iniA = NO_OF_INIT_VECTORS-1) then
							myState <= readssTempR;
						else
							myState <= IniRegSt;
						end if;
					end if;
				end if;				
			when stRetry =>
				if (retryCnt = 0) then
					myState <= stError;
				elsif (waitCnt = 0) then
					myState <= IniRegSt;
				end if;
				
			when readssTempR =>
            if (DoneTWI = '1') then
					if (errTwi = '1') then
						myState <= stError;
					else
						myState <= readssTempD1;
					end if;
				end if;
			when readssTempD1 =>
            if (DoneTWI = '1') then
					if (errTwi = '1') then
						myState <= stError;
					else
						myState <= readssTempD2;
					end if;
				end if;
			when readssTempD2 =>
            if (DoneTWI = '1') then
					if (errTwi = '1') then
						myState <= stError;
					else
						myState <= readssTempR;
					end if;
				end if;

         when stError =>
				null;
				
         when others =>
            myState <= idle;
      end case;      
   end process;

end Behavioral;
