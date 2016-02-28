

architecture rtl of i2ctransfercontroller is

  -- defining states and internal signals
  TYPE StateType IS (Idle,Sending,ReadingFiFo,ReadingAck,Receiving,Finishing,ReadingErrAck);

  SIGNAL   ByteCount_s            : std_logic_vector(ReadCountWidth_g-1 downto 0);
  -- 2^(ReadCountWidth_g-1)=Maximum nr. of bytes to read.
  -- Its also recommended not to write more than this number of Bytes in one
  -- writing-process. Writing more than ByteCount_s bytes may cause in
  -- incorrect error-messages in the ReadingAck-State.
  SIGNAL   NextByteCount_s        : std_logic_vector(ReadCountWidth_g-1 downto 0);
  SIGNAL   State                  : StateType;
  SIGNAL   NextState              : StateType;

  SIGNAL   ErrBusColl_s           : STD_LOGIC;
  SIGNAL   NextErrBusColl_s       : STD_LOGIC;
  SIGNAL   ErrFiFoFull_s          : STD_LOGIC;
  SIGNAL   NextErrFiFoFull_s      : STD_LOGIC;
  SIGNAL   ErrGotNAck_s           : STD_LOGIC;
  SIGNAL   NextErrGotNAck_s       : STD_LOGIC;
  SIGNAL   ErrCoreBusy_s          : STD_LOGIC;
  SIGNAL   NextErrCoreBusy_s      : STD_LOGIC;
  SIGNAL   ErrFiFoEmpty_s         : STD_LOGIC;
  SIGNAL   NextErrFiFoEmpty_s     : STD_LOGIC;
  SIGNAL   ErrCoreStopped_s       : STD_LOGIC;
  SIGNAL   NextErrCoreStopped_s   : STD_LOGIC;
  SIGNAL   ErrDevNotPresent_s     : STD_LOGIC;
  SIGNAL   NextErrDevNotPresent_s : STD_LOGIC;
  SIGNAL   ErrReadCountZero_s     : STD_LOGIC;
  SIGNAL   NextErrReadCountZero_s : STD_LOGIC;
begin

  -- sequential statements
  sequential: process(Clk_i,Reset_i)
  begin

    -- Asynchronous Reset
    if Reset_i='1' then
      State                <= Idle;
      ByteCount_s          <= (others => '0');

      ErrBusColl_s         <= '0';
      ErrFiFoFull_s        <= '0';
      ErrGotNAck_s         <= '0';
      ErrCoreBusy_s        <= '0';
      ErrFiFoEmpty_s       <= '0';
      ErrCoreStopped_s     <= '0';
      ErrDevNotPresent_s   <= '0';
      ErrReadCountZero_s   <= '0';

    -- Clk_i Tick
    elsif rising_edge(Clk_i) then
      State       <= NextState;
      ByteCount_s <= NextByteCount_s;

      ErrBusColl_s         <= NextErrBusColl_s;
      ErrFiFoFull_s        <= NextErrFiFoFull_s;
      ErrGotNAck_s         <= NextErrGotNAck_s;
      ErrCoreBusy_s        <= NextErrCoreBusy_s;
      ErrFiFoEmpty_s       <= NextErrFiFoEmpty_s;
      ErrCoreStopped_s     <= NextErrCoreStopped_s;
      ErrDevNotPresent_s   <= NextErrDevNotPresent_s;
      ErrReadCountZero_s   <= NextErrReadCountZero_s;
    end if;
  end process;


  -- combinational statements
  combinational: process(State,ByteCount_s,ErrBusColl_s,ErrFiFoFull_s,ErrGotNAck_s,ErrCoreBusy_s,ErrFiFoEmpty_s,ErrCoreStopped_s,ErrDevNotPresent_s,ErrReadCountZero_s,ReadCount_i,StartProcess_i,ReceiveSend_n_i,FiFoEmpty_i,FiFoFull_i,CoreAckRx_i,CoreAckValid_i,CoreBusy_i,CoreByteReady_i,CoreBusErr_i,ErrAck_i) --Sensitivity-List: State,Inputs & Signals
  begin

    --Setting Defaults
    NextState              <= Idle;
    NextByteCount_s        <= ByteCount_s;

    NextErrBusColl_s       <= ErrBusColl_s;
    NextErrFiFoFull_s      <= ErrFiFoFull_s;
    NextErrGotNAck_s       <= ErrGotNAck_s;
    NextErrCoreBusy_s      <= ErrCoreBusy_s;
    NextErrFiFoEmpty_s     <= ErrFiFoEmpty_s;
    NextErrCoreStopped_s   <= ErrCoreStopped_s;
    NextErrDevNotPresent_s <= ErrDevNotPresent_s;
    NextErrReadCountZero_s <= ErrReadCountZero_s;

    Busy_o                 <= '1';
    FiFoReadNext_o         <= '0';
    FiFoWrite_o            <= '0';
    CoreDoTransfer_o       <= '0';
    CoreReadWrite_n_o      <= ReceiveSend_n_i;
    CoreAckTx_o            <= '0';

    case State is

      when Idle =>
            -- Error: FiFo is empty
            if StartProcess_i = '1' AND FiFoEmpty_i = '1' then
               NextErrFiFoEmpty_s     <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Core is busy
            elsif StartProcess_i = '1' AND CoreBusy_i = '1' then
               NextErrCoreBusy_s      <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Bus-Collision
            elsif StartProcess_i = '1' AND CoreBusErr_i = '1' then
               NextErrBusColl_s       <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Receive-Mode but ReadCount_i = 0
            elsif StartProcess_i = '1' AND ReceiveSend_n_i = '1' AND CONV_INTEGER(ReadCount_i) = 0 then
               NextErrReadCountZero_s <= '1';
               NextState              <= ReadingErrAck;
            -- No Errors & StartProcess
            elsif StartProcess_i = '1' AND FiFoEmpty_i = '0' AND CoreBusy_i = '0' then
               CoreDoTransfer_o       <= '1';
               NextState              <= Sending;
            -- No Action - Stay in Idle-State
            else
               Busy_o                 <= '0';
               NextState              <= Idle;
            end if;

      when Sending =>
            -- Error: Bus-Collision
            if CoreBusErr_i = '1' then
               NextErrBusColl_s       <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Core stopped unexpected
            elsif CoreBusy_i = '0' then
               NextErrCoreStopped_s   <= '1';
               NextState              <= ReadingErrAck;
            -- Got CoreByteReady_i > Addressbyte sent, reading FiFo
            elsif CoreByteReady_i = '1' then
               FiFoReadNext_o         <= '1';
               CoreDoTransfer_o       <= '1';
               NextState              <= ReadingFiFo;
            -- Stay in Sending-State
            else
               CoreDoTransfer_o       <= '1';
               NextState              <= Sending;
            end if;

      when ReadingFiFo =>
            -- Error: Bus-Collision
            if CoreBusErr_i = '1' then
               NextErrBusColl_s       <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Core stopped unexpected
            elsif CoreBusy_i = '0' then
               NextErrCoreStopped_s   <= '1';
               NextState              <= ReadingErrAck;
            -- Sent all Bytes: FiFo is empty
            elsif FiFoEmpty_i = '1' then
               -- CoreDoTransfer_o    <= '0';
               NextState              <= ReadingAck;
            else
               CoreDoTransfer_o       <= '1';
               NextState              <= ReadingAck;
            end if;

      when ReadingAck =>
            -- Error: Bus-Collision
            if CoreBusErr_i = '1' then
               NextErrBusColl_s       <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Receiving but FiFo is full
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '1' AND ReceiveSend_n_i = '1' AND FiFoFull_i = '1' then
               NextErrFiFoFull_s      <= '1';
               NextState              <= ReadingErrAck;
            -- Error: NAck > Addressed device is not present on the bus
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '0' AND ByteCount_s = 0 then
               NextErrDevNotPresent_s <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Device sent NAck
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '0' AND ByteCount_s > 0 then
               NextErrGotNAck_s       <= '1';
               NextState              <= ReadingErrAck;
            -- Error: Core stopped unexpected
            elsif CoreBusy_i = '0' then
               NextErrCoreStopped_s   <= '1';
               NextState              <= ReadingErrAck;
            -- Got Ack - Continue sending next Byte
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '1' AND ReceiveSend_n_i = '0' AND FiFoEmpty_i = '0' then
               CoreDoTransfer_o       <= '1';
               NextByteCount_s        <= ByteCount_s+1;
               NextState              <= Sending;
            -- Got Ack on addressbyte - Continue with receiving phase
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '1' AND ReceiveSend_n_i = '1' AND FiFoFull_i = '0' then
               CoreDoTransfer_o       <= '1';
               CoreAckTx_o            <= '1';
               NextState              <= Receiving;
            -- All bytes sent - Continue with finishing phase
            elsif CoreAckValid_i = '1' AND CoreAckRx_i = '1' AND ReceiveSend_n_i = '0' AND FiFoEmpty_i = '1' then
               -- CoreDoTransfer_o    <= '0';
               NextState              <= Finishing;
            -- don't sample Ack
            elsif CoreAckValid_i = '0' then
               NextState              <= ReadingAck;
            else -- should not be possible
               -- back to idle
            end if;


      when Receiving =>
            -- Setting CoreAckTx_o = 0 for last Byte-Transfer
            if CONV_INTEGER(ReadCount_i) = (ByteCount_s) then
               CoreAckTx_o <= '0';
            else
               CoreAckTx_o <= '1';
            end if;

            -- Error: Bus-Collision
            if CoreBusErr_i = '1' then
               NextErrBusColl_s     <= '1';
               NextState            <= ReadingErrAck;
            -- Error: FiFo is full
            elsif FiFoFull_i = '1' then
               CoreAckTx_o          <= '0';
               NextErrFiFoFull_s    <= '1';
               NextState            <= ReadingErrAck;
            -- Error: Core stopped unexpected
            elsif CoreBusy_i = '0' then
               NextErrCoreStopped_s <= '1';
               NextState            <= ReadingErrAck;
            elsif CoreByteReady_i = '1' AND CONV_INTEGER(ReadCount_i) > (ByteCount_s + 1) then
               NextByteCount_s      <= ByteCount_s + 1;
               CoreDoTransfer_o     <= '1';
               FiFoWrite_o          <= '1';
               NextState      <= Receiving;
            elsif CoreByteReady_i = '1' AND CONV_INTEGER(ReadCount_i) = (ByteCount_s + 1) then
               NextByteCount_s      <= ByteCount_s + 1;
               -- CoreDoTransfer_o  <= '0';
               FiFoWrite_o          <= '1';
               NextState            <= Finishing;
            else
               CoreDoTransfer_o     <= '1';
               NextState            <= Receiving;
            end if;

      when Finishing =>
            -- Core finished
            if CoreBusy_i = '0' then
               Busy_o           <= '0';
               NextState        <= Idle;
               NextByteCount_s  <= (others => '0');
            -- Core still working
            else
               NextState        <= Finishing;
            end if;


      when ReadingErrAck =>
            -- Error(s) were acknowledged - Delete all Errors and go to finishing-state
            if ErrAck_i = '1' then
               NextErrBusColl_s        <= '0';
               NextErrFiFoFull_s       <= '0';
               NextErrGotNAck_s        <= '0';
               NextErrCoreBusy_s       <= '0';
               NextErrFiFoEmpty_s      <= '0';
               NextErrCoreStopped_s    <= '0';
               NextErrDevNotPresent_s  <= '0';
               NextErrReadCountZero_s  <= '0';
               NextState               <= Finishing;
            else
               NextState               <= ReadingErrAck;
            end if;

      when others =>
               -- nothing

    end case;

  end process;

  -- Setting Outputs
  ErrBusColl_o       <= ErrBusColl_s;
  ErrFiFoFull_o      <= ErrFiFoFull_s;
  ErrGotNAck_o       <= ErrGotNAck_s;
  ErrCoreBusy_o      <= ErrCoreBusy_s;
  ErrFiFoEmpty_o     <= ErrFiFoEmpty_s;
  ErrCoreStopped_o   <= ErrCoreStopped_s;
  ErrDevNotPresent_o <= ErrDevNotPresent_s;
  ErrReadCountZero_o <= ErrReadCountZero_s;

end rtl;

