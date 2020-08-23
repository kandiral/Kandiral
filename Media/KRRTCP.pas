(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRTCP                                                                    *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRTCP;

interface

uses Windows, SysUtils, KRTypes, KRThread, KRRTP, KRSockets, lgop, MMSystem,
 Funcs;

const
  KRRTCP_Version = 2;

type
  TKRRTCP = class(TKRThread)
  private
    FRTP: TKRRTP;
    FClient: TKRSocketClient;
    FSilence,FBuf,buf: TKRBuffer;
    FSendPacketCount, FSendPacketCount2: Cardinal;
    FSendOctetCount: Cardinal;
    FLastSRTimestamp: cardinal;
    FSendSilence: boolean;
    FSilenceLen: integer;
    FSilenceLastTm, FSilenceTm: cardinal;
    FGoodbye: boolean;
    SR: boolean;
    procedure SetSendSilence(const Value: boolean);
    procedure DoSilence;
    procedure SendReport;
  protected
    procedure KRExecute; override;
  public
    constructor CreateRTCP(AClient: TKRSocketClient; ARTP: TKRRTP);
    procedure Send(APack: PKRRTPPack);
    property RTP: TKRRTP read FRTP;
    property SendSilence: boolean read FSendSilence write SetSendSilence;
    procedure Silence(var ABuf; ALength, ATime: integer);
    procedure Goodbye;
  end;

implementation

type
  TNTPTime=record
    Seconds:cardinal; // seconds since 01/01/1900 00:00
    Fracs  :cardinal; // Round($FFFFFFFF * (ms/1000))
  end;
  PNTPTime=^TNTPTime;

procedure NTPNow(PTime:PNTPTime);
var
  T:TSystemTime;
  D:TDateTime;
begin
  GetSystemTime(T);
  D:=EncodeDate(T.wYear,T.wMonth,T.wDay)+EncodeTime(T.wHour,T.wMinute,T.wSecond,T.wMilliseconds);
  PTime^.Seconds:=LSwap(Round((D-2)*SecsPerDay));
  PTime^.Fracs  :=LSwap(Round($FFFFFFFF*(T.wMilliseconds/1000)));
end;

{ TKRRTCP }

constructor TKRRTCP.CreateRTCP(AClient: TKRSocketClient; ARTP: TKRRTP);
begin
  FGoodbye:=false;
  FSendSilence:=false;
  FClient:=AClient;
  FRTP:=ARTP;
  FillChar(FBuf, Sizeof(TKRBuffer), #0);
  FBuf[0]:=KRRTCP_Version shl 6;
  FBuf[0]:=SetBit(FBuf[0],0);
  FBuf[1]:=200;
  FBuf[3]:=12;
  PCardinal(@FBuf[4])^:=FRTP.OutSoureceID;
  PCardinal(@FBuf[28])^:=FRTP.OutSoureceID;
  FBuf[52]:=KRRTCP_Version shl 6;
  FBuf[52]:=SetBit(FBuf[52],0);
  FBuf[53]:=202;
  FBuf[55]:=2;
  PCardinal(@FBuf[56])^:=FRTP.OutSoureceID;
  FBuf[60]:=1;
  FBuf[64]:=KRRTCP_Version shl 6;
  FBuf[64]:=SetBit(FBuf[64],0);
  FBuf[65]:=203;
  FBuf[67]:=1;
  PCardinal(@FBuf[68])^:=FRTP.OutSoureceID;
  FSendPacketCount:=0;
  FLastSRTimestamp:=0;
  FSendPacketCount2:=0;
  SR:=true;
  inherited Create;
  WaitTime:=5;
end;


procedure TKRRTCP.DoSilence;
var
  Pack: TKRRTPPack;
begin
  if ElapsedTime(FSilenceLastTm)>=FSilenceTm then begin
    FSilenceLastTm:=timeGetTime;
    Pack.PayloadType:=0;
    Pack.Timestamp:=getTickCount;
    Pack.Payload:=@FSilence[0];
    Pack.PayloadLength:=FSilenceLen;
    Send(@Pack);
  end;
end;

procedure TKRRTCP.Goodbye;
begin
  FGoodbye:=true;
  SendReport;
  Active:=false;
  FRTP.Active:=false;
end;

procedure TKRRTCP.KRExecute;
var
  n: integer;
  tm:TNTPTime;
begin
  if FGoodbye then exit;

  if FSendSilence then DoSilence;

  n:=FClient.ReceiveBuf(buf,1024);
  if n>0 then begin
    NTPNow(@tm);
    FLastSRTimestamp:=tm.Seconds shr 16+tm.Fracs shl 16;
    SR:=true;
  end;
end;

procedure TKRRTCP.Send(APack: PKRRTPPack);
begin
  if FGoodbye then exit;

  FRTP.Send(APack);
  inc(FSendPacketCount);
  inc(FSendPacketCount2);
  inc(FSendOctetCount,APack^.PayloadLength);
  if (FSendPacketCount2> 150)and(SR) then begin
    SR:=false;
    FSendPacketCount2:=0;
    SendReport;
  end;
end;

procedure TKRRTCP.SendReport;
begin
  NTPNow(@FBuf[8]);
  PCardinal(@FBuf[16])^:=LSwap(GetTickCount);
  PCardinal(@FBuf[20])^:=LSwap(FSendPacketCount);
  PCardinal(@FBuf[24])^:=LSwap(FSendOctetCount);
  PWORD(@FBuf[38])^:=swap(FRTP.LastRecvSequence);
  if FLastSRTimestamp>0 then begin
    PCardinal(@FBuf[44])^:=FLastSRTimestamp;
    PCardinal(@FBuf[48])^:=PNTPTime(@FBuf[8])^.Seconds shr 16+PNTPTime(@FBuf[8])^.Fracs shl 16-FLastSRTimestamp;
  end;
  if FGoodbye then
    FClient.SendBuf(FBuf,72)
  else
    FClient.SendBuf(FBuf,64);
end;

procedure TKRRTCP.SetSendSilence(const Value: boolean);
begin
  FSendSilence := Value;
  if FSendSilence then begin
    WaitTime:=1;
    FSilenceLastTm:=0;
  end else WaitTime:=5;
end;

procedure TKRRTCP.Silence(var ABuf; ALength, ATime: integer);
var
  I: Integer;
  B: PByte;
begin
  B:=@ABuf;
  FSilenceLen:=ALength;
  FSilenceTm:=ATime;
  for I := 0 to ALength-1 do FSilence[i]:=B[i];
end;

end.
