(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRTP                                                                     *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRTP;

interface

uses Windows, KRThread, KRSockets, KRTypes, lgop;

const
  KRRTP_Version = 2;

type
  TKRRTPPack = record
    PayloadType: byte;
    Sequence: Word;
    Timestamp: Cardinal;
    Payload: PByte;
    PayloadLength: integer;
  end;
  PKRRTPPack = ^TKRRTPPack;

  TKRRTPData = procedure(ASender: TObject; APack: PKRRTPPack) of Object;

  TKRRTP = class(TKRThread)
  private
    FSequence: word;
    FClient: TKRSocketClient;
    FBuf: TKRBuffer;
    buf: TKRBuffer;
    FInSoureceID: cardinal;
    FOnRecv: TKRRTPData;
    FLastRecvSequence: Word;
    function GetMarker: boolean;
    procedure SetMarker(const Value: boolean);
    function GetOutSoureceID: Cardinal;
    procedure SetOutSoureceID(const Value: Cardinal);
  protected
    procedure KRExecute; override;
  public
    constructor CreateRTP(AClient: TKRSocketClient; APayloadType: byte);
    property Marker: boolean read GetMarker write SetMarker;
    procedure Send(APack: PKRRTPPack);
    property LastRecvSequence: Word read FLastRecvSequence;
    property OnRecv: TKRRTPData read FOnRecv write FOnRecv;
    property InSoureceID: Cardinal read FInSoureceID write FInSoureceID;
    property OutSoureceID: Cardinal read GetOutSoureceID write SetOutSoureceID;
  end;

implementation

{ TKRRTP }

constructor TKRRTP.CreateRTP(AClient: TKRSocketClient; APayloadType: byte);
begin
  FClient:=AClient;
  FillChar(FBuf, Sizeof(TKRBuffer), #0);
  FBuf[0]:=KRRTP_Version shl 6;
  FBuf[1]:=APayloadType;
  SetOutSoureceID(getTickCount*100+cardinal(self) mod 100);
  FSequence:=0;
  inherited Create;
  WaitTime:=2;
end;

function TKRRTP.GetMarker: boolean;
begin
  Result:=GetBit(FBuf[1],7);
end;

function TKRRTP.GetOutSoureceID: Cardinal;
begin
  Result:=PCardinal(@FBuf[8])^;
end;

procedure TKRRTP.KRExecute;
var
  n: integer;
  pack: PKRRTPPack;
begin
  n:=FClient.ReceiveBuf(buf,1024);
  if n>12 then begin
    new(pack);
    pack^.PayloadType:=ClearBit(buf[1],7);
    pack^.Sequence:=BytesToWord(buf[3],buf[2]);
    FLastRecvSequence:=pack^.Sequence;
    pack^.Timestamp:=LSwap(PCardinal(@buf[4])^);
    pack^.PayloadLength:=n-12;
    pack^.Payload:=@buf[12];
    if Assigned(FOnRecv) then FOnRecv(Self,pack);
    Dispose(pack);
  end;
end;

procedure TKRRTP.Send(APack: PKRRTPPack);
var
  i: integer;
begin
  FBuf[1]:=SetBitTo(FBuf[1],7,FSequence=0);
  inc(FSequence);
  PWORD(@FBuf[2])^:=swap(FSequence);
  PCardinal(@FBuf[4])^:=LSwap(GetTickCount);
  for I := 0 to APack^.PayloadLength do FBuf[i+12]:=APack^.Payload[i];
  FClient.SendBuf(FBuf,APack^.PayloadLength+12);
end;

procedure TKRRTP.SetMarker(const Value: boolean);
begin
  FBuf[1]:=SetBitTo(FBuf[1],7,Value);
end;

procedure TKRRTP.SetOutSoureceID(const Value: Cardinal);
begin
  PCardinal(@FBuf[8])^:=Value;
end;

end.
