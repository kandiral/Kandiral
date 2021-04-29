(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRSDP                                                                     *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRSDP;

interface

uses Classes, SysUtils, StrUtils, Funcs, KRStrUtils, KRTypes;


type
  TKRSDP_IN_Addr = (spdinaddrIP4, spdinaddrIP6);
  TKRSDP_MediaType = (spdmAudio, spdmVideo, spdmApplication, spdmMessage);
  TKRSDP_Direction = (spddNotSet, spddRecvOnly, spddSendRecv, spddSendOnly,
    spddInactive);

  TKRSDP_MediaFormat = record
    PayloadType: byte;
    EncodingName: String;
    ClockRate: Cardinal;
    EncodingParameters: String;
    FormatParameters: String;
  end;
  PKRSDP_MediaFormat=^TKRSDP_MediaFormat;

  TKRSDP_Media = class
  private
    FFormats: array of TKRSDP_MediaFormat;
    FFormatsCount: integer;
    FMedia: TKRSDP_MediaType;
    FProto: String;
    FPort: word;
    FPortsCount: integer;
    function GetFormat(Index: integer): TKRSDP_MediaFormat;
  public
    constructor create(AMedia: TKRSDP_MediaType; APort: word; APortsCount: integer;
      ATransportProtocol: String);
    property Media: TKRSDP_MediaType read FMedia;
    property Port: word read FPort write FPort;
    property PortsCount: integer read FPortsCount write FPortsCount;
    property TransportProtocol: String read FProto;
    property Formats[Index: integer]: TKRSDP_MediaFormat read GetFormat;
    property FotmatsCount: integer read FFormatsCount;
    procedure AddFormat(APayloadType: byte; AEncodingName: String; AClockRate: Cardinal;
      AEncodingParameters, AFormatParameters: String);
    function GetFormatIndexByType(APayloadType: byte): integer;
  end;

  TKRSDP = class
  private
    FVersion: integer;
    FUserName: String;
    FSessionID: String;
    FSessionVersion: String;
    FAddrType: TKRSDP_IN_Addr;
    FAddr: String;
    FSessionName: String;
    FTTL: integer;
    FStopTime: Cardinal;
    FStartTime: Cardinal;
    FMedia: array of TKRSDP_Media;
    FMediaCount: integer;
    FPtime: word;
    FDirection: TKRSDP_Direction;
    function GetSessDescr: String;
    procedure SetSessDescr(const Value: String);
    function GetMedia(Index: integer): TKRSDP_Media;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Reset;
    property SessionDescription: String read GetSessDescr write SetSessDescr;
    property Version: integer read FVersion write FVersion;
    property UserName: String read FUserName write FUserName;
    property SessionID: String read FSessionID write FSessionID;
    property SessionVersion: String read FSessionVersion write FSessionVersion;
    property AddrType: TKRSDP_IN_Addr read FAddrType write FAddrType;
    property Addr: String read FAddr write FAddr;
    property TTL: integer read FTTL write FTTL;
    property SessionName: String read FSessionName write FSessionName;
    property StartTime: Cardinal read FStartTime write FStartTime;
    property StopTime: Cardinal read FStopTime write FStopTime;
    property Media[Index: integer]: TKRSDP_Media read GetMedia;
    property MediaCount: integer read FMediaCount;
    function AddMedia(AMedia: TKRSDP_MediaType; APort: word; APortsCount: integer;
      ATransportProtocol: String):TKRSDP_Media;
    function GetFormat(APayloadType: byte):PKRSDP_MediaFormat;
    property PacketTime: word read FPtime write FPtime;
    property Direction: TKRSDP_Direction read FDirection write FDirection;
  end;

implementation

{ TKRSDP }

function TKRSDP.AddMedia(AMedia: TKRSDP_MediaType; APort: word;
  APortsCount: integer; ATransportProtocol: String):TKRSDP_Media;
begin
  SetLength(FMedia,FMediaCount+1);
  FMedia[FMediaCount]:=TKRSDP_Media.create(AMedia, APort, APortsCount, ATransportProtocol);
  Result:=FMedia[FMediaCount];
  Inc(FMediaCount);
end;

constructor TKRSDP.Create;
begin
  FMediaCount:=0;
  Reset;
end;

destructor TKRSDP.Destroy;
begin
  Reset;
  inherited;
end;

function TKRSDP.GetFormat(APayloadType: byte): PKRSDP_MediaFormat;
var
  I,n: Integer;
begin
  Result:=nil;
  for I := 0 to FMediaCount-1 do begin
    n:=FMedia[i].GetFormatIndexByType(APayloadType);
    if n>-1 then begin
      result:=@FMedia[i].FFormats[n];
      break;
    end;
  end;

end;

function TKRSDP.GetMedia(Index: integer): TKRSDP_Media;
begin
  Result:=FMedia[Index];
end;

function TKRSDP.GetSessDescr: String;
var
  i,j: integer;
begin
  Result:='v='+IntToStr(FVersion)+CKR_CRLF+
    'o='+FUserName+#32+FSessionID+#32+FSessionVersion+' IN ';
  if FAddrType=spdinaddrIP4 then Result:=Result+'IP4 '
  else Result:=Result+'IP6 ';
  Result:=Result+FAddr+CKR_CRLF+
    's='+FSessionName+CKR_CRLF+
    'c=IN ';
  if FAddrType=spdinaddrIP4 then Result:=Result+'IP4 '
  else Result:=Result+'IP6 ';
  Result:=Result+FAddr;
  if FTTL>0 then Result:=Result+'/'+IntToStr(FTTL);
  Result:=Result+CKR_CRLF+
    't='+IntToStr(FStartTime)+#32+IntToStr(FStopTime)+CKR_CRLF;
  for I := 0 to FMediaCount-1 do begin
    Result:=Result+'m=';
    case FMedia[i].FMedia of
    spdmAudio:Result:=Result+'audio ';
    spdmVideo:Result:=Result+'video ';
    spdmApplication:Result:=Result+'application ';
    spdmMessage:Result:=Result+'message ';
    end;
    Result:=Result+IntToStr(FMedia[i].FPort);
    if FMedia[i].FPortsCount>1 then Result:=Result+'/'+IntToStr(FMedia[i].FPortsCount);
    Result:=Result+#32+FMedia[i].FProto;
    for j := 0 to FMedia[i].FFormatsCount-1 do
      Result:=Result+#32+IntToStr(FMedia[i].FFormats[j].PayloadType);
    Result:=Result+CKR_CRLF;
  end;
  for I := 0 to FMediaCount-1 do
    for j := 0 to FMedia[i].FFormatsCount-1 do begin
      Result:=Result+'a=rtpmap:'+IntToStr(FMedia[i].FFormats[j].PayloadType)+#32+
        FMedia[i].FFormats[j].EncodingName;
      if FMedia[i].FFormats[j].ClockRate>0 then begin
        Result:=Result+'/'+IntToStr(FMedia[i].FFormats[j].ClockRate);
        if FMedia[i].FFormats[j].EncodingParameters<>'' then
          Result:=Result+'/'+FMedia[i].FFormats[j].EncodingParameters;
      end;
      Result:=Result+CKR_CRLF;
      if FMedia[i].FFormats[j].FormatParameters<>'' then
        Result:=Result+'a=fmtp:'+IntToStr(FMedia[i].FFormats[j].PayloadType)+#32+
          FMedia[i].FFormats[j].FormatParameters+CKR_CRLF;
    end;
  if FPtime>0 then Result:=Result+'a=ptime:'+IntToStr(FPtime)+CKR_CRLF;
  if FDirection<>spddNotSet then
    case FDirection of
      spddRecvOnly: Result:=Result+'a=recvonly'+CKR_CRLF;
      spddSendRecv: Result:=Result+'a=sendrecv'+CKR_CRLF;
      spddSendOnly: Result:=Result+'a=sendonly'+CKR_CRLF;
      spddInactive: Result:=Result+'a=inactive'+CKR_CRLF;
    end;

end;

procedure TKRSDP.Reset;
var i: integer;
begin
  FVersion:=0;
  FUserName:='';
  FSessionID:='';
  FSessionVersion:='';
  FAddrType:=spdinaddrIP4;
  FAddr:='0.0.0.0';
  FTTL:=0;
  FSessionName:='';
  FStartTime:=0;
  FStopTime:=0;
  for I := 0 to FMediaCount-1 do FMedia[i].Free;
  FMediaCount:=0;
  SetLength(FMedia,0);
  FPtime:=0;
  FDirection:=spddNotSet;
end;

procedure TKRSDP.SetSessDescr(const Value: String);
var
  sl,sl0,sl1: TStringList;
  i,n: integer;
  s: String;
  m: TKRSDP_MediaType;
  wd: word;
  md: TKRSDP_Media;
  f: PKRSDP_MediaFormat;

  procedure ConnectionData(nettype, addrtype, addr: String);
  var n: integer;
  begin
    if trim(nettype)<>'IN' then exit;
    if trim(addrtype)='IP6' then FAddrType:=spdinaddrIP6;
    n:= Pos('/',addr);
    if n>0 then begin
      FAddr:=trim(KRCopyFromTo(addr,1,n-1));
      FTTL:=StrToIntDef(trim(KRCopyFromTo(addr,n+1,Length(addr))),0);
    end else FAddr:=trim(addr);
  end;

begin
  Reset;
  m:=spdmAudio;
  sl:=TStringList.Create;
  sl.Text:=Value;
  for I := 0 to sl.Count-1 do begin
    n:=Length(sl[i]);
    if n>2 then begin
      if sl[i][2]<>#61 then continue;
      s:=RightStr(sl[i],n-2);
      case Ord(sl[i][1]) of
        118: FVersion:=StrToIntDef(s,0);//Protocol Version  ("v=")
        111: begin //Origin ("o=")
          sl0:=Explode(' ',s);
          if sl0.Count>0 then begin
            FUserName:=Trim(sl0[0]);
            if sl0.Count>1 then begin
              FSessionID:=Trim(sl0[1]);
              if sl0.Count>2 then begin
                FSessionVersion:=Trim(sl0[2]);
                if sl0.Count>5 then ConnectionData(sl0[3],sl0[4],sl0[5]);
              end;
            end;
          end;
          sl0.Free;
        end;
        115: FSessionName:=s;//Session Name ("s=")
        99: begin // Connection Data ("c=")
          sl0:=Explode(' ',s);
          if sl0.Count>2 then ConnectionData(sl0[0],sl0[1],sl0[2]);
          sl0.Free;
        end;
        116: begin //Timing ("t=")
          sl0:=Explode(' ',s);
          if sl0.Count>0 then begin
            FStartTime:=StrToIntDef(Trim(sl0[0]),0);
            if sl0.Count>1 then begin
              FStopTime:=StrToIntDef(Trim(sl0[1]),0);
            end;
          end;
          sl0.Free;
        end;
        109: begin //Media Descriptions ("m=")
          sl0:=Explode(' ',s);
          if sl0.Count>2 then begin
            s:=trim(sl0[0]);
            if s='audio' then m:=spdmAudio
            else if s='video' then m:=spdmVideo
            else if s='application' then m:=spdmApplication
            else if s='message' then m:=spdmMessage
            else begin
              sl0.Free;
              continue;
            end;
            s:=trim(sl0[1]);
            n:=Pos('/',s);
            if n>0 then begin
              wd:=StrToIntDef(trim(KRCopyFromTo(s,1,n-1)),0);
              n:=StrToIntDef(trim(KRCopyFromTo(s,n+1,Length(s))),0);
            end else begin
              wd:=StrToIntDef(trim(s),0);
              n:=0;
            end;
            md:=AddMedia(m,wd,n,trim(sl0[2]));
            for n := 3 to sl0.Count-1 do
              md.AddFormat(StrToIntDef(sl0[n],0),'',0,'','');
          end;
          sl0.Free;
        end;
        97: begin //Attributes ("a=")
          sl0:=Explode(' ',s);
          n:=Pos(':',sl0[0]);
          if n>0 then begin
            s:=trim(KRCopyFromTo(sl0[0],1,n-1));
            n:=StrToIntDef(trim(KRCopyFromTo(sl0[0],n+1,Length(sl0[0]))),0);
            if s='rtpmap' then begin
              f:=GetFormat(n);
              if(f<>nil)and(sl0.Count>1)then begin
                sl1:=explode('/',trim(sl0[1]));
                f^.EncodingName:=trim(sl1[0]);
                if sl1.Count>1 then begin
                  f^.ClockRate:=StrToIntDef(trim(sl1[1]),0);
                  if sl1.Count>2 then
                    f^.EncodingParameters:=trim(sl1[2]);
                end;
                sl1.free;
              end;
            end else if s='fmtp' then begin
              f:=GetFormat(n);
              if(f<>nil)and(sl0.Count>1)then begin
                sl0.Delete(0);
                f^.FormatParameters:=Implode(' ',sl0);
              end;
            end else if s='ptime' then begin
              FPTime:=n;
            end;

          end else begin
            if trim(sl0[0])='recvonly' then
              FDirection:=spddRecvOnly
            else if trim(sl0[0])='sendrecv' then
              FDirection:=spddSendRecv
            else if trim(sl0[0])='sendonly' then
              FDirection:=spddSendOnly
            else if trim(sl0[0])='inactive' then
              FDirection:=spddInactive
          end;
          sl0.Free;
        end;
      end;
    end;
  end;

  sl.Free;
end;

{ TKRSDP_Media }

procedure TKRSDP_Media.AddFormat(APayloadType: byte; AEncodingName: String;
  AClockRate: Cardinal; AEncodingParameters, AFormatParameters: String);
begin
  SetLength(FFormats,FFormatsCount+1);
  FFormats[FFormatsCount].PayloadType:=APayloadType;
  FFormats[FFormatsCount].EncodingName:=AEncodingName;
  FFormats[FFormatsCount].ClockRate:=AClockRate;
  FFormats[FFormatsCount].EncodingParameters:=AEncodingParameters;
  FFormats[FFormatsCount].FormatParameters:=AFormatParameters;
  inc(FFormatsCount);
end;

constructor TKRSDP_Media.create(AMedia: TKRSDP_MediaType; APort: word;
  APortsCount: integer; ATransportProtocol: String);
begin
  FMedia:=AMedia;
  FPort:=APort;
  FPortsCount:=APortsCount;
  FProto:=ATransportProtocol;
  FFormatsCount:=0;
end;

function TKRSDP_Media.GetFormat(Index: integer): TKRSDP_MediaFormat;
begin
  Result:=FFormats[Index];
end;

function TKRSDP_Media.GetFormatIndexByType(APayloadType: byte): integer;
var
  I: Integer;
begin
  Result:=-1;
  for I := 0 to FFormatsCount-1 do
    if FFormats[i].PayloadType=APayloadType then begin
      Result:=i;
      break;
    end;
end;

end.
