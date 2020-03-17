(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRSIPClient                                                               *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRSIPClient;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Winapi.MMSystem, System.SysUtils,
    System.StrUtils,
  {$ELSE}
    Windows, Messages, Classes, MMSystem, SysUtils, StrUtils,
  {$IFEND}
    lgop, KRRTP, KRRTCP, KRTypes, Funcs, KRThread, KRTCPSocketClient, JwaWinsock2,
    md5, KRTCPSocketServer, KRSDP, KRG711, KRDTMF, KRWavePlayer, KRStrUtils,
    KRThreadQueue;

type
  TKRSIPClientError = integer;

const
  KRSIPVersion = 'SIP/2.0';

  KRSIPC_ERR_OK = TKRSIPClientError(0);
  KRSIPC_ERR_UNKNOWN = TKRSIPClientError(1);
  KRSIPC_ERR_CONNECT = TKRSIPClientError(2);
  KRSIPC_ERR_REGISTER = TKRSIPClientError(3);
  KRSIPC_ERR_AUTHENTICATE = TKRSIPClientError(4);

  KRSIPC_ERR_RTPLOCALPORT = TKRSIPClientError(100);
  KRSIPC_ERR_RTPCONNECT = TKRSIPClientError(101);


type
  TKRSIPClientState = (
    krsipcConnecting, krsipcRegister, krsipcConnected, krsipcDisconnected, krsipcTRYING,
    krsipcDial, krsipcCancel, krsipcBeeps, krsipcServiceUnavailable, krsipcBusyHere,
    krsipcBusy, krsipcAccepted, krsipcUnavailable, krsipcRinging, krsipcCallBeingForwarded,
    krsipcCallQueued, krsipcSessionProgress, krsipcAuto);

  TKRSIPClientErrorEvent = procedure(ASender: TObject; AError: TKRSIPClientError; ARespMsg: String) of object;
  TKRSIPClientStateEvent = procedure(ASender: TObject; AState: TKRSIPClientState) of object;
  TKRSIPClientSampleEvent = procedure(ASender: TObject; var Buf; ALength: integer; var FIsAuto: boolean) of object;

  TKRSIPClientThread = class;
  TKRSIPSamplesThread = class;

  TKRSIPClient = class(TComponent)
  private
    FQueue: TKRThreadQueue;
    dtmf: TKRDTMF;
    FSmpThread: TKRSIPSamplesThread;
    FThread: TKRSIPClientThread;
    FClient: TKRTCPSocketClient;
    FDomain: String;
    FAuthorized: boolean;
    FPort: word;
    FOnAuthorized: TNotifyEvent;
    FUser: String;
    FOnError: TKRSIPClientErrorEvent;
    FOnState: TKRSIPClientStateEvent;
    FDevOut: integer;
    FDevIn: integer;
    FStartDialTM: cardinal;
    FPlayer: TKRWavePlayer;
    FSample: TKRSIPClientSampleEvent;
    function GetSound: boolean;
    procedure SetSound(const Value: boolean);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function GetQueueCount: integer;
    property Authorized: boolean read FAuthorized;
    property User: String read FUser;
    procedure Connect(AUserID, APassword: String);
    procedure Disonnect;
    procedure Dial(ANum: String);
    procedure Cancel;
    property AudioDevOut: integer read FDevOut write FDevOut;
    property AudioDevIn: integer read FDevIn write FDevIn;
    property StartDialTM: cardinal read FStartDialTM;
    property Sound: boolean read GetSound write SetSound;
    property Player: TKRWavePlayer read FPlayer write FPlayer;
    property OnSample:TKRSIPClientSampleEvent read FSample write FSample;

    function isAuto1: boolean;
    function isAuto2: boolean;
  published
    property Domain: String read FDomain write FDomain;
    property Port: word read FPort write FPort default 5060;
    property OnAuthorized: TNotifyEvent read FOnAuthorized write FOnAuthorized;
    property OnError: TKRSIPClientErrorEvent read FOnError write FOnError;
    property OnState: TKRSIPClientStateEvent read FOnState write FOnState;
  end;

  TKRSIPClientAct = record
    Method: String;
    CSeq: String;
    CallID: String;
    HeaderData: String;
    State: String;
    Content, From, _To, Branch, Contact, Via, URI: String;
    SendTM: Cardinal;
  end;
  PKRSIPClientAct=^TKRSIPClientAct;

  TKRSIPHeaderParam = record
    Name, Value: STring;
  end;

  TKRSIPResponse = class
  public
    Method: String;
    RespMsg: String;
    URI: String;
    State: integer;
    Valid: boolean;
    Params: Array of TKRSIPHeaderParam;
    Content, tagTo, tagFrom: String;
    Branch: String;
    CSeqNum: integer;
    constructor Create(AData: AnsiString);
    function GetParamValue(AName: String): String;
  end;

  TKRSIPClientThread = class(TKRThread)
  private
    FINVITEAUTH: TKRSIPClientAct;
    FRespMsg: String;
    FRTP: TKRRTP;
    FRTP_UDP, FRTCP_UDP: TKRTCPSocketClient;
    FRTCP: TKRRTCP;
    FSDPLoc, FSDPRem: TKRSDP;
    FRTPPort: word;
    FSIPClient: TKRSIPClient;
    FTransport: String;
    FRegisred: integer;
    FSubMessageSummary, FSubPresenceWinfo: byte;
    FUserID, FURI, FContact, FFrom, FTo, FVia: String;
    FPassword: String;
    FError: TKRSIPClientError;
    FList: TList;
    fSessionID :integer;   // SDP session
    FPack: PKRSIPClientAct;
    FState: TKRSIPClientState;
    FSequence, FExpires, FMaxForwards, FLastSend: Cardinal;
    FNum, FCallID: String;
    FSilenceBuf: TKRBuffer;
    FCancel: boolean;
    FDial, FACK, FSendCancel: boolean;
    realm,nonce,opaque: String;
    mediaDataState: integer;
    FStartDestroy: boolean;
    FSound: boolean;
    FStartCallTM: Cardinal;
    FCallStarted: boolean;
    FFromTag, FToTag, FBranch: String;
    Ringing: boolean;
    TRYING: integer;
    function getValue(const key,str:string):string;
    procedure SendState(AState: TKRSIPClientState);
    procedure SendState_;
    procedure SendError(AError: TKRSIPClientError; ARespMsg: String);
    procedure SendError_;
    procedure Send;
    procedure Recv;
    function BuildCallID: String;
    function BuildBranch: String;
    function BuildTag: String;
    function Crunch(const UserID, Realm:string):string;
    procedure HandleResponse(AResp: TKRSIPResponse);
    procedure WWWAuthenticate(AResp: TKRSIPResponse);
    function GetPack(AList: TList; ACSeq: String):PKRSIPClientAct;
    procedure Disconnect;
    function getPort(AFrom: word): word;
    procedure RTPRecv(ASender: TObject; APack: PKRRTPPack);
    function AuthResponse(AURI: String):String;
    procedure RTPClear;
    procedure RTCPGoodbye;
    procedure SessionProgress(ASessionDescription: String);
  protected
    procedure SetActive(const Value: boolean);override;
    procedure KRExecute;override;
    procedure KRExecutePousedFirst;override;
  public
    constructor CreateThread(ASIPClient: TKRSIPClient);
    destructor Destroy;override;
  end;

  TKRSIPSamplesThread = class(TKRThread)
  private
    FSIPClient: TKRSIPClient;
    FDTMFCallCount, FDTMFCallCount3: Cardinal;
    FDTMFCall, FDTMFCall3: boolean;
    FDTFMCallCnt, FDTFMCallCnt3: integer;
    FStat_, FStartSilence: boolean;
    FSilence: Cardinal;

    auto1: integer;
    auto1n: integer;
    auto1is: boolean;

    auto2: integer;
    auto2n: integer;
    auto2is: boolean;
  protected
    procedure KRExecute; override;
  public
    procedure stat(v: boolean);
    constructor CreateThread(ASIPClient: TKRSIPClient);
  end;

implementation

{ TKRSIPClientThread }

function TKRSIPClientThread.AuthResponse(AURI: String): String;
var
  A1,A2: String;
begin
  A1:=Crunch(fUserID,realm);
  A2:=LowerCase(MD5DigestToStr(MD5String(
    WideStringToString(FPack^.Method+':'+AURI,1251)
  )));
  Result:=LowerCase(MD5DigestToStr(MD5String(
    WideStringToString(A1+':'+nonce+':'+A2,1251)
  )));
end;

function TKRSIPClientThread.BuildBranch: String;
begin
  Result:='z9hG4bK-524287-1---'+Copy(BuildCallID,8,16);
end;

function TKRSIPClientThread.BuildCallID: String;
begin
  Result:=LowerCase(
    MD5DigestToStr(
      MD5String(
        WideStringToString(
          IntToStr(timeGetTime)+IntToStr(Random(99999))
          ,1251
        )
      )
    ));
end;

function TKRSIPClientThread.BuildTag: String;
begin
  Result:=Copy(BuildCallID,10,8);
end;

constructor TKRSIPClientThread.CreateThread(ASIPClient: TKRSIPClient);
var
  i: integer;
begin
  Randomize;
  FStartDestroy:=false;
  for I := 0 to 159 do FSilenceBuf[i]:=$fe;
  FRTP:=nil;
  FRTP_UDP:=nil;
  FRTCP:=nil;
  FRTCP_UDP:=nil;
  FSDPRem:=TKRSDP.Create;
  FSDPLoc:=TKRSDP.Create;
  FSDPLoc.UserName:='KR';
  FSDPLoc.SessionName:='kandiral';
  FSDPLoc.AddMedia(spdmAudio,0,0,'RTP/AVP');
  FSDPLoc.Media[0].AddFormat(0,'PCMU',8000,'','');
  FSDPLoc.Media[0].AddFormat(8,'PCMA',8000,'','');
  fSessionID:=getTickCount;
  FList:=TList.Create;
  FExpires:=1000;
  FMaxForwards:=70;
  FSIPClient:=ASIPClient;
  FTransport:='UDP';
  ASIPClient.FClient.PROTO:=IPPROTO_UDP;
  ASIPClient.FClient.SocketType:=SOCK_DGRAM;
  ASIPClient.FClient.ConnectTimeout:=2000;
  inherited Create;
  WaitTime:=5;
end;

function TKRSIPClientThread.Crunch(const UserID, Realm: string): string;
begin
  if (Length(FPassword)=2+32)and(FPassword[1]='{')and(FPassword[34]='}') then
    Result:=copy(FPassword,2,32)
  else
    Result:=LowerCase(MD5DigestToStr(MD5String(
      WideStringToString(UserID+':'+Realm+':'+FPassword,1251)
    )));
end;

destructor TKRSIPClientThread.Destroy;
begin
  RTPClear;
  FreeAndNil(FSDPLoc);
  FreeAndNil(FSDPRem);
  FreeAndNil(FList);
  FStartDestroy:=true;
  inherited;
end;

procedure TKRSIPClientThread.Disconnect;
begin
  FCancel:=true;
  try
    FSIPClient.FClient.Close;
  except end;
  SendState(krsipcDisconnected);
end;

function TKRSIPClientThread.GetPack(AList: TList;
  ACSeq: String): PKRSIPClientAct;
var
  I: Integer;
begin
  Result:=nil;
  for I := 0 to AList.Count-1 do
    if PKRSIPClientAct(AList[i]).CSeq=ACSeq then begin
      Result:=AList[i];
      break;
    end;
end;

function TKRSIPClientThread.getPort(AFrom: word): word;
var
  srv: TKRTCPSocketServer;
  p: Word;

  function chackPort(APort: word):boolean;
  begin
    try
      srv.Open;
    finally
      Result:=srv.Connected;
      try
        srv.Close;
      except end;
    end;
  end;

begin
  srv:=TKRTCPSocketServer.Create;
  p:=AFrom;
  Result:=0;
  while p<65000 do begin
    if chackPort(p) and chackPort(p+1) then begin
      Result:=p;
      break;
    end;
    Inc(p,2);
  end;
end;

function TKRSIPClientThread.getValue(const key, str: string): string;
var
  i,j:integer;
begin
  Result:='';
  i:=pos(key,str);
  if i=0 then exit;
  inc(i,length(key));
  if i+2>length(str) then exit;
  if str[i]<>#61 then exit;
  inc(i);
  if str[i]=#34 then begin
    inc(i);
    j:=i;
    while (j<=length(str)) and (str[j]<>#34) do inc(j);
  end else begin
    j:=i;
    while (j<=length(str))and(not CharInSet(str[j], [#32,#44])) do inc(j);
  end;
   Result:=copy(str,i,j-i);
end;

procedure TKRSIPClientThread.HandleResponse(AResp: TKRSIPResponse);
var
  pAct: PKRSIPClientAct;
  s: String;
  sl: TStringList;
  i: integer;
begin
  if AResp.Valid then begin
    if AResp.Method='RESPONSE' then begin
      pAct:=GetPack(FList,AResp.GetParamValue('CSeq'));
      if pAct=nil then exit;
      case AResp.State of
        100: if CompareStr(pAct^.State,'100')<>0 then begin
            SendState(krsipcTRYING);
            pAct^.State:='100';
            TRYING:=AResp.CSeqNum;
            FBranch:=AResp.Branch;
            FCallID:=AResp.GetParamValue('Call-ID');
            FStartCallTM:=timeGetTime;
            FSIPClient.FStartDialTM:=FStartCallTM;
          end;
        180: if CompareStr(pAct^.State,'180')<>0 then begin
            if AResp.Content<>'' then SessionProgress(AResp.Content);
            if not FCallStarted then
              SendState(krsipcRinging);
            pAct^.State:='180';
            Ringing:=true;
            FStartCallTM:=timeGetTime;
            FSIPClient.FStartDialTM:=FStartCallTM;
          end;
        181: if CompareStr(pAct^.State,'181')<>0 then begin
            if AResp.Content<>'' then SessionProgress(AResp.Content);
            SendState(krsipcCallBeingForwarded);
            pAct^.State:='181';
          end;
        182: if CompareStr(pAct^.State,'182')<>0 then begin
            if AResp.Content<>'' then SessionProgress(AResp.Content);
            SendState(krsipcCallQueued);
            pAct^.State:='182';
          end;
        183: if CompareStr(pAct^.State,'183')<>0 then begin
            if AResp.Content<>'' then SessionProgress(AResp.Content);
            pAct^.State:='183';
          end;
        200: begin
            if CompareStr(pAct^.State,'200')<>0 then begin
              if pAct^.Method='REGISTER' then begin
                FRegisred:=-1;
                SendState(krsipcConnected);
              end else if pAct^.Method='INVITE' then begin

                FSDPRem.SessionDescription:=AResp.Content;
                if (FSDPRem.SessionName<>'')and(CompareStr(FSDPRem.SessionName,'kandiral')<>0) then
                  SessionProgress(AResp.Content);
                FCallID:=AResp.GetParamValue('Call-ID');
                FFromTag:=Aresp.tagFrom;
                FToTag:=Aresp.tagTo;

                new(FPack);
                FPack^.Method:='ACK';
                FPack^.URI:=KRCopyFromTo(AResp.GetParamValue('To'),'<','>');
                FPack^.Branch:=AResp.Branch;
                FPack^.Contact:='';
                FPack^._To:=AResp.GetParamValue('To');
                FPack^.From:=AResp.GetParamValue('From');
                FPack^.CallID:=AResp.GetParamValue('Call-ID');
                FPack^.CSeq:=IntToStr(AResp.CSeqNum)+' '+FPack^.Method;
                FPack^.HeaderData:='';
                FPack^.Content:='';

                sleep(200);
                FSIPClient.FSmpThread.Stat(false);
                SendState(krsipcAccepted);

              end else if pAct^.Method='CANCEL' then begin
              end;
              pAct^.State:='200';
            end;
          end;
        401: if CompareStr(pAct^.State,'401')<>0 then begin
            new(FPack);
            inc(FSequence);
            FPack^.Method:=pAct^.Method;
            FPack^.URI:=pAct^.URI;
            FPack^.Branch:=BuildBranch;
            FPack^.Contact:=FContact;
            FPack^._To:=FTo;
            FPack^.From:=FFrom+';tag='+FFromTag;
            FPack^.CallID:=pAct^.CallID;
            FPack^.CSeq:=IntToStr(FSequence)+' '+FPack^.Method;
            FPack^.HeaderData:='';
            sl:=TStringList.Create;
            sl.Text:=pAct^.HeaderData;
            for I := 0 to sl.Count-1 do
              if CompareStr(LeftStr(sl[i],14),'Authorization:')<>0 then
                FPack^.HeaderData:=FPack^.HeaderData+sl[i]+CKR_CRLF;
            FPack^.Content:=pAct^.Content;
            WWWAuthenticate(AResp);
            if(pAct^.Method='REGISTER')and(FRegisred=-1)then begin
              Dispose(FPack);
              FPack:=nil;
            end else if pAct^.Method='INVITE' then begin
              if TRYING=0 then begin
                FINVITEAUTH:=FPack^;
                FPack^.Method:='ACK';
                FPack^.URI:=KRCopyFromTo(AResp.GetParamValue('To'),'<','>');
                FPack^.Branch:=AResp.Branch;
                FPack^.Contact:='';
                FPack^._To:=AResp.GetParamValue('To');
                FPack^.From:=AResp.GetParamValue('From');
                FPack^.CallID:=AResp.GetParamValue('Call-ID');
                FPack^.CSeq:=IntToStr(AResp.CSeqNum)+' '+FPack^.Method;
                FPack^.HeaderData:='';
                FPack^.Content:='';
              end else begin
                Dispose(FPack);
                FPack:=nil;
              end;
            end;
            pAct^.State:='401';
          end;
        403: begin
          SendError(KRSIPC_ERR_AUTHENTICATE,AResp.RespMsg);
        end;
        404: begin
          if CompareStr(pAct^.State,'404')<>0 then begin
            if pAct^.Method='INVITE' then begin
              SendState(krsipcUnavailable);
              FSendCancel:=true;
            end;
            pAct^.State:='404';
          end;
        end;
        481: begin end;
        486: begin
          if CompareStr(pAct^.State,'486')<>0 then begin
            SendState(krsipcBusyHere);
            FSendCancel:=true;
            pAct^.State:='486';
          end;
        end;
        487: if CompareStr(pAct^.State,'486')<>0 then begin
            if pAct^.Method='INVITE' then begin
              new(FPack);
              FPack^.Method:='ACK';
              FPack^.URI:=KRCopyFromTo(AResp.GetParamValue('To'),'<','>');
              FPack^.Branch:=AResp.Branch;
              FPack^.Contact:='';
              FPack^._To:=AResp.GetParamValue('To');
              FPack^.From:=AResp.GetParamValue('From');
              FPack^.CallID:=AResp.GetParamValue('Call-ID');
              FPack^.CSeq:=IntToStr(AResp.CSeqNum)+' '+FPack^.Method;
              FPack^.HeaderData:='';
              FPack^.Content:='';
            end;
            pAct^.State:='487';
        end;
        491: begin end;
        503: begin
          if CompareStr(pAct^.State,'503')<>0 then begin
            if StrToIntDef(AResp.GetParamValue('X-Asterisk-HangupCauseCode'),-1)=18 then
              SendState(krsipcServiceUnavailable)
            else
              SendState(krsipcUnavailable);
            FSendCancel:=true;
            pAct^.State:='503';
          end;
        end
        else begin
          if (AResp.State>=400) then
            SendError(KRSIPC_ERR_UNKNOWN,AResp.RespMsg);
        end;
      end;
    end else begin
      New(FPack);
      FPack^.Via:=AResp.GetParamValue('Via');
      FPack^.Contact:=AResp.GetParamValue('Contact');
      FPack^._To:=AResp.GetParamValue('To');
      FPack^.From:=AResp.GetParamValue('From');;
      FPack^.CallID:=AResp.GetParamValue('Call-ID');
      FPack^.CSeq:=AResp.GetParamValue('CSeq');
      FPack^.Content:='';
      if AResp.Method='OPTIONS' then begin
        FPack^.HeaderData:='Accept: application/sdp, application/sdp'+CKR_CRLF+
          'Accept-Language: en'+CKR_CRLF;
      end else if AResp.Method='NOTIFY' then begin
        s:=AResp.GetParamValue('Event');
      end;
      FPack^.State:='200 OK';
    end;
  end;
  AResp.Free;
end;

procedure TKRSIPClientThread.KRExecute;
var
  p: PKRSIPClientAct;
begin
  if FCancel then exit;

  if Ringing and (not FCallStarted)and(ElapsedTime(FStartCallTM)>7000)  then
    sendState(krsipcBeeps);

  if mediaDataState<>0 then begin
    sendState(TKRSIPClientState(mediaDataState));
    mediaDataState:=0;
    if FState=krsipcUnavailable then FSendCancel:=true;
  end;

  if ((TRYING<>0) or FCallStarted) and(ElapsedTime(FStartCallTM)>125000{65000}) then begin
    SendState(krsipcUnavailable);
    FSendCancel:=true;
  end;

  if not FSIPClient.FClient.Connected then begin
    SendState(krsipcConnecting);
    FSIPClient.FClient.Port:=FSIPClient.FPort;
    FSIPClient.FClient.Addr:=WideStringToString(FSIPClient.FDomain,1251);
    try
      FSIPClient.FClient.Open;
    except
      try
        FSIPClient.FClient.Close;
      except end;
    end;
    if not FSIPClient.FClient.Connected then begin
      SendError(KRSIPC_ERR_CONNECT,'');
    end else begin
      FRegisred:=0;
      FSIPClient.FClient.GetLocalData;
      FURI:='sip:'+FSIPClient.FDomain+';transport='+FTransport;
      FVia:=KRSIPVersion+'/'+FTransport+' '+String(FSIPClient.FClient.LocalAddr)+':'+
        IntToStr(FSIPClient.FClient.LocalPort)+';'+'branch=';
      FContact:='<sip:'+FUserID+'@'+String(FSIPClient.FClient.LocalAddr)+':'+
        IntToStr(FSIPClient.FClient.LocalPort)+';transport='+FTransport+'>';
      FTo:='<sip:'+FUserID+'@'+FSIPClient.FDomain+';transport='+FTransport+'>';
      FFrom:='<sip:'+FUserID+'@'+FSIPClient.FDomain+';transport='+FTransport+'>';
    end;
    exit;
  end;

  if FRegisred>4 then begin
    if FPack<>nil then Dispose(FPack);
    SendError(KRSIPC_ERR_REGISTER,'');
    exit;
  end;

  if FPack=nil then begin
    if(FRegisred>-1)then begin
      if(FState<>krsipcRegister)or(ElapsedTime(FLastSend)>1000)then begin
        inc(FRegisred);
        New(FPack);
        if FRegisred=1 then begin
          FCallID:=BuildCallID;
          FBranch:=BuildBranch;
          FFromTag:=BuildTag;
          inc(FSequence);
        end;
        FPack^.Method:='REGISTER';
        FPack^.URI:=FURI;
        FPack^.Branch:=FBranch;
        FPack^.Contact:=FContact;
        FPack^._To:=FTo;
        FPack^.From:=FFrom+';tag='+FFromTag;
        FPack^.CallID:=FCallID;
        FPack^.CSeq:=IntToStr(FSequence)+' '+FPack^.Method;
        FPack^.HeaderData:='';
        FPack^.Content:='';
        if(FState<>krsipcRegister)then SendState(krsipcRegister);
      end;
    end else if FSendCancel then begin

      if(TRYING<>0)or(FState=krsipcAccepted)then begin
        FCallStarted:=false;
        Ringing:=false;
        RTCPGoodbye;

        New(FPack);

        if FState=krsipcAccepted then begin
          FBranch:=BuildBranch;
          inc(FSequence);
          FPack^.Method:='BYE';
          FPack^.URI:='sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport;
          FPack^.Branch:=FBranch;
          FPack^.Contact:=FContact;
          FPack^._To:='<sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport+'>;tag='+FToTag;
          FPack^.From:=FFrom+';tag='+FFromTag;
          FPack^.CallID:=FCallID;
          FPack^.CSeq:=IntToStr(FSequence)+' '+FPack^.Method;
        end else begin
          FPack^.Method:='CANCEL';
          FPack^.URI:='sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport;
          FPack^.Branch:=FBranch;
          FPack^.Contact:=FContact;
          FPack^._To:='<sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport+'>';
          FPack^.From:=FFrom+';tag='+FFromTag;
          FPack^.CallID:=FCallID;
          FPack^.CSeq:=IntToStr(TRYING)+' '+FPack^.Method;
        end;

        SendState(krsipcCancel);

        FPack^.HeaderData:=FPack^.HeaderData+'Authorization: Digest username="'+fUserID+'",realm="'+
          realm+'",nonce="'+nonce+'",response="'+AuthResponse(FPack^.URI)+'",uri="'+FURI+
          '",algorithm=MD5,opaque="'+opaque+'"'+CKR_CRLF;;
        FPack^.Content:='';

        TRYING:=0;
      end;

      FSendCancel:=false;
    end else if FDial then begin
      FDial:=false;
      SendState(krsipcDial);
      FRTPPort:=getPort(8000);
      if FRTPPort>0 then begin
        FCallStarted:=false;
        TRYING:=0;
        Ringing:=false;

        FCallID:=BuildCallID;
        FBranch:=BuildBranch;
        FFromTag:=BuildTag;
        inc(FSequence);
        New(FPack);
        FPack^.Method:='INVITE';
        FPack^.URI:='sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport;
        FPack^.Branch:=FBranch;
        FPack^.Contact:=FContact;
        FPack^._To:='<sip:'+FNum+'@'+FSIPClient.FDomain+';transport='+FTransport+'>';
        FPack^.From:=FFrom+';tag='+FFromTag;
        FPack^.CallID:=FCallID;
        FPack^.CSeq:=IntToStr(FSequence)+' '+FPack^.Method;
        FPack^.HeaderData:='Content-Type: application/sdp'+CKR_CRLF;
        FSDPLoc.SessionID:=IntToStr(getTickCount);
        FSDPLoc.SessionVersion:=FSDPLoc.SessionID;
        FSDPLoc.Addr:=String(FSIPClient.FClient.LocalAddr);
        FSDPLoc.Direction:=spddSendRecv;
        FSDPLoc.Media[0].Port:=FRTPPort;
        FPack^.Content:=FSDPLoc.SessionDescription;
      end else SendError(KRSIPC_ERR_RTPLOCALPORT,'');
    end else if FINVITEAUTH.Method='INVITE' then begin
      New(FPack);
      FPack^:=FINVITEAUTH;
      FINVITEAUTH.Method:='';
    end;

  end;

  if FPack<>nil then begin
    Send;
    if FPack^.Method='' then begin
      Dispose(FPack);
    end else begin
      Flist.Add(FPack);
      while true do begin
        if Flist.Count=0 then break;
        if ElapsedTime(PKRSIPClientAct(Flist[0])^.SendTM)<300000 then break;
        p:=Flist[0];
        Flist.Delete(0);
        Dispose(p);
      end;
    end;
    FPack:=nil;
  end;
  Recv;
end;

procedure TKRSIPClientThread.KRExecutePousedFirst;
begin
  try
    FSIPClient.FClient.Close
  except end;
end;

procedure TKRSIPClientThread.Recv;
var
  data: AnsiString;
  l: integer;
begin
  SetLength(data,1024);
  l:=FSIPClient.FClient.ReceiveBuf(data[1],1024);
  if l>0 then begin
    SetLength(data,l);
    HandleResponse(TKRSIPResponse.Create(data));
  end;
end;

procedure TKRSIPClientThread.RTCPGoodbye;
begin
  if Assigned(FRTCP) then
    if FRTCP.Active then
      FRTCP.Goodbye;

  RTPClear;

  FSIPClient.FSmpThread.Stat(false);

  FSIPClient.FQueue.LockQueue;
  while FSIPClient.FQueue.Count>0 do Dispose(PKRLBuffer(FSIPClient.FQueue.Pop));
  FSIPClient.FQueue.UnlockQueue;

end;

procedure TKRSIPClientThread.RTPClear;
begin
  if Assigned(FRTCP) then begin
    FRTCP.Active:=false;
    FreeAndNil(FRTCP);
  end;
  if Assigned(FRTP) then begin
    FRTP.Active:=false;
    FreeAndNil(FRTCP);
  end;
  if Assigned(FRTCP_UDP) then FreeAndNil(FRTCP_UDP);
  if Assigned(FRTP_UDP) then FreeAndNil(FRTP_UDP);
end;

procedure TKRSIPClientThread.RTPRecv(ASender: TObject; APack: PKRRTPPack);
var
  i,n: integer;
  s: SmallInt;
  buf: PKRLBuffer;
  bufLen: integer;
begin

  case APack^.PayloadType of
    0,8: begin
      bufLen:=APack^.PayloadLength*2;
    end;
    else exit;
  end;

  new(buf);
  buf^.len:=buflen;
  n:=0;
  for I := 0 to APack^.PayloadLength-1 do begin
    case APack^.PayloadType of
      0: begin
        s:=KRULaw2PCM(APack^.Payload[i]);
        BytesFromWord(s,buf^.buf[n],buf^.buf[n+1]);
        Inc(n,2);
      end;
      8: begin
        s:=KRALaw2PCM(APack^.Payload[i]);
        BytesFromWord(s,buf^.buf[n],buf^.buf[n+1]);
        Inc(n,2);
      end;
    end;
  end;

  if FSound then
    if Assigned(FSIPClient.FPlayer) then
      FSIPClient.FPlayer.Play(buf^.buf,bufLen);

  FSIPClient.FQueue.LockQueue;
  FSIPClient.FQueue.Push(buf);
  FSIPClient.FQueue.UnlockQueue;
end;

procedure TKRSIPClientThread.Send;
var
  data0: String;
  data: AnsiString;
  viaPort: String;
begin
  if FPack^.Method='' then begin
    if RightStr(FPack^.Via,6)=';rport' then viaPort:=IntToStr(FSIPClient.FPort) else viaPort:='';

    data0:=
      KRSIPVersion+' '+FPack^.State+CKR_CRLF+
      'Via: '+FPack^.Via+viaPort+CKR_CRLF+
      'Contact: '+FPack^.Contact+CKR_CRLF+
      'To: '+FPack^._To+';tag='+IntToStr(GetTickCount)+CKR_CRLF+
      'From: '+FPack^.From+CKR_CRLF+
      'Call-ID: '+FPack^.CallID+CKR_CRLF+
      'CSeq: '+FPack^.CSeq+CKR_CRLF
  end else begin
    data0:=
      FPack^.Method+' '+FPack^.URI+' '+KRSIPVersion+CKR_CRLF+
      'Via: '+FVia+FPack^.Branch+CKR_CRLF+
      'Max-Forwards: '+IntToStr(FMaxForwards)+CKR_CRLF;
    if FPack^.Contact<>'' then data0:=data0+
      'Contact: '+FPack^.Contact+CKR_CRLF;
    data0:=data0+
      'To: '+FPack^._To+CKR_CRLF+
      'From: '+FPack^.From+CKR_CRLF+
      'Call-ID: '+FPack^.CallID+CKR_CRLF+
      'CSeq: '+FPack^.CSeq+CKR_CRLF+
      'User-Agent: Kandiral'+CKR_CRLF;
      //'Supported: replaces, norefersub, extended-refer, timer, outbound, path, X-cisco-serviceuri'+CKR_CRLF;
      //'Allow-Events: presence, kpml'+CKR_CRLF;
      //'Expires: '+IntToStr(FExpires)+CKR_CRLF+
      //'Allow: INVITE, ACK, CANCEL, BYE, NOTIFY, REFER, MESSAGE, OPTIONS, INFO, SUBSCRIBE'+CKR_CRLF+
  end;

  data:=WideStringToString(data0+FPack^.HeaderData+
    'Content-Length: '+IntToStr(Length(FPack^.Content))+
    CKR_CRLF+CKR_CRLF+FPack^.Content,1251);
  FSIPClient.FClient.SendBuf(data[1],Length(data));
  FLastSend:=timeGetTime;
  FPack^.SendTM:=FLastSend;
  FPack^.State:='SEND';
end;

procedure TKRSIPClientThread.SendError(AError: TKRSIPClientError; ARespMsg: String);
begin
  FError:=AError;
  FRespMsg:=ARespMsg;
  if Assigned(FSIPClient.FOnError) then Synchronize(SendError_);
  if FError<100 then Disconnect;
end;

procedure TKRSIPClientThread.SendError_;
begin
  if Assigned(FSIPClient.FOnError) then FSIPClient.FOnError(FSIPClient,FError,FRespMsg);
end;

procedure TKRSIPClientThread.SendState(AState: TKRSIPClientState);
begin
  if FState<>AState then begin
    FState:=AState;
    if Assigned(FSIPClient.FOnState) then Synchronize(SendState_);
  end;
end;

procedure TKRSIPClientThread.SendState_;
begin
  if Assigned(FSIPClient.FOnState) then FSIPClient.FOnState(FSIPClient,FState);
end;

procedure TKRSIPClientThread.SessionProgress(ASessionDescription: String);
var
  sa:TSockAddrIn;
begin
  RTPClear;
  SendState(krsipcSessionProgress);
  FSDPRem.SessionDescription:=ASessionDescription;
  FRTP_UDP:=TKRTCPSocketClient.Create;
  FRTP_UDP.PROTO:=IPPROTO_UDP;
  FRTP_UDP.SocketType:=SOCK_DGRAM;
  FRTP_UDP.Addr:=WideStringToString(FSDPRem.Addr,1251);
  FRTP_UDP.Port:=FSDPRem.Media[0].Port;
  try
    FRTP_UDP.Activate;
    FillChar(sa,SizeOf(sa),0);
    sa.sin_family:=AF_INET;
    sa.sin_port:=htons(FRTPPort);
    bind(FRTP_UDP.Handle,@sa,SizeOf(sa));
    FRTP_UDP.Open;
  except
    try
      FRTP_UDP.Close;
    except end;
  end;
  FRTCP_UDP:=TKRTCPSocketClient.Create;
  FRTCP_UDP.PROTO:=IPPROTO_UDP;
  FRTCP_UDP.SocketType:=SOCK_DGRAM;
  FRTCP_UDP.Addr:=WideStringToString(FSDPRem.Addr,1251);
  FRTCP_UDP.Port:=FSDPRem.Media[0].Port+1;
  try
    FRTCP_UDP.Activate;
    FillChar(sa,SizeOf(sa),0);
    sa.sin_family:=AF_INET;
    sa.sin_port:=htons(FRTPPort+1);
    bind(FRTCP_UDP.Handle,@sa,SizeOf(sa));
    FRTCP_UDP.Open;
  except
    try
      FRTCP_UDP.Close;
    except end;
  end;
  if(not FRTP_UDP.Connected)or(not FRTCP_UDP.Connected)then begin
    if Assigned(FRTCP_UDP) then FreeAndNil(FRTCP_UDP);
    if Assigned(FRTP_UDP) then FreeAndNil(FRTP_UDP);
    SendError(KRSIPC_ERR_RTPCONNECT,'');
    FSendCancel:=true;
  end else begin

    FSIPClient.FSmpThread.Stat(true);

    FSIPClient.FSmpThread.FDTMFCall:=false;
    FSIPClient.FSmpThread.FDTMFCall3:=false;
    FSIPClient.FSmpThread.FDTFMCallCnt:=0;
    FSIPClient.FSmpThread.FDTFMCallCnt3:=0;

    FRTP:=TKRRTP.CreateRTP(FRTP_UDP,0);
    FRTP.OnRecv:=RTPRecv;
    FRTCP:=TKRRTCP.CreateRTCP(FRTCP_UDP,FRTP);
    FRTCP.Silence(FSilenceBuf,160,20);
    FRTCP.SendSilence:=true;
    FRTCP.Active:=true;
    FRTP.Active:=true;
    if not FCallStarted then begin
      FCallStarted:=true;
      FStartCallTM:=timeGetTime;
      FSIPClient.FStartDialTM:=FStartCallTM;
    end;
  end;
end;

procedure TKRSIPClientThread.SetActive(const Value: boolean);
var
  p: PKRSIPClientAct;
begin
  if Value then begin
    FError:=KRSIPC_ERR_OK;
    FRegisred:=0;
    FSubMessageSummary:=0;
    FSubPresenceWinfo:=0;
    FPack:=nil;
    FState:=krsipcDisconnected;
    FSequence:=0;
    FDial:=false;
    FACK:=false;
    FSendCancel:=false;
    FCancel:=false;
    mediaDataState:=0;
  end;
  inherited;
  if (not FStartDestroy)and (not Value) then begin
    while FList.Count>0 do begin
      p:=FList[0];
      FList.Delete(0);
      Dispose(p);
    end;
  end;
end;

procedure TKRSIPClientThread.WWWAuthenticate(AResp: TKRSIPResponse);
var
  s,algo: String;
begin
  s:=AResp.GetParamValue('WWW-Authenticate');
  s:=RightStr(s,Length(s)-7);
  if s='' then exit;
  algo:=getValue('algorithm',s);
  if (algo<>'MD5') then exit;
  realm :=getValue('realm' ,s);
  nonce :=getValue('nonce' ,s);
  opaque:=getValue('opaque',s);

  FPack^.HeaderData:=FPack^.HeaderData+'Authorization: Digest username="'+fUserID+'",realm="'+
    realm+'",nonce="'+nonce+'",response="'+AuthResponse(FURI)+'",uri="'+FURI+
    '",algorithm=MD5,opaque="'+opaque+'"'+CKR_CRLF;

end;

{ TKRSIPClient }

procedure TKRSIPClient.Cancel;
begin
  FThread.FSendCancel:=true;
end;

procedure TKRSIPClient.Connect(AUserID, APassword: String);
begin
  FThread.Active:=false;
  FAuthorized:=false;
  FThread.FUserID:=AUserID;
  FThread.FPassword:=APassword;
  FThread.Active:=true;
end;

constructor TKRSIPClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthorized:=false;
  FPort:=5060;
  dtmf:=TKRDTMF.Create;
  FQueue:=TKRThreadQueue.Create;
  FSmpThread:=TKRSIPSamplesThread.CreateThread(Self);
  FSmpThread.Active:=true;
  FClient:=TKRTCPSocketClient.Create;
  FThread:=TKRSIPClientThread.CreateThread(Self);
end;

destructor TKRSIPClient.Destroy;
begin
//REAddLog('Start Destroy');
  try
    FThread.Active:=false;
//REAddLog('FThread.Active:=false;');
  finally
    FreeAndNil(FThread);
  end;
//REAddLog('1');
  try
    FClient.Close;
  finally
    FreeAndNil(FClient);
  end;
//REAddLog('2');
  FQueue.LockQueue;
  while FQueue.Count>0 do Dispose(PKRLBuffer(FQueue.Pop));
  FQueue.UnlockQueue;
//REAddLog('3');
  try
    FSmpThread.Active:=false;
//REAddLog('FSmpThread.Active:=false;');
  finally
    FreeAndNil(FSmpThread);
  end;
//REAddLog('4');
  FQueue.Free;
//REAddLog('5');
  dtmf.Free;
//REAddLog('dtmf.Free');
  inherited;
end;

procedure TKRSIPClient.Dial(ANum: String);
begin
  FThread.FNum:=ANum;
  FThread.FDial:=true;
end;

procedure TKRSIPClient.Disonnect;
begin
  FThread.Disconnect;
  FThread.Active:=false;
end;

function TKRSIPClient.GetQueueCount: integer;
begin
  result:=FQueue.Count;
end;

function TKRSIPClient.GetSound: boolean;
begin
  result:=FThread.FSound;
end;

function TKRSIPClient.isAuto1: boolean;
begin
  result:=Self.FSmpThread.auto1is;
end;

function TKRSIPClient.isAuto2: boolean;
begin
  result:=Self.FSmpThread.auto2is;
end;

procedure TKRSIPClient.SetSound(const Value: boolean);
begin
  FThread.FSound:=Value;
end;

{ TKRSIPResponse }

constructor TKRSIPResponse.Create(AData: AnsiString);
var
  sl,sl0: TStringList;
  s: String;
  n,p: integer;
begin
  Valid:=true;
  sl:=TStringList.Create;
  sl.Text:=StringToWideString(AData,1251);

  if sl.Count=0 then begin
    Valid:=false;
    sl.Free;
    exit;
  end;

  sl0:=Explode(' ',sl[0]);
  if sl0.Count>0 then begin
    if sl0[0]=KRSIPVersion then begin
      Method:='RESPONSE';
      State:=0;
      sl0.Delete(0);
      if(sl0.Count>0)then
        State:=StrToIntDef(sl0[0],0);
      RespMsg:=Implode(' ',sl0);
    end else begin
      Method:=sl0[0];
      if sl0.Count>1 then URI:=sl0[1]
      else Valid:=false;
    end;
  end else Valid:=false;
  sl0.Free;

  if not Valid then begin
    sl.Free;
    exit;
  end;

  sl.Delete(0);
  p:=0;
  while true do begin
    if sl.Count=0 then break;
    s:=sl[0];
    n:=Pos(':',s);
    if n=0 then begin
      Valid:=false;
      break;
    end;
    SetLength(Params,p+1);
    Params[p].Name:=LeftStr(s,n-1);
    Params[p].Value:=RightStr(s,Length(s)-n-1);
    inc(p);
    sl.Delete(0);
    if Params[p-1].Name='Content-Length' then begin
      if(StrToIntDef(Params[p-1].Value,-1)>-1)and(sl.Count>0)then begin
        sl.Delete(0);
        Content:=sl.Text;
        break;
      end else begin
        Valid:=false;
        break;
      end;
    end else if Params[p-1].Name='From' then begin
      tagFrom:=KRCopyFromTo(s,'tag=',';');
    end else if Params[p-1].Name='To' then begin
      tagTo:=KRCopyFromTo(s,'tag=',';');
    end else if Params[p-1].Name='Via' then begin
      sl0:=Explode(';',s);
      for n:=0 to sl0.Count-1 do
        if LeftStr(sl0[n],7)='branch=' then begin
          Branch:=RightStr(sl0[n],Length(sl0[n])-7);
          break;
        end;
      sl0.Free;
    end else if Params[p-1].Name='CSeq' then begin
      n:=Pos(' ',Params[p-1].Value);
      if n>0 then CSeqNum:=StrToIntDef(LeftStr(Params[p-1].Value,n-1),0);
    end;
  end;
  sl.Free;
end;

function TKRSIPResponse.GetParamValue(AName: String): String;
var
  I: Integer;
begin
  Result:='';
  for I := 0 to Length(Params)-1 do
    if Params[i].Name=AName then begin
      Result:=Params[i].Value;
      break;
    end;
end;

{ TKRSIPSamplesThread }

constructor TKRSIPSamplesThread.CreateThread(ASIPClient: TKRSIPClient);
begin
  FSIPClient:=ASIPClient;
  inherited Create;
  WaitTime:=1;
end;

procedure TKRSIPSamplesThread.KRExecute;
var
  buf: PKRLBuffer;
  n: integer;
begin

  FSIPClient.FQueue.LockQueue;
  if FSIPClient.FQueue.Count=0 then begin
    FSIPClient.FQueue.UnlockQueue;
    exit;
  end;
  buf:=FSIPClient.FQueue.Pop;
  FSIPClient.FQueue.UnlockQueue;

  if not FStat_ then begin
    dispose(buf);
    exit;
  end;

  n:=FSIPClient.dtmf.Detecting(0,buf^.buf,buf^.len);

  if FSIPClient.dtmf.fr then begin

    if(FSIPClient.dtmf.Row=3)and(FSIPClient.dtmf.Column=7) then begin
      inc(auto1);
      auto1n:=0;
    end else begin
      inc(auto1n);
      if auto1n>4 then begin
        auto1n:=0;
        auto1:=0;
      end;
    end;

    if(FSIPClient.dtmf.Row=0)and((FSIPClient.dtmf.Column=9)or(FSIPClient.dtmf.Column=7))then begin
      inc(auto2);
      auto2n:=0;
    end else if not((FSIPClient.dtmf.Row=1)and(FSIPClient.dtmf.Column=6)) then begin
      inc(auto2n);
      if auto2n>2 then begin
        auto2n:=0;
        auto2:=0;
      end;
    end;
    //REAddLog('auto2='+intToStr(auto2)+'; auto2n='+intToStr(auto2n));
  end;

  {if n=254 then begin
    inc(FSilence,buf^.len div 2);

  end else begin
    if FStartSilence then begin
      if FSilence>35000 then begin
        FStat_:=false;
        FSIPClient.FThread.mediaDataState:=integer(krsipcUnavailable);
      end;
      FStartSilence:=false;
    end;

    if Assigned(FSIPClient.FSample) then begin
      b:=false;
      FSIPClient.FSample(FSIPClient,buf^.buf,buf^.len,b);
      if b then begin
        FStat_:=false;
        FSIPClient.FThread.mediaDataState:=integer(krsipcAuto);
      end;
    end;

  end;    }


  case n of
    2: begin
      if FDTMFCall then inc(FDTMFCallCount,buf^.len div 2)
      else begin
        FDTMFCall:=true;
        FDTMFCallCount:=buf^.len div 2;
      end;
    end;
    3: begin
      if FDTMFCall3 then inc(FDTMFCallCount3,buf^.len div 2)
      else begin
        FDTMFCall3:=true;
        FDTMFCallCount3:=buf^.len div 2;
      end;
    end else begin
      if FDTMFCall then begin
        FDTMFCall:=false;
        if (FDTMFCallCount>=7000)then inc(FDTFMCallCnt);
        if (FDTFMCallCnt>0)then begin
          FStat_:=false;
          FSIPClient.FThread.mediaDataState:=integer(krsipcBeeps);
        end;
      end;
      if FDTMFCall3 then begin
        FDTMFCall3:=false;
        if (FDTMFCallCount3>=7000)then inc(FDTFMCallCnt3);
        if (FDTFMCallCnt3>0)then begin
          FStat_:=false;
          FSIPClient.FThread.mediaDataState:=integer(krsipcBeeps);
        end;
      end;
    end;
  end;
  dispose(buf);
end;

procedure TKRSIPSamplesThread.stat(v: boolean);
begin
  if FStat_=v then exit;
  FStat_:=v;
  if FStat_ then begin
    FSilence:=0;
    FStartSilence:=true;

    //REAddLog('SamplesThread clear data');
    auto1:=0;
    auto1n:=0;

    auto2:=0;
    auto2n:=0;

  end else begin
    auto1is:=auto1>3;
    auto2is:=auto2>5;
  end;
end;

end.
