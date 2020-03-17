(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRModem                                                                   *)
(*  Ver.: 26.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRModem;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.MMSystem, System.Classes, System.SysUtils, System.Math, System.StrUtils,
  {$ELSE}
    MMSystem, Classes, SysUtils, Math, StrUtils,
  {$IFEND}
  KRCOMPort, KRThread, KRRuntimeErrors, KRTypes, KRThreadQueue, Funcs;

const
  KRMODEM_INIT_COMMNDS : array[0..14] of String = (
    'ATE0'#13,
    'ATV1'#13,
    'ATH'#13,
    'AT+CIPSHUT'#13,
    'AT+CMEE=0'#13,
    'AT+CR=0'#13,
    'AT&D0'#13,
    'AT+CSSN=0,0'#13,
    'AT+CLIP=1'#13,
    'AT+CSCS="UCS2"'#13,
    'AT+CMGF=0'#13,
    'AT+CNMI=2,2,0,0'#13,
    'AT+CGATT=1'#13,
    'AT+CIPATS=0'#13,
    'AT+CIPHEAD=1'#13
  );

type
  TKRModemState = (msNotActive, msDisconnected, msConnecting, msIitialization,
    msNoResponse, msActive);

  TKRModemCallBack = procedure(Sender: TObject; AData: String) of Object;
  TKRModemData = procedure(Sender: TObject; AData: String) of Object;
  TKRModemData2 = procedure(Sender: TObject; AData: TStrings) of Object;

  TKRModemCommand = record
    cmd: String;
    data: String;
    timeout: cardinal;
    callback: TKRModemCallBack;
  end;
  PKRModemCommand = ^TKRModemCommand;

  TKRModemThread = class;

  TKRModem = class(TComponent)
  private
    FThread:  TKRModemThread;
    FComPort: TKRComPort;
    FQueue: TKRThreadQueue;
    FOnRecvAsync: TKRModemData;
    FOnRecv: TKRModemData;
    FOnSendAsync: TKRModemData;
    FOnSend: TKRModemData;
    FWaitRespTime: Cardinal;
    FOnRING: TNotifyEvent;
    FOnCLIP: TKRModemData2;
    procedure FreeCB(Sender: TObject; AData: String);
    function GetActive: boolean;
    function GetBaudRate: integer;
    function GetDataBits: integer;
    function GetFlowControl: TKRFlowControl;
    function GetParity: integer;
    function GetPort: String;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    function GetStopBits: integer;
    procedure SetActive(const Value: boolean);
    procedure SetBaudRate(const Value: integer);
    procedure SetDataBits(const Value: integer);
    procedure SetFlowControl(const Value: TKRFlowControl);
    procedure SetParity(const Value: integer);
    procedure SetPort(const Value: String);
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    procedure SetStopBits(const Value: integer);
    function GetState: TKRModemState;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property State: TKRModemState read GetState;
    property Active: boolean read GetActive write SetActive;
    procedure Send(ACommand: String); overload;
    procedure Send(ACommand: String; ATimeout: Cardinal); overload;
    procedure Send(ACommand: String; ATimeout: Cardinal; ACallback: TKRModemCallBack); overload;
    procedure Send(ACommand: String; ATimeout: Cardinal; ACallback: TKRModemCallBack;
      AData: String); overload;
  published
    property OnSend: TKRModemData read FOnSend write FOnSend;
    property OnRecv: TKRModemData read FOnRecv write FOnRecv;
    property OnSendAsync: TKRModemData read FOnSendAsync write FOnSendAsync;
    property OnRecvAsync: TKRModemData read FOnRecvAsync write FOnRecvAsync;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
    property Port: String read GetPort write SetPort;
    property Parity: integer read GetParity write SetParity;
    property StopBits: integer read GetStopBits write SetStopBits;
    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property DataBits: integer read GetDataBits write SetDataBits;
    property FlowControl: TKRFlowControl read GetFlowControl write SetFlowControl;
    property WaitRespTime: Cardinal read FWaitRespTime write FWaitRespTime;
    property OnRING: TNotifyEvent read FOnRING write FOnRING;
    property OnCLIP: TKRModemData2 read FOnCLIP write FOnCLIP;
  end;

  TKRModemThread = class(TKRThread)
  private
    bf: String;
    FModem: TKRModem;
    FInit: byte;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FState: TKRModemState;
    FCommand: PKRModemCommand;
    FStartCmdTime: Cardinal;
    btRec: integer;
    atErr: byte;
    tmpStr: String;
    procedure RING;
    procedure CLIP;
    procedure SendPack;
    procedure RecvPack;
    procedure SendPackAsync;
    procedure RecvPackAsync;
    function isOK(AResult: String):boolean;
    procedure isFn;
    procedure cbAT(Sender: TObject; AData: String);
    procedure cbInit(Sender: TObject; AData: String);
    function GetParams(AStr: String): TStringList;
  protected
    procedure SetActive(const Value: boolean);override;
    procedure KRExecute; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AModem: TKRModem);
    procedure send(cmd: String; timeout: cardinal; callback: TKRModemCallBack;
      data: String);
  end;

implementation

{ TKRModem }

constructor TKRModem.Create(AOwner: TComponent);
begin
  inherited;
  FWaitRespTime:=35;
  FComPort:=TKRComPort.Create;
  //FComPort.InputBuffer:=255;
  //FComPort.OutputBuffer:=255;
  FQueue:=TKRThreadQueue.Create;
  FThread:=TKRModemThread.CreateTh(Self);
end;

destructor TKRModem.Destroy;
begin
  SetActive(false);
  FreeAndNil(FThread);
  while FQueue.Count>0 do Dispose(PKRModemCommand(FQueue.Pop));
  FQueue.Free;
  FComPort.Free;
  inherited;
end;

procedure TKRModem.FreeCB(Sender: TObject; AData: String);
begin

end;

function TKRModem.GetActive: boolean;
begin
  Result:=FThread.Active;
end;

function TKRModem.GetBaudRate: integer;
begin
  Result:=FComPort.BaudRate;
end;

function TKRModem.GetDataBits: integer;
begin
  Result:=FComPort.DataBits;
end;

function TKRModem.GetFlowControl: TKRFlowControl;
begin
  Result:=FComPort.FlowControl;
end;

function TKRModem.GetParity: integer;
begin
  Result:=FComPort.Parity;
end;

function TKRModem.GetPort: String;
begin
  Result:=FComPort.Port;
end;

function TKRModem.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FThread.FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRModem.GetState: TKRModemState;
begin
  Result:=FThread.FState;
end;

function TKRModem.GetStopBits: integer;
begin
  Result:=FComPort.StopBits;
end;

procedure TKRModem.Send(ACommand: String; ATimeout: Cardinal);
begin
  Send(ACommand,ATimeout,FreeCB);
end;

procedure TKRModem.Send(ACommand: String);
begin
  Send(ACommand,1000);
end;

procedure TKRModem.Send(ACommand: String; ATimeout: Cardinal;
  ACallback: TKRModemCallBack; AData: String);
begin
  FThread.send(ACommand,ATimeout,ACallback,AData);
end;

procedure TKRModem.Send(ACommand: String; ATimeout: Cardinal;
  ACallback: TKRModemCallBack);
begin
  Send(ACommand,ATimeout,ACallback,'');
end;

procedure TKRModem.SetActive(const Value: boolean);
begin
  FThread.Active:=Value;
end;

procedure TKRModem.SetBaudRate(const Value: integer);
begin
  FComPort.BaudRate:=Value;
end;

procedure TKRModem.SetDataBits(const Value: integer);
begin
  FComPort.DataBits:=Value;
end;

procedure TKRModem.SetFlowControl(const Value: TKRFlowControl);
begin
  FComPort.FlowControl:=Value;
end;

procedure TKRModem.SetParity(const Value: integer);
begin
  FComPort.Parity:=Value;
end;

procedure TKRModem.SetPort(const Value: String);
begin
  FComPort.Port:=Value;
end;

procedure TKRModem.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FThread.FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

procedure TKRModem.SetStopBits(const Value: integer);
begin
  FComPort.StopBits:=Value;
end;

{ TKRModemThread }

procedure TKRModemThread.cbAT(Sender: TObject; AData: String);
begin
  if isOK(AData) then begin
    atErr:=0;
    if FState=msConnecting then begin
      FState:=msIitialization;
      FInit:=0;
      send(KRMODEM_INIT_COMMNDS[0],1000,cbInit,'');
    end;
  end else begin
    inc(atErr);
    if atErr>2 then FState:=msNoResponse;
  end;
end;

procedure TKRModemThread.cbInit(Sender: TObject; AData: String);
begin
  inc(FInit);
  if FInit<Length(KRMODEM_INIT_COMMNDS) then
    send(KRMODEM_INIT_COMMNDS[FInit],1000,cbInit,'')
  else FState:=msActive;
end;

procedure TKRModemThread.CLIP;
var sl: TStringList;
begin
  if Assigned(FModem.FOnCLIP) then begin
    sl:=GetParams(RightStr(tmpStr,Length(tmpStr)-7));
    FModem.FOnCLIP(FModem,sl);
    sl.Free
  end;
end;

constructor TKRModemThread.CreateTh(AModem: TKRModem);
begin
  FModem:=AModem;
  FState:=msNotActive;
  inherited Create;
end;

function TKRModemThread.GetParams(AStr: String): TStringList;
var
  sl: TStringList;
  i: integer;
begin
  Result:=TStringList.Create;
  sl:=Explode(',',AStr);
  for I := 0 to sl.Count - 1 do begin
    if Length(sl[i])>0 then
      if sl[i][1]='"' then sl[i]:=copy(sl[i],2,Length(sl[i])-2);
    Result.Add(sl[i]);
  end;
end;

procedure TKRModemThread.isFn;
var
  sl: TStringList;
  i: integer;
begin
  sl:=Explode(#13#10,bf);
  i:=0;
  while i<sl.count do begin
    if sl[i]='' then begin
      sl.Delete(i);
    end else if sl[i]='RING' then begin
      Synchronize(RING);
      sl.Delete(i);
    end else if LeftStr(sl[i],7)='+CLIP: ' then begin
      tmpStr:=sl[i];
      Synchronize(CLIP);
      sl.Delete(i);
    end else if sl[i]='STOPRING' then begin
      sl.Delete(i);
    end else inc(i);
  end;

  bf:='';
  for I := 0 to sl.Count - 1 do begin
    if i>0 then bf:=bf+#13#10;
    bf:=bf+sl[i];
  end;

  sl.Free;
end;

function TKRModemThread.isOK(AResult: String): boolean;
begin
  Result:=RightStr(AResult,2)='OK';
end;

procedure TKRModemThread.KRExecute;
var
  i: integer;
  tm0: cardinal;
  _buf: AnsiString;
begin
  try
    if not FModem.FComPort.Connected then begin
      FState:=msConnecting;
      FModem.FComPort.Open;
    end;

    if FModem.FComPort.Connected then begin

      if FCommand=nil then begin
        if(FState=msConnecting)or(ElapsedTime(FStartCmdTime)>30000)then begin
          New(FCommand);
          FCommand^.cmd:=AnsiString('AT'#13);
          FCommand^.data:='';
          FCommand^.timeout:=1000;
          FCommand^.callback:=cbAT;
        end else if FModem.FQueue.Count>0 then
          FCommand:=FModem.FQueue.Pop;

        if FCommand<>nil then begin
          SendPack;
          FModem.FComPort.Write(FCommand^.cmd[1],Length(FCommand^.cmd));
          FStartCmdTime:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
        end;
      end;


      if FModem.FComPort.InputCount>0 then begin
        tm0:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
        btRec:=0;
        SetLength(_buf,255);
        while true do begin
          i:=FModem.FCOMPort.InputCount;
          REAddLog('i='+IntToStr(i));
          if i>0 then begin
            btRec:=btRec+FModem.FComPort.Read(Pointer(@_buf[btRec+1])^,min(255-btRec,i));
          REAddLog('btRec='+IntToStr(btRec));
            if btRec>254 then break;
            tm0:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
            continue;
          end;
          REAddLog('ElapsedTime='+IntToStr(ElapsedTime(tm0)));
          if ElapsedTime(tm0)>FModem.FWaitRespTime then break;
        end;

        SetLength(_buf,btRec);
        bf:=StringToWideString(_buf,1251);
        RecvPack;

        isFn;
        if Length(bf)>0 then begin
          if FCommand<>nil then begin
            if(bf='>')and(Length(FCommand^.data)>0)then
              FModem.FComPort.Write(FCommand^.data[1],Length(FCommand^.data))
            else begin
              FCommand^.callback(FModem,bf);
              Dispose(FCommand);
              FCommand:=nil;
            end;
          end;
        end;
      end;

      if FCommand<>nil then begin
        if ElapsedTime(FStartCmdTime)>FCommand^.timeout then begin
          SetLength(bf,0);
          FCommand^.callback(FModem,bf);
          Dispose(FCommand);
          FCommand:=nil;
        end;
      end;

    end else FState:=msDisconnected;
  except on E: Exception do
    REEvent(FRuntimeErrorEv,FModem,'TKRModemThread[Name="'+
      FModem.Name+'"] procedure KRExecute;',E);
  end;
end;

procedure TKRModemThread.KRExecutePausedFirst;
begin
  try
    while FModem.FQueue.Count>0 do Dispose(PKRModemCommand(FModem.FQueue.Pop));
    if FModem.FComPort.Connected then FModem.FComPort.Close;
  except end;
  FState:=msNotActive;
end;

procedure TKRModemThread.RecvPack;
begin
  if Assigned(FModem.FOnRecv) then FModem.FOnRecv(FModem,bf);
  if Assigned(FModem.FOnRecvAsync) then Synchronize(RecvPackAsync);
end;

procedure TKRModemThread.RecvPackAsync;
begin
  if Assigned(FModem.FOnRecvAsync) then FModem.FOnRecvAsync(FModem,bf);
end;

procedure TKRModemThread.RING;
begin
  if Assigned(FModem.FOnRING) then FModem.FOnRING(FModem);
end;

procedure TKRModemThread.send(cmd: String; timeout: cardinal;
  callback: TKRModemCallBack; data: String);
var
  _cmd: PKRModemCommand;
begin
  New(_cmd);
  _cmd^.cmd:=cmd;
  _cmd^.data:=data;
  _cmd^.timeout:=timeout;
  _cmd^.callback:=callback;
  FModem.FQueue.Push(_cmd);
end;

procedure TKRModemThread.SendPack;
begin
  if Assigned(FModem.FOnSend) then FModem.FOnSend(FModem,FCommand^.cmd);
  if Assigned(FModem.FOnSendAsync) then Synchronize(SendPackAsync);
end;

procedure TKRModemThread.SendPackAsync;
begin
  if Assigned(FModem.FOnSendAsync) then FModem.FOnSendAsync(FModem,FCommand^.cmd);
end;

procedure TKRModemThread.SetActive(const Value: boolean);
begin
  atErr:=0;
  FCommand:=nil;
  inherited;
end;

end.
