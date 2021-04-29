(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRConnector                                                               *)
(*  Ver.: 18.02.2021                                                          *)
(*  https://kandiral.ru/delphi/krconnector.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRConnector;

interface

uses
  {$IF CompilerVersion >= 23}
    WinApi.Windows, System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Windows, Classes, SysUtils, SyncObjs,
  {$IFEND}
  KRConnectorLng, KRTypes, KRSpeedInfo, KRRuntimeErrors, KRThreadEvent;

const
  CE_QUEUE_MAX_ITEMS = 255;

type

  TKRConnectorType = (ctCOMPort, ctTCPClient, ctBluetooth);

  TKRConnectorStat = (cstNotActive, cstDisconnected, cstConnected, cstConnecting, cstWaitReconnecting);

  TKRConnectorStatEv = procedure(Sender: TObject; AStat: TKRConnectorStat; AReconnectTime: Cardinal)of object;
  TKRConnectorCallBack = procedure (AError: integer; APack: PByte; ALength: integer; AData: Pointer) of Object;
  TKRConnectorPackEv = procedure (Sender: TObject; APack: PByte; ALength: integer) of Object;


  TKRConnectorPack = record
    WaitResult: boolean;
    Length, RLen: integer;
    Error: integer;
    Pack: PByte;
    pData: Pointer;
    CallBack: TKRConnectorCallBack;
  end;
  PKRConnectorPack=^TKRConnectorPack;

  TKRConnectorThread = class;
  TKRConnectorThreadOut = class;

  TKRConnector = class(TComponent, IKRSpeedInfo)
  private
    CS: TCriticalSection;
    FThreadOut: TKRConnectorThreadOut;
    FOnRecv: TKRConnectorPackEv;
    FOnSend: TKRConnectorPackEv;
    FOnRecvAsync: TKRConnectorPackEv;
    FOnSendAsync: TKRConnectorPackEv;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FCountErrorsForReconnect: integer;
    FConnectionStatus: TKRConnectorStatEv;
    FInterval: cardinal;
    FInPackEvent, FOutPackEvent: THandle;
    FList: array[0..CE_QUEUE_MAX_ITEMS-1] of Pointer;
    FListPosIn, FListPosOut, FListCount: integer;
    function GetQueueCount: integer;
    function GetLastConnectTime: Cardinal;
    function GetQueueOutCount: integer;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
  protected
    FConnectorType: TKRConnectorType;
    FThread: TKRConnectorThread;
    FReconnectTime: Cardinal;
    FStat: TKRConnectorStat;
    function GetConnected: boolean;virtual;
    procedure CreateThread; virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure DoRuntimeError(ADesc: String; E: Exception);
    function GetCounter: Cardinal; stdcall;
    procedure Send(APack: PByte; ALength: integer; ACallBack: TKRConnectorCallBack;
      AData: Pointer = nil; AWaitResult: boolean = true; ARecvLen: integer = 0);
    function ErrorMsg(AError: TKRConnectorError): String;
    property Active: boolean read GetActive write SetActive;
    property QueueCount: integer read GetQueueCount;
    property QueueOutCount: integer read GetQueueOutCount;
    property Stat: TKRConnectorStat read FStat;
    property LastConnectTime: Cardinal read GetLastConnectTime;
    property ConnectorType: TKRConnectorType read FConnectorType;
    property Connected: boolean read GetConnected;
  published
    property OnSend: TKRConnectorPackEv read FOnSend write FOnSend;
    property OnRecv: TKRConnectorPackEv read FOnRecv write FOnRecv;
    property OnSendAsync: TKRConnectorPackEv read FOnSendAsync write FOnSendAsync;
    property OnRecvAsync: TKRConnectorPackEv read FOnRecvAsync write FOnRecvAsync;
    property OnConnectionStatus: TKRConnectorStatEv read FConnectionStatus write FConnectionStatus;
    property CountErrorsForReconnect: integer read FCountErrorsForReconnect write FCountErrorsForReconnect default 3;
    property ReconnectTime: Cardinal read FReconnectTime write FReconnectTime default 5000;
    property Interval: cardinal read FInterval write FInterval default 5;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
  end;

  TKRConnectorThread = class(TKRThreadEvent)
  private
    FLastStatusSendTm: Cardinal;
    procedure SendPackAsync;
    procedure RecvPackAsync;
  protected
    FCounter: cardinal;
    FConnector: TKRConnector;
    FInterval: cardinal;
    FReconnectTime, FLastConnectTime: Cardinal;
    FPk: PKRConnectorPack;
    procedure DoCallBack;
    procedure SendPack;
    procedure RecvPack;
    procedure _exec;virtual;abstract;
    procedure KRExecute; override;
    procedure KRExecuteFirst; override;
    procedure KRExecuteLast; override;
    procedure SetStat(AStat: TKRConnectorStat);
    procedure Status;
  public
    constructor CreateTh(AConnector: TKRConnector);
  end;

  TKRConnectorThreadOut = class(TKRThreadEvent)
  private
    CS: TCriticalSection;
    FConnector: TKRConnector;
    FOutPackEvent: THandle;
    FList: array[0..CE_QUEUE_MAX_ITEMS-1] of Pointer;
    FListPosIn, FListPosOut, FListCount: integer;
  protected
    procedure KRExecute; override;
    procedure KRExecuteLast; override;
  public
    constructor CreateTh(AConnector: TKRConnector; AOutPackEvent: THandle);
    procedure push(p: PKRConnectorPack);
  end;

implementation

{ TKRConnector }

constructor TKRConnector.Create(AOwner: TComponent);
begin
  inherited;
  FStat:=cstNotActive;
  FReconnectTime:=5000;
  FCountErrorsForReconnect:=3;
  FInterval:=5;
  FListPosIn:=0;
  FListPosOut:=0;
  FListCount:=0;

  CS:=TCriticalSection.Create;
  FInPackEvent:=CreateEvent(nil, true, false, nil);
  FOutPackEvent:=CreateEvent(nil, true, false, nil);
  FThreadOut:=TKRConnectorThreadOut.CreateTh(Self, FOutPackEvent);
end;

procedure TKRConnector.CreateThread;
begin
  FThread.FReconnectTime:=FReconnectTime;
  FThread.FInterval:=FInterval;
  FThread.FLastConnectTime:=getTickCount-FReconnectTime-1;
  ResetEvent(FInPackEvent)
end;

destructor TKRConnector.Destroy;
begin
  SetActive(false);

  FThreadOut.Free;
  CloseHandle(FOutPackEvent);

  CS.Free;

  CloseHandle(FInPackEvent);
  inherited;
end;

procedure TKRConnector.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRConnector.ErrorMsg(AError: TKRConnectorError): String;
begin
  if(AError>=0)and(AError<CE_ERRORS_COUNT)then Result:=CONNECTOR_ERRORS_MSG[AError]
  else Result:='Неизвестная ошибка.';

end;

function TKRConnector.GetActive: boolean;
begin
  CS.Enter;
  try
    Result:=Assigned(FThread);
  finally
    CS.Leave;
  end;
end;

function TKRConnector.GetConnected: boolean;
begin
  Result:=false;
end;

function TKRConnector.GetCounter: Cardinal;
begin
  if FThread=nil then result:=0
  else Result:=FThread.FCounter;
end;

function TKRConnector.GetLastConnectTime: Cardinal;
begin
  if FThread=nil then result:=0
  else result:=FThread.FLastConnectTime
end;

function TKRConnector.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRConnector.GetQueueCount: integer;
begin
  Result:=FListCount;
end;

function TKRConnector.GetQueueOutCount: integer;
begin
  Result:=FThreadOut.FListCount;
end;

procedure TKRConnector.Send(APack: PByte; ALength: integer;
      ACallBack: TKRConnectorCallBack; AData: Pointer = nil; AWaitResult: boolean = true;
      ARecvLen: integer = 0);
var
  pk: PKRConnectorPack;
  er: integer;
begin
  er:=0;
  CS.Enter;
  try
    if not Assigned(FThread) then er:=ceNotActive
    else if FListCount=CE_QUEUE_MAX_ITEMS then er:=ceQueueOverflowed
    else begin
      new(pk);
      pk^.Pack:=APack;
      pk^.Length:=ALength;
      pk^.CallBack:=ACallBack;
      pk^.pData:=AData;
      pk^.RLen:=ARecvLen;
      pk^.WaitResult:=AWaitResult;
      FList[FListPosIn]:=pk;
      if FListPosIn=CE_QUEUE_MAX_ITEMS-1 then FListPosIn:=0 else inc(FListPosIn);
      inc(FListCount);
      SetEvent(FInPackEvent);
    end;
  finally
    CS.Leave;
  end;
  if er>0 then begin
//    APack^.Error:=er;
    ACallBack(er, APack, 0, AData)
  end;
end;

procedure TKRConnector.SetActive(const Value: boolean);
var
  pk: PKRConnectorPack;
  apk: array of TKRConnectorPack;
  i,n: integer;
begin
  n:=-1;
  CS.Enter;
  try
    if Value then begin
      if Assigned(FThread) then exit;
      CreateThread;
    end else begin
      if not Assigned(FThread) then exit;
      FreeAndNil(FThread);
      SetLength(apk,FListCount);
      n:=FListCount-1;
      i:=0;
      while FListCount>0 do begin
        pk:=FList[FListPosOut];
        if FListPosOut=CE_QUEUE_MAX_ITEMS-1 then FListPosOut:=0 else inc(FListPosOut);
        dec(FListCount);
        apk[i]:=pk^;
        inc(i);
        dispose(pk);
      end;
    end;
  finally
    CS.Leave;
  end;
  for i := 0 to n do
    if Assigned(apk[i].CallBack) then begin
      apk[i].Error:=ceNotActive;
      apk[i].CallBack(apk[i].Error,apk[i].Pack,apk[i].Length,apk[i].pData);
    end;
end;

procedure TKRConnector.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

{ TKRConnectorThread }

constructor TKRConnectorThread.CreateTh(AConnector: TKRConnector);
begin
  FConnector:=AConnector;

  FCounter:=0;
  FLastConnectTime:=getTickCount-FReconnectTime-1;
  inherited Create(FConnector.FInPackEvent);
end;

procedure TKRConnectorThread.DoCallBack;
begin
  FConnector.FThreadOut.push(FPk);
end;

procedure TKRConnectorThread.KRExecute;
begin
  FConnector.CS.Enter;
  FPk:=FConnector.FList[FConnector.FListPosOut];
  if FConnector.FListPosOut=CE_QUEUE_MAX_ITEMS-1 then FConnector.FListPosOut:=0 else inc(FConnector.FListPosOut);
  dec(FConnector.FListCount);
  FConnector.CS.Leave;

  try
    _exec;
  except on E: Exception do
    FConnector.DoRuntimeError('TKRConnectorThread.KRExecute[FConnector.Name="'+
      FConnector.Name+'"]',E);
  end;

  if Terminated then exit;

  FConnector.CS.Enter;
  if FConnector.FListCount=0 then ResetEvent(FConnector.FInPackEvent);
  FConnector.CS.Leave;
end;

procedure TKRConnectorThread.KRExecuteFirst; begin end;

procedure TKRConnectorThread.KRExecuteLast;
begin
  SetStat(cstNotActive);
end;

procedure TKRConnectorThread.RecvPack;
begin
  if Assigned(FConnector.FOnRecv) then FConnector.FOnRecv(FConnector,FPk^.Pack,FPk^.Length);
  if Assigned(FConnector.FOnRecvAsync) then Synchronize(RecvPackAsync);
end;

procedure TKRConnectorThread.RecvPackAsync;
begin
  if Assigned(FConnector.FOnRecvAsync) then FConnector.FOnRecvAsync(FConnector,FPk^.Pack,FPk^.Length);
end;

procedure TKRConnectorThread.SendPack;
begin
  if Assigned(FConnector.FOnSend) then FConnector.FOnSend(FConnector,FPk^.Pack,FPk^.Length);
  if Assigned(FConnector.FOnSendAsync) then Synchronize(SendPackAsync);
end;

procedure TKRConnectorThread.SendPackAsync;
begin
  if Assigned(FConnector.FOnSendAsync) then FConnector.FOnSendAsync(FConnector,FPk^.Pack,FPk^.Length);
end;

procedure TKRConnectorThread.SetStat(AStat: TKRConnectorStat);
begin
  if FConnector.FStat<>AStat then begin
    FConnector.FStat:=AStat;
    if Assigned(FConnector.FConnectionStatus) then begin
      FLastStatusSendTm:=getTickCount;
      Synchronize(Status);
    end;
  end else if(Assigned(FConnector.FConnectionStatus))and(AStat=cstWaitReconnecting)and((getTickCount-FLastStatusSendTm)>500)then begin
    FLastStatusSendTm:=getTickCount;
    Synchronize(Status);
  end;
end;

procedure TKRConnectorThread.Status;
begin
  if Assigned(FConnector.FConnectionStatus) then
    FConnector.FConnectionStatus(FConnector,FConnector.FStat,getTickCount-FLastConnectTime);
end;

{ TKRConnectorThreadOut }

constructor TKRConnectorThreadOut.CreateTh(AConnector: TKRConnector; AOutPackEvent: THandle);
begin
  CS:=TCriticalSection.Create;
  FConnector:=AConnector;
  FOutPackEvent:=AOutPackEvent;

  FListPosIn:=0;
  FListPosOut:=0;
  FListCount:=0;
  inherited Create(FOutPackEvent);
end;

procedure TKRConnectorThreadOut.KRExecute;
var
  pk: PKRConnectorPack;
begin
  pk:=FList[FListPosOut];
  if FListPosOut=CE_QUEUE_MAX_ITEMS-1 then FListPosOut:=0 else inc(FListPosOut);
  try
    if Assigned(pk^.CallBack) then
      pk^.CallBack(pk^.Error,pk^.Pack,pk^.Length,pk^.pData);
  except on E: Exception do
    FConnector.DoRuntimeError('TKRConnectorThreadOut.KRExecute[FConnector.Name="'+
      FConnector.Name+'"]',E);
  end;
  Dispose(pk);
  CS.Enter;
  dec(FListCount);
  if FListCount=0 then ResetEvent(FOutPackEvent);
  CS.Leave;
end;

procedure TKRConnectorThreadOut.KRExecuteLast;
begin
  CS.Free;
end;

procedure TKRConnectorThreadOut.push(p: PKRConnectorPack);
begin
  if FListCount=CE_QUEUE_MAX_ITEMS then begin
    try
      if Assigned(p^.CallBack) then
        p^.CallBack(p^.Error,p^.Pack,p^.Length,p^.pData);
    except on E: Exception do
      FConnector.DoRuntimeError('TKRConnectorThreadOut.push[FConnector.Name="'+
        FConnector.Name+'"]',E);
    end;
    Dispose(p);
  end else begin
    CS.Enter;
    FList[FListPosIn]:=p;
    if FListPosIn=CE_QUEUE_MAX_ITEMS-1 then FListPosIn:=0 else inc(FListPosIn);
    inc(FListCount);
    SetEvent(FOutPackEvent);
    CS.Leave;
  end;
end;

end.
