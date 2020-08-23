(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRConnector                                                               *)
(*  Ver.: 23.08.2020                                                          *)
(*  https://kandiral.ru/delphi/krconnector.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRConnector;

interface

{$I '..\Includes\language.inc'}

uses
  {$IF CompilerVersion >= 23}
    WinApi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
    Windows, Classes, SysUtils,
  {$IFEND}
  KRTypes, KRSpeedInfo, KRRuntimeErrors, KRThread, KRThreadQueue;

type
  TKRConnectorError = integer;

const
  CE_QUEUE_MAX_ITEMS = 255;

  CE_ERRORS_COUNT = 8;

  ceOK = TKRConnectorError(0);
  ceQueueOverflowed = TKRConnectorError(1);
  ceNotConnection = TKRConnectorError(2);
  ceDataNotSended = TKRConnectorError(3);
  ceNoResponse = TKRConnectorError(4);
  ceRequestTimeout = TKRConnectorError(5);
  ceResponseTimeout = TKRConnectorError(6);
  ceNotActive = TKRConnectorError(7);

  CONNECTOR_ERRORS_MSG : array[0..CE_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
    'Нет ошибок',
    'Очередь коннектора переполнена',
    'Соединение не установлено',
    'Не удалось отправить данные',
    'Ответ не получен',
    'Истекло время ожидания отправки данных',
    'Истекло время ожидания получения ответа',
    'Соединение не активно'
{$ELSE}
    'No errors',
    'Connector queue overflow',
    'Connection not set',
    'Failed to send data',
    'No response received',
    'Sending data timeout',
    'Response receiving timeout',
    'Connection not active'
{$ENDIF}
  );

type

  TKRConnectorType = (ctCOMPort, ctTCPClient, ctBluetooth);

  TKRConnectorStat = (cstNotActive, cstDisconnected, cstConnected, cstConnecting, cstWaitReconnecting);

  TKRConnectorStatEv = procedure(Sender: TObject; AStat: TKRConnectorStat; AReconnectTime: Cardinal)of object;
  TKRConnectorCallBack = procedure (AError: integer; APack: PKRBuffer; ALength: integer; AData: Pointer) of Object;
  TKRConnectorPackEv = procedure (Sender: TObject; APack: PKRBuffer; ALength: integer) of Object;


  TKRConnectorPack = record
    WaitResult: boolean;
    Length, RLen: integer;
    DelimiterLen: integer;
    Error: integer;
    Delimiter: Cardinal;
    Pack: PKRBuffer;
    pData: Pointer;
    CallBack: TKRConnectorCallBack;
  end;
  PKRConnectorPack=^TKRConnectorPack;

  TKRConnectorThread = class;
  TKRConnectorThreadOut = class;

  TKRConnector = class(TComponent, IKRSpeedInfo)
  private
    FThreadOut: TKRConnectorThreadOut;
    FOnRecv: TKRConnectorPackEv;
    FOnSend: TKRConnectorPackEv;
    FOnRecvAsync: TKRConnectorPackEv;
    FOnSendAsync: TKRConnectorPackEv;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FCountErrorsForReconnect: integer;
    FConnectionStatus: TKRConnectorStatEv;
    function GetQueueCount: integer;
    function GetReadTimeout: Cardinal;
    function GetReconnectTime: Cardinal;
    function GetWriteTimeout: Cardinal;
    procedure SetReadTimeout(const Value: Cardinal);
    procedure SetReconnectTime(const Value: Cardinal);
    procedure SetWriteTimeout(const Value: Cardinal);
    function GetConnectTimeout: Cardinal;
    procedure SetConnectTimeout(const Value: Cardinal);
    function GetLastConnectTime: Cardinal;
    function GetQueueOutCount: integer;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function GetWaitPauseTime: Cardinal;
    function GetWaitTime: Cardinal;
    procedure SetWaitPauseTime(const Value: Cardinal);
    procedure SetWaitTime(const Value: Cardinal);
    function GetWaitOutTime: Cardinal;
    function GetWaitPauseOutTime: Cardinal;
    procedure SetWaitOutTime(const Value: Cardinal);
    procedure SetWaitPauseOutTime(const Value: Cardinal);
    function GetWaitRespTime: Cardinal;
    procedure SetWaitRespTime(const Value: Cardinal);
    function GetStat: TKRConnectorStat;
  protected
    FConnectorType: TKRConnectorType;
    FThread: TKRConnectorThread;
    FQueue: TKRThreadQueue;
    FQueueOut: TKRThreadQueue;
    function GetConnected: boolean;virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure DoRuntimeError(ADesc: String; E: Exception);
    function GetCounter: Cardinal; stdcall;
    procedure Send(APack: PKRBuffer; ALength: integer; ACallBack: TKRConnectorCallBack;
      AData: Pointer = nil; AWaitResult: boolean = true; ARecvLen: integer = 0;
      ADelimiter: Cardinal = 0; ADelimiterLen: integer = 0);
    function ErrorMsg(AError: TKRConnectorError): String;
    property Active: boolean read GetActive write SetActive;
    property QueueCount: integer read GetQueueCount;
    property QueueOutCount: integer read GetQueueOutCount;
    property Stat: TKRConnectorStat read GetStat;
    property LastConnectTime: Cardinal read GetLastConnectTime;
    property ConnectorType: TKRConnectorType read FConnectorType;
    property Connected: boolean read GetConnected;
  published
    property OnSend: TKRConnectorPackEv read FOnSend write FOnSend;
    property OnRecv: TKRConnectorPackEv read FOnRecv write FOnRecv;
    property OnSendAsync: TKRConnectorPackEv read FOnSendAsync write FOnSendAsync;
    property OnRecvAsync: TKRConnectorPackEv read FOnRecvAsync write FOnRecvAsync;
    property OnConnectionStatus: TKRConnectorStatEv read FConnectionStatus write FConnectionStatus;
    property ReadTimeout: Cardinal read GetReadTimeout write SetReadTimeout default 100;
    property WriteTimeout: Cardinal read GetWriteTimeout write SetWriteTimeout default 100;
    property WaitRespTime: Cardinal read GetWaitRespTime write SetWaitRespTime default 5;
    property CountErrorsForReconnect: integer read FCountErrorsForReconnect write FCountErrorsForReconnect default 3;
    property ConnectTimeout: Cardinal read GetConnectTimeout write SetConnectTimeout default 1000;
    property ReconnectTime: Cardinal read GetReconnectTime write SetReconnectTime default 5000;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
    property WaitTime: Cardinal read GetWaitTime write SetWaitTime default 20;
    property WaitPauseTime: Cardinal read GetWaitPauseTime write SetWaitPauseTime default 1000;
    property WaitOutTime: Cardinal read GetWaitOutTime write SetWaitOutTime default 20;
    property WaitPauseOutTime: Cardinal read GetWaitPauseOutTime write SetWaitPauseOutTime default 1000;
  end;

  TKRConnectorThread = class(TKRThread)
  private
    FLastStatusSendTm: Cardinal;
    procedure SendPackAsync;
    procedure RecvPackAsync;
  protected
    FStat: TKRConnectorStat;
    FCounter: cardinal;
    FQueue: TKRThreadQueue;
    FQueueOut: TKRThreadQueue;
    FConnector: TKRConnector;
    FPK: PKRConnectorPack;
    FWaitRespTime: Cardinal;
    FReconnectTime, FLastConnectTime: Cardinal;
    FReadTimeout, FWriteTimeout, FConnectTimeout: Cardinal;
    procedure DoCallBack;
    procedure SendPack;
    procedure RecvPack;
    procedure _exec;virtual;
    procedure KRExecute; override;
    procedure KRExecutePaused; override;
    procedure KRExecutePausedFirst; override;
    procedure SetStat(AStat: TKRConnectorStat);
    procedure Status;
  public
    constructor CreateTh(AConnector: TKRConnector; AQueue: TKRThreadQueue;
      AQueueOut: TKRThreadQueue);overload;
    property WaitRespTime: Cardinal read FWaitRespTime write FWaitRespTime;
  end;

  TKRConnectorThreadOut = class(TKRThread)
  private
    FConnector: TKRConnector;
    FQueueOut: TKRThreadQueue;
  protected
    procedure KRExecute; override;
  public
    constructor CreateTh(AConnector: TKRConnector; AQueueOut: TKRThreadQueue);
  end;

implementation

{ TKRConnector }

constructor TKRConnector.Create(AOwner: TComponent);
begin
  inherited;
  FCountErrorsForReconnect:=3;
  FQueue:=TKRThreadQueue.Create;
  FQueueOut:=TKRThreadQueue.Create;
  FThreadOut:=TKRConnectorThreadOut.CreateTh(Self,FQueueOut);
end;

destructor TKRConnector.Destroy;
begin
  FThreadOut.Free;
  FQueue.Free;
  FQueueOut.Free;
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
  Result:=FThread.Active and FThreadOut.Active;
end;

function TKRConnector.GetConnected: boolean;
begin
  Result:=false;
end;

function TKRConnector.GetConnectTimeout: Cardinal;
begin
  Result:=FThread.FConnectTimeout;
end;

function TKRConnector.GetCounter: Cardinal;
begin
  Result:=FThread.FCounter;
end;

function TKRConnector.GetLastConnectTime: Cardinal;
begin
  result:=FThread.FLastConnectTime
end;

function TKRConnector.GetReadTimeout: Cardinal;
begin
  Result:=FThread.FReadTimeout;
end;

function TKRConnector.GetReconnectTime: Cardinal;
begin
  Result:=FThread.FReconnectTime;
end;

function TKRConnector.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRConnector.GetStat: TKRConnectorStat;
begin
  Result:=FThread.FStat;
end;

function TKRConnector.GetQueueCount: integer;
begin
  Result:=FQueue.Count;
end;

function TKRConnector.GetQueueOutCount: integer;
begin
  Result:=FQueueOut.Count;
end;

function TKRConnector.GetWaitOutTime: Cardinal;
begin
  Result:=FThreadOut.WaitTime;
end;

function TKRConnector.GetWaitPauseOutTime: Cardinal;
begin
  Result:=FThreadOut.WaitPauseTime;
end;

function TKRConnector.GetWaitPauseTime: Cardinal;
begin
  Result:=FThread.WaitPauseTime;
end;

function TKRConnector.GetWaitRespTime: Cardinal;
begin
  Result:=FThread.WaitRespTime;
end;

function TKRConnector.GetWaitTime: Cardinal;
begin
  Result:=FThread.WaitTime;
end;

function TKRConnector.GetWriteTimeout: Cardinal;
begin
  Result:=FThread.FWriteTimeout;
end;

procedure TKRConnector.Send(APack: PKRBuffer; ALength: integer;
      ACallBack: TKRConnectorCallBack; AData: Pointer = nil; AWaitResult: boolean = true;
      ARecvLen: integer = 0; ADelimiter: Cardinal = 0; ADelimiterLen: integer = 0);
var
  pk: PKRConnectorPack;
begin
  new(pk);
  pk^.Pack:=APack;
  pk^.Length:=ALength;
  pk^.CallBack:=ACallBack;
  pk^.pData:=AData;
  pk^.RLen:=ARecvLen;
  pk^.DelimiterLen:=0;
  pk^.Delimiter:=ADelimiter;
  pk^.DelimiterLen:=ADelimiterLen;
  pk^.WaitResult:=AWaitResult;
  if FQueue.AtLeast(CE_QUEUE_MAX_ITEMS) then begin
    ACallBack(ceQueueOverflowed, APack, 0, AData);
    Dispose(pk);
  end else FQueue.Push(pk)
end;

procedure TKRConnector.SetActive(const Value: boolean);
begin
  FThread.Active:=Value;
  FThreadOut.Active:=Value;
end;

procedure TKRConnector.SetConnectTimeout(const Value: Cardinal);
begin
  if(Value>9)and(Value<10001)then FThread.FConnectTimeout:=Value;
end;

procedure TKRConnector.SetReadTimeout(const Value: Cardinal);
begin
  if(Value>9)and(Value<10001)then FThread.FReadTimeout:=Value;
end;

procedure TKRConnector.SetReconnectTime(const Value: Cardinal);
begin
  FThread.FReconnectTime:=Value;
end;

procedure TKRConnector.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

procedure TKRConnector.SetWaitOutTime(const Value: Cardinal);
begin
  FThreadOut.WaitTime:=value;
end;

procedure TKRConnector.SetWaitPauseOutTime(const Value: Cardinal);
begin
  FThreadOut.WaitPauseTime:=value;
end;

procedure TKRConnector.SetWaitPauseTime(const Value: Cardinal);
begin
  FThread.WaitPauseTime:=value;
end;

procedure TKRConnector.SetWaitRespTime(const Value: Cardinal);
begin
  FThread.WaitRespTime:=value;
end;

procedure TKRConnector.SetWaitTime(const Value: Cardinal);
begin
  FThread.WaitTime:=value;
end;

procedure TKRConnector.SetWriteTimeout(const Value: Cardinal);
begin
  if(Value>9)and(Value<10001)then FThread.FWriteTimeout:=Value;
end;

{ TKRConnectorThread }

constructor TKRConnectorThread.CreateTh(AConnector: TKRConnector; AQueue,
  AQueueOut: TKRThreadQueue);
begin
  FConnector:=AConnector;
  FStat:=cstNotActive;
  FCounter:=0;
  FConnectTimeout:=1000;
  FReconnectTime:=5000;
  FLastConnectTime:=getTickCount-FReconnectTime-1;
  FReadTimeout:=100;
  FWriteTimeout:=100;
  FWaitRespTime:=5;
  FQueue:=AQueue;
  FQueueOut:=AQueueOut;
  inherited Create;
end;

procedure TKRConnectorThread.DoCallBack;
begin
//  if FPK^.Error=0 then FConnector.SetStat(cstConnected) else FConnector.SetStat(cstDisconnected);
  if FQueueOut.AtLeast(CE_QUEUE_MAX_ITEMS) then begin
    FConnector.DoRuntimeError('TKRConnectorThread[FConnector.Name="'+
      FConnector.Name+'"] procedure DoCallBack;',Exception.Create('Исходящая очередь коннектора переполнена'));
    Dispose(FPK);
  end else FQueueOut.Push(FPK);
  FPK:=nil;
end;

procedure TKRConnectorThread.KRExecute;
begin
  try
    if FQueue.Count>0 then begin
      FPk:=PKRConnectorPack(FQueue.Pop);
      _exec;
    end;
  except on E: Exception do
    FConnector.DoRuntimeError('TKRConnectorThreadOut[FConnector.Name="'+
      FConnector.Name+'"] procedure Execute;',E);
  end;
end;

procedure TKRConnectorThread.KRExecutePaused;
begin

end;

procedure TKRConnectorThread.KRExecutePausedFirst;
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
  if FStat<>AStat then begin
    FStat:=AStat;
    FLastStatusSendTm:=getTickCount;
    Synchronize(Status);
  end else if(FStat=cstWaitReconnecting)and((getTickCount-FLastStatusSendTm)>500)then begin
    FLastStatusSendTm:=getTickCount;
    Synchronize(Status);
  end;
end;

procedure TKRConnectorThread.Status;
begin
  if Assigned(FConnector.FConnectionStatus) then
    FConnector.FConnectionStatus(FConnector,FStat,getTickCount-FLastConnectTime);
end;

procedure TKRConnectorThread._exec;
begin

end;

{ TKRConnectorThreadOut }

constructor TKRConnectorThreadOut.CreateTh(AConnector: TKRConnector;
  AQueueOut: TKRThreadQueue);
begin
  FConnector:=AConnector;
  FQueueOut:=AQueueOut;
  inherited Create;
end;

procedure TKRConnectorThreadOut.KRExecute;
var
  pk: PKRConnectorPack;
begin
  if FQueueOut.Count>0 then begin
    pk:=PKRConnectorPack(FQueueOut.Pop);
    try
      if Assigned(pk^.CallBack) then
        pk^.CallBack(pk^.Error,pk^.Pack,pk^.Length,pk^.pData);
    except on E: Exception do
      FConnector.DoRuntimeError('TKRConnectorThreadOut[FConnector.Name="'+
        FConnector.Name+'"] procedure Execute;',E);
    end;
    Dispose(pk);
  end;
end;

end.
