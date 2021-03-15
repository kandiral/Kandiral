(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRServer                                                                  *)
(*  Ver.: 17.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRServer;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms,
  {$ELSE}
    Windows, Classes, SysUtils, Forms,
  {$IFEND}
  KRTypes, KRRuntimeErrors, KRThread, KRSockets;

type
  TKRServerEvent = procedure(Sender: TObject; APack: PKRBuffer; var ALength: integer) of Object;
  TKRServerPackEv = procedure(Sender: TObject; APack: PKRBuffer; ALength: integer) of Object;

  TKRServer = class;
  TKRServerThread = class;

  TKRSrvClient = class(TKRThread)
  private
    FServerThread: TKRServerThread;
    FClient: TKRSocketSrvClient;
    FLastDataTime, FTimeout: Cardinal;
    FPack: PKRBuffer;
    FLength: Integer;
    procedure SendPack(APack: PKRBuffer; ALength: integer);
    procedure RecvPack(APack: PKRBuffer; ALength: integer);
    procedure SendPackAsync;
    procedure RecvPackAsync;
  protected
    procedure KRExecute; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateClient(AServerThread: TKRServerThread; AClient: TKRSocketSrvClient);
    destructor Destroy;override;
  end;

  TKRServerThread = class(TKRThread)
  private
    FMainServer: TKRServer;
    FEvent: TKRServerEvent;
    FReconnectTime, FLastConnectTime: Cardinal;
    FLastCheckClientsTime, FCheckClientsTime: Cardinal;
    FWaitRespTime: cardinal;
    FWaitClientTimeout: cardinal;
    FClientDisconnect: TNotifyEvent;
  protected
    function CreateClient(AClient: TKRSocketSrvClient): TKRSrvClient; virtual;
    procedure KRExecute; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AServer: TKRServer);
    destructor Destroy;override;
  end;

  TKRServer = class(TComponent)
  private
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FClients: TThreadList;
    FOnRecvAsync: TKRServerPackEv;
    FOnRecv: TKRServerPackEv;
    FOnSendAsync: TKRServerPackEv;
    FOnSend: TKRServerPackEv;
    procedure DoRuntimeError(ADesc: String; E: Exception);
    function GetActive: boolean;
    function GetEvent: TKRServerEvent;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetActive(const Value: boolean);
    procedure SetEvent(const Value: TKRServerEvent);
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    function GetWaitRespTime: cardinal;
    procedure SetWaitRespTime(const Value: cardinal);
    function GetCheckClientsTime: Cardinal;
    function GetReconnectTime: Cardinal;
    procedure SetCheckClientsTime(const Value: Cardinal);
    procedure SetReconnectTime(const Value: Cardinal);
    function GetWaitClientTimeout: cardinal;
    procedure SetWaitClientTimeout(const Value: cardinal);
    function GetClientDisconnect: TNotifyEvent;
    procedure SetClientDisconnect(const Value: TNotifyEvent);
  protected
    FThread: TKRServerThread;
  public
    FServer: TKRSocketServer;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Active: boolean read GetActive write SetActive;
    procedure SendBuf(var ABuf; ALength: integer);
  published
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
    property ReconnectTime: Cardinal read GetReconnectTime write SetReconnectTime;
    property CheckClientsTime: Cardinal read GetCheckClientsTime write SetCheckClientsTime;
    property WaitRespTime:cardinal read GetWaitRespTime write SetWaitRespTime;
    property WaitClientTimeout: cardinal read GetWaitClientTimeout write SetWaitClientTimeout;
    property OnEvent: TKRServerEvent read GetEvent write SetEvent;
    property OnClientDisconnect: TNotifyEvent read GetClientDisconnect write SetClientDisconnect;
    property OnSend: TKRServerPackEv read FOnSend write FOnSend;
    property OnRecv: TKRServerPackEv read FOnRecv write FOnRecv;
    property OnSendAsync: TKRServerPackEv read FOnSendAsync write FOnSendAsync;
    property OnRecvAsync: TKRServerPackEv read FOnRecvAsync write FOnRecvAsync;
  end;

implementation

{ TKRServer }

constructor TKRServer.Create(AOwner: TComponent);
begin
  inherited;
  FClients:=TThreadList.Create;
  FThread:=TKRServerThread.CreateTh(Self);
end;

destructor TKRServer.Destroy;
begin
  FThread.Free;
  FServer.Free;
  FClients.Free;
  inherited;
end;

procedure TKRServer.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRServer.GetActive: boolean;
begin
  Result:=FThread.Active;
end;

function TKRServer.GetEvent: TKRServerEvent;
begin
  Result:=FThread.FEvent;
end;

function TKRServer.GetCheckClientsTime: Cardinal;
begin
  Result:=FThread.FCheckClientsTime;
end;

function TKRServer.GetClientDisconnect: TNotifyEvent;
begin
  Result:=FThread.FClientDisconnect;
end;

function TKRServer.GetReconnectTime: Cardinal;
begin
  Result:=FThread.FReconnectTime;
end;

function TKRServer.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRServer.GetWaitClientTimeout: cardinal;
begin
  Result:=FThread.FWaitClientTimeout;
end;

function TKRServer.GetWaitRespTime: cardinal;
begin
  Result:=FThread.FWaitRespTime;
end;

procedure TKRServer.SendBuf(var ABuf; ALength: integer);
var
  list: TList;
  I: Integer;
begin
  list:=FClients.LockList;
  for I := 0 to list.Count-1 do
    TKRSrvClient(List[i]).FClient.SendBuf(ABuf,ALength);
  FClients.UnlockList;
end;

procedure TKRServer.SetActive(const Value: boolean);
begin
  FThread.Active:=Value;
end;

procedure TKRServer.SetEvent(const Value: TKRServerEvent);
begin
  FThread.FEvent:=Value;
end;

procedure TKRServer.SetCheckClientsTime(const Value: Cardinal);
begin
  FThread.FCheckClientsTime:=Value;
end;

procedure TKRServer.SetClientDisconnect(const Value: TNotifyEvent);
begin
  FThread.FClientDisconnect:=Value;
end;

procedure TKRServer.SetReconnectTime(const Value: Cardinal);
begin
  FThread.FReconnectTime:=Value;
end;

procedure TKRServer.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

procedure TKRServer.SetWaitClientTimeout(const Value: cardinal);
begin
  FThread.FWaitClientTimeout:=Value;
end;

procedure TKRServer.SetWaitRespTime(const Value: cardinal);
begin
  FThread.FWaitRespTime:=Value;
end;

{ TKRServerThread }

function TKRServerThread.CreateClient(AClient: TKRSocketSrvClient): TKRSrvClient;
begin
  result:=TKRSrvClient.CreateClient(Self,AClient);
end;

constructor TKRServerThread.CreateTh(AServer: TKRServer);
begin
  FMainServer:=AServer;
  FEvent:=nil;
  FReconnectTime:=5000;
  FLastConnectTime:=getTickCount-FReconnectTime-1;
  FCheckClientsTime:=30000;
  FLastCheckClientsTime:=getTickCount;
  FWaitRespTime:=35;
  FWaitClientTimeout:=30000;
  inherited Create;
end;

destructor TKRServerThread.Destroy;
begin
  inherited;
end;

procedure TKRServerThread.KRExecute;
var
  client: TKRSocketSrvClient;
  client_t: TKRSrvClient;
  i: integer;
  List: TList;
begin
  if not FMainServer.FServer.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        FMainServer.FServer.Open;
      except on E: Exception do
        FMainServer.DoRuntimeError('TKRTCPIPServerThread[FMainServer.Name="'+
          FMainServer.Name+'"; procedure KRExecute; FServer.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    exit;
  end;

  client:=FMainServer.FServer.Accept;
  if client<>nil then begin
    client_t:=CreateClient(client);
    client_t.Active:=true;
    FMainServer.FClients.Add(client_t);
  end else begin
    if((getTickCount-FLastCheckClientsTime)>FCheckClientsTime)then begin
      i:=0;
      List:=FMainServer.FClients.LockList;
      while i<List.Count do begin
        client_t:=TKRSrvClient(List[i]);
        if not client_t.Active then begin
          List.Delete(i);
          client_t.Free;
        end else inc(i);
      end;
      FMainServer.FClients.UnlockList;
      FLastCheckClientsTime:=getTickCount;
    end;
  end;
end;

procedure TKRServerThread.KRExecutePausedFirst;
var
  List: TList;
  client: TKRSrvClient;
begin
//REAddLog('TKRServerThread.KRExecutePausedFirst BEGIN');
  List:=FMainServer.FClients.LockList;
//REAddLog('1');
  while List.Count>0 do begin
//REAddLog('2');
    client:=TKRSrvClient(List[0]);
//REAddLog('3');
    client.Active:=false;
//REAddLog('4');
    List.Delete(0);
//REAddLog('5');
    client.Free;
//REAddLog('6');
  end;
//REAddLog('7');
  FMainServer.FClients.UnlockList;
//REAddLog('8');
  FMainServer.FServer.Close;
//REAddLog('TKRServerThread.KRExecutePausedFirst END');
end;

{ TKRSrvClient }

constructor TKRSrvClient.CreateClient(AServerThread: TKRServerThread;
  AClient: TKRSocketSrvClient);
begin
  FServerThread:=AServerThread;
  FClient:=AClient;
  FTimeout:=FServerThread.FWaitClientTimeout;
  FLastDataTime:=getTickCount;
  UseProcessMessages:=false;
  inherited Create;
end;

destructor TKRSrvClient.Destroy;
begin
  FClient.Free;
  inherited;
end;

procedure TKRSrvClient.KRExecute;
var
  buf: TKRBuffer;
  n: integer;
begin
  n:=FClient.ReceiveBuf(buf,1024);
  if n>0 then begin
    RecvPack(@buf,n);
    FServerThread.FEvent(FClient,@buf,n);
    if n>0 then begin
      FClient.SendBuf(buf,n);
      SendPack(@buf,n);
    end;
    FLastDataTime:=getTickCount;
  end else begin
    if((getTickCount-FLastDataTime)>FTimeout)then begin
      FClient.Close;
      Pause:=true;
    end;
  end;
end;

procedure TKRSrvClient.KRExecutePausedFirst;
begin
//REAddLog('TKRSrvClient.KRExecutePausedFirst BEGIN');
  FClient.Close;
//REAddLog('1');
  if(assigned(FServerThread.FClientDisconnect))then
    FServerThread.FClientDisconnect(FClient);
//REAddLog('TKRSrvClient.KRExecutePausedFirst END');
end;

procedure TKRSrvClient.RecvPack(APack: PKRBuffer; ALength: integer);
begin
  if Assigned(FServerThread.FMainServer.FOnRecv) then
    FServerThread.FMainServer.FOnRecv(FClient,APack,ALength);
  if Assigned(FServerThread.FMainServer.FOnRecvAsync) then begin
    FPack:=APack;
    FLength:=ALength;
    Synchronize(RecvPackAsync);
  end;
end;

procedure TKRSrvClient.RecvPackAsync;
begin
  if Assigned(FServerThread.FMainServer.FOnRecvAsync) then
    FServerThread.FMainServer.FOnRecvAsync(FClient,FPack,FLength);
end;

procedure TKRSrvClient.SendPack(APack: PKRBuffer; ALength: integer);
begin
  if Assigned(FServerThread.FMainServer.FOnSend) then
    FServerThread.FMainServer.FOnSend(FClient,APack,ALength);
  if Assigned(FServerThread.FMainServer.FOnSendAsync) then begin
    FPack:=APack;
    FLength:=ALength;
    Synchronize(SendPackAsync);
  end;
end;

procedure TKRSrvClient.SendPackAsync;
begin
  if Assigned(FServerThread.FMainServer.FOnSendAsync) then
    FServerThread.FMainServer.FOnSendAsync(FClient,FPack,FLength);
end;

end.
