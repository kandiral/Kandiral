(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTCPConnector                                                            *)
(*  Ver.: 14.07.2020                                                          *)
(*  https://kandiral.ru/delphi/krtcpconnector.pas.html                        *)
(*                                                                            *)
(******************************************************************************)
unit KRTCPConnector;

interface

uses
  {$IF CompilerVersion >= 23}
    WinApi.Windows, System.Classes, Vcl.Forms, System.SysUtils,
  {$ELSE}
    Windows, Classes, Forms, SysUtils,
  {$IFEND}
  KRConnector, KRTCPSocketClient, KRThreadQueue;

type
  TKRTCPConnector = class;

  TKRTCPThread = class(TKRConnectorThread)
  private
    FErrCntr: integer;
    FTCPClient: TKRTCPSocketClient;
  protected
    procedure _exec; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AConnector: TKRConnector; ATCPClient: TKRTCPSocketClient; AQueue: TKRThreadQueue;
      AQueueOut: TKRThreadQueue);
  end;

  TKRTCPConnector = class(TKRConnector)
  private
    FTCPClient: TKRTCPSocketClient;
    FPort: Word;
    FIP: String;
    procedure SetIP(const Value: String);
    procedure SetPort(const Value: Word);
  protected
    function GetConnected: boolean;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property IP: String read FIP write SetIP;
    property Port: Word read FPort write SetPort;
  end;

implementation

uses Funcs;

{ TKRTCPConnector }

constructor TKRTCPConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctTCPClient;
  FTCPClient:=TKRTCPSocketClient.Create;
  FThread:=TKRTCPThread.CreateTh(Self,FTCPClient,FQueue,FQueueOut);
  SetIP('10.0.6.11');
  SetPort(502);
end;

destructor TKRTCPConnector.Destroy;
begin
  FThread.Terminate;
  while FThread.Working do Application.ProcessMessages;
  FThread.Free;
  if FTCPClient.Connected then begin
    try
      FTCPClient.Close;
    finally
      FTCPClient.Free;
    end;
  end;
  inherited;
end;

function TKRTCPConnector.GetConnected: boolean;
begin
  Result:=FTCPClient.Connected;
end;

procedure TKRTCPConnector.SetIP(const Value: String);
var
  b: boolean;
begin
  b:=FThread.Pause;
  FThread.Pause:=true;
  if not b then
    while not FThread.Paused do Application.ProcessMessages;
  FTCPClient.Close;
  FTCPClient.Addr:=AnsiString(Value);
  FIP:=Value;
  FThread.Pause:=b;
end;

procedure TKRTCPConnector.SetPort(const Value: Word);
var
  b: boolean;
begin
  b:=FThread.Pause;
  FThread.Pause:=true;
  if not b then
    while not FThread.Paused do Application.ProcessMessages;
  FTCPClient.Close;
  FTCPClient.Port:=Value;
  FPort:=Value;
  FThread.Pause:=b;
end;

{ TKRTCPThread }

constructor TKRTCPThread.CreateTh(AConnector: TKRConnector;
  ATCPClient: TKRTCPSocketClient; AQueue, AQueueOut: TKRThreadQueue);
begin
  FWaitRespTime:=5;
  FTCPClient:=ATCPClient;
  FErrCntr:=0;
  inherited CreateTh(AConnector, AQueue, AQueueOut);
end;

procedure TKRTCPThread.KRExecutePausedFirst;
begin
  try
    if FTCPClient.Connected then FTCPClient.Close;
  except end;
  inherited;
end;

procedure TKRTCPThread._exec;
var
  n: integer;
  tm{,tm0}: Cardinal;

  procedure _error(AErr: TKRConnectorError);
  begin
    inc(FErrCntr);
    FPk^.Error:=AErr;
    FPk^.Length:=0;
    if(FConnector.CountErrorsForReconnect>0)and(FErrCntr>=FConnector.CountErrorsForReconnect)then begin
      FTCPClient.Close;
      SetStat(cstDisconnected);
      FLastConnectTime:=getTickCount;
      FErrCntr:=0;
    end;
  end;

begin
  if not FTCPClient.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        SetStat(cstConnecting);
        FTcpClient.ConnectTimeout:=FConnectTimeout;
        SetStat(cstConnecting);
        FTcpClient.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FTcpClient.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if not FTCPClient.Connected then begin
      SetStat(cstWaitReconnecting);
      FPk^.Error:=ceNotConnection;
      FPk^.Length:=0;
      DoCallBack;
      exit;
    end else SetStat(cstConnected);
    FErrCntr:=0;
  end;

  tm:=0;
  FPk^.Error:=ceOK;

  try
    SendPack;
    tm:=getTickCount;
    n:=FTCPClient.SendBuf(FPk^.Pack^,FPk^.Length);
    if n<>FPk^.Length then _error(ceDataNotSended);
  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FTCPClient.SendBuf(FPack.Pack^,FPack.Length);',E);
      _error(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try
    FTCPClient.WaitForData(FReadTimeout);
    n:=FTCPClient.ReceiveBuf(FPk^.Pack^,255);

    if n>0 then begin
      Inc(FCounter);
      FPk^.Length:=n;
      FErrCntr:=0;
      RecvPack;
    end else begin
      if((getTickCount-tm)>FWriteTimeout+FReadTimeout)then
        _error(ceResponseTimeout)
      else _error(ceNoResponse);
    end

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FTCPClient.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  DoCallBack;

end;

end.
