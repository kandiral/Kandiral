(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBTConnector                                                             *)
(*  Ver.: 17.03.2020                                                          *)
(*  https://kandiral.ru/delphi/krbtconnector.pas.html                         *)
(*                                                                            *)
(******************************************************************************)
unit KRBTConnector;

interface

uses
  {$IF CompilerVersion >= 23}
    WinApi.Windows, System.Classes, System.SysUtils, Vcl.Forms,
  {$ELSE}
    Windows, Classes, SysUtils, Forms,
  {$IFEND}
  KRConnector, KRBTSocketClient, KRThreadQueue;

type
  TKRBTConnector = class;

  TKRBTThread = class(TKRConnectorThread)
  private
    FErrCntr: integer;
    FBTClient: TKRBTSocketClient;
  protected
    procedure _exec; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AConnector: TKRConnector; ABTClient: TKRBTSocketClient; AQueue: TKRThreadQueue;
      AQueueOut: TKRThreadQueue);
  end;

  TKRBTConnector = class(TKRConnector)
  private
    FBTClient: TKRBTSocketClient;
    FAddr: int64;
    procedure SetAddr(const Value: int64);
  protected
    function GetConnected: boolean;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Addr: int64 read FAddr write SetAddr;
  end;

implementation

uses Funcs;

{ TKRBTThread }

constructor TKRBTThread.CreateTh(AConnector: TKRConnector;
  ABTClient: TKRBTSocketClient; AQueue, AQueueOut: TKRThreadQueue);
begin
  FBTClient:=ABTClient;
  FErrCntr:=0;
  FReadTimeout:=150;
  FWriteTimeout:=150;
  FWaitRespTime:=60;
  inherited CreateTh(AConnector, AQueue, AQueueOut);
end;

procedure TKRBTThread.KRExecutePausedFirst;
begin
  try
    if FBTClient.Connected then FBTClient.Close;
  except end;
  inherited;
end;

procedure TKRBTThread._exec;
var
  n: integer;
  tm,tm0: Cardinal;

  procedure _error(AErr: TKRConnectorError);
  begin
    inc(FErrCntr);
    FPk^.Error:=AErr;
    FPk^.Length:=0;
    if(FConnector.CountErrorsForReconnect>0)and(FErrCntr>=FConnector.CountErrorsForReconnect)then begin
      FBTClient.Close;
      FLastConnectTime:=getTickCount;
      FErrCntr:=0;
    end;
  end;

begin
  if not FBTClient.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        SetStat(cstConnecting);
        FBTClient.ConnectTimeout:=FConnectTimeout;
        FBTClient.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FBTClient.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if not FBTClient.Connected then begin
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
    n:=FBTClient.SendBuf(FPk^.Pack^,FPk^.Length);
    if n<>FPk^.Length then _error(ceDataNotSended);
  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FBTClient.SendBuf(FPack.Pack^,FPack.Length);',E);
      _error(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try
    n:=0;

    tm0:=getTickCount+FWaitRespTime;
    while(getTickCount-tm<FWriteTimeout+FReadTimeout)do begin
      if FBTClient.WaitForData(FWaitRespTime) then begin
        n:=n+FBTClient.ReceiveBuf(Pointer(Integer(FPk^.Pack)+n)^,255-n);
        if(FPk.RLen>0)and(n>=FPk.RLen)then break;
        tm0:=getTickCount+FWaitRespTime;
        continue;
      end;
      if tm0<getTickCount then break;
    end;

    if n>0 then begin
      Inc(FCounter);
      FPk^.Length:=n;
      FErrCntr:=0;
      RecvPack;
    end else
      if(getTickCount-tm>FWriteTimeout+FReadTimeout)then
        _error(ceResponseTimeout)
      else _error(ceNoResponse);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FBTClient.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  DoCallBack;

end;

{ TKRBTConnector }

constructor TKRBTConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctBluetooth;
  FBTClient:=TKRBTSocketClient.Create;
  FThread:=TKRBTThread.CreateTh(Self,FBTClient,FQueue,FQueueOut);
  SetAddr(0);
end;

destructor TKRBTConnector.Destroy;
begin
  FThread.Terminate;
  while FThread.Working do Application.ProcessMessages;
  FThread.Free;
  if FBTClient.Connected then begin
    try
      FBTClient.Close;
    finally
      FBTClient.Free;
    end;
  end;
  inherited;
end;

function TKRBTConnector.GetConnected: boolean;
begin
  Result:=FBTClient.Connected;
end;

procedure TKRBTConnector.SetAddr(const Value: int64);
var
  b: boolean;
begin
  b:=FThread.Pause;
  FThread.Pause:=true;
  if not b then
    while not FThread.Paused do Application.ProcessMessages;
  FBTClient.Close;
  FBTClient.Addr:=Value;
  FAddr:=Value;
  FThread.Pause:=b;
end;

end.
