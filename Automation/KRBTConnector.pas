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
  KRConnectorLng, KRConnector, KRBTSocketClient, KRTypes;

type
  TKRBTConnector = class;

  TKRBTThread = class(TKRConnectorThread)
  private
    FErrCntr: integer;
    FBTClient: TKRBTSocketClient;
    tm: cardinal;
  protected
    procedure _exec; override;
    procedure KRExecuteLast; override;
  public
    constructor CreateTh(AConnector: TKRConnector; ABTClient: TKRBTSocketClient);
  end;

  TKRBTConnector = class(TKRConnector)
  private
    FBTClient: TKRBTSocketClient;
    FAddr: int64;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    FConnectTimeout: Cardinal;
    FWaitRespTime: Cardinal;
  protected
    function GetConnected: boolean;override;
    procedure CreateThread; override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property ConnectTimeout: Cardinal read FConnectTimeout write FConnectTimeout default 3000;
    property WriteTimeout: Cardinal read FWriteTimeout write FWriteTimeout default 500;
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout default 500;
    property WaitRespTime: Cardinal read FWaitRespTime write FWaitRespTime default 35;
    property Addr: int64 read FAddr write FAddr;
  end;

implementation

{ TKRBTConnector }

constructor TKRBTConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctBluetooth;
  FBTClient:=TKRBTSocketClient.Create;
  FConnectTimeout:=3000;
  FWriteTimeout:=500;
  FReadTimeout:=500;
  FWaitRespTime:=35;
  FAddr:=0;
end;

procedure TKRBTConnector.CreateThread;
begin
  FThread:=TKRBTThread.CreateTh(Self,FBTClient);
  inherited;
end;

destructor TKRBTConnector.Destroy;
begin
  Active:=false;
  FBTClient.Free;
  inherited;
end;

function TKRBTConnector.GetConnected: boolean;
begin
  Result:=FBTClient.Connected;
end;

{ TKRBTThread }

constructor TKRBTThread.CreateTh(AConnector: TKRConnector; ABTClient: TKRBTSocketClient);
begin
  FBTClient:=ABTClient;
  inherited CreateTh(AConnector);
end;

procedure TKRBTThread.KRExecuteLast;
begin
  try
    if FBTClient.Connected then FBTClient.Close;
  except end;
  inherited;
end;

procedure TKRBTThread._exec;
var
  n, rlen, timeout, startTm: cardinal;
  bf: TKRBuffer;

  procedure _error(AErr: TKRConnectorError);
  begin
    inc(FErrCntr);
    FPk^.Error:=AErr;
    FPk^.Length:=0;
    if(FConnector.CountErrorsForReconnect>0)and(FErrCntr>=FConnector.CountErrorsForReconnect)then begin
      FBTClient.Close;
      SetStat(cstDisconnected);
      FLastConnectTime:=getTickCount;
      FErrCntr:=0;
    end;
  end;

begin
  if not FBTClient.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        FBTClient.Addr:=TKRBTConnector(FConnector).FAddr;
        SetStat(cstConnecting);
        FBTClient.ConnectTimeout:=TKRBTConnector(FConnector).FConnectTimeout;
        FBTClient.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FBTClient.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if FBTClient.Connected then begin
      SetStat(cstConnected);
      tm:=getTickCount;
    end else begin
      SetStat(cstWaitReconnecting);
      FPk^.Error:=ceNotConnection;
      FPk^.Length:=0;
      DoCallBack;
      exit;
    end;
    FErrCntr:=0;
  end;

  FPk^.Error:=ceOK;

  if FInterval>0 then begin
    tm:=getTickCount-tm;
    if tm<FInterval then sleep(FInterval-tm);
  end;

  try
    while not FBTClient.IsReadBufferEmpty do
      FBTClient.ReceiveBuf(bf,SizeOf(TKRBuffer));
  except end;

  try
    SendPack;

    if FBTClient.WaitForWrite(TKRBTConnector(FConnector).FWriteTimeout) then begin
      n:=FBTClient.SendBuf(FPk^.Pack^,FPk^.Length);
      if n<Cardinal(FPk^.Length) then _error(ceDataNotSended);
    end else _error(ceRequestTimeout);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FBTClient.SendBuf(FPack.Pack^,FPack.Length);',E);
      _error(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    tm:=getTickCount;
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try

    n:=0;
    if FPk^.RLen>0 then rlen:=FPk^.RLen else rlen:=SizeOf(TKRBuffer);

    timeout:=TKRBTConnector(FConnector).FReadTimeout;
    startTm:=getTickCount;

    while FBTClient.WaitForRead(timeout) do begin
      inc(n,FBTClient.ReceiveBuf(Pointer(Integer(FPk^.Pack)+n)^,rlen-n));
      if(FPk^.RLen>0)and(n>=FPk^.RLen)then break;
      if(getTickCount-startTm)>TKRBTConnector(FConnector).FReadTimeout then break;
      timeout:=TKRBTConnector(FConnector).FWaitRespTime;
    end;

    if n>0 then begin
      Inc(FCounter);
      FPk^.Length:=n;
      FErrCntr:=0;
      RecvPack;
    end else _error(ceNoResponse);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRBTThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FBTClient.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  tm:=getTickCount;
  DoCallBack;

end;

end.
