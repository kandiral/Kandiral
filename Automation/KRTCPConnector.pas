(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTCPConnector                                                            *)
(*  Ver.: 16.01.2021                                                          *)
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
  KRConnectorLng, KRConnector, KRTCPSocketClient, KRTypes;

type
  TKRTCPConnector = class;

  TKRTCPThread = class(TKRConnectorThread)
  private
    FErrCntr: integer;
    FTCPClient: TKRTCPSocketClient;
    tm: cardinal;
  protected
    procedure _exec; override;
    procedure KRExecuteLast; override;
  public
    constructor CreateTh(AConnector: TKRConnector; ATCPClient: TKRTCPSocketClient);
  end;

  TKRTCPConnector = class(TKRConnector)
  private
    FTCPClient: TKRTCPSocketClient;
    FPort: Word;
    FIP: String;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    FConnectTimeout: Cardinal;
  protected
    function GetConnected: boolean;override;
    procedure CreateThread; override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property IP: String read FIP write FIP;
    property Port: Word read FPort write FPort default 502;
    property ConnectTimeout: Cardinal read FConnectTimeout write FConnectTimeout default 3000;
    property WriteTimeout: Cardinal read FWriteTimeout write FWriteTimeout default 500;
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout default 500;
  end;

implementation

{ TKRTCPConnector }

constructor TKRTCPConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctTCPClient;
  FTCPClient:=TKRTCPSocketClient.Create;
  FConnectTimeout:=3000;
  FWriteTimeout:=500;
  FReadTimeout:=500;
  FIP:='192.168.0.1';
  FPort:=502;
end;

procedure TKRTCPConnector.CreateThread;
begin
  FThread:=TKRTCPThread.CreateTh(Self,FTCPClient);
  inherited;
end;

destructor TKRTCPConnector.Destroy;
begin
  Active:=false;
  FTCPClient.Free;
  inherited;
end;

function TKRTCPConnector.GetConnected: boolean;
begin
  Result:=FTCPClient.Connected;
end;

{ TKRTCPThread }

constructor TKRTCPThread.CreateTh(AConnector: TKRConnector; ATCPClient: TKRTCPSocketClient);
begin
  FTCPClient:=ATCPClient;
  inherited CreateTh(AConnector);
end;

procedure TKRTCPThread.KRExecuteLast;
begin
  try
    if FTCPClient.Connected then FTCPClient.Close;
  except end;
  inherited;
end;

procedure TKRTCPThread._exec;
var
  n, rlen: cardinal;
  bf: TKRBuffer;

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
        FTcpClient.Addr:=AnsiString(TKRTCPConnector(FConnector).FIP);
        FTcpClient.Port:=TKRTCPConnector(FConnector).FPort;
        SetStat(cstConnecting);
        FTcpClient.ConnectTimeout:=TKRTCPConnector(FConnector).FConnectTimeout;
        FTcpClient.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FTcpClient.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if FTCPClient.Connected then begin
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
    while not FTCPClient.IsReadBufferEmpty do
      FTCPClient.ReceiveBuf(bf,SizeOf(TKRBuffer));
  except end;

  try
    SendPack;

    if FTCPClient.WaitForWrite(TKRTCPConnector(FConnector).FWriteTimeout) then begin
      n:=FTCPClient.SendBuf(FPk^.Pack^,FPk^.Length);
      if n<Cardinal(FPk^.Length) then _error(ceDataNotSended);
    end else _error(ceRequestTimeout);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FTCPClient.SendBuf(FPack.Pack^,FPack.Length);',E);
      _error(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    tm:=getTickCount;
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try

    if FTCPClient.WaitForRead(TKRTCPConnector(FConnector).FReadTimeout) then begin

      if FPk^.RLen>0 then rlen:=FPk^.RLen else rlen:=SizeOf(TKRBuffer);
      n:=FTCPClient.ReceiveBuf(FPk^.Pack^,rlen);

      if n>0 then begin
        Inc(FCounter);
        FPk^.Length:=n;
        FErrCntr:=0;
        RecvPack;
      end else _error(ceNoResponse);
    end else _error(ceResponseTimeout);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRTCPThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FTCPClient.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  tm:=getTickCount;
  DoCallBack;

end;

end.
