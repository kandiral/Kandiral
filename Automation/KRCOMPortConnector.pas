(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCOMPortConnector                                                        *)
(*  Ver.: 17.03.2020                                                          *)
(*  https://kandiral.ru/delphi/krcomportconnector.pas.html                    *)
(*                                                                            *)
(******************************************************************************)
unit KRCOMPortConnector;

interface

uses
  {$IF CompilerVersion >= 23}
    WinApi.Windows, System.Classes, System.SysUtils, Vcl.Forms,
  {$ELSE}
    Windows, Classes, SysUtils, Forms,
  {$IFEND}
  KRCOMPort, KRConnector, KRConnectorLng, KRComPortSets, KRTypes;

type
  TKRCOMPortConnector = class;

  TKRCOMPortThread = class(TKRConnectorThread)
  private
    FErrCntr: integer;
    FComPort: TKRCOMPort;
    FOverlapped: TOverlapped;
    tm: cardinal;
  protected
    procedure _exec; override;
    procedure KRExecuteLast; override;
  public
    constructor CreateTh(AConnector: TKRConnector; AComPort: TKRCOMPort);
  end;

  TKRCOMPortConnector = class(TKRConnector, IKRCOMPortSets)
  private
    FComPort: TKRCOMPort;
    FReadTotalTimeoutMultiplier: Cardinal;
    FReadTotalTimeoutConstant: Cardinal;
    FWriteTotalTimeoutMultiplier: Cardinal;
    FWriteTotalTimeoutConstant: Cardinal;
    FReadIntervalTimeout: Cardinal;
    function GetBaudRate: integer;
    function GetDataBits: integer;
    function GetFlowControl: TKRFlowControl;
    function GetParity: integer;
    function GetPort: String;
    function GetStopBits: Integer;
    procedure SetBaudRate(const Value: integer);
    procedure SetDataBits(const Value: integer);
    procedure SetFlowControl(const Value: TKRFlowControl);
    procedure SetParity(const Value: integer);
    procedure SetPort(const Value: String);
    procedure SetStopBits(const Value: Integer);
  protected
    function GetConnected: boolean;override;
    procedure CreateThread; override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Port: String read GetPort write SetPort;
    property Parity: integer read GetParity write SetParity;
    property StopBits: integer read GetStopBits write SetStopBits;
    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property DataBits: integer read GetDataBits write SetDataBits;
    property FlowControl: TKRFlowControl read GetFlowControl write SetFlowControl;

    property ReadTotalTimeoutMultiplier: Cardinal read FReadTotalTimeoutMultiplier
      write FReadTotalTimeoutMultiplier default 5;
    property ReadIntervalTimeout: Cardinal read FReadIntervalTimeout
      write FReadIntervalTimeout default 1;
    property ReadTotalTimeoutConstant: Cardinal read FReadTotalTimeoutConstant
      write FReadTotalTimeoutConstant default 15;
    property WriteTotalTimeoutMultiplier: Cardinal read FWriteTotalTimeoutMultiplier
      write FWriteTotalTimeoutMultiplier default 2;
    property WriteTotalTimeoutConstant: Cardinal read FWriteTotalTimeoutConstant
      write FWriteTotalTimeoutConstant default 1;
  end;

implementation

{ TKRCOMPortThread }

constructor TKRCOMPortThread.CreateTh(AConnector: TKRConnector;
  AComPort: TKRCOMPort);
begin
  FComPort:=AComPort;
  FOverlapped.hEvent := CreateEvent(nil, True, True, nil);
  inherited CreateTh(AConnector);
end;

procedure TKRCOMPortThread.KRExecuteLast;
begin
  try
    FComPort.Close;
  except end;
  CloseHandle(FOverlapped.hEvent);
  inherited;
end;

procedure TKRCOMPortThread._exec;
var
  n, rlen: Cardinal;
  Signaled: DWORD;

  procedure _error(AErr: TKRConnectorError);
  begin
    inc(FErrCntr);
    FPk^.Error:=AErr;
    FPk^.Length:=0;
    if(FConnector.CountErrorsForReconnect>0)and(FErrCntr>=FConnector.CountErrorsForReconnect)then begin
      FComPort.Close;
      FLastConnectTime:=getTickCount;
      FErrCntr:=0;
    end;
  end;

begin
  if not FComPort.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        FComPort.ReadTotalTimeoutMultiplier := TKRCOMPortConnector(FConnector).FReadTotalTimeoutMultiplier;
        FComPort.ReadIntervalTimeout := TKRCOMPortConnector(FConnector).FReadIntervalTimeout;
        FComPort.ReadTotalTimeoutConstant := TKRCOMPortConnector(FConnector).FReadTotalTimeoutConstant;
        FComPort.WriteTotalTimeoutMultiplier := TKRCOMPortConnector(FConnector).FWriteTotalTimeoutMultiplier;
        FComPort.WriteTotalTimeoutConstant := TKRCOMPortConnector(FConnector).FWriteTotalTimeoutConstant;
        SetStat(cstConnecting);
        FComPort.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FComPort.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if FComPort.Connected then begin
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
    SendPack;

    FOverlapped.Internal:=0;
    FOverlapped.InternalHigh:=0;
    FOverlapped.Offset:=0;
    FOverlapped.OffsetHigh:=0;
    ResetEvent(FOverlapped.hEvent);
    if not(WriteFile(FComPort.Handle, FPk^.Pack^, FPk^.Length, n, @FOverlapped) or
        (GetLastError = ERROR_IO_PENDING))then _error(ceDataNotSended) else begin
      Signaled := WaitForSingleObject(FOverlapped.hEvent, INFINITE);
      if not((Signaled = WAIT_OBJECT_0) and
        (GetOverlappedResult(FComPort.Handle, FOverlapped, n, False))) then _error(ceDataNotSended);
    end;

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FComPort.Write(FPk^.Pack^,FPk^.Length);',E);
      _error(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    tm:=getTickCount;
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try

    FOverlapped.Internal:=0;
    FOverlapped.InternalHigh:=0;
    FOverlapped.Offset:=0;
    FOverlapped.OffsetHigh:=0;
    ResetEvent(FOverlapped.hEvent);

    if FPk^.RLen>0 then rlen:=FPk^.RLen else rlen:=SizeOf(TKRBuffer);

    if(ReadFile(FComPort.Handle, FPk^.Pack^, rlen, n, @FOverlapped)and
        (GetLastError<>ERROR_IO_PENDING))then begin

      if n>0 then begin
        Inc(FCounter);
        FPk^.Length:=n;
        FErrCntr:=0;
        RecvPack;
      end else _error(ceNoResponse);

    end else _error(ceResponseTimeout);

  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FComPort.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  tm:=getTickCount;
  DoCallBack;
end;

{ TKRCOMPortConnector }

constructor TKRCOMPortConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctCOMPort;
  FComPort:=TKRCOMPort.Create;
  FReadTotalTimeoutMultiplier := 5;
  FReadIntervalTimeout := 1;
  FReadTotalTimeoutConstant := 15;
  FWriteTotalTimeoutMultiplier := 2;
  FWriteTotalTimeoutConstant := 1;
end;

procedure TKRCOMPortConnector.CreateThread;
begin
  FThread:=TKRCOMPortThread.CreateTh(Self,FComPort);
  inherited;
end;

destructor TKRCOMPortConnector.Destroy;
begin
  Active:=false;
  FComPort.Free;
  inherited;
end;

function TKRCOMPortConnector.GetBaudRate: integer;
begin
  Result:=FComPort.BaudRate;
end;

function TKRCOMPortConnector.GetConnected: boolean;
begin
  Result:=FComPort.Connected;
end;

function TKRCOMPortConnector.GetDataBits: integer;
begin
  Result:=FComPort.DataBits;
end;

function TKRCOMPortConnector.GetFlowControl: TKRFlowControl;
begin
  Result:=FComPort.FlowControl
end;

function TKRCOMPortConnector.GetParity: integer;
begin
  Result:=FComPort.Parity;
end;

function TKRCOMPortConnector.GetPort: String;
begin
  Result:=FComPort.Port;
end;

function TKRCOMPortConnector.GetStopBits: integer;
begin
  Result:=FComPort.StopBits;
end;

procedure TKRCOMPortConnector.SetBaudRate(const Value: integer);
begin
  FComPort.BaudRate:=Value;
end;

procedure TKRCOMPortConnector.SetDataBits(const Value: integer);
begin
  FComPort.DataBits:=Value;
end;

procedure TKRCOMPortConnector.SetFlowControl(const Value: TKRFlowControl);
begin
  FComPort.FlowControl:=Value;
end;

procedure TKRCOMPortConnector.SetParity(const Value: integer);
begin
  FComPort.Parity:=Value;
end;

procedure TKRCOMPortConnector.SetPort(const Value: String);
begin
  FComPort.Port:=Value;
end;

procedure TKRCOMPortConnector.SetStopBits(const Value: integer);
begin
  FComPort.StopBits:=Value;
end;

end.
