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
  KRThreadQueue, KRCOMPort, KRConnector, KRComPortSets;

type
  TKRCOMPortConnector = class;

  TKRCOMPortThread = class(TKRConnectorThread)
  private
    FErrCntr,FErrCntr2: integer;
    FComPort: TKRCOMPort;
  protected
    procedure _exec; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AConnector: TKRConnector; AComPort: TKRCOMPort; AQueue: TKRThreadQueue;
      AQueueOut: TKRThreadQueue);
  end;

  TKRCOMPortConnector = class(TKRConnector, IKRCOMPortSets)
  private
    FComPort: TKRCOMPort;
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
  end;

implementation

uses Funcs, Math;

{ TKRCOMPortThread }

constructor TKRCOMPortThread.CreateTh(AConnector: TKRConnector;
  AComPort: TKRCOMPort; AQueue, AQueueOut: TKRThreadQueue);
begin
  FWaitRespTime:=35;
  FErrCntr:=0;
  FErrCntr2:=0;
  FComPort:=AComPort;
  inherited CreateTh(AConnector, AQueue, AQueueOut);
end;

procedure TKRCOMPortThread.KRExecutePausedFirst;
begin
  try
    if FComPort.Connected then FComPort.Close;
  except end;
  inherited;
end;

procedure TKRCOMPortThread._exec;
var
  n,l: integer;
  tm,tm0: Cardinal;

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

  procedure _error2(AErr: TKRConnectorError);
  begin
    inc(FErrCntr2);
    FPk^.Error:=AErr;
    FPk^.Length:=0;
    if(FErrCntr2>=7)then begin
      FComPort.Close;
      FLastConnectTime:=getTickCount;
      FErrCntr2:=0;
    end;
  end;

begin
  if not FComPort.Connected then begin
    if((getTickCount-FLastConnectTime)>FReconnectTime)then begin
      try
        SetStat(cstConnecting);
        FComPort.WriteTotalTimeoutConstant:=FWriteTimeout;
        FComPort.WriteTotalTimeoutMultiplier:=Max(FWriteTimeout div 10,10);
        FComPort.Open;
      except on E: Exception do
        FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
          FConnector.Name+'"; procedure _exec; FComPort.Open;',E);
      end;
      FLastConnectTime:=getTickCount;
    end;
    if not FComPort.Connected then begin
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
    n:=FComPort.Write(FPk^.Pack^,FPk^.Length);
    if n<>FPk^.Length then _error2(ceDataNotSended);
  except on E: Exception do begin
       FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FComPort.Write(FPk^.Pack^,FPk^.Length);',E);
      _error2(ceDataNotSended);
    end;
  end;

  if FPk^.Error<>ceOK then begin
    DoCallBack;
    exit;
  end;

  if FPk^.WaitResult then try
    n:=0;
    if(FPk^.RLen>0)then begin
      l:=(FPk.RLen div 2)*3;
      if l>255 then l:=255;
    end else l:=255;

    tm0:=getTickCount+FWaitRespTime;
    while(getTickCount-tm<FWriteTimeout+FReadTimeout)do begin
      if FCOMPort.InputCount>0 then begin
        n:=n+FComPort.Read(Pointer(Integer(FPk^.Pack)+n)^,l-n);
        if(FPk^.RLen>0)and(n>=FPk^.RLen)then break;
        if FPk^.DelimiterLen>0 then begin
          case FPk^.DelimiterLen of
            1: if FPk^.Pack^[n-1]=FPK^.Delimiter then break;
            2: if (Word(FPk^.Pack^[n-2]) shr 8) + FPk^.Pack^[n-1]=FPK^.Delimiter then break;
            3: if (Cardinal(FPk^.Pack^[n-3]) shr 16) + (Word(FPk^.Pack^[n-2]) shr 8) + FPk^.Pack^[n-1]=FPK^.Delimiter then break;
            4: if (Cardinal(FPk^.Pack^[n-4]) shr 24) + (Cardinal(FPk^.Pack^[n-3]) shr 16) + (Word(FPk^.Pack^[n-2]) shr 8) + FPk^.Pack^[n-1]=FPK^.Delimiter then break;
          end;
        end;
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
       FConnector.DoRuntimeError('TKRCOMPortThread[FConnector.Name="'+
         FConnector.Name+'"; procedure _exec; n:=FComPort.ReceiveBuf(FPack.Pack^,255);',E);
      _error(ceNoResponse);
    end;
  end;

  DoCallBack;

end;

{ TKRCOMPortConnector }

constructor TKRCOMPortConnector.Create(AOwner: TComponent);
begin
  inherited;
  FConnectorType:=ctCOMPort;
  FComPort:=TKRCOMPort.Create;
  FThread:=TKRCOMPortThread.CreateTh(Self,FComPort,FQueue,FQueueOut);
end;

destructor TKRCOMPortConnector.Destroy;
begin
  FThread.Terminate;
  while FThread.Working do Application.ProcessMessages;
  FThread.Free;
  if FComPort.Connected then begin
    try
      FComPort.Close;
    finally
      FComPort.Free;
    end;
  end;
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
