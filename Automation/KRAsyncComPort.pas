(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRAsyncComPort                                                            *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRAsyncComPort;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  {$ELSE}
    Windows, Messages, SysUtils, Classes,
  {$IFEND}
  KRCOMPort, KRCOMPortSets;

type
  TKRACPDTRFlowControl = (dtrDisable, dtrEnable, dtrHandshake);
  TKRACPRTSFlowControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);
  TKRACPEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR,
    evError, evRLSD, evRx80Full);
  TKRACPEvents = set of TKRACPEvent;
  TKRACPSignal = (csCTS, csDSR, csRing, csRLSD);
  TKRACPSignals = set of TKRACPSignal;
  TKRACPError = (ceFrame, ceRxParity, ceOverrun, ceBreak, ceIO, ceMode, ceRxOver,
    ceTxFull);
  TKRACPErrors = set of TKRACPError;
  TKRACPRxCharEvent = procedure(Sender: TObject; Count: Integer) of object;
  TKRACPRxBufEvent = procedure(Sender: TObject; const Buffer;
    Count: Integer) of object;
  TKRACPErrorEvent = procedure(Sender: TObject; Errors: TKRACPErrors) of object;
  TKRACPSignalEvent = procedure(Sender: TObject; OnOff: Boolean) of object;

  TKRACPOperationKind = (okWrite, okRead);
  TKRACPAsync = record
    Overlapped: TOverlapped;
    Kind: TKRACPOperationKind;
    Data: Pointer;
    Size: Integer;
  end;
  PKRACPAsync = ^TKRACPAsync;

  TKRAsyncComPort = class;

  TKRACPThread = class(TThread)
  private
    FComPort: TKRAsyncComPort;
    FStopEvent: THandle;
    FEvents: TKRACPEvents;
  protected
    procedure DoEvents;
    procedure Execute; override;
    procedure Stop;
  public
    constructor Create(AComPort: TKRAsyncComPort);
    destructor Destroy; override;
  end;

  TKRACPTimeouts = class(TPersistent)
  private
    FComPort: TKRAsyncComPort;
    FReadInterval: Integer;
    FReadTotalM: Integer;
    FReadTotalC: Integer;
    FWriteTotalM: Integer;
    FWriteTotalC: Integer;
    procedure SetComPort(const AComPort: TKRAsyncComPort);
    procedure SetReadInterval(const Value: Integer);
    procedure SetReadTotalM(const Value: Integer);
    procedure SetReadTotalC(const Value: Integer);
    procedure SetWriteTotalM(const Value: Integer);
    procedure SetWriteTotalC(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TKRAsyncComPort read FComPort;
  published
    property ReadInterval: Integer read FReadInterval write SetReadInterval default -1;
    property ReadTotalMultiplier: Integer read FReadTotalM write SetReadTotalM default 0;
    property ReadTotalConstant: Integer read FReadTotalC write SetReadTotalC default 0;
    property WriteTotalMultiplier: Integer
      read FWriteTotalM write SetWriteTotalM default 100;
    property WriteTotalConstant: Integer
      read FWriteTotalC write SetWriteTotalC default 1000;
  end;

  TKRACPFlowControl = class(TPersistent)
  private
    FComPort: TKRAsyncComPort;
    FOutCTSFlow: Boolean;
    FOutDSRFlow: Boolean;
    FControlDTR: TKRACPDTRFlowControl;
    FControlRTS: TKRACPRTSFlowControl;
    FXonXoffOut: Boolean;
    FXonXoffIn:  Boolean;
    FDSRSensitivity: Boolean;
    FTxContinueOnXoff: Boolean;
    FXonChar: AnsiChar;
    FXoffChar: AnsiChar;
    procedure SetComPort(const AComPort: TKRAsyncComPort);
    procedure SetOutCTSFlow(const Value: Boolean);
    procedure SetOutDSRFlow(const Value: Boolean);
    procedure SetControlDTR(const Value: TKRACPDTRFlowControl);
    procedure SetControlRTS(const Value: TKRACPRTSFlowControl);
    procedure SetXonXoffOut(const Value: Boolean);
    procedure SetXonXoffIn(const Value: Boolean);
    procedure SetDSRSensitivity(const Value: Boolean);
    procedure SetTxContinueOnXoff(const Value: Boolean);
    procedure SetXonChar(const Value: AnsiChar);
    procedure SetXoffChar(const Value: AnsiChar);
    procedure SetFlowControl(const Value: TKRFlowControl);
    function GetFlowControl: TKRFlowControl;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TKRAsyncComPort read FComPort;
  published
    property FlowControl: TKRFlowControl read GetFlowControl write SetFlowControl stored False;
    property OutCTSFlow: Boolean read FOutCTSFlow write SetOutCTSFlow;
    property OutDSRFlow: Boolean read FOutDSRFlow write SetOutDSRFlow;
    property ControlDTR: TKRACPDTRFlowControl read FControlDTR write SetControlDTR;
    property ControlRTS: TKRACPRTSFlowControl read FControlRTS write SetControlRTS;
    property XonXoffOut: Boolean read FXonXoffOut write SetXonXoffOut;
    property XonXoffIn:  Boolean read FXonXoffIn write SetXonXoffIn;
    property DSRSensitivity: Boolean
      read FDSRSensitivity write SetDSRSensitivity default False;
    property TxContinueOnXoff: Boolean
      read FTxContinueOnXoff write SetTxContinueOnXoff default False;
    property XonChar: AnsiChar read FXonChar write SetXonChar default #17;
    property XoffChar: AnsiChar read FXoffChar write SetXoffChar default #19;
  end;

  TKRACPParity = class(TPersistent)
  private
    FComPort: TKRAsyncComPort;
    FBits: Integer;
    FCheck: Boolean;
    FReplace: Boolean;
    FReplaceChar: AnsiChar;
    procedure SetComPort(const AComPort: TKRAsyncComPort);
    procedure SetBits(const Value: Integer);
    procedure SetCheck(const Value: Boolean);
    procedure SetReplace(const Value: Boolean);
    procedure SetReplaceChar(const Value: AnsiChar);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TKRAsyncComPort read FComPort;
  published
    property Bits: Integer read FBits write SetBits;
    property Check: Boolean read FCheck write SetCheck default False;
    property Replace: Boolean read FReplace write SetReplace default False;
    property ReplaceChar: AnsiChar read FReplaceChar write SetReplaceChar default #0;
  end;

  TKRACPBuffer = class(TPersistent)
  private
    FComPort: TKRAsyncComPort;
    FInputSize: Integer;
    FOutputSize: Integer;
    procedure SetComPort(const AComPort: TKRAsyncComPort);
    procedure SetInputSize(const Value: Integer);
    procedure SetOutputSize(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TKRAsyncComPort read FComPort;
  published
    property InputSize: Integer read FInputSize write SetInputSize default 1024;
    property OutputSize: Integer read FOutputSize write SetOutputSize default 1024;
  end;

  TKRAsyncComPort = class(TComponent, IKRCOMPortSets)
  private
    FEventThread: TKRACPThread;
    FThreadCreated: Boolean;
    FHandle: THandle;
    FUpdateCount: Integer;
    FTriggersOnRxChar: Boolean;
    FEventThreadPriority: TThreadPriority;
    FConnected: Boolean;
    FDiscardNull: Boolean;
    FEventChar: AnsiChar;
    FEvents: TKRACPEvents;
    FBuffer: TKRACPBuffer;
    FParity: TKRACPParity;
    FTimeouts: TKRACPTimeouts;
    FFlowControl: TKRACPFlowControl;
    FOnRxChar: TKRACPRxCharEvent;
    FOnRxBuf: TKRACPRxBufEvent;
    FOnTxEmpty: TNotifyEvent;
    FOnBreak: TNotifyEvent;
    FOnRing: TNotifyEvent;
    FOnCTSChange: TKRACPSignalEvent;
    FOnDSRChange: TKRACPSignalEvent;
    FOnRLSDChange: TKRACPSignalEvent;
    FOnError: TKRACPErrorEvent;
    FOnRxFlag: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnRx80Full: TNotifyEvent;
    FBaudRate: integer;
    FStopBits: integer;
    FDataBits: integer;
    FPort: String;
    function GetTriggersOnRxChar: Boolean;
    procedure SetTriggersOnRxChar(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    procedure SetPort(const Value: String);
    procedure SetStopBits(const Value: integer);
    procedure SetDataBits(const Value: Integer);
    procedure SetDiscardNull(const Value: Boolean);
    procedure SetEventChar(const Value: AnsiChar);
    procedure SetEventThreadPriority(const Value: TThreadPriority);
    procedure SetParity(const Value: Integer);
    function GetParity: Integer;
    procedure SetTimeouts(const Value: TKRACPTimeouts);
    procedure SetBuffer(const Value: TKRACPBuffer);
    procedure SetFlowControl(const Value: TKRFlowControl);
    procedure CheckSignals(Open: Boolean);
    procedure CallRxChar;
    procedure CallTxEmpty;
    procedure CallBreak;
    procedure CallRing;
    procedure CallRxFlag;
    procedure CallCTSChange;
    procedure CallDSRChange;
    procedure CallError;
    procedure CallRLSDChange;
    procedure CallRx80Full;
    procedure SetBaudRate(const Value: Integer);
    function GetBaudRate: integer;
    function GetStopBits: integer;
    function GetDataBits: Integer;
    function GetPort: String;
    procedure SetParityObj(const Value: TKRACPParity);
    procedure SetFlowControlObj(const Value: TKRACPFlowControl);
    function GetFlowControl: TKRFlowControl;
  protected
    procedure Loaded; override;
    procedure DoRxChar(Count: Integer); dynamic;
    procedure DoRxBuf(const Buffer; Count: Integer); dynamic;
    procedure DoTxEmpty; dynamic;
    procedure DoBreak; dynamic;
    procedure DoRing; dynamic;
    procedure DoRxFlag; dynamic;
    procedure DoCTSChange(OnOff: Boolean); dynamic;
    procedure DoDSRChange(OnOff: Boolean); dynamic;
    procedure DoError(Errors: TKRACPErrors); dynamic;
    procedure DoRLSDChange(OnOff: Boolean); dynamic;
    procedure DoRx80Full; dynamic;
    procedure DestroyHandle; virtual;
    procedure ApplyDCB; dynamic;
    procedure ApplyTimeouts; dynamic;
    procedure ApplyBuffer; dynamic;
    procedure SetupComPort; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Open;
    procedure Close;
    function InputCount: Integer;
    function OutputCount: Integer;
    function Signals: TKRACPSignals;
    function StateFlags: TComStateFlags;
    procedure SetDTR(OnOff: Boolean);
    procedure SetRTS(OnOff: Boolean);
    procedure SetXonXoff(OnOff: Boolean);
    procedure SetBreak(OnOff: Boolean);
    procedure ClearBuffer(Input, Output: Boolean);
    function LastErrors: TKRACPErrors;
    function Write(const Buffer; Count: Integer): Integer;
    function WriteStr(const Str: AnsiString): Integer;
    function Read(var Buffer; Count: Integer): Integer;
    function ReadStr(var Str: AnsiString; Count: Integer): Integer;
    function WriteAsync(const Buffer; Count: Integer;
      var AsyncPtr: PKRACPAsync): Integer;
    function WriteStrAsync(const Str: AnsiString; var AsyncPtr: PKRACPAsync): Integer;
    function ReadAsync(var Buffer; Count: Integer;
      var AsyncPtr: PKRACPAsync): Integer;
    function ReadStrAsync(var Str: AnsiString; Count: Integer;
      var AsyncPtr: PKRACPAsync): Integer;
    function WaitForAsync(var AsyncPtr: PKRACPAsync): Integer;
    function IsAsyncCompleted(AsyncPtr: PKRACPAsync): Boolean;
    procedure WaitForEvent(var Events: TKRACPEvents; StopEvent: THandle;
      Timeout: Integer);
    procedure AbortAllAsync;
    procedure TransmitChar(Ch: AnsiChar);
    property Handle: THandle read FHandle;
    property TriggersOnRxChar: Boolean
      read GetTriggersOnRxChar write SetTriggersOnRxChar;
    property EventThreadPriority: TThreadPriority
      read FEventThreadPriority write SetEventThreadPriority;
  published
    property Connected: Boolean read FConnected write SetConnected default False;
    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property Port: String read GetPort write SetPort;
    property Parity: TKRACPParity read FParity write SetParityObj;
    property StopBits: integer read GetStopBits write SetStopBits;
    property DataBits: Integer read GetDataBits write SetDataBits;
    property DiscardNull: Boolean read FDiscardNull write SetDiscardNull default False;
    property EventChar: AnsiChar read FEventChar write SetEventChar default #0;
    property Events: TKRACPEvents read FEvents write FEvents;
    property Buffer: TKRACPBuffer read FBuffer write SetBuffer;
    property FlowControl: TKRACPFlowControl
      read FFlowControl write SetFlowControlObj;
    property Timeouts: TKRACPTimeouts read FTimeouts write SetTimeouts;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforeClose: TNotifyEvent
      read FOnBeforeClose write FOnBeforeClose;
    property OnRxChar: TKRACPRxCharEvent read FOnRxChar write FOnRxChar;
    property OnRxBuf: TKRACPRxBufEvent read FOnRxBuf write FOnRxBuf;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnBreak: TNotifyEvent read FOnBreak write FOnBreak;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnCTSChange: TKRACPSignalEvent read FOnCTSChange write FOnCTSChange;
    property OnDSRChange: TKRACPSignalEvent read FOnDSRChange write FOnDSRChange;
    property OnRLSDChange: TKRACPSignalEvent
      read FOnRLSDChange write FOnRLSDChange;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnError: TKRACPErrorEvent read FOnError write FOnError;
    property OnRx80Full: TNotifyEvent read FOnRx80Full write FOnRx80Full;
  end;

  EKRAsyncComPort = class(Exception)
  private
    FWinCode: Integer;
    FCode: Integer;
  public
    constructor Create(ACode: Integer; AWinCode: Integer);
    constructor CreateNoWinCode(ACode: Integer);
    property WinCode: Integer read FWinCode write FWinCode;
    property Code: Integer read FCode write FCode;
  end;

procedure KRACPInitAsync(var AsyncPtr: PKRACPAsync);
procedure KRACPDoneAsync(var AsyncPtr: PKRACPAsync);

const
  WaitInfinite = Integer(INFINITE);

  CError_OpenFailed      = 1;
  CError_WriteFailed     = 2;
  CError_ReadFailed      = 3;
  CError_InvalidAsync    = 4;
  CError_PurgeFailed     = 5;
  CError_AsyncCheck      = 6;
  CError_SetStateFailed  = 7;
  CError_TimeoutsFailed  = 8;
  CError_SetupComFailed  = 9;
  CError_ClearComFailed  = 10;
  CError_ModemStatFailed = 11;
  CError_EscapeComFailed = 12;
  CError_TransmitFailed  = 13;
  CError_ConnChangeProp  = 14;
  CError_EnumPortsFailed = 15;
  CError_StoreFailed     = 16;
  CError_LoadFailed      = 17;
  CError_RegFailed       = 18;
  CError_LedStateFailed  = 19;
  CError_ThreadCreated   = 20;
  CError_WaitFailed      = 21;
  CError_HasLink         = 22;
  CError_RegError        = 23;

implementation

uses
  Controls, Forms, WinSpool;

const
  ComErrorMessages: array[1..23] of string =
    ('Unable to open com port',
     'WriteFile function failed',
     'ReadFile function failed',
     'Invalid Async parameter',
     'PurgeComm function failed',
     'Unable to get async status',
     'SetCommState function failed',
     'SetCommTimeouts failed',
     'SetupComm function failed',
     'ClearCommError function failed',
     'GetCommModemStatus function failed',
     'EscapeCommFunction function failed',
     'TransmitCommChar function failed',
     'Cannot set property while connected',
     'EnumPorts function failed',
     'Failed to store settings',
     'Failed to load settings',
     'Link (un)registration failed',
     'Cannot change led state if ComPort is selected',
     'Cannot wait for event if event thread is created',
     'WaitForEvent method failed',
     'A component is linked to OnRxBuf event',
     'Registry error');

  dcb_Binary           = $00000001;
  dcb_Parity           = $00000002;
  dcb_OutxCTSFlow      = $00000004;
  dcb_OutxDSRFlow      = $00000008;
  dcb_DTRControl       = $00000030;
  dcb_DSRSensivity     = $00000040;
  dcb_TxContinueOnXoff = $00000080;
  dcb_OutX             = $00000100;
  dcb_InX              = $00000200;
  dcb_ErrorChar        = $00000400;
  dcb_Null             = $00000800;
  dcb_RTSControl       = $00003000;
  dcb_AbortOnError     = $00004000;

function EventsToInt(const Events: TKRACPEvents): Integer;
begin
  Result := 0;
  if evRxChar in Events then
    Result := Result or EV_RXCHAR;
  if evRxFlag in Events then
    Result := Result or EV_RXFLAG;
  if evTxEmpty in Events then
    Result := Result or EV_TXEMPTY;
  if evRing in Events then
    Result := Result or EV_RING;
  if evCTS in Events then
    Result := Result or EV_CTS;
  if evDSR in Events then
    Result := Result or EV_DSR;
  if evRLSD in Events then
    Result := Result or EV_RLSD;
  if evError in Events then
    Result := Result or EV_ERR;
  if evBreak in Events then
    Result := Result or EV_BREAK;
  if evRx80Full in Events then
    Result := Result or EV_RX80FULL;
end;

function IntToEvents(Mask: Integer): TKRACPEvents;
begin
  Result := [];
  if (EV_RXCHAR and Mask) <> 0 then
    Result := Result + [evRxChar];
  if (EV_TXEMPTY and Mask) <> 0 then
    Result := Result + [evTxEmpty];
  if (EV_BREAK and Mask) <> 0 then
    Result := Result + [evBreak];
  if (EV_RING and Mask) <> 0 then
    Result := Result + [evRing];
  if (EV_CTS and Mask) <> 0 then
    Result := Result + [evCTS];
  if (EV_DSR and Mask) <> 0 then
    Result := Result + [evDSR];
  if (EV_RXFLAG and Mask) <> 0 then
    Result := Result + [evRxFlag];
  if (EV_RLSD and Mask) <> 0 then
    Result := Result + [evRLSD];
  if (EV_ERR and Mask) <> 0 then
    Result := Result + [evError];
  if (EV_RX80FULL and Mask) <> 0 then
    Result := Result + [evRx80Full];
end;

constructor TKRACPThread.Create(AComPort: TKRAsyncComPort);
begin
  inherited Create(true);
  FStopEvent := CreateEvent(nil, True, False, nil);
  FComPort := AComPort;
  Priority := FComPort.EventThreadPriority;
  SetCommMask(FComPort.Handle, EventsToInt(FComPort.Events));
  Resume;
end;

destructor TKRACPThread.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TKRACPThread.Execute;
var
  EventHandles: array[0..1] of THandle;
  Overlapped: TOverlapped;
  Signaled, BytesTrans, Mask: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  EventHandles[0] := FStopEvent;
  EventHandles[1] := Overlapped.hEvent;
  repeat
    WaitCommEvent(FComPort.Handle, Mask, @Overlapped);
    Signaled := WaitForMultipleObjects(2, @EventHandles, False, INFINITE);
    if(Signaled=WAIT_OBJECT_0+1)and GetOverlappedResult(FComPort.Handle,Overlapped,BytesTrans,False)then begin
      FEvents := IntToEvents(Mask);
      Synchronize(DoEvents);
    end;
  until Signaled <> (WAIT_OBJECT_0 + 1);
  SetCommMask(FComPort.Handle, 0);
  PurgeComm(FComPort.Handle, PURGE_TXCLEAR or PURGE_RXCLEAR);
  CloseHandle(Overlapped.hEvent);
  CloseHandle(FStopEvent);
end;

procedure TKRACPThread.Stop;
begin
  SetEvent(FStopEvent);
  Sleep(0);
end;

procedure TKRACPThread.DoEvents;
begin
  if evError in FEvents then FComPort.CallError;
  if evRxChar in FEvents then FComPort.CallRxChar;
  if evTxEmpty in FEvents then FComPort.CallTxEmpty;
  if evBreak in FEvents then FComPort.CallBreak;
  if evRing in FEvents then FComPort.CallRing;
  if evCTS in FEvents then FComPort.CallCTSChange;
  if evDSR in FEvents then FComPort.CallDSRChange;
  if evRxFlag in FEvents then FComPort.CallRxFlag;
  if evRLSD in FEvents then FComPort.CallRLSDChange;
  if evRx80Full in FEvents then FComPort.CallRx80Full;
end;

constructor TKRACPTimeouts.Create;
begin
  inherited Create;
  FReadInterval := -1;
  FWriteTotalM := 100;
  FWriteTotalC := 1000;
end;

procedure TKRACPTimeouts.AssignTo(Dest: TPersistent);
begin
  if Dest is TKRACPTimeouts then begin
    with TKRACPTimeouts(Dest) do begin
      FReadInterval := Self.ReadInterval;
      FReadTotalM   := Self.ReadTotalMultiplier;
      FReadTotalC   := Self.ReadTotalConstant;
      FWriteTotalM  := Self.WriteTotalMultiplier;
      FWriteTotalC  := Self.WriteTotalConstant;
    end
  end else inherited AssignTo(Dest);
end;

procedure TKRACPTimeouts.SetComPort(const AComPort: TKRAsyncComPort);
begin
  FComPort := AComPort;
end;

procedure TKRACPTimeouts.SetReadInterval(const Value: Integer);
begin
  if Value <> FReadInterval then begin
    FReadInterval := Value;
    if FComPort <> nil then FComPort.ApplyTimeouts;
  end;
end;

procedure TKRACPTimeouts.SetReadTotalC(const Value: Integer);
begin
  if Value <> FReadTotalC then begin
    FReadTotalC := Value;
    if FComPort <> nil then FComPort.ApplyTimeouts;
  end;
end;

procedure TKRACPTimeouts.SetReadTotalM(const Value: Integer);
begin
  if Value <> FReadTotalM then begin
    FReadTotalM := Value;
    if FComPort <> nil then FComPort.ApplyTimeouts;
  end;
end;

procedure TKRACPTimeouts.SetWriteTotalC(const Value: Integer);
begin
  if Value <> FWriteTotalC then begin
    FWriteTotalC := Value;
    if FComPort <> nil then FComPort.ApplyTimeouts;
  end;
end;

procedure TKRACPTimeouts.SetWriteTotalM(const Value: Integer);
begin
  if Value <> FWriteTotalM then begin
    FWriteTotalM := Value;
    if FComPort <> nil then FComPort.ApplyTimeouts;
  end;
end;

constructor TKRACPFlowControl.Create;
begin
  inherited Create;
  FXonChar := #17;
  FXoffChar := #19;
end;

procedure TKRACPFlowControl.AssignTo(Dest: TPersistent);
begin
  if Dest is TKRACPFlowControl then begin
    with TKRACPFlowControl(Dest) do begin
      FOutCTSFlow       := Self.OutCTSFlow;
      FOutDSRFlow       := Self.OutDSRFlow;
      FControlDTR       := Self.ControlDTR;
      FControlRTS       := Self.ControlRTS;
      FXonXoffOut       := Self.XonXoffOut;
      FXonXoffIn        := Self.XonXoffIn;
      FTxContinueOnXoff := Self.TxContinueOnXoff;
      FDSRSensitivity   := Self.DSRSensitivity;
      FXonChar          := Self.XonChar;
      FXoffChar         := Self.XoffChar;
    end
  end else inherited AssignTo(Dest);
end;

procedure TKRACPFlowControl.SetComPort(const AComPort: TKRAsyncComPort);
begin
  FComPort := AComPort;
end;

procedure TKRACPFlowControl.SetControlDTR(const Value: TKRACPDTRFlowControl);
begin
  if Value <> FControlDTR then begin
    FControlDTR := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetControlRTS(const Value: TKRACPRTSFlowControl);
begin
  if Value <> FControlRTS then begin
    FControlRTS := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetOutCTSFlow(const Value: Boolean);
begin
  if Value <> FOutCTSFlow then begin
    FOutCTSFlow := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetOutDSRFlow(const Value: Boolean);
begin
  if Value <> FOutDSRFlow then begin
    FOutDSRFlow := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetXonXoffIn(const Value: Boolean);
begin
  if Value <> FXonXoffIn then begin
    FXonXoffIn := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetXonXoffOut(const Value: Boolean);
begin
  if Value <> FXonXoffOut then begin
    FXonXoffOut := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetDSRSensitivity(const Value: Boolean);
begin
  if Value <> FDSRSensitivity then begin
    FDSRSensitivity := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetTxContinueOnXoff(const Value: Boolean);
begin
  if Value <> FTxContinueOnXoff then begin
    FTxContinueOnXoff := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetXonChar(const Value: AnsiChar);
begin
  if Value <> FXonChar then begin
    FXonChar := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPFlowControl.SetXoffChar(const Value: AnsiChar);
begin
  if Value <> FXoffChar then begin
    FXoffChar := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

function TKRACPFlowControl.GetFlowControl: TKRFlowControl;
begin
  if(FControlRTS=rtsHandshake)and(FOutCTSFlow)and(not FXonXoffIn)and(not FXonXoffOut)then Result := krfcHardware
  else
    if(FControlRTS=rtsDisable)and(not FOutCTSFlow)and(FXonXoffIn)and(FXonXoffOut)then Result := krfcSoftware
    else
      if(FControlRTS=rtsDisable)and (not FOutCTSFlow)and(not FXonXoffIn)and(not FXonXoffOut)then Result := krfcNone
      else Result := krfcCustom;
end;

procedure TKRACPFlowControl.SetFlowControl(const Value: TKRFlowControl);
begin
  if Value <> krfcCustom then begin
    FControlRTS := rtsDisable;
    FOutCTSFlow := False;
    FXonXoffIn := False;
    FXonXoffOut := False;
    case Value of
      krfcHardware: begin
        FControlRTS := rtsHandshake;
        FOutCTSFlow := True;
      end;
      krfcSoftware: begin
        FXonXoffIn := True;
        FXonXoffOut := True;
      end;
    end;
  end;
  if FComPort <> nil then FComPort.ApplyDCB;
end;

constructor TKRACPParity.Create;
begin
  inherited Create;
  FBits := NOPARITY;
end;

procedure TKRACPParity.AssignTo(Dest: TPersistent);
begin
  if Dest is TKRACPParity then begin
    with TKRACPParity(Dest) do begin
      FBits        := Self.Bits;
      FCheck       := Self.Check;
      FReplace     := Self.Replace;
      FReplaceChar := Self.ReplaceChar;
    end
  end else inherited AssignTo(Dest);
end;

procedure TKRACPParity.SetComPort(const AComPort: TKRAsyncComPort);
begin
  FComPort := AComPort;
end;

procedure TKRACPParity.SetBits(const Value: Integer);
begin
  if Value <> FBits then begin
    FBits := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPParity.SetCheck(const Value: Boolean);
begin
  if Value <> FCheck then begin
    FCheck := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPParity.SetReplace(const Value: Boolean);
begin
  if Value <> FReplace then begin
    FReplace := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

procedure TKRACPParity.SetReplaceChar(const Value: AnsiChar);
begin
  if Value <> FReplaceChar then begin
    FReplaceChar := Value;
    if FComPort <> nil then FComPort.ApplyDCB;
  end;
end;

constructor TKRACPBuffer.Create;
begin
  inherited Create;
  FInputSize := 1024;
  FOutputSize := 1024;
end;

procedure TKRACPBuffer.AssignTo(Dest: TPersistent);
begin
  if Dest is TKRACPBuffer then begin
    with TKRACPBuffer(Dest) do begin
      FOutputSize  := Self.OutputSize;
      FInputSize   := Self.InputSize;
    end
  end else inherited AssignTo(Dest);
end;

procedure TKRACPBuffer.SetComPort(const AComPort: TKRAsyncComPort);
begin
  FComPort := AComPort;
end;

procedure TKRACPBuffer.SetInputSize(const Value: Integer);
begin
  if Value <> FInputSize then begin
    FInputSize := Value;
    if (FInputSize mod 2) = 1 then Dec(FInputSize);
    if FComPort <> nil then FComPort.ApplyBuffer;
  end;
end;

procedure TKRACPBuffer.SetOutputSize(const Value: Integer);
begin
  if Value <> FOutputSize then begin
    FOutputSize := Value;
    if (FOutputSize mod 2) = 1 then Dec(FOutputSize);
    if FComPort <> nil then FComPort.ApplyBuffer;
  end;
end;

constructor TKRAsyncComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentStyle := FComponentStyle - [csInheritable];
  FTriggersOnRxChar := True;
  FEventThreadPriority := tpNormal;
  FBaudRate := 9600;
  FPort := 'COM1';
  FStopBits := ONESTOPBIT;
  FDataBits := 8;
  FEvents := [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak,
             evCTS, evDSR, evError, evRLSD, evRx80Full];
  FHandle := INVALID_HANDLE_VALUE;
  FParity := TKRACPParity.Create;
  FParity.SetComPort(Self);
  FFlowControl := TKRACPFlowControl.Create;
  FFlowControl.SetComPort(Self);
  FTimeouts := TKRACPTimeouts.Create;
  FTimeouts.SetComPort(Self);
  FBuffer := TKRACPBuffer.Create;
  FBuffer.SetComPort(Self);
end;

destructor TKRAsyncComPort.Destroy;
begin
  Close;
  FBuffer.Free;
  FFlowControl.Free;
  FTimeouts.Free;
  FParity.Free;
  inherited Destroy;
end;

procedure TKRAsyncComPort.DestroyHandle;
begin
  if FHandle <> INVALID_HANDLE_VALUE then CloseHandle(FHandle);
end;

procedure TKRAsyncComPort.Loaded;
begin
  inherited Loaded;
  if FConnected and not (csDesigning in ComponentState) then begin
    FConnected := False;
    try
      Open;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TKRAsyncComPort.BeginUpdate;
begin
  FUpdateCount := FUpdateCount + 1;
end;

procedure TKRAsyncComPort.EndUpdate;
begin
  if FUpdateCount > 0 then begin
    FUpdateCount := FUpdateCount - 1;
    if FUpdateCount = 0 then SetupComPort;
  end;
end;

procedure TKRAsyncComPort.Open;
begin
  if not FConnected and not (csDesigning in ComponentState) then begin
    if Assigned(FOnBeforeOpen) then FOnBeforeOpen(Self);

    FHandle := CreateFile(
      PChar('\\.\' + FPort),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_OVERLAPPED,
      0);

    if FHandle = INVALID_HANDLE_VALUE then
      raise EKRAsyncComPort.Create(CError_OpenFailed, GetLastError);

    FConnected := True;
    try
      SetupComPort;
    except
      DestroyHandle;
      FConnected := False;
      raise;
    end;
    if (FEvents = []) then FThreadCreated := False else begin
      FEventThread := TKRACPThread.Create(Self);
      FThreadCreated := True;
    end;
    if Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
    CheckSignals(True);
  end;
end;

procedure TKRAsyncComPort.Close;
begin
  if FConnected and not (csDesigning in ComponentState) then begin
    CheckSignals(False);
    if Assigned(FOnBeforeClose) then FOnBeforeClose(Self);

    AbortAllAsync;
    if FThreadCreated then begin
      FEventThread.Free;
      FThreadCreated := False;
    end;
    DestroyHandle;
    FConnected := False;
    if Assigned(FOnAfterClose) then FOnAfterClose(Self);
  end;
end;

procedure TKRAsyncComPort.ApplyDCB;
const
  CControlRTS: array[TKRACPRTSFlowControl] of Integer =
    (RTS_CONTROL_DISABLE shl 12,
     RTS_CONTROL_ENABLE shl 12,
     RTS_CONTROL_HANDSHAKE shl 12,
     RTS_CONTROL_TOGGLE shl 12);
  CControlDTR: array[TKRACPDTRFlowControl] of Integer =
    (DTR_CONTROL_DISABLE shl 4,
     DTR_CONTROL_ENABLE shl 4,
     DTR_CONTROL_HANDSHAKE shl 4);

var
  DCB: TDCB;

begin
  if FConnected and(FUpdateCount=0)and not((csDesigning in ComponentState)or(csLoading in ComponentState))then begin
    //GetCommState(FHandle,DCB);
    DCB.DCBlength := SizeOf(TDCB);
    DCB.XonLim := FBuffer.InputSize div 4;
    DCB.XoffLim := DCB.XonLim;
    DCB.EvtChar := AnsiChar(FEventChar);
    //DCB.EofChar:=#$C0;
    DCB.Flags := dcb_Binary;
    if FDiscardNull then DCB.Flags := DCB.Flags or dcb_Null;
    with FFlowControl do begin
      DCB.XonChar := XonChar;
      DCB.XoffChar := XoffChar;
      if OutCTSFlow then DCB.Flags := DCB.Flags or dcb_OutxCTSFlow;
      if OutDSRFlow then DCB.Flags := DCB.Flags or dcb_OutxDSRFlow;
      DCB.Flags := DCB.Flags or CControlDTR[ControlDTR]or CControlRTS[ControlRTS];
      if XonXoffOut then DCB.Flags := DCB.Flags or dcb_OutX;
      if XonXoffIn then DCB.Flags := DCB.Flags or dcb_InX;
      if DSRSensitivity then DCB.Flags := DCB.Flags or dcb_DSRSensivity;
      if TxContinueOnXoff then DCB.Flags := DCB.Flags or dcb_TxContinueOnXoff;
    end;
    DCB.Parity := FParity.Bits;
    DCB.StopBits := FStopBits;
    DCB.BaudRate := FBaudRate;
    DCB.ByteSize := FDataBits;

    if FParity.Check then begin
      DCB.Flags := DCB.Flags or dcb_Parity;
      if FParity.Replace then begin
        DCB.Flags := DCB.Flags or dcb_ErrorChar;
        DCB.ErrorChar := AnsiChar(FParity.ReplaceChar);
      end;
    end;
    if not SetCommState(FHandle, DCB) then
      raise EKRAsyncComPort.Create(CError_SetStateFailed, GetLastError);
  end;
end;

procedure TKRAsyncComPort.ApplyTimeouts;
var
  Timeouts: TCommTimeouts;

  function GetTOValue(const Value: Integer): DWORD;
  begin
    if Value = -1 then Result := MAXDWORD
    else Result := Value;
  end;

begin
  if FConnected and(FUpdateCount=0)and not((csDesigning in ComponentState)or(csLoading in ComponentState))then begin
    Timeouts.ReadIntervalTimeout := GetTOValue(FTimeouts.ReadInterval);
    Timeouts.ReadTotalTimeoutMultiplier := GetTOValue(FTimeouts.ReadTotalMultiplier);
    Timeouts.ReadTotalTimeoutConstant := GetTOValue(FTimeouts.ReadTotalConstant);
    Timeouts.WriteTotalTimeoutMultiplier := GetTOValue(FTimeouts.WriteTotalMultiplier);
    Timeouts.WriteTotalTimeoutConstant := GetTOValue(FTimeouts.WriteTotalConstant);
    if not SetCommTimeouts(FHandle, Timeouts) then
      raise EKRAsyncComPort.Create(CError_TimeoutsFailed, GetLastError);
  end;
end;

procedure TKRAsyncComPort.ApplyBuffer;
begin
  if FConnected and(FUpdateCount=0)and not((csDesigning in ComponentState)or(csLoading in ComponentState))then
    if not SetupComm(FHandle, FBuffer.InputSize, FBuffer.OutputSize) then
      raise EKRAsyncComPort.Create(CError_SetupComFailed, GetLastError);
end;

procedure TKRAsyncComPort.SetupComPort;
begin
  ApplyBuffer;
  ApplyDCB;
  ApplyTimeouts;
end;

function TKRAsyncComPort.InputCount: Integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    raise EKRAsyncComPort.Create(CError_ClearComFailed, GetLastError);
  Result := ComStat.cbInQue;
end;

function TKRAsyncComPort.OutputCount: Integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    raise EKRAsyncComPort.Create(CError_ClearComFailed, GetLastError);
  Result := ComStat.cbOutQue;
end;

function TKRAsyncComPort.Signals: TKRACPSignals;
var
  Status: DWORD;
begin
  if not GetCommModemStatus(FHandle, Status) then
    raise EKRAsyncComPort.Create(CError_ModemStatFailed, GetLastError);
  Result := [];

  if (MS_CTS_ON and Status) <> 0 then Result := Result + [csCTS];
  if (MS_DSR_ON and Status) <> 0 then Result := Result + [csDSR];
  if (MS_RING_ON and Status) <> 0 then Result := Result + [csRing];
  if (MS_RLSD_ON and Status) <> 0 then Result := Result + [csRLSD];
end;

function TKRAsyncComPort.StateFlags: TComStateFlags;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    raise EKRAsyncComPort.Create(CError_ClearComFailed, GetLastError);
  Result := ComStat.Flags;
end;

procedure TKRAsyncComPort.SetBreak(OnOff: Boolean);
var
  Act: Integer;
begin
  if OnOff then Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.SETBREAK
  else Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.CLRBREAK;
  if not EscapeCommFunction(FHandle, Act) then
    raise EKRAsyncComPort.Create(CError_EscapeComFailed, GetLastError);
end;

procedure TKRAsyncComPort.SetDTR(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.SETDTR
  else Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.CLRDTR;
  if not EscapeCommFunction(FHandle, Act) then
    raise EKRAsyncComPort.Create(CError_EscapeComFailed, GetLastError);
end;

procedure TKRAsyncComPort.SetRTS(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.SETRTS
  else Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.CLRRTS;
  if not EscapeCommFunction(FHandle, Act) then
    raise EKRAsyncComPort.Create(CError_EscapeComFailed, GetLastError);
end;

procedure TKRAsyncComPort.SetXonXoff(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.SETXON
  else Act := {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.SETXOFF;
  if not EscapeCommFunction(FHandle, Act) then
    raise EKRAsyncComPort.Create(CError_EscapeComFailed, GetLastError);
end;

procedure TKRAsyncComPort.ClearBuffer(Input, Output: Boolean);
var
  Flag: DWORD;
begin
  Flag := 0;
  if Input then Flag := PURGE_RXCLEAR;
  if Output then Flag := Flag or PURGE_TXCLEAR;
  if not PurgeComm(FHandle, Flag) then
    raise EKRAsyncComPort.Create(CError_PurgeFailed, GetLastError);
end;

function TKRAsyncComPort.LastErrors: TKRACPErrors;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    raise EKRAsyncComPort.Create(CError_ClearComFailed, GetLastError);
  Result := [];
  if (CE_FRAME and Errors) <> 0 then Result := Result + [ceFrame];
  if ((CE_RXPARITY and Errors) <> 0) and FParity.Check then Result := Result + [ceRxParity];
  if (CE_OVERRUN and Errors) <> 0 then Result := Result + [ceOverrun];
  if (CE_RXOVER and Errors) <> 0 then Result := Result + [ceRxOver];
  if (CE_TXFULL and Errors) <> 0 then Result := Result + [ceTxFull];
  if (CE_BREAK and Errors) <> 0 then Result := Result + [ceBreak];
  if (CE_IOE and Errors) <> 0 then Result := Result + [ceIO];
  if (CE_MODE and Errors) <> 0 then Result := Result + [ceMode];
end;

procedure PrepareAsync(AKind: TKRACPOperationKind; const Buffer;
  Count: Integer; AsyncPtr: PKRACPAsync);
begin
  with AsyncPtr^ do begin
    Kind := AKind;
    if Data <> nil then FreeMem(Data);
    GetMem(Data, Count);
    Move(Buffer, Data^, Count);
    Size := Count;
  end;
end;

function TKRAsyncComPort.WriteAsync(const Buffer; Count: Integer; var AsyncPtr: PKRACPAsync): Integer;
var
  Success: Boolean;
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    raise EKRAsyncComPort.CreateNoWinCode(CError_InvalidAsync);
  PrepareAsync(okWrite, Buffer, Count, AsyncPtr);
  Success := WriteFile(FHandle, Buffer, Count, BytesTrans, @AsyncPtr^.Overlapped)
    or (GetLastError = ERROR_IO_PENDING);
  if not Success then
    raise EKRAsyncComPort.Create(CError_WriteFailed, GetLastError);
  Result := BytesTrans;
end;

function TKRAsyncComPort.Write(const Buffer; Count: Integer): Integer;
var
  AsyncPtr: PKRACPAsync;
begin
  KRACPInitAsync(AsyncPtr);
  try
    WriteAsync(Buffer, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    KRACPDoneAsync(AsyncPtr);
  end;
end;

function TKRAsyncComPort.WriteStrAsync(const Str: AnsiString; var AsyncPtr: PKRACPAsync): Integer;
begin
  if Length(Str) > 0 then
    Result := WriteAsync(Str[1], Length(Str), AsyncPtr)
  else
    Result := 0;
end;

function TKRAsyncComPort.WriteStr(const Str: AnsiString): Integer;
var
  AsyncPtr: PKRACPAsync;
begin
  KRACPInitAsync(AsyncPtr);
  try
    WriteStrAsync(Str, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    KRACPDoneAsync(AsyncPtr);
  end;
end;

function TKRAsyncComPort.ReadAsync(var Buffer; Count: Integer; var AsyncPtr: PKRACPAsync): Integer;
var
  Success: Boolean;
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    raise EKRAsyncComPort.CreateNoWinCode(CError_InvalidAsync);
  AsyncPtr^.Kind := okRead;

  Success := ReadFile(FHandle, Buffer, Count, BytesTrans, @AsyncPtr^.Overlapped)
    or (GetLastError = ERROR_IO_PENDING);

  if not Success then
    raise EKRAsyncComPort.Create(CError_ReadFailed, GetLastError);

  Result := BytesTrans;
end;

function TKRAsyncComPort.Read(var Buffer; Count: Integer): Integer;
var
  AsyncPtr: PKRACPAsync;
begin
  KRACPInitAsync(AsyncPtr);
  try
    ReadAsync(Buffer, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    KRACPDoneAsync(AsyncPtr);
  end;
end;

function TKRAsyncComPort.ReadStrAsync(var Str: AnsiString; Count: Integer; var AsyncPtr: PKRACPAsync): Integer;
begin
  SetLength(Str, Count);
  if Count > 0 then
    Result := ReadAsync(Str[1], Count, AsyncPtr)
  else
    Result := 0;
end;

function TKRAsyncComPort.ReadStr(var Str: AnsiString; Count: Integer): Integer;
var
  AsyncPtr: PKRACPAsync;
begin
  KRACPInitAsync(AsyncPtr);
  try
    ReadStrAsync(Str, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
    SetLength(Str, Result);
  finally
    KRACPDoneAsync(AsyncPtr);
  end;
end;

function ErrorCode(AsyncPtr: PKRACPAsync): Integer;
begin
  Result := 0;
  case AsyncPtr^.Kind of
    okWrite: Result := CError_WriteFailed;
    okRead:  Result := CError_ReadFailed;
  end;
end;

function TKRAsyncComPort.WaitForAsync(var AsyncPtr: PKRACPAsync): Integer;
var
  BytesTrans, Signaled: DWORD;
  Success: Boolean;
begin
  if AsyncPtr = nil then
    raise EKRAsyncComPort.CreateNoWinCode(CError_InvalidAsync);

  Signaled := WaitForSingleObject(AsyncPtr^.Overlapped.hEvent, INFINITE);
  Success := (Signaled = WAIT_OBJECT_0) and
      (GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False));

  if not Success then
    raise EKRAsyncComPort.Create(ErrorCode(AsyncPtr), GetLastError);

  Result := BytesTrans;
end;

procedure TKRAsyncComPort.AbortAllAsync;
begin
  if not PurgeComm(FHandle, PURGE_TXABORT or PURGE_RXABORT) then
    raise EKRAsyncComPort.Create(CError_PurgeFailed, GetLastError);
end;

function TKRAsyncComPort.IsAsyncCompleted(AsyncPtr: PKRACPAsync): Boolean;
var
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    raise EKRAsyncComPort.CreateNoWinCode(CError_InvalidAsync);

  Result := GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False);
  if not Result then
    if (GetLastError <> ERROR_IO_PENDING) and (GetLastError <> ERROR_IO_INCOMPLETE) then
      raise EKRAsyncComPort.Create(CError_AsyncCheck, GetLastError);
end;

procedure TKRAsyncComPort.WaitForEvent(var Events: TKRACPEvents;
  StopEvent: THandle; Timeout: Integer);
var
  Overlapped: TOverlapped;
  Mask: DWORD;
  Success: Boolean;
  Signaled, EventHandleCount: Integer;
  EventHandles: array[0..1] of THandle;
begin
  if FThreadCreated then
    raise EKRAsyncComPort.CreateNoWinCode(CError_ThreadCreated);

  FillChar(Overlapped, SizeOf(TOverlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);
  EventHandles[0] := Overlapped.hEvent;
  if StopEvent <> 0 then begin
    EventHandles[1] := StopEvent;
    EventHandleCount := 2;
  end else EventHandleCount := 1;
  try
    SetCommMask(FHandle, EventsToInt(Events));
    Success := WaitCommEvent(FHandle, Mask, @Overlapped);
    if (Success) or (GetLastError = ERROR_IO_PENDING) then begin
      Signaled := WaitForMultipleObjects(EventHandleCount, @EventHandles, False, Timeout);
      Success := (Signaled = WAIT_OBJECT_0) or (Signaled = WAIT_OBJECT_0 + 1) or (Signaled = WAIT_TIMEOUT);
      SetCommMask(FHandle, 0);
    end;
    if not Success then
      raise EKRAsyncComPort.Create(CError_WaitFailed, GetLastError);
    Events := IntToEvents(Mask);
  finally
    CloseHandle(Overlapped.hEvent);
  end;
end;

procedure TKRAsyncComPort.TransmitChar(Ch: AnsiChar);
begin
  if not TransmitCommChar(FHandle, Ch) then
    raise EKRAsyncComPort.Create(CError_TransmitFailed, GetLastError);
end;

procedure TKRAsyncComPort.DoRxChar(Count: Integer);
begin
  if Assigned(FOnRxChar) then FOnRxChar(Self, Count);
end;

procedure TKRAsyncComPort.DoRxBuf(const Buffer; Count: Integer);
begin
  if Assigned(FOnRxBuf) then FOnRxBuf(Self, Buffer, Count);
end;

procedure TKRAsyncComPort.DoBreak;
begin
  if Assigned(FOnBreak) then FOnBreak(Self);
end;

procedure TKRAsyncComPort.DoTxEmpty;
begin
  if Assigned(FOnTxEmpty) then FOnTxEmpty(Self);
end;

procedure TKRAsyncComPort.DoRing;
begin
  if Assigned(FOnRing) then FOnRing(Self);
end;

procedure TKRAsyncComPort.DoCTSChange(OnOff: Boolean);
begin
  if Assigned(FOnCTSChange) then FOnCTSChange(Self, OnOff);
end;

procedure TKRAsyncComPort.DoDSRChange(OnOff: Boolean);
begin
  if Assigned(FOnDSRChange) then FOnDSRChange(Self, OnOff);
end;

procedure TKRAsyncComPort.DoRLSDChange(OnOff: Boolean);
begin
  if Assigned(FOnRLSDChange) then FOnRLSDChange(Self, OnOff);
end;

procedure TKRAsyncComPort.DoError(Errors: TKRACPErrors);
begin
  if Assigned(FOnError) then FOnError(Self, Errors);
end;

procedure TKRAsyncComPort.DoRxFlag;
begin
  if Assigned(FOnRxFlag) then FOnRxFlag(Self);
end;

procedure TKRAsyncComPort.DoRx80Full;
begin
  if Assigned(FOnRx80Full) then FOnRx80Full(Self);
end;

procedure TKRAsyncComPort.CheckSignals(Open: Boolean);
begin
  if Open then begin
    CallCTSChange;
    CallDSRChange;
    CallRLSDChange;
  end else begin
    DoCTSChange(False);
    DoDSRChange(False);
    DoRLSDChange(False);
  end;
end;

procedure TKRAsyncComPort.CallBreak;
begin
  DoBreak;
end;

procedure TKRAsyncComPort.CallCTSChange;
var
  OnOff: Boolean;
begin
  OnOff := csCTS in Signals;
  DoCTSChange(OnOff);
end;

procedure TKRAsyncComPort.CallDSRChange;
var
  OnOff: Boolean;
begin
  OnOff := csDSR in Signals;
  DoDSRChange(OnOff);
end;

procedure TKRAsyncComPort.CallRLSDChange;
var
  OnOff: Boolean;
begin
  OnOff := csRLSD in Signals;
  DoRLSDChange(OnOff);
end;

procedure TKRAsyncComPort.CallError;
var
  Errors: TKRACPErrors;
begin
  Errors := LastErrors;
  if Errors <> [] then DoError(Errors);
end;

procedure TKRAsyncComPort.CallRing;
begin
  DoRing;
end;

procedure TKRAsyncComPort.CallRx80Full;
begin
  DoRx80Full;
end;

procedure TKRAsyncComPort.CallRxChar;
var
  Count: Integer;

  procedure PerformRead(var P: Pointer);
  begin
    GetMem(P, Count);
    Read(P^, Count);
    DoRxBuf(P^, Count);
  end;

begin
  Count := InputCount;
  if Count > 0 then DoRxChar(Count);
end;

procedure TKRAsyncComPort.CallRxFlag;
begin
  DoRxFlag;
end;

procedure TKRAsyncComPort.CallTxEmpty;
begin
  DoTxEmpty;
end;

procedure TKRAsyncComPort.SetConnected(const Value: Boolean);
begin
  if not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then begin
    if Value <> FConnected then
      if Value then Open
      else Close;
  end else FConnected := Value;
end;

procedure TKRAsyncComPort.SetBaudRate(const Value: integer);
begin
  if Value <> FBaudRate then begin
    FBaudRate := Value;
    ApplyDCB;
  end;
end;

procedure TKRAsyncComPort.SetDataBits(const Value: Integer);
begin
  if Value <> FDataBits then begin
    FDataBits := Value;
    ApplyDCB;
  end;
end;

procedure TKRAsyncComPort.SetDiscardNull(const Value: Boolean);
begin
  if Value <> FDiscardNull then begin
    FDiscardNull := Value;
    ApplyDCB;
  end;
end;

procedure TKRAsyncComPort.SetEventChar(const Value: AnsiChar);
begin
  if Value <> FEventChar then begin
    FEventChar := Value;
    ApplyDCB;
  end;
end;

procedure TKRAsyncComPort.SetPort(const Value: String);
begin
  if Value <> FPort then begin
    FPort := Value;
    if FConnected and not ((csDesigning in ComponentState) or
      (csLoading in ComponentState)) then begin
      Close;
      Open;
    end;
  end;
end;

procedure TKRAsyncComPort.SetStopBits(const Value: Integer);
begin
  if Value <> FStopBits then begin
    FStopBits := Value;
    ApplyDCB;
  end;
end;

procedure TKRAsyncComPort.SetTriggersOnRxChar(const Value: Boolean);
begin
  FTriggersOnRxChar := Value;
end;

procedure TKRAsyncComPort.SetEventThreadPriority(const Value: TThreadPriority);
begin
  if Value <> FEventThreadPriority then begin
    if FConnected and not ((csDesigning in ComponentState) or
      (csLoading in ComponentState))
    then
      raise EKRAsyncComPort.CreateNoWinCode(CError_ConnChangeProp)
    else
      FEventThreadPriority := Value;
  end;
end;

function TKRAsyncComPort.GetBaudRate: integer;
begin
  Result:=FBaudRate;
end;

function TKRAsyncComPort.GetDataBits: Integer;
begin
  Result:=FDataBits;
end;

function TKRAsyncComPort.GetFlowControl: TKRFlowControl;
begin
  Result:=FFlowControl.FlowControl;
end;

function TKRAsyncComPort.GetParity: Integer;
begin
  Result:=FParity.FBits;
end;

function TKRAsyncComPort.GetPort: String;
begin
  Result:=FPort;
end;

function TKRAsyncComPort.GetStopBits: Integer;
begin
  Result:=FStopBits;
end;

function TKRAsyncComPort.GetTriggersOnRxChar: Boolean;
begin
  Result := FTriggersOnRxChar;
end;

procedure TKRAsyncComPort.SetFlowControl(const Value: TKRFlowControl);
begin
  FFlowControl.SetFlowControl(Value);
end;

procedure TKRAsyncComPort.SetFlowControlObj(const Value: TKRACPFlowControl);
begin
  FFlowControl.Assign(Value);
  ApplyDCB;
end;

procedure TKRAsyncComPort.SetParity(const Value: integer);
begin
  FParity.SetBits(Value);
end;

procedure TKRAsyncComPort.SetParityObj(const Value: TKRACPParity);
begin
  FParity.Assign(Value);
  ApplyDCB;
end;

procedure TKRAsyncComPort.SetTimeouts(const Value: TKRACPTimeouts);
begin
  FTimeouts.Assign(Value);
  ApplyTimeouts;
end;

procedure TKRAsyncComPort.SetBuffer(const Value: TKRACPBuffer);
begin
  FBuffer.Assign(Value);
  ApplyBuffer;
end;

constructor EKRAsyncComPort.Create(ACode: Integer; AWinCode: Integer);
begin
  FWinCode := AWinCode;
  FCode := ACode;
  inherited CreateFmt(ComErrorMessages[ACode] + ' (win error code: %d)', [AWinCode]);
end;

constructor EKRAsyncComPort.CreateNoWinCode(ACode: Integer);
begin
  FWinCode := -1;
  FCode := ACode;
  inherited Create(ComErrorMessages[ACode]);
end;

procedure KRACPInitAsync(var AsyncPtr: PKRACPAsync);
begin
  New(AsyncPtr);
  with AsyncPtr^ do begin
    FillChar(Overlapped, SizeOf(TOverlapped), 0);
    Overlapped.hEvent := CreateEvent(nil, True, True, nil);
    Data := nil;
    Size := 0;
  end;
end;

procedure KRACPDoneAsync(var AsyncPtr: PKRACPAsync);
begin
  with AsyncPtr^ do begin
    CloseHandle(Overlapped.hEvent);
    if Data <> nil then FreeMem(Data);
  end;
  Dispose(AsyncPtr);
  AsyncPtr := nil;
end;

end.
