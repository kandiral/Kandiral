(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCOMPort                                                                 *)
(*  Ver.: 25.11.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCOMPort;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils;
  {$ELSE}
    Windows, Classes, SysUtils;
  {$IFEND}

type
  TKRFlowControl = integer;

const
   krfcNone = TKRFlowControl(0);
   krfcHardware = TKRFlowControl(1);
   krfcSoftware = TKRFlowControl(2);
   krfcCustom = TKRFlowControl(3);

const
  // infinite wait
  WaitInfinite = Integer(INFINITE);

  // error codes
  KRCOMPortError_OpenFailed      = 1;
  KRCOMPortError_WriteFailed     = 2;
  KRCOMPortError_ReadFailed      = 3;
  KRCOMPortError_InvalidAsync    = 4;
  KRCOMPortError_PurgeFailed     = 5;
  KRCOMPortError_AsyncCheck      = 6;
  KRCOMPortError_SetStateFailed  = 7;
  KRCOMPortError_TimeoutsFailed  = 8;
  KRCOMPortError_SetupComFailed  = 9;
  KRCOMPortError_ClearComFailed  = 10;
  KRCOMPortError_ModemStatFailed = 11;
  KRCOMPortError_EscapeComFailed = 12;
  KRCOMPortError_TransmitFailed  = 13;
  KRCOMPortError_ConnChangeProp  = 14;
  KRCOMPortError_EnumPortsFailed = 15;
  KRCOMPortError_StoreFailed     = 16;
  KRCOMPortError_LoadFailed      = 17;
  KRCOMPortError_RegFailed       = 18;
  KRCOMPortError_LedStateFailed  = 19;
  KRCOMPortError_ThreadCreated   = 20;
  KRCOMPortError_WaitFailed      = 21;
  KRCOMPortError_HasLink         = 22;
  KRCOMPortError_RegError        = 23;

type
  TKRCOMPort = class
  private
    FHandle: THandle;
    FPort: String;
    FConnected: boolean;
    FLastError: integer;
    FOutputBuffer: integer;
    FInputBuffer: integer;
    FEventChar: AnsiChar;
    FDiscardNull: boolean;
    FXonChar: AnsiChar;
    FXoffChar: AnsiChar;
    FXonXoffOut: Boolean;
    FXonXoffIn: Boolean;
    FDSRSensitivity: Boolean;
    FOutDSRFlow: Boolean;
    FOutCTSFlow: Boolean;
    FControlDTR: Cardinal;
    FControlRTS: Cardinal;
    FTxContinueOnXoff: Boolean;
    FParity: integer;
    FParityCheck: boolean;
    FParityReplace: boolean;
    FParityReplaceChar: AnsiChar;
    FStopBits: integer;
    FBaudRate: integer;
    FDataBits: integer;
    FReadTotalTimeoutMultiplier: cardinal;
    FReadTotalTimeoutConstant: cardinal;
    FWriteTotalTimeoutMultiplier: cardinal;
    FWriteTotalTimeoutConstant: cardinal;
    FReadIntervalTimeout: cardinal;
    procedure DestroyHandle;
    function GetFlowControl: TKRFlowControl;
    procedure SetFlowControl(const Value: TKRFlowControl);
  public
    constructor Create;virtual;
    destructor Destroy;override;

    function Open: boolean;
    procedure Close;
    property Connected: boolean read FConnected;
    property Handle: THandle read FHandle;

    property Port: String read FPort write FPort;
    property LastError: integer read FLastError;
    property InputBuffer: integer read FInputBuffer write FInputBuffer;
    property OutputBuffer: integer read FOutputBuffer write FOutputBuffer;
    property EventChar: AnsiChar read FEventChar write FEventChar;
    property DiscardNull: boolean read FDiscardNull write FDiscardNull;
    property XonChar: AnsiChar read FXonChar write FXonChar;
    property XoffChar: AnsiChar read FXoffChar write FXoffChar;
    property OutCTSFlow: Boolean read FOutCTSFlow write FOutCTSFlow;
    property OutDSRFlow: Boolean read FOutDSRFlow write FOutDSRFlow;
    property ControlDTR: Cardinal read FControlDTR write FControlDTR;
    property ControlRTS: Cardinal read FControlRTS write FControlRTS;
    property XonXoffOut: Boolean read FXonXoffOut write FXonXoffOut;
    property XonXoffIn:  Boolean read FXonXoffIn write FXonXoffIn;
    property DSRSensitivity: Boolean read FDSRSensitivity write FDSRSensitivity;
    property TxContinueOnXoff: Boolean read FTxContinueOnXoff write FTxContinueOnXoff;
    property FlowControl: TKRFlowControl read GetFlowControl write SetFlowControl;
    property Parity: integer read FParity write FParity;
    property ParityCheck: boolean read FParityCheck write FParityCheck;
    property ParityReplace: boolean read FParityReplace write FParityReplace;
    property ParityReplaceChar: AnsiChar read FParityReplaceChar write FParityReplaceChar;
    property StopBits: integer read FStopBits write FStopBits;
    property BaudRate: integer read FBaudRate write FBaudRate;
    property DataBits: integer read FDataBits write FDataBits;
    property ReadIntervalTimeout: cardinal read FReadIntervalTimeout write FReadIntervalTimeout;
    property ReadTotalTimeoutMultiplier: cardinal read FReadTotalTimeoutMultiplier write FReadTotalTimeoutMultiplier;
    property ReadTotalTimeoutConstant: cardinal read FReadTotalTimeoutConstant write FReadTotalTimeoutConstant;
    property WriteTotalTimeoutMultiplier: cardinal read FWriteTotalTimeoutMultiplier write FWriteTotalTimeoutMultiplier;
    property WriteTotalTimeoutConstant: cardinal read FWriteTotalTimeoutConstant write FWriteTotalTimeoutConstant;
    function Write(const Buffer; Count: Integer): Integer;
    function InputCount: integer;
    function Read(var Buffer; Count: Integer): Integer;
  end;

  function BaudRateFromIndex(AIndex: integer): integer;
  function IndexFromBaudRate(ABaudRate: integer): integer;
  function DataBitsFromIndex(AIndex: integer): integer;
  function IndexFromDataBits(ADataBits: integer): integer;
  function StopBitsFromIndex(AIndex: integer): integer;
  function IndexFromStopBits(AStopBits: integer): integer;
  function ParityFromIndex(AIndex: integer): integer;
  function IndexFromParity(AParity: integer): integer;
  function FlowControlFromIndex(AIndex: integer): TKRFlowControl;
  function IndexFromFlowControl(AFlowControl: TKRFlowControl): integer;

var
  KRComPortMsgs: array[1..23] of string;

implementation

function BaudRateFromIndex(AIndex: integer): integer;
begin
  Result:=0;
  case AIndex of
  0: Result:=CBR_110;
  1: Result:=CBR_300;
  2: Result:=CBR_600;
  3: Result:=CBR_1200;
  4: Result:=CBR_2400;
  5: Result:=CBR_4800;
  6: Result:=CBR_9600;
  7: Result:=CBR_14400;
  8: Result:=CBR_19200;
  9: Result:=CBR_38400;
  10: Result:=CBR_56000;
  11: Result:=CBR_57600;
  12: Result:=CBR_115200;
  13: Result:=CBR_128000;
  14: Result:=CBR_256000;
  end;
end;

function IndexFromBaudRate(ABaudRate: integer): integer;
begin
  Result:=0;
  case ABaudRate of
  CBR_110: Result:=0;
  CBR_300: Result:=1;
  CBR_600: Result:=2;
  CBR_1200: Result:=3;
  CBR_2400: Result:=4;
  CBR_4800: Result:=5;
  CBR_9600: Result:=6;
  CBR_14400: Result:=7;
  CBR_19200: Result:=8;
  CBR_38400: Result:=9;
  CBR_56000: Result:=10;
  CBR_57600: Result:=11;
  CBR_115200: Result:=12;
  CBR_128000: Result:=13;
  CBR_256000: Result:=14;
  end;
end;

function DataBitsFromIndex(AIndex: integer): integer;
begin
  Result:=0;
  case AIndex of
  0: Result:=5;
  1: Result:=6;
  2: Result:=7;
  3: Result:=8;
  end;
end;

function IndexFromDataBits(ADataBits: integer): integer;
begin
  Result:=0;
  case ADataBits of
  5: Result:=0;
  6: Result:=1;
  7: Result:=2;
  8: Result:=3;
  end;
end;

function StopBitsFromIndex(AIndex: integer): integer;
begin
  Result:=0;
  case AIndex of
  0: Result:=ONESTOPBIT;
  1: Result:=ONE5STOPBITS;
  2: Result:=TWOSTOPBITS;
  end;
end;

function IndexFromStopBits(AStopBits: integer): integer;
begin
  Result:=0;
  case AStopBits of
  ONESTOPBIT: Result:=0;
  ONE5STOPBITS: Result:=1;
  TWOSTOPBITS: Result:=2;
  end;
end;

function ParityFromIndex(AIndex: integer): integer;
begin
  Result:=0;
  case AIndex of
  0: Result:=NOPARITY;
  1: Result:=ODDPARITY;
  2: Result:=EVENPARITY;
  3: Result:=MARKPARITY;
  4: Result:=SPACEPARITY;
  end;
end;

function IndexFromParity(AParity: integer): integer;
begin
  Result:=0;
  case AParity of
  NOPARITY: Result:=0;
  ODDPARITY: Result:=1;
  EVENPARITY: Result:=2;
  MARKPARITY: Result:=3;
  SPACEPARITY: Result:=4;
  end;
end;

function FlowControlFromIndex(AIndex: integer): TKRFlowControl;
begin
  Result:=krfcNone;
  case AIndex of
  0: Result:=krfcNone;
  1: Result:=krfcHardware;
  2: Result:=krfcSoftware;
  3: Result:=krfcCustom;
  end;
end;

function IndexFromFlowControl(AFlowControl: TKRFlowControl): integer;
begin
  Result:=krfcNone;
  case AFlowControl of
  krfcNone: Result:=0;
  krfcHardware: Result:=1;
  krfcSoftware: Result:=2;
  krfcCustom: Result:=3;
  end;
end;


{ TKRCOMPort }

procedure TKRCOMPort.Close;
begin
  DestroyHandle;
  FConnected:=false;
end;

constructor TKRCOMPort.Create;
begin
  Port:='COM1';
  FConnected:=false;
  FHandle := INVALID_HANDLE_VALUE;
  FOutputBuffer:=1024;
  FInputBuffer:=1024;
  FEventChar:=#0;
  FDiscardNull:=false;
  FXonChar := #17;
  FXoffChar := #19;
  FOutCTSFlow:=false;
  FOutDSRFlow:=false;
  FControlDTR:=DTR_CONTROL_DISABLE;
  FControlRTS:=RTS_CONTROL_DISABLE;
  FXonXoffOut:=false;
  FXonXoffIn:=false;
  FDSRSensitivity:=false;
  FTxContinueOnXoff:=false;
  FParity:=NOPARITY;
  FParityCheck:=false;
  FParityReplace:=false;
  FParityReplaceChar:=#0;
  FStopBits:=ONESTOPBIT;
  FBaudRate:=CBR_4800;
  FDataBits:=8;
  FReadTotalTimeoutMultiplier:=0;
  FReadTotalTimeoutConstant:=0;
  FWriteTotalTimeoutMultiplier:=100;
  FWriteTotalTimeoutConstant:=1000;
  FReadIntervalTimeout:=MAXDWORD;
  FLastError:=0;
end;

destructor TKRCOMPort.Destroy;
begin
  Close;
  inherited;
end;

procedure TKRCOMPort.DestroyHandle;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;
end;

function TKRCOMPort.GetFlowControl: TKRFlowControl;
begin
  if (FControlRTS = RTS_CONTROL_HANDSHAKE) and (FOutCTSFlow)
    and (not FXonXoffIn) and (not FXonXoffOut)
  then
    Result := krfcHardware
  else
    if (FControlRTS = RTS_CONTROL_DISABLE) and (not FOutCTSFlow)
      and (FXonXoffIn) and (FXonXoffOut)
    then
      Result := krfcSoftware
    else
      if (FControlRTS = RTS_CONTROL_DISABLE) and (not FOutCTSFlow)
        and (not FXonXoffIn) and (not FXonXoffOut)
      then
        Result := krfcNone
      else
        Result := krfcCustom;
end;

function TKRCOMPort.InputCount: integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  Result:=0;
  if ClearCommError(FHandle, Errors, @ComStat) then Result := ComStat.cbInQue;
end;

function TKRCOMPort.Open: boolean;
const
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

var
  DCB: TDCB;
  Timeouts: TCommTimeouts;

begin
  Result:=FConnected;
  FLastError:=0;
  if Result then exit;

  FHandle := CreateFile(
    PChar('\\.\' + FPort),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, //FILE_FLAG_OVERLAPPED,
    0);

  if FHandle = INVALID_HANDLE_VALUE then begin
    FLastError:=KRCOMPortError_OpenFailed;
    exit;
  end;

  if not SetupComm(FHandle, FInputBuffer, FOutputBuffer) then begin
    DestroyHandle;
    FLastError:=KRCOMPortError_SetupComFailed;
    exit;
  end;

  DCB.DCBlength := SizeOf(TDCB);
  DCB.XonLim := FInputBuffer div 4;
  DCB.XoffLim := DCB.XonLim;
  DCB.EvtChar := FEventChar;

  DCB.Flags := dcb_Binary;
  if FDiscardNull then DCB.Flags := DCB.Flags or dcb_Null;

  DCB.XonChar := XonChar;
  DCB.XoffChar := XoffChar;
  if FOutCTSFlow then DCB.Flags := DCB.Flags or dcb_OutxCTSFlow;
  if FOutDSRFlow then DCB.Flags := DCB.Flags or dcb_OutxDSRFlow;
  DCB.Flags := DCB.Flags or Integer((FControlDTR shl 4) or (FControlRTS shl 12));
  if FXonXoffOut then DCB.Flags := DCB.Flags or dcb_OutX;
  if FXonXoffIn then DCB.Flags := DCB.Flags or dcb_InX;
  if FDSRSensitivity then DCB.Flags := DCB.Flags or dcb_DSRSensivity;
  if FTxContinueOnXoff then DCB.Flags := DCB.Flags or dcb_TxContinueOnXoff;

  DCB.Parity := FParity;
  if FParityCheck then begin
    DCB.Flags := DCB.Flags or dcb_Parity;
    if FParityReplace then begin
        DCB.Flags := DCB.Flags or dcb_ErrorChar;
        DCB.ErrorChar := FParityReplaceChar;
    end;
  end;
  DCB.StopBits := FStopBits;
  DCB.BaudRate := FBaudRate;
  DCB.ByteSize := FDataBits;

  if not SetCommState(FHandle, DCB) then begin
    DestroyHandle;
    FLastError:=3;
    exit;
  end;

  Timeouts.ReadIntervalTimeout := FReadIntervalTimeout;
  Timeouts.ReadTotalTimeoutMultiplier := FReadTotalTimeoutMultiplier;
  Timeouts.ReadTotalTimeoutConstant := FReadTotalTimeoutConstant;
  Timeouts.WriteTotalTimeoutMultiplier := FWriteTotalTimeoutMultiplier;
  Timeouts.WriteTotalTimeoutConstant := FWriteTotalTimeoutConstant;

  if not SetCommTimeouts(FHandle, Timeouts) then begin
    DestroyHandle;
    FLastError:=4;
    exit;
  end;

  FConnected:=true;
  Result:=true;
end;

function TKRCOMPort.Read(var Buffer; Count: Integer): Integer;
var
  cnt: Cardinal;
begin
  cnt:=0;
  ReadFile(FHandle, Buffer, Count, cnt, nil);
  Result:=cnt;
end;

procedure TKRCOMPort.SetFlowControl(const Value: TKRFlowControl);
begin
  if Value <> krfcCustom then
  begin
    FControlRTS := RTS_CONTROL_DISABLE;
    FOutCTSFlow := False;
    FXonXoffIn := False;
    FXonXoffOut := False;
    case Value of
      krfcHardware:
      begin
        FControlRTS := RTS_CONTROL_HANDSHAKE;
        FOutCTSFlow := True;
      end;
      krfcSoftware:
      begin
        FXonXoffIn := True;
        FXonXoffOut := True;
      end;
    end;
  end;
end;

function TKRCOMPort.Write(const Buffer; Count: Integer): Integer;
var
  cnt: Cardinal;
begin
  cnt:=0;
  WriteFile(FHandle,Buffer,Count,cnt,nil);
  Result:=cnt;
end;

initialization
  KRComPortMsgs[1]:='Unable to open com port';
  KRComPortMsgs[2]:='WriteFile function failed';
  KRComPortMsgs[3]:='ReadFile function failed';
  KRComPortMsgs[4]:='Invalid Async parameter';
  KRComPortMsgs[5]:='PurgeComm function failed';
  KRComPortMsgs[6]:='Unable to get async status';
  KRComPortMsgs[7]:='SetCommState function failed';
  KRComPortMsgs[8]:='SetCommTimeouts failed';
  KRComPortMsgs[9]:='SetupComm function failed';
  KRComPortMsgs[10]:='ClearCommError function failed';
  KRComPortMsgs[11]:='GetCommModemStatus function failed';
  KRComPortMsgs[12]:='EscapeCommFunction function failed';
  KRComPortMsgs[13]:='TransmitCommChar function failed';
  KRComPortMsgs[14]:='Cannot set property while connected';
  KRComPortMsgs[15]:='EnumPorts function failed';
  KRComPortMsgs[16]:='Failed to store settings';
  KRComPortMsgs[17]:='Failed to load settings';
  KRComPortMsgs[18]:='Link (un)registration failed';
  KRComPortMsgs[19]:='Cannot change led state if ComPort is selected';
  KRComPortMsgs[20]:='Cannot wait for event if event thread is created';
  KRComPortMsgs[21]:='WaitForEvent method failed';
  KRComPortMsgs[22]:='A component is linked to OnRxBuf event';
  KRComPortMsgs[23]:='Registry error';

end.
