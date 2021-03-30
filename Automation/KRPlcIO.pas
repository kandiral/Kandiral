(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRPlcIO                                                                   *)
(*  Ver.: 15.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRPlcIO;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.Forms,
  {$ELSE}
    Classes, SysUtils, Forms,
  {$IFEND}
  KRWindows, KRThread, KRStrUtils;

type
  TKRPLCIOERROR = record
    code: Cardinal;
    msg: String;
  end;

const
  KRPLCIOERRORS :array[0..9] of TKRPLCIOERROR = (
    (code: 100; msg: 'Произошла ошибка связи с ПЛК!'),
    (code: 110; msg: 'Не удалось открыть один из заданных файлов на ПК!'),
    (code: 111; msg: 'В параметрах указано слишком длинное имя файла!'),
    (code: 112; msg: 'Невозможно удалить файл на ПЛК!'),
    (code: 120; msg: 'Произошла единичная ошибка при чтении файла с ПЛК!'),
    (code: 121; msg: 'Произошли множественные ошибки при чтении файлов с ПЛК!'),
    (code: 130; msg: 'Произошла единичная ошибка при записи файла на ПЛК!'),
    (code: 131; msg: 'Произошли множественные ошибки при записи файлов на ПЛК!'),
    (code: 200; msg: 'Неправильно заданы параметры командной строки!'),
    (code: 201; msg: 'Внутренняя ошибка приложения!')
  );

type
  TKRPLCIOConnectType = (ctTCP, ctCOM);
  TKRPLCIOCmd = procedure(Sender: TObject; ACmd: String) of object;
  TKRPLCIOCmdRes = procedure(Sender: TObject; AStdOut, AStdErr: String; AErrorLevel: Cardinal) of object;

  TKRPLCIOThread = class;

  TKRPLCIO = class(TComponent)
  private
    FLError: String;
    FAddr: String;
    FPort: String;
    FCType: TKRPLCIOConnectType;
    FPlc_io, FPlc_io_old, FPlc_con: String;
    FWaitingEnd: TNotifyEvent;
    FWaitingBegin: TNotifyEvent;
    FSimulation: boolean;
    FSimulationPath: String;
    FOnCmdRes: TKRPLCIOCmdRes;
    FOnCmd: TKRPLCIOCmd;
    FThread: TKRPLCIOThread;
    procedure ParsPath(AFileName: String; out APath, AFile: String);
    procedure UpdateCType;
    procedure SetAddr(const Value: String);
    procedure SetCType(const Value: TKRPLCIOConnectType);
    procedure SetPort(const Value: String);
    function docmd(ACmd: String; ATOut: byte; AisOem: boolean):boolean;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property LastError: String read FLError;
    function CopyToPLC(AFileName: String; attempts: byte): boolean;overload;
    function CopyToPLC(AFileName: String): boolean;overload;
    function CopyToPC(AFileName: String; attempts: byte): boolean;overload;
    function CopyToPC(AFileName: String): boolean;overload;
    function DelFromPLC(AFileName: String; attempts: byte): boolean;overload;
    function DelFromPLC(AFileName: String): boolean;overload;
    function PLCInfo(var AList: String; attempts: byte): boolean;overload;
    function PLCInfo(var AList: String): boolean;overload;
    function PLCDir(AList: TStrings; attempts: byte): boolean;overload;
    function PLCDir(AList: TStrings): boolean;overload;
  published
    property ConnectType: TKRPLCIOConnectType read FCType write SetCType;
    property Simulation: boolean read FSimulation write FSimulation default false;
    property SimulationPath: String read FSimulationPath write FSimulationPath;
    property Addr: String read FAddr write SetAddr;
    property Port: String read FPort write SetPort;
    property OnWaitingBegin: TNotifyEvent read FWaitingBegin write FWaitingBegin;
    property OnWaitingEnd: TNotifyEvent read FWaitingEnd write FWaitingEnd;
    property OnCmd: TKRPLCIOCmd read FOnCmd write FOnCmd;
    property OnCmdRes: TKRPLCIOCmdRes read FOnCmdRes write FOnCmdRes;
  end;

  TKRPLCIOThread = class(TKRThread)
  private
    FDo: boolean;
    FStdOut, FStdErr: String;
    FErrorLevel: Cardinal;
    FTimeout: byte;
    FCmd, FRes: String;
    FisOem: boolean;
  protected
    procedure KRExecute; override;
  end;

implementation

{ TKRPLCIO }

function TKRPLCIO.CopyToPC(AFileName: String): boolean;
begin
  Result:=CopyToPC(AFileName,3);
end;

function TKRPLCIO.CopyToPC(AFileName: String; attempts: byte): boolean;
var
  fl,pt: String;
  i: integer;
begin
  Result:=false;
  if FSimulation then begin
    Result:=docmd('cmd /c copy '+FSimulationPath+ExtractFileName(AFileName)+' '+AFileName,120,true);
  end else begin
    ParsPath(AFileName,pt,fl);
    for I := 1 to attempts do begin
      Result:=docmd(FPlc_io+FPlc_con+pt+' /get "'+fl+'"',120,false);
      if Result then break;
      Sleep(1000);
    end;
  end;
end;

function TKRPLCIO.CopyToPLC(AFileName: String): boolean;
begin
  Result:=CopyToPLC(AFileName,3);
end;

function TKRPLCIO.CopyToPLC(AFileName: String; attempts: byte): boolean;
var
  fl,pt: String;
  i: integer;
begin
  Result:=false;
  if FSimulation then begin
    Result:=docmd('cmd /c copy '+AFileName+' '+FSimulationPath+ExtractFileName(AFileName),120,true);
  end else begin
    ParsPath(AFileName,pt,fl);
    for I := 1 to attempts do begin
      Result:=docmd(FPlc_io_old+FPlc_con+pt+' /up "'+fl+'"',120,true);
      if Result then break;
      Sleep(1000);
    end;
  end;
end;

constructor TKRPLCIO.Create(AOwner: TComponent);
begin
  inherited;
  FThread:=TKRPLCIOThread.Create;
  FPlc_io:=ExtractFilePath(Application.ExeName)+'plc_io.exe';
  FPlc_io_old:=ExtractFilePath(Application.ExeName)+'plc_io_old.exe';
  FLError:='';
  FAddr:='10.0.6.10';
  FSimulationPath:='';
  FPort:='COM2';
  FCType:=ctTCP;
  UpdateCType;
end;

function TKRPLCIO.DelFromPLC(AFileName: String): boolean;
begin
  Result:=DelFromPLC(AFileName,3);
end;

function TKRPLCIO.DelFromPLC(AFileName: String; attempts: byte): boolean;
var i: integer;
begin
  Result:=false;
  if FSimulation then begin
    Result:=docmd('cmd /c del "'+FSimulationPath+AFileName+'"',120,true);
  end else begin
    for I := 1 to attempts do begin
      Result:=docmd(FPlc_io+FPlc_con+' /del "'+AFileName+'"',60,false);
      if Result then break;
      Sleep(1000);
    end;
  end;
end;

destructor TKRPLCIO.Destroy;
begin
  FThread.Terminate;
  while FThread.Working do Application.ProcessMessages;
  FThread.Free;
  inherited;
end;

function TKRPLCIO.docmd(ACmd: String; ATOut: byte; AisOem: boolean): boolean;
var
  i : integer;
begin
  if Assigned(FWaitingBegin) then FWaitingBegin(Self);
  if Assigned(FOnCmd) then FOnCmd(Self,ACmd);
  FThread.FCmd:=ACmd;
  FThread.FTimeout:=ATOut;
  FThread.FDo:=true;
  FThread.FisOem:=AisOem;
  FThread.Active:=true;
  while FThread.FDo do Application.ProcessMessages;
  FThread.Active:=false;
  FLError:=FThread.FRes;
  if(FThread.FErrorLevel<>0)then begin
    for i:=0 to Length(KRPLCIOERRORS)-1 do begin
      if KRPLCIOERRORS[i].code=FThread.FErrorLevel then begin
        FLError:=KRPLCIOERRORS[i].msg;
        break;
      end;
    end;
  end;
  if FLError<>'' then FLError:='['+IntToStr(FThread.FErrorLevel)+'] '+FLError+' {'+FThread.FStdErr+'}';
  Result:=FLError='';
  if Assigned(FOnCmdRes) then FOnCmdRes(Self,FThread.FStdOut,FThread.FStdErr,FThread.FErrorLevel);
  if Assigned(FWaitingEnd) then FWaitingEnd(Self);
end;

procedure TKRPLCIO.ParsPath(AFileName: String; out APath, AFile: String);
begin
  APath:=ExtractFilePath(AFileName);
  APath:=Copy(APath,1,Length(APath)-1);
  APath:=' /FLDR"'+APath+'"';
  AFile:=ExtractFileName(AFileName);
end;

function TKRPLCIO.PLCDir(AList: TStrings): boolean;
begin
  Result:=PLCDir(AList,3);
end;

function TKRPLCIO.PLCDir(AList: TStrings; attempts: byte): boolean;
var
  i: integer;
  sl: TStringList;

  function CompareA(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    Result := ANSICompareText(List[Index1], List[Index2]);
  end;

  function CompareD(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    Result := ANSICompareText(List[Index2], List[Index1]);
  end;

begin
  Result:=false;
  if FSimulation then Result:=docmd('cmd /c dir "'+FSimulationPath+'*.*" /a:-d /b',30,true)
  else begin
    for I := 1 to attempts do begin
      Result:=docmd(FPlc_io+FPlc_con+' /dir',30,false);
      if Result then break;
      Sleep(1000);
    end;
  end;

  if Result then begin
    sl:=TStringList.Create;
    try
      KRSplitStr(FThread.FSTDOut,#$D#$A,sl);
      Alist.Clear;
      for i := 0 to sl.Count-1 do if Trim(sl[i])<>'' then AList.Add(Trim(sl[i]));
    finally
      sl.Free
    end;
  end;
end;

function TKRPLCIO.PLCInfo(var AList: String): boolean;
begin
  Result:=PLCInfo(AList,3);
end;

function TKRPLCIO.PLCInfo(var AList: String; attempts: byte): boolean;
var i: integer;
begin
  Result:=false;
  if FSimulation then begin
    Result:=true;
    AList:='KRPLCIO Simulation';
  end else begin
    for I := 1 to attempts do begin
      Result:=docmd(FPlc_io+FPlc_con+' /info',30,false);
      if Result then break;
      Sleep(1000);
    end;
    if Result then
      Alist:=FThread.FStdOut;
  end;
end;

procedure TKRPLCIO.SetAddr(const Value: String);
begin
  FAddr := Value;
  UpdateCType;
end;

procedure TKRPLCIO.SetCType(const Value: TKRPLCIOConnectType);
begin
  FCType := Value;
  UpdateCType;
end;

procedure TKRPLCIO.SetPort(const Value: String);
begin
  FPort := Value;
  UpdateCType;
end;

procedure TKRPLCIO.UpdateCType;
begin
  case FCType of
    ctTCP: FPlc_con:=' /TCP'+FAddr;
    ctCOM: FPlc_con:=' /'+FPort;
  end;
end;

{ TKRPLCIOThread }

procedure TKRPLCIOThread.KRExecute;
begin
  if FDo then begin
    FRes:=KRCmd(FCmd,FTimeOut,FErrorLevel,'',FStdOut,FStdErr,FisOem);
    FDo:=false;
  end;
end;

end.
