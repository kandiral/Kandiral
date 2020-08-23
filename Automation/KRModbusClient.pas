(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRModbusClient                                                            *)
(*  Ver.: 23.08.2020                                                          *)
(*  https://kandiral.ru/delphi/krmodbusclient.pas.html                        *)
(*                                                                            *)
(******************************************************************************)
unit KRModbusClient;

interface

{$I '..\Includes\language.inc'}

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, System.AnsiStrings, System.Variants,
    System.StrUtils, Vcl.Forms, Winapi.Messages,
  {$ELSE}
    Windows, Classes, SysUtils, AnsiStrings, Variants, StrUtils, Forms, Messages,
  {$IFEND}
  KRModbus, KRModbusMaster, KRVariables, KRComponentCollection, Funcs, lgop,
  KRIniConfig, KRTypes, KRThreadQueue;

type
  TMBCError = integer;

const
  MBC_ERRORS_COUNT = 7;

  mbceAskLimit = TMBCError(65536);
  mbceFileNotFount = TMBCError(65537);
  mbceFileDeleteError = TMBCError(65538);
  mbceFileOpenForReadError = TMBCError(65539);
  mbceFileReadError = TMBCError(65540);
  mbceFileOpenForWriteError = TMBCError(65541);
  mbceFileWriteError = TMBCError(65542);

  MBC_ERRORS_MSG : array[0..MBC_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
    'Превышен лимит ожидания ответа от потока',
    'Файл не найден',
    'Не удалось удалить файл',
    'Не удается открыть файл для чтения',
    'Ошибка чтения файла',
    'Не удается открыть файл для записи',
    'Ошибка записи в файла'
{$ELSE}
    'Timeout receive response from the thread',
    'File not found',
    'Failed to delete file',
    'Can''t open file for reading',
    'File read error',
    'Can''t open file for writing',
    'Error writing to file'
{$ENDIF}
  );

type
  TMBReadFunction = (mbrfReadHoldingRegisters, mbrfReadInputRegisters);
  TMBWriteFunction = (mbwfWriteHoldingRegister, mbwfWriteHoldingRegisters);
  TMCVarType = (MCT_BYTE, MCT_WORD, MCT_DWORD, MCT_SMALLINT, MCT_INT,
    MCT_SINGLE, MCT_STRING, MCT_DWSTR, MCT_WD_STREAM, MCT_DW_STREAM, MCT_BT_STREAM,
    MCT_SINT_STREAM, MCT_INT64, MCT_FILE, MCT_ARRAY, MCT_UINT64, MCT_DOUBLE);

  TMBUpStreamProc = procedure of Object;
  TMBUpStreamProcDW = procedure(AFirst: boolean) of object;
  TMBSetStreamProcBT = procedure(AValue: byte) of object;
  TMBSetStreamProcWD = procedure(AValue: word) of object;
  TMBSetStreamProcSINT = procedure(AValue: Smallint) of object;
  TMBSetStreamProcDW = procedure(AFirst: boolean; AValue: Cardinal) of object;

  TKRMBRegFileInfo = record
    crc16: Word;
    size: Cardinal;
    dtCreation: TDateTime; (* Создание *)
    dtLastAccess: TDateTime; (* Доступ *)
    dtLastModification: TDateTime; (* Изменение *)
  end;
  PKRMBRegFileInfo = ^TKRMBRegFileInfo;

  TKRModbusClient = class;

  TKRMBRegister = class(TKRVariable)
  private
    __Data:TKRRegisters;
    hiByte: byte;
    _mb: TKRModbusMaster;
    _mbc: TKRModbusClient;
    FReadFunction: TMBReadFunction;
    FWriteFunction: TMBWriteFunction;
    FIndex: Word;
    FCount: byte;
    FError: integer;
    FTmpVal: Variant;
    FTmpArray: TKRLBuffer;
    FTmpArrayI: integer;
    FTmpWD0, FTmpWD1: Word;
    FTmpDW0, FTmpDW1: Cardinal;
    FDWSTR: AnsiString;
    FDWSTR_end: boolean;
    FDW_STREAM_First: byte;
    FCfgReadFunc: TKRIniCfgParam;
    FCfgWriteFunc: TKRIniCfgParam;
    FCfgRegisterIndex: TKRIniCfgParam;
    FReadPack: TKRBuffer;
    FReadPackLen,FReadPackRLen: byte;
    FReadPack2: TKRBuffer;
    FReadPackLen2,FReadPackRLen2: byte;
    FReadPack3: TKRBuffer;
    FReadPackLen3,FReadPackRLen3: byte;
    FReadPackFunc: TMBFunc;
    FMCVarType: TMCVarType;
    FStrLen: Byte;
    FStreamIndex: byte;
    FAskLimit, FAskCnt: byte;
    FUpIndex: integer;
    FString: AnsiString;
    FStringLen, FStringIndex: integer;
    FFileRegs: byte;
    FFileDo: byte;
    FFileName: String;
    FFilePC: TStream;
    FFileProcess: byte;
    FOnFile: TNotifyEvent;
    FFileErrors: integer;
    FFileDataSended: integer;
    FFileCMD: word;
    FFileStat: byte;
    FFileData: pointer;
    FArrayLen: word;
    FAUpdated: boolean;
    FHighByteFirst: boolean;
    FHighWordFirst: boolean;
    FHighDWordFirst: boolean;
    procedure _RCallBack(AError: integer; AData: Pointer);
    procedure _WCallBack(AError: integer; AData: Pointer);
    procedure _WCallBack0(AError: integer; AData: Pointer);
    procedure _WCBUInt64_1(AError: integer; AData: Pointer);
    procedure _WCBUInt64_2(AError: integer; AData: Pointer);
    procedure _WCBUInt64_3(AError: integer; AData: Pointer);
    procedure _StrCallBack(AError: integer; AData: Pointer);
    procedure _ArrayWCallBack(AError: integer; AData: Pointer);
    procedure DWSTR_up_wcb(AError: integer; AData: Pointer);
    procedure DWSTR_up_wcb0(AError: integer; AData: Pointer);
    procedure DWSTR_up(ACmd: Cardinal);
    procedure DWSTR_up_r;
    procedure DWSTR_up_rcb(AError: integer; AData: Pointer);
    procedure DWSTR_wr_wcb(AError: integer; AData: Pointer);
    procedure DWSTR_wr_wcb0(AError: integer; AData: Pointer);
    procedure DWSTR_wr(ACmd: Byte);
    procedure DWSTR_wr_r;
    procedure DWSTR_wr_rcb(AError: integer; AData: Pointer);
    procedure WD_STREAM_up;
    procedure WD_STREAM_up_wcb(AError: integer; AData: Pointer);
    procedure WD_STREAM_up_wcb0(AError: integer; AData: Pointer);
    procedure WD_STREAM_up_r;
    procedure WD_STREAM_up_rcb(AError: integer; AData: Pointer);
    procedure WD_STREAM_wr(AValue: Word);
    procedure WD_STREAM_wr_wcb(AError: integer; AData: Pointer);
    procedure WD_STREAM_wr_wcb0(AError: integer; AData: Pointer);
    procedure WD_STREAM_wr_r;
    procedure WD_STREAM_wr_rcb(AError: integer; AData: Pointer);
    procedure DW_STREAM_up(AFirst: boolean);
    procedure DW_STREAM_up_wcb(AError: integer; AData: Pointer);
    procedure DW_STREAM_up_wcb0(AError: integer; AData: Pointer);
    procedure DW_STREAM_up_r;
    procedure DW_STREAM_up_rcb(AError: integer; AData: Pointer);
    procedure DW_STREAM_wr(AFirst: boolean; AValue: Cardinal);
    procedure DW_STREAM_wr_wcb(AError: integer; AData: Pointer);
    procedure DW_STREAM_wr_wcb0(AError: integer; AData: Pointer);
    procedure DW_STREAM_wr_r;
    procedure DW_STREAM_wr_rcb(AError: integer; AData: Pointer);
    procedure BT_STREAM_up;
    procedure BT_STREAM_up_wcb(AError: integer; AData: Pointer);
    procedure BT_STREAM_up_r;
    procedure BT_STREAM_up_rcb(AError: integer; AData: Pointer);
    procedure BT_STREAM_wr(AValue: Byte);
    procedure BT_STREAM_wr_wcb(AError: integer; AData: Pointer);
    procedure BT_STREAM_wr_r;
    procedure BT_STREAM_wr_rcb(AError: integer; AData: Pointer);
    procedure SINT_STREAM_up;
    procedure SINT_STREAM_up_wcb(AError: integer; AData: Pointer);
    procedure SINT_STREAM_up_wcb0(AError: integer; AData: Pointer);
    procedure SINT_STREAM_up_r;
    procedure SINT_STREAM_up_rcb(AError: integer; AData: Pointer);
    procedure SINT_STREAM_wr(AValue: SmallInt);
    procedure SINT_STREAM_wr_wcb(AError: integer; AData: Pointer);
    procedure SINT_STREAM_wr_wcb0(AError: integer; AData: Pointer);
    procedure SINT_STREAM_wr_r;
    procedure SINT_STREAM_wr_rcb(AError: integer; AData: Pointer);
    procedure SetCfgReadFunc(const Value: TKRIniCfgParam);
    procedure SetCfgWriteFunc(const Value: TKRIniCfgParam);
    procedure SetCfgRegisterIndex(const Value: TKRIniCfgParam);
    procedure SetIndex(const Value: Word);
    procedure SetReadFunction(const Value: TMBReadFunction);
    procedure SetWriteFunction(const Value: TMBWriteFunction);
    procedure SetMCVarType(const Value: TMCVarType);
    procedure DoError(isUp: boolean);
    procedure SetFileRegs(const Value: byte);
    procedure FileSendName;
    procedure FileSendName_;
    procedure FileSendName_CB(AError: integer; AData: Pointer);
    procedure FileSendName_RES_CB(AError: integer; AData: Pointer);
    procedure FileRead_CB(AError: integer; AData: Pointer);
    procedure FileRead_RES_CB(AError: integer; AData: Pointer);
    procedure FileRead_RES_CB2(AError: integer; AData: Pointer);
    procedure FileWrite_;
    procedure FileWrite_CB(AError: integer; AData: Pointer);
    procedure FileWrite_RES_CB(AError: integer; AData: Pointer);
    procedure FileInfo_CB(AError: integer; AData: Pointer);
    procedure SetArrayLen(const Value: word);
  protected
    function GetError: integer;override;
    procedure SetType(const Value: TVarType);override;
    procedure SetValue_(const Value: Variant);override;
    procedure SetValue_c(AValue: Variant);override;
    function GetErrorMsg: String;override;
    procedure SetStrLen(const Value: Byte);
    procedure UpdateValue_;override;
    procedure UpdateValue_c(AVal: Variant);override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure _ParamChange(AParam: TKRIniCfgParam);override;
    procedure SetInterval(const AValue: Integer);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure UpdateReadPack;
    procedure UpdateValueAsync;
    procedure FileDelete(AFileName: String);
    procedure FileRead(AFileName: String; AFile: TStream);
    procedure FileWrite(AFileName: String; AFile: TStream);
    function FileDeleteAsinc(AFileName: String): boolean;
    function FileReadAsinc(AFileName: String; AFile: TStream): boolean;
    function FileWriteAsinc(AFileName: String; AFile: TStream): boolean;
    function FileInfoAsinc(AFileName: String; AFileInfo: PKRMBRegFileInfo): boolean;
    procedure FileInfo(AFileName: String; AFileInfo: PKRMBRegFileInfo);
    function FileSizeAsinc(AFileName: String; AFileSize: PDWORD): boolean;
    procedure FileSize(AFileName: String; AFileSize: PDWORD);
  published
    property ReadFunction: TMBReadFunction read FReadFunction write SetReadFunction default mbrfReadHoldingRegisters;
    property WriteFunction: TMBWriteFunction read FWriteFunction write SetWriteFunction default mbwfWriteHoldingRegister;
    property MCVarType: TMCVarType read FMCVarType write SetMCVarType;
    property RegisterIndex: Word read FIndex write SetIndex default 0;
    property CfgReadFunc: TKRIniCfgParam read FCfgReadFunc write SetCfgReadFunc;
    property CfgWriteFunc: TKRIniCfgParam read FCfgWriteFunc write SetCfgWriteFunc;
    property CfgRegisterIndex: TKRIniCfgParam read FCfgRegisterIndex write SetCfgRegisterIndex;
    property ArrayLen: word read FArrayLen write SetArrayLen default 8;
    property StrLen: Byte read FStrLen write SetStrLen default 4;
    property StreamIndex: byte read FStreamIndex write FStreamIndex default 0;
    property AskLimit: byte read FAskLimit write FAskLimit;
    property FileRegs: byte read FFileRegs write SetFileRegs default 5;
    property HighByteFirst: boolean read FHighByteFirst write FHighByteFirst default true;
    property HighWordFirst: boolean read FHighWordFirst write FHighWordFirst default false;
    property HighDWordFirst: boolean read FHighDWordFirst write FHighDWordFirst default false;
    property Interval default 0;
    property CfgInterval;
    property CfgValue;
    property UpdateType default vutAfter;
    property OnValUpdated;
    property OnError;
    property UpAftWrite;
    property UserError;
    property UserErrorMsg;
    property OnRuntimeError;
    property WaitForUpdates;
    property OnFile: TNotifyEvent read FOnFile write FOnFile;
  end;

  TKRMCStream = record
    i: integer;
    _stack: TKRThreadQueue;
  end;

  TKRMCWDStream = record
    val: Word;
    upProc: TMBUpStreamProc;
    setProc: TMBSetStreamProcWD;
  end;
  PKRMCWDStream=^TKRMCWDStream;

  TKRMCDWStream = record
    val: DWord;
    upProc: TMBUpStreamProcDW;
    setProc: TMBSetStreamProcDW;
  end;
  PKRMCDWStream=^TKRMCDWStream;

  TKRMCBTStream = record
    val: Byte;
    upProc: TMBUpStreamProc;
    setProc: TMBSetStreamProcBT;
  end;
  PKRMCBTStream=^TKRMCBTStream;

  TKRMCSINTStream = record
    val: Smallint;
    upProc: TMBUpStreamProc;
    setProc: TMBSetStreamProcSINT;
  end;
  PKRMCSINTStream=^TKRMCSINTStream;

  TKRModbusClient = class(TKRVariabels)
  private
    FAddres: byte;
    FMBType: TMBType;
    FModbus: TKRModBusMaster;

    FWDStreams,  FDWStreams, FBTStreams, FSINTStreams: array of TKRMCStream;
    FWDStreamsW, FDWStreamsW, FBTStreamsW, FSINTStreamsW: array of boolean;
    KRMBC_DW_lock, KRMBC_WD_lock, KRMBC_BT_lock, KRMBC_SINT_lock: TObject;

    function WDStreamAdd(ARegIndex: integer): integer;
    function DWStreamAdd(ARegIndex: integer): integer;
    function BTStreamAdd(ARegIndex: integer): integer;
    function SINTStreamAdd(ARegIndex: integer): integer;

    procedure WDStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
    procedure DWStreamUp(AIndex: integer; AProc: TMBUpStreamProcDW);
    procedure BTStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
    procedure SINTStreamUp(AIndex: integer; AProc: TMBUpStreamProc);

    procedure WDStreamSet(AIndex: integer; AProc: TMBSetStreamProcWD; AValue: Word);
    procedure DWStreamSet(AIndex: integer; AProc: TMBSetStreamProcDW; AValue: DWord);
    procedure BTStreamSet(AIndex: integer; AProc: TMBSetStreamProcBT; AValue: Byte);
    procedure SINTStreamSet(AIndex: integer; AProc: TMBSetStreamProcSINT; AValue: Smallint);

    procedure BTStreamDel(AIndex: integer);
    procedure WDStreamDel(AIndex: integer);
    procedure DWStreamDel(AIndex: integer);
    procedure SINTStreamDel(AIndex: integer);


    procedure SetModbus(const Value: TKRModbusMaster);
    procedure SetAddres(const Value: byte);
    procedure SetMBType(const Value: TMBType);
    function GetActive: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);override;
    procedure SetActive(const Value: boolean);override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    property Active: boolean read GetActive write SetActive;
    function AddVar(AName: String; AType: TMCVarType; AIndex: word; AInterval: Integer = 0):TKRMBRegister;
  published
    property Modbus: TKRModbusMaster read FModbus write SetModbus;
    property Addres: byte read FAddres write SetAddres default 0;
    property MBType: TMBType read FMBType write SetMBType default mbtTCP;
  end;

implementation

{ TModbusClient }

function TKRModbusClient.AddVar(AName: String; AType: TMCVarType; AIndex: word;
  AInterval: Integer): TKRMBRegister;
begin
  Result:=TKRMBRegister.Create(Self);
  Result.Name:=AName;
  Self.AddItem(Result);
  Result.MCVarType:=AType;
  Result.RegisterIndex:=AIndex;
  Result.Interval:=AInterval;
end;

procedure TKRModbusClient.AftAddItem(var AItem: TKRComponentCollectionItem);
begin
  TKRMBRegister(AItem)._mb:=FModbus;
  TKRMBRegister(AItem)._mbc:=Self;
  TKRMBRegister(AItem).UpdateReadPack;
  inherited;
end;

function TKRModbusClient.BTStreamAdd(ARegIndex: integer): integer;
var
  i: integer;
begin
  System.TMonitor.Enter(KRMBC_BT_lock);
  Result:=-1;
  for i := 0 to Length(FBTStreams)-1 do
    if FBTStreams[i].i=ARegIndex then begin
      Result:=i;
      break;
    end;
  if Result=-1 then begin
    Result:=Length(FBTStreams);
    SetLength(FBTStreams,Result+1);
    SetLength(FBTStreamsW,Result+1);
    FBTStreamsW[Result]:=False;
    FBTStreams[Result].i:=ARegIndex;
    FBTStreams[Result]._stack:=TKRThreadQueue.Create;
  end;
  System.TMonitor.Exit(KRMBC_BT_lock);
end;

procedure TKRModbusClient.BTStreamDel(AIndex: integer);
var
  _p: PKRMCBTStream;
begin
  System.TMonitor.Enter(KRMBC_BT_lock);
  FBTStreamsW[AIndex]:=false;
  if FBTStreams[AIndex]._stack.Count>0 then begin
    _p:=FBTStreams[AIndex]._stack.Pop;
    if Assigned(_p.upProc) then begin _p.upProc;FBTStreamsW[AIndex]:=true;end else
    if Assigned(_p.setProc) then begin _p.setProc(_p.val);FBTStreamsW[AIndex]:=true;end;
    Dispose(_p);
  end;
  System.TMonitor.Exit(KRMBC_BT_lock);
end;

procedure TKRModbusClient.BTStreamSet(AIndex: integer;
  AProc: TMBSetStreamProcBT; AValue: Byte);
var
  p: PKRMCBTStream;
begin
  System.TMonitor.Enter(KRMBC_BT_lock);
  if FBTStreamsW[AIndex] then begin
    New(p);
    p.upProc:=nil;
    p.setProc:=AProc;
    p.val:=AValue;
    FBTStreams[AIndex]._stack.Push(p);
  end else begin
    FBTStreamsW[AIndex]:=true;
    AProc(AValue);
  end;
  System.TMonitor.Exit(KRMBC_BT_lock);
end;

procedure TKRModbusClient.BTStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
var
  p: PKRMCBTStream;
begin
  System.TMonitor.Enter(KRMBC_BT_lock);
  if FBTStreamsW[AIndex] then begin
    New(p);
    p.upProc:=AProc;
    FBTStreams[AIndex]._stack.Push(p);
  end else begin
    FBTStreamsW[AIndex]:=true;
    AProc;
  end;
  System.TMonitor.Exit(KRMBC_BT_lock);
end;

constructor TKRModbusClient.Create(AOwner: TComponent);
begin
  inherited;
  KRMBC_DW_lock := TObject.Create;
  KRMBC_WD_lock := TObject.Create;
  KRMBC_BT_lock := TObject.Create;
  KRMBC_SINT_lock := TObject.Create;

  FAddres:=0;
  FMBType:=mbtTCP;
  inherited SetItemClass(TKRMBRegister);
end;

destructor TKRModbusClient.Destroy;
var
  i: integer;
  pWD: PKRMCWDStream;
  pDW: PKRMCDWStream;
  pBT: PKRMCBTStream;
  pSINT: PKRMCSINTStream;
begin
  KRMBC_DW_lock.Free;
  KRMBC_WD_lock.Free;
  KRMBC_BT_lock.Free;
  KRMBC_SINT_lock.Free;

  for i := 0 to Length(FWDStreams)-1 do
    while FWDStreams[i]._stack.Count>0 do begin
      pWD:=FWDStreams[i]._stack.Pop;
      Dispose(pWD);
    end;

  for i := 0 to Length(FDWStreams)-1 do
    while FDWStreams[i]._stack.Count>0 do begin
      pDW:=FDWStreams[i]._stack.Pop;
      Dispose(pDW);
    end;

  for i := 0 to Length(FBTStreams)-1 do
    while FBTStreams[i]._stack.Count>0 do begin
      pBT:=FBTStreams[i]._stack.Pop;
      Dispose(pBT);
    end;

  for i := 0 to Length(FSINTStreams)-1 do
    while FSINTStreams[i]._stack.Count>0 do begin
      pSINT:=FSINTStreams[i]._stack.Pop;
      Dispose(pSINT);
    end;

  inherited;
end;

function TKRModbusClient.DWStreamAdd(ARegIndex: integer): integer;
var
  i: integer;
begin
  System.TMonitor.Enter(KRMBC_DW_lock);
  Result:=-1;
  for i := 0 to Length(FDWStreams)-1 do
    if FDWStreams[i].i=ARegIndex then begin
      Result:=i;
      break;
    end;
  if Result=-1 then begin
    Result:=Length(FDWStreams);
    SetLength(FDWStreams,Result+1);
    SetLength(FDWStreamsW,Result+1);
    FDWStreamsW[Result]:=False;
    FDWStreams[Result].i:=ARegIndex;
    FDWStreams[Result]._stack:=TKRThreadQueue.Create;
  end;
  System.TMonitor.Exit(KRMBC_DW_lock);
end;

procedure TKRModbusClient.DWStreamDel(AIndex: integer);
var
  _p: PKRMCDWStream;
begin
  System.TMonitor.Enter(KRMBC_DW_lock);
  FDWStreamsW[AIndex]:=false;
  if FDWStreams[AIndex]._stack.Count>0 then begin
    _p:=FDWStreams[AIndex]._stack.Pop;
    if Assigned(_p.upProc) then begin _p.upProc(true);FDWStreamsW[AIndex]:=true;end else
    if Assigned(_p.setProc) then begin _p.setProc(true,_p.val);FDWStreamsW[AIndex]:=true;end;
    Dispose(_p);
  end;
  System.TMonitor.Exit(KRMBC_DW_lock);
end;

procedure TKRModbusClient.DWStreamSet(AIndex: integer;
  AProc: TMBSetStreamProcDW; AValue: DWord);
var
  p: PKRMCDWStream;
begin
  System.TMonitor.Enter(KRMBC_DW_lock);
  if FDWStreamsW[AIndex] then begin
    New(p);
    p.upProc:=nil;
    p.setProc:=AProc;
    p.val:=AValue;
    FDWStreams[AIndex]._stack.Push(p);
  end else begin
    FDWStreamsW[AIndex]:=true;
    AProc(true,AValue);
  end;
  System.TMonitor.Exit(KRMBC_DW_lock);
end;

procedure TKRModbusClient.DWStreamUp(AIndex: integer; AProc: TMBUpStreamProcDW);
var
  p: PKRMCDWStream;
begin
  System.TMonitor.Enter(KRMBC_DW_lock);
  if FDWStreamsW[AIndex] then begin
    New(p);
    p.upProc:=AProc;
    FDWStreams[AIndex]._stack.Push(p);
  end else begin
    FDWStreamsW[AIndex]:=true;
    AProc(true);
  end;
  System.TMonitor.Exit(KRMBC_DW_lock);
end;

function TKRModbusClient.GetActive: boolean;
begin
  result:=Inherited Active;
end;

procedure TKRModbusClient.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FModbus)then begin
      FModbus:= nil;
      for i := 0 to ItemsCount-1 do begin
        TKRMBRegister(Items[i])._mb:=nil;
        TKRMBRegister(Items[i]).FReadPackLen:=0;
      end;
    end;
end;

procedure TKRModbusClient.SetActive(const Value: boolean);
begin
  if GetActive=Value then exit;
  
  if(Value)and(not Assigned(FModbus))then exit;
  inherited;
end;

procedure TKRModbusClient.SetAddres(const Value: byte);
var
  i: integer;
begin
  FAddres := Value;
  for i := 0 to ItemsCount-1 do TKRMBRegister(Items[i]).UpdateReadPack
end;

procedure TKRModbusClient.SetMBType(const Value: TMBType);
var
  i: integer;
begin
  FMBType := Value;
  for i := 0 to ItemsCount-1 do TKRMBRegister(Items[i]).UpdateReadPack
end;

procedure TKRModbusClient.SetModbus(const Value: TKRModbusMaster);
var
  i: integer;
  b: boolean;
begin
  if FModbus<>Value then begin
    b:=Active;
    Active:=false;
    if Assigned(FModbus) then FModbus.RemoveFreeNotification(Self);
    FModbus := Value;
    if Assigned(FModbus) then begin
      FModbus.FreeNotification(Self);
      for i := 0 to ItemsCount-1 do begin
        TKRMBRegister(Items[i])._mb:=FModbus;
        TKRMBRegister(Items[i]).UpdateReadPack
      end;
      if b then Active:=true;
    end;
  end;
end;

function TKRModbusClient.SINTStreamAdd(ARegIndex: integer): integer;
var
  i: integer;
begin
  System.TMonitor.Enter(KRMBC_SINT_lock);
  Result:=-1;
  for i := 0 to Length(FSINTStreams)-1 do
    if FSINTStreams[i].i=ARegIndex then begin
      Result:=i;
      break;
    end;
  if Result=-1 then begin
    Result:=Length(FSINTStreams);
    SetLength(FSINTStreams,Result+1);
    SetLength(FSINTStreamsW,Result+1);
    FSINTStreamsW[Result]:=False;
    FSINTStreams[Result].i:=ARegIndex;
    FSINTStreams[Result]._stack:=TKRThreadQueue.Create;
  end;
  System.TMonitor.Exit(KRMBC_SINT_lock);
end;

procedure TKRModbusClient.SINTStreamDel(AIndex: integer);
var
  _p: PKRMCSINTStream;
begin
  System.TMonitor.Enter(KRMBC_SINT_lock);
  FSINTStreamsW[AIndex]:=false;
  if FSINTStreams[AIndex]._stack.Count>0 then begin
    _p:=FSINTStreams[AIndex]._stack.Pop;
    if Assigned(_p.upProc) then begin _p.upProc;FSINTStreamsW[AIndex]:=true;end else
    if Assigned(_p.setProc) then begin _p.setProc(_p.val);FSINTStreamsW[AIndex]:=true;end;
    Dispose(_p);
  end;
  System.TMonitor.Exit(KRMBC_SINT_lock);
end;

procedure TKRModbusClient.SINTStreamSet(AIndex: integer;
  AProc: TMBSetStreamProcSINT; AValue: Smallint);
var
  p: PKRMCSINTStream;
begin
  System.TMonitor.Enter(KRMBC_SINT_lock);
  if FSINTStreamsW[AIndex] then begin
    New(p);
    p.upProc:=nil;
    p.setProc:=AProc;
    p.val:=AValue;
    FSINTStreams[AIndex]._stack.Push(p);
  end else begin
    FSINTStreamsW[AIndex]:=true;
    AProc(AValue);
  end;
  System.TMonitor.Exit(KRMBC_SINT_lock);
end;

procedure TKRModbusClient.SINTStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
var
  p: PKRMCSINTStream;
begin
  System.TMonitor.Enter(KRMBC_SINT_lock);
  if FSINTStreamsW[AIndex] then begin
    New(p);
    p.upProc:=AProc;
    FSINTStreams[AIndex]._stack.Push(p);
  end else begin
    FSINTStreamsW[AIndex]:=true;
    AProc;
  end;
  System.TMonitor.Exit(KRMBC_SINT_lock);
end;

function TKRModbusClient.WDStreamAdd(ARegIndex: integer): integer;
var
  i: integer;
begin
  System.TMonitor.Enter(KRMBC_WD_lock);
  Result:=-1;
  for i := 0 to Length(FWDStreams)-1 do
    if FWDStreams[i].i=ARegIndex then begin
      Result:=i;
      break;
    end;
  if Result=-1 then begin
    Result:=Length(FWDStreams);
    SetLength(FWDStreams,Result+1);
    SetLength(FWDStreamsW,Result+1);
    FWDStreamsW[Result]:=false;
    FWDStreams[Result].i:=ARegIndex;
    FWDStreams[Result]._stack:=TKRThreadQueue.Create;
  end;
  System.TMonitor.Exit(KRMBC_WD_lock);
end;

procedure TKRModbusClient.WDStreamDel(AIndex: integer);
var
  _p: PKRMCWDStream;
begin
  System.TMonitor.Enter(KRMBC_WD_lock);
  FWDStreamsW[AIndex]:=false;
  if FWDStreams[AIndex]._stack.Count>0 then begin
    _p:=FWDStreams[AIndex]._stack.Pop;
    if Assigned(_p.upProc) then begin _p.upProc;FWDStreamsW[AIndex]:=true;end else
    if Assigned(_p.setProc) then begin _p.setProc(_p.val);FWDStreamsW[AIndex]:=true;end;
    Dispose(_p);
  end;
  System.TMonitor.Exit(KRMBC_WD_lock);
end;

procedure TKRModbusClient.WDStreamSet(AIndex: integer;
  AProc: TMBSetStreamProcWD; AValue: Word);
var
  p: PKRMCWDStream;
begin
  System.TMonitor.Enter(KRMBC_WD_lock);
  if FWDStreamsW[AIndex] then begin
    New(p);
    p.upProc:=nil;
    p.setProc:=AProc;
    p.val:=AValue;
    FWDStreams[AIndex]._stack.Push(p);
  end else begin
    FWDStreamsW[AIndex]:=true;
    AProc(AValue);
  end;
  System.TMonitor.Exit(KRMBC_WD_lock);
end;

procedure TKRModbusClient.WDStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
var
  p: PKRMCWDStream;
begin
  System.TMonitor.Enter(KRMBC_WD_lock);
  if FWDStreamsW[AIndex] then begin
    New(p);
    p.upProc:=AProc;
    FWDStreams[AIndex]._stack.Push(p);
  end else begin
    FWDStreamsW[AIndex]:=true;
    AProc;
  end;
  System.TMonitor.Exit(KRMBC_WD_lock);
end;

{ TKRMBRegister }

procedure TKRMBRegister.BT_STREAM_up;
var
  wd: Word;
  _Data: TKRRegisters;
begin
  wd:=BytesToWord(0,FStreamIndex or $40);
  case FWriteFunction of
    mbwfWriteHoldingRegister:
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd,BT_STREAM_up_wcb);
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,1);
      _Data[0]:=wd;
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,BT_STREAM_up_wcb);
    end;
  end;
end;

procedure TKRMBRegister.BT_STREAM_up_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,BT_STREAM_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.BT_STREAM_up_rcb(AError: integer; AData: Pointer);
var
  wd: Word;
begin
  FError:=AError;
  if AError=0 then begin
    wd:=MBRegsToWord(PKRRegisters(AData)^);
    if wd shr 14 = 0 then begin
      UpdateValue_c(Byte(wd));
      _mbc.BTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then BT_STREAM_up_r else begin
        _mbc.BTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(true);
      end;
    end;
  end else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.BT_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then BT_STREAM_up_r
  else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.BT_STREAM_wr(AValue: Byte);
var
  wd: Word;
  _Data: TKRRegisters;
begin
  wd:=BytesToWord(AValue,FStreamIndex or $80);
  case FWriteFunction of
    mbwfWriteHoldingRegister:
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd,BT_STREAM_wr_wcb);
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,1);
      _Data[0]:=wd;
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,BT_STREAM_wr_wcb);
    end;
  end;
end;

procedure TKRMBRegister.BT_STREAM_wr_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,BT_STREAM_wr_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.BT_STREAM_wr_rcb(AError: integer; AData: Pointer);
var
  wd: Word;
begin
  FError:=AError;
  if AError=0 then begin
    wd:=MBRegsToWord(PKRRegisters(AData)^);
    if wd shr 14 = 0 then begin
      SetValue_c(FTmpVal);
      _mbc.BTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then BT_STREAM_wr_r else begin
        _mbc.BTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(false);
      end;
    end;
  end else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.BT_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then BT_STREAM_wr_r
  else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError(false);
  end;
end;

constructor TKRMBRegister.Create(AOwner: TComponent);
begin
  inherited;

//  tLog:='';

  FIndex:=0;
  FError:=0;
  FCount:=1;
  FReadPackLen:=0;
  SetStrLen(4);
  FReadFunction:=mbrfReadHoldingRegisters;
  FWriteFunction:=mbwfWriteHoldingRegister;
  FAskLimit:=5;
  FFileRegs:=5;
  FFileDo:=0;
  FFileProcess:=0;
  FTmpArray.len:=0;
  FArrayLen:=8;
  FAUpdated:=true;
  FHighByteFirst:=true;
  FHighWordFirst:=false;
  FHighDWordFirst:=false;
  hiByte:=0;
  inherited SetType(VT_WORD);
end;

destructor TKRMBRegister.Destroy;
begin
  if Assigned(FCfgReadFunc) then FCfgReadFunc.DelMon(Self);
  if Assigned(FCfgWriteFunc) then FCfgWriteFunc.DelMon(Self);
  if Assigned(FCfgRegisterIndex) then FCfgRegisterIndex.DelMon(Self);
  inherited;
end;

procedure TKRMBRegister.DoError(isUp: boolean);
begin
  inherited DoError(isUp);
end;

procedure TKRMBRegister.DWSTR_up(ACmd: Cardinal);
var
  wd0:word;
  _Data:TKRRegisters;
begin
//tLog:=tLog+#13#10+'DWSTR_up cmd='+IntToStr(ACmd);

  if ACmd=$3000000 then FDWSTR:='';
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(ACmd,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,DWSTR_up_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(ACmd,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,DWSTR_up_wcb);
    end;
  end;
end;

procedure TKRMBRegister.DWSTR_up_r;
begin
//tLog:=tLog+#13#10+'DWSTR_up_r';
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,DWSTR_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.DWSTR_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
  wd0,wd1: word;
  bt0,bt1,bt2,bt3: byte;
begin
//tLog:=tLog+#13#10+'DWSTR_up_rcb';
  FError:=AError;
  if AError=0 then begin
//tLog:=tLog+#13#10+'DWSTR_up_rcb AError=0';
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
//tLog:=tLog+#13#10+'DWSTR_up_rcb  dw shr 24 = 0';
      FAskCnt:=0;
      WordsFromDWord(dw,wd0,wd1);
      BytesFromWord(wd0,bt0,bt1);
      BytesFromWord(wd1,bt2,bt3);
      if bt0>0 then begin
        FDWSTR:=FDWSTR+AnsiChar(bt0);
        if bt1>0 then begin
          FDWSTR:=FDWSTR+AnsiChar(bt1);
          if bt2>0 then begin
            FDWSTR:=FDWSTR+AnsiChar(bt2);
//tLog:=tLog+#13#10+'DWSTR_up_rcb  FDWSTR='+FDWSTR;
            DWSTR_up($4000000);
          end else UpdateValue_c(StringToWideString(FDWSTR,1251));
        end else UpdateValue_c(StringToWideString(FDWSTR,1251));
      end else UpdateValue_c(StringToWideString(FDWSTR,1251));
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DWSTR_up_r else begin
        FError:=mbceAskLimit;
        DoError(true);
      end;
    end;
  end else DoError(true);
end;

procedure TKRMBRegister.DWSTR_up_wcb(AError: integer; AData: Pointer);
begin
//tLog:=tLog+#13#10+'DWSTR_up_wcb';
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DWSTR_up_r
  else DoError(true);
end;

procedure TKRMBRegister.DWSTR_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DWSTR_up_wcb)
  else DoError(true);
end;

procedure TKRMBRegister.DWSTR_wr(ACmd: byte);
var
  wd0: word;
  dw: Cardinal;
  bt0,bt1,bt2: byte;
  _Data: TKRRegisters;
begin
//tLog:=tLog+#13#10+'DWSTR_wr Cmd='+IntToStr(ACmd)+' FDWSTR='+FDWSTR;
  bt1:=0;bt2:=0;
  if Length(FDWSTR)>0 then begin
    bt0:=Ord(FDWSTR[1]);
    FDWSTR:=AnsiRightStr(FDWSTR,Length(FDWSTR)-1);
    if Length(FDWSTR)>0 then begin
      bt1:=Ord(FDWSTR[1]);
      FDWSTR:=AnsiRightStr(FDWSTR,Length(FDWSTR)-1);
      if Length(FDWSTR)>0 then begin
        bt2:=Ord(FDWSTR[1]);
        FDWSTR:=AnsiRightStr(FDWSTR,Length(FDWSTR)-1);
      end else begin bt2:=0;FDWSTR_end:=true;end;
    end else begin bt1:=0;FDWSTR_end:=true;end;
  end else begin bt0:=0;FDWSTR_end:=true;end;
  dw:=WordsToDWord(BytesToWord(bt0,bt1),BytesToWord(bt2,ACmd));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,DWSTR_wr_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,DWSTR_wr_wcb);
    end;
  end;
end;

procedure TKRMBRegister.DWSTR_wr_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,DWSTR_wr_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.DWSTR_wr_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      FAskCnt:=0;
      if FDWSTR_end then begin
        SetValue_c(FTmpVal)
      end else
        DWSTR_wr(2);
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError(false);
      end else DWSTR_wr_r;
    end;
  end else DoError(false);
end;

procedure TKRMBRegister.DWSTR_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DWSTR_wr_r
  else DoError(false);
end;

procedure TKRMBRegister.DWSTR_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DWSTR_wr_wcb)
  else DoError(false);
end;

procedure TKRMBRegister.DW_STREAM_up(AFirst: boolean);
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
  if AFirst then
    FDW_STREAM_First:=1
  else FDW_STREAM_First:=2;
  dw:=WordsToDWord(0,BytesToWord(FStreamIndex,FDW_STREAM_First));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,DW_STREAM_up_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,DW_STREAM_up_wcb);
    end;
  end;
end;

procedure TKRMBRegister.DW_STREAM_up_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,DW_STREAM_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.DW_STREAM_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      if FDW_STREAM_First=1 then begin
        FTmpWD1:=Word(dw);
        DW_STREAM_up(false);
      end else begin
        dw:=WordsToDWORD(FTmpWD1,Word(dw));
        UpdateValue_c(dw);
        _mbc.DWStreamDel(FUpIndex);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DW_STREAM_up_r else begin
        _mbc.DWStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(true);
      end;
    end;
  end else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.DW_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DW_STREAM_up_r
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.DW_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DW_STREAM_up_wcb)
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr(AFirst: boolean; AValue: Cardinal);
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
  if AFirst then begin
    FTmpDW0:=AValue;
    wd0:=Word(FTmpDW0);
    FDW_STREAM_First:=3
  end else begin
    wd0:=Word(FTmpDW0 shr 16);
    FDW_STREAM_First:=4;
  end;
  dw:=WordsToDWord(wd0,BytesToWord(FStreamIndex,FDW_STREAM_First));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,DW_STREAM_wr_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,DW_STREAM_wr_wcb);
    end;
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,DW_STREAM_wr_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.DW_STREAM_wr_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      if FDW_STREAM_First=3 then DW_STREAM_wr(false,FTmpDW0)
      else begin
        SetValue_c(FTmpVal);
        _mbc.DWStreamDel(FUpIndex);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DW_STREAM_wr_r else begin
        _mbc.DWStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(false);
      end;
    end;
  end else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DW_STREAM_wr_r
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DW_STREAM_wr_wcb)
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.FileDelete(AFileName: String);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFileDo:=1;
  UpdateValue;
end;

function TKRMBRegister.FileDeleteAsinc(AFileName: String): boolean;
begin
  FileDelete(AFileName);
  while FFileProcess=1 do Application.ProcessMessages;
  Result:=FFileProcess=0;
end;

procedure TKRMBRegister.FileInfo(AFileName: String; AFileInfo: PKRMBRegFileInfo);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFileData:=AFileInfo;
  FFileDo:=4;
  UpdateValue;
end;

function TKRMBRegister.FileInfoAsinc(AFileName: String; AFileInfo: PKRMBRegFileInfo): boolean;
begin
  FileInfo(AFileName,AFileInfo);
  while FFileProcess=1 do Application.ProcessMessages;
  Result:=FFileProcess=0;
end;

procedure TKRMBRegister.FileInfo_CB(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then begin
    FAskCnt:=0;
    PKRMBRegFileInfo(FFileData)^.crc16:=PKRRegisters(AData)^[0];
		PKRMBRegFileInfo(FFileData)^.Size:=Cardinal(PKRRegisters(AData)^[1]) or (Cardinal(PKRRegisters(AData)^[2]) shl 16);
    PKRMBRegFileInfo(FFileData)^.dtCreation:=MKTimeToDateTime(Cardinal(PKRRegisters(AData)^[3]) or (Cardinal(PKRRegisters(AData)^[4]) shl 16));
    PKRMBRegFileInfo(FFileData)^.dtLastAccess:=MKTimeToDateTime(Cardinal(PKRRegisters(AData)^[5]) or (Cardinal(PKRRegisters(AData)^[6]) shl 16));
    PKRMBRegFileInfo(FFileData)^.dtLastModification:=MKTimeToDateTime(Cardinal(PKRRegisters(AData)^[7]) or (Cardinal(PKRRegisters(AData)^[8]) shl 16));
		UpdateValue_c(NULL);
		FFileProcess:=0;
  end else begin
    inc(FAskCnt);
    if FAskCnt>=FAskLimit then begin
      FError:=mbceAskLimit;
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.ReadHoldingRegisters(_mbc.MBType,_mbc.FAddres,FIndex+1, 3, FileInfo_CB)
  end;
end;

procedure TKRMBRegister.FileRead(AFileName: String; AFile: TStream);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFilePC:=AFile;
  FFileDo:=2;
  FFilePC.Size:=0;
  UpdateValue;
end;

function TKRMBRegister.FileReadAsinc(AFileName: String;
  AFile: TStream): boolean;
begin
  FileRead(AFileName,AFile);
  while FFileProcess=1 do Application.ProcessMessages;
  Result:=FFileProcess=0;
end;

procedure TKRMBRegister.FileRead_CB(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then begin
    FAskCnt:=0;
    _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileRead_RES_CB,FReadPackRLen2)
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileRead_CB);
  end;
end;

procedure TKRMBRegister.FileRead_RES_CB(AError: integer; AData: Pointer);
var wd: word;
begin
  FError:=AError;
  if AError=0 then begin
    wd:=MBRegsToWORD(PKRRegisters(AData)^);
    if(wd shr 8 = $FD)then begin
      if wd and $FF = $FF then begin
        FError:=mbceFileReadError;
        FFileProcess:=2;
        DoError(false);
        if Assigned(FOnFile) then FOnFile(Self);
      end else begin
        FFileStat:=wd and $FF;
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack3,FReadPackLen3,FileRead_RES_CB2,FReadPackRLen3)
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError(false);
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileRead_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileRead_RES_CB,FReadPackRLen2)
  end;
end;

procedure TKRMBRegister.FileRead_RES_CB2(AError: integer; AData: Pointer);
var
  bt: byte;
  I: Integer;
  buf: TKRBuffer;
begin
  FError:=AError;
  if AError=0 then begin
    bt:=FFileStat and $7F;
    for I := 0 to bt-1 do begin
      if Length(PKRRegisters(AData)^)<=i div 2 then break;
      if i and 1 = 1 then
        buf[i]:=PKRRegisters(AData)^[i div 2] shr 8
      else
        buf[i]:=PKRRegisters(AData)^[i div 2];
    end;
    FFilePC.Size:=FFilePC.Size+bt;
    FFilePC.Position:=FFilePC.Size-bt;
    FFilePC.WriteBuffer(buf,bt);
    if GetBit(FFileStat,7) then begin
      UpdateValue_c(NULL);
      FFileProcess:=0;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileRead_CB);
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack3,FReadPackLen3,FileRead_RES_CB2,FReadPackRLen3)
  end;
end;

procedure TKRMBRegister.FileSendName;
var _do: byte;
begin
  if FFileDo=0 then begin
    FError:=mbceFileNotFount;
    DoError(false);
    exit;
  end;
  _do:=FFileDo;
  FFileDo:=0;
  FFileCmd:=$0100 or _do;
  FFileCmd:=SetBit(FFileCmd,7);
  FFileErrors:=0;
  FFileDataSended:=0;
  FileSendName_;
end;

procedure TKRMBRegister.FileSendName_;
var
  bt: byte;
begin
  SetLength(__Data,1);
  __Data[0]:=FFileCmd;
  while true do begin
    inc(FFileDataSended);
    if FFileDataSended>Length(FFileName) then bt:=0
    else bt:=Ord(FFileName[FFileDataSended]);
    if FFileDataSended and 1 = 1 then begin
      SetLength(__Data,Length(__Data)+1);
      __Data[Length(__Data)-1]:=word(bt) shl 8;
    end else begin
      __Data[Length(__Data)-1]:=__Data[Length(__Data)-1] or bt;
      if Length(__Data)=FFileRegs+1 then break;
    end;
    if(bt=0)then break;
  end;
  FFileCmd:=ClearBit(FFileCmd,7);
  _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileSendName_CB);
end;

procedure TKRMBRegister.FileSendName_CB(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then begin
    FAskCnt:=0;
    _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileSendName_RES_CB,FReadPackRLen2)
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileSendName_CB);
  end;
end;

procedure TKRMBRegister.FileSendName_RES_CB(AError: integer; AData: Pointer);
var wd: word;
begin
  FError:=AError;
  if AError=0 then begin
    wd:=MBRegsToWORD(PKRRegisters(AData)^);
    if(wd shr 8 = $FE)then begin
      if wd and $FF = $FF then FileSendName_ else begin
        wd:=wd and $FF;
        if wd=0 then begin
          case (FFileCmd and $7F) of
          1: begin
              UpdateValue_c(NULL);
              FFileProcess:=0;
              if Assigned(FOnFile) then FOnFile(Self);
            end;
          2: begin
              SetLength(__Data,1);
              __Data[0]:=$0200;
              _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileRead_CB);
            end;
          3: begin
              SetLength(__Data,FFileRegs+1);
              FileWrite_;
            end;
          4: begin
              FAskCnt:=0;
              _mb.ReadHoldingRegisters(_mbc.MBType,_mbc.FAddres,FIndex+1, 3, FileInfo_CB)
            end;
          5: begin
               PDWORD(FFileData)^:=Cardinal(PKRRegisters(AData)^[1]) or (Cardinal(PKRRegisters(AData)^[2]) shl 16);
              UpdateValue_c(NULL);
              FFileProcess:=0;
              if Assigned(FOnFile) then FOnFile(Self);
            end;
          end;
        end else begin
          case wd of
          1: FError:=mbceFileNotFount;
          2: FError:=mbceFileDeleteError;
          3: FError:=mbceFileOpenForReadError;
          4: FError:=mbceFileOpenForWriteError;
          end;
          FFileProcess:=2;
          DoError(false);
          if Assigned(FOnFile) then FOnFile(Self);
        end;
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError(false);
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileSendName_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileSendName_RES_CB,FReadPackRLen2)
  end;
end;

procedure TKRMBRegister.FileSize(AFileName: String; AFileSize: PDWORD);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFileData:=AFileSize;
  FFileDo:=5;
  UpdateValue;
end;

function TKRMBRegister.FileSizeAsinc(AFileName: String;
  AFileSize: PDWORD): boolean;
begin
  FileSize(AFileName,AFileSize);
  while FFileProcess=1 do Application.ProcessMessages;
  Result:=FFileProcess=0;
end;

procedure TKRMBRegister.FileWrite(AFileName: String; AFile: TStream);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFilePC:=AFile;
  FFilePC.Position:=0;
  FFileDo:=3;
  UpdateValue;
end;

function TKRMBRegister.FileWriteAsinc(AFileName: String;
  AFile: TStream): boolean;
begin
  FileWrite(AFileName,AFile);
  while FFileProcess=1 do Application.ProcessMessages;
  Result:=FFileProcess=0;
end;

procedure TKRMBRegister.FileWrite_;
var buf: TKRBuffer;
  i, n: integer;
begin
  n:=FFilePC.Read(buf,FFileRegs+FFileRegs);
  for I := 0 to n-1 do
    if i and 1 = 1 then __Data[1 + (i div 2)]:=__Data[1 + (i div 2)] or (word(buf[i])shl 8)
    else __Data[1 + (i div 2)]:=buf[i];
  __Data[0]:=$0300 or n;
  __Data[0]:=SetBitTo(__Data[0],7,FFilePC.Position=FFilePC.Size);
  _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileWrite_CB);
end;

procedure TKRMBRegister.FileWrite_CB(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then begin
    FAskCnt:=0;
    _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileWrite_RES_CB,FReadPackRLen2)
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileWrite_CB);
  end;
end;

procedure TKRMBRegister.FileWrite_RES_CB(AError: integer; AData: Pointer);
var wd: word;
begin
  FError:=AError;
  if AError=0 then begin
    wd:=MBRegsToWORD(PKRRegisters(AData)^);
    if(wd shr 8 = $FC)then begin
      if wd and $FF = 0 then begin
        if GetBit(__Data[0],7) then begin
          UpdateValue_c(NULL);
          FFileProcess:=0;
          if Assigned(FOnFile) then FOnFile(Self);
        end else FileWrite_;
      end else begin
        FError:=mbceFileWriteError;
        FFileProcess:=2;
        DoError(false);
        if Assigned(FOnFile) then FOnFile(Self);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError(false);
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileWrite_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError(false);
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileWrite_RES_CB,FReadPackRLen2)
  end;
end;

function TKRMBRegister.GetError: integer;
begin
  if UserError then Result:=-2147483648 else Result:=FError;
end;

function TKRMBRegister.GetErrorMsg: String;
var
  s: String;
begin
  Result:='';
  if UserError then Result:=UserErrorMsg else
  if(FError>65535)and(FError-65536<MBC_ERRORS_COUNT)then begin
    Result:=MBC_ERRORS_MSG[FError-65536];
  end else if(FError<>0)and(Assigned(_mb))then begin
    Result:=_mb.ErrorMsg(FError,s);
    Result:=s+': '+Result;
  end;
end;

procedure TKRMBRegister.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgWriteFunc)then FCfgWriteFunc:=nil;
    if (AComponent = FCfgReadFunc)then FCfgReadFunc:=nil;
end;

procedure TKRMBRegister.SetArrayLen(const Value: word);
begin
  FArrayLen := Value;
  FCount:=((FArrayLen - 1) div 2) + 1;
  UpdateReadPack;
end;

procedure TKRMBRegister.SetCfgReadFunc(const Value: TKRIniCfgParam);
begin
  if FCfgReadFunc<>Value then begin
    if Assigned(FCfgReadFunc) then FCfgReadFunc.DelMon(Self);
    FCfgReadFunc:=Value;
    if Assigned(FCfgReadFunc) then begin
      FCfgReadFunc.FreeNotification(Self);
      FCfgReadFunc.AddMon(Self);
      _ParamChange(FCfgReadFunc);
    end;
  end;
end;

procedure TKRMBRegister.SetCfgRegisterIndex(const Value: TKRIniCfgParam);
begin
  if FCfgRegisterIndex<>Value then begin
    if Assigned(FCfgRegisterIndex) then FCfgRegisterIndex.DelMon(Self);
    FCfgRegisterIndex:=Value;
    if Assigned(FCfgRegisterIndex) then begin
      FCfgRegisterIndex.FreeNotification(Self);
      FCfgRegisterIndex.AddMon(Self);
      _ParamChange(FCfgRegisterIndex);
    end;
  end;
end;

procedure TKRMBRegister.SetCfgWriteFunc(const Value: TKRIniCfgParam);
begin
  if FCfgWriteFunc<>Value then begin
    if Assigned(FCfgWriteFunc) then FCfgWriteFunc.DelMon(Self);
    FCfgWriteFunc:=Value;
    if Assigned(FCfgWriteFunc) then begin
      FCfgWriteFunc.FreeNotification(Self);
      FCfgWriteFunc.AddMon(Self);
      _ParamChange(FCfgWriteFunc);
    end;
  end;
end;

procedure TKRMBRegister.SetFileRegs(const Value: byte);
begin
  if(Value<2)or(Value>100) then exit;
  FFileRegs := Value;
  if FMCVarType=MCT_FILE then begin
    FCount:=Value+1;
    UpdateReadPack;
  end;
end;

procedure TKRMBRegister.SetIndex(const Value: Word);
begin
  if FIndex<>Value then begin
    FIndex := Value;
    case FMCVarType of
      MCT_WD_STREAM: FUpIndex:=_mbc.WDStreamAdd(FIndex);
      MCT_DW_STREAM: FUpIndex:=_mbc.DWStreamAdd(FIndex);
      MCT_BT_STREAM: FUpIndex:=_mbc.BTStreamAdd(FIndex);
      MCT_SINT_STREAM: FUpIndex:=_mbc.SINTStreamAdd(FIndex);
    end;
    if Assigned(FCfgRegisterIndex) then FCfgRegisterIndex.Value:=FIndex;
    UpdateReadPack;
  end;
end;

procedure TKRMBRegister.SetInterval(const AValue: Integer);
begin
  if FMCVarType<>MCT_FILE then inherited SetInterval(AValue);
end;

procedure TKRMBRegister.SetMCVarType(const Value: TMCVarType);
begin
  FMCVarType := Value;
  case FMCVarType of
    MCT_BYTE: begin inherited SetType(VT_BYTE);FCount:=1;end;
    MCT_WORD: begin inherited SetType(VT_WORD);FCount:=1;end;
    MCT_SMALLINT: begin inherited SetType(VT_SMALLINT);FCount:=1;end;
    MCT_DWORD: begin inherited SetType(VT_DWORD);FCount:=2;end;
    MCT_INT: begin inherited SetType(VT_INT);FCount:=2;end;
    MCT_SINGLE: begin inherited SetType(VT_SINGLE);FCount:=2;end;
    MCT_UINT64: begin inherited SetType(VT_UINT64);FCount:=4;end;
    MCT_INT64: begin inherited SetType(VT_INT64);FCount:=4;end;
    MCT_DOUBLE: begin inherited SetType(VT_DOUBLE);FCount:=4;end;
    MCT_STRING: begin inherited SetType(VT_STRING);SetStrLen(FStrLen);end;
    MCT_ARRAY: begin
      inherited SetType(VT_WORD);
      FTmpVal:=NativeInt(@FTmpArray);
      FCount:=((FArrayLen - 1) div 2) + 1;
    end;
    MCT_FILE: begin
      inherited SetInterval(0);
      inherited SetType(VT_WORD);
      SetWriteFunction(mbwfWriteHoldingRegisters);
      SetFileRegs(FFileRegs);
    end;
    MCT_DWSTR: begin inherited SetType(VT_STRING);FCount:=2;end;
    MCT_WD_STREAM: begin
      inherited SetType(VT_WORD);
      FCount:=2;
      FUpIndex:=_mbc.WDStreamAdd(FIndex);
    end;
    MCT_DW_STREAM: begin
      inherited SetType(VT_DWORD);
      FCount:=2;
      FUpIndex:=_mbc.DWStreamAdd(FIndex);
    end;
    MCT_BT_STREAM: begin
      inherited SetType(VT_BYTE);
      FCount:=1;
      FUpIndex:=_mbc.BTStreamAdd(FIndex);
    end;
    MCT_SINT_STREAM: begin
      inherited SetType(VT_SMALLINT);
      FCount:=2;
      FUpIndex:=_mbc.SINTStreamAdd(FIndex);
    end;
  end;
  UpdateReadPack;
end;

procedure TKRMBRegister.SetReadFunction(const Value: TMBReadFunction);
begin
  FReadFunction := Value;
  if Assigned(FCfgReadFunc) then
    if FReadFunction=mbrfReadHoldingRegisters then FCfgReadFunc.Value:=0 else FCfgReadFunc.Value:=1;
  UpdateReadPack;
end;

procedure TKRMBRegister.SetStrLen(const Value: Byte);
begin
  if Value=0 then exit;
  FStrLen:=Value;
  if FMCVarType=MCT_STRING then begin
    FCount:=Value div 2;
    if Value mod 2 > 0 then Inc(FCount);
    UpdateReadPack;
  end;
end;

procedure TKRMBRegister.SetType(const Value: TVarType);
begin
  //
end;

procedure TKRMBRegister.SetValue_(const Value: Variant);
var
  bt0,bt1: BYTE;
  wd0: Word;
  dw0: Cardinal;
  _Data: TKRRegisters;
  p: Pointer;
  sI: SmallInt;
  sg: Single;
  lI: longint;
  i: Integer;
  i64: int64;
  ui64: uint64;
  db: Double;

  procedure sendWord;
  begin
    if not FHighByteFirst then wd0:=(wd0 shr 8)or(wd0 shl 8);
    case FWriteFunction of
      mbwfWriteHoldingRegister: _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,_WCallBack);
      mbwfWriteHoldingRegisters: begin
        SetLength(_Data,1);
        _Data[0]:=wd0;
        _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,_WCallBack);
      end;
    end;
  end;

  procedure sendDWord;
  begin
    if FHighWordFirst then WordsFromDWord(dw0,FTmpWD0,wd0)
    else WordsFromDWord(dw0,wd0,FTmpWD0);
    if not FHighByteFirst then begin
      wd0:=(wd0 shr 8)or(wd0 shl 8);
      FTmpWD0:=(FTmpWD0 shr 8)or(FTmpWD0 shl 8);
    end;
    case FWriteFunction of
      mbwfWriteHoldingRegister: _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,_WCallBack0);
      mbwfWriteHoldingRegisters: begin
        SetLength(_Data,2);
        _Data[0]:=wd0;
        _Data[1]:=FTmpWD0;
        _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,_WCallBack);
      end;
    end;
  end;

  procedure sendUInt64;
  begin
    if FHighDWordFirst then begin
      FTmpDW1:=ui64 and $FFFFFFFF;
      FTmpDW0:=(ui64 shr 32) and $FFFFFFFF;
    end else begin
      FTmpDW0:=ui64 and $FFFFFFFF;
      FTmpDW1:=(ui64 shr 32) and $FFFFFFFF;
    end;
    case FWriteFunction of
      mbwfWriteHoldingRegister: begin
        if FHighWordFirst then WordsFromDWord(FTmpDW0,FTmpWD0,wd0)
        else WordsFromDWord(FTmpDW0,wd0,FTmpWD0);
        if not FHighByteFirst then begin
          wd0:=(wd0 shr 8)or(wd0 shl 8);
          FTmpWD0:=(FTmpWD0 shr 8)or(FTmpWD0 shl 8);
        end;
        _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,_WCBUInt64_1);
      end;
      mbwfWriteHoldingRegisters: begin
        SetLength(_Data,4);
        if FHighWordFirst then WordsFromDWord(FTmpDW0,FTmpWD0,wd0)
        else WordsFromDWord(FTmpDW0,wd0,FTmpWD0);
        if not FHighByteFirst then begin
          wd0:=(wd0 shr 8)or(wd0 shl 8);
          FTmpWD0:=(FTmpWD0 shr 8)or(FTmpWD0 shl 8);
        end;
        _Data[0]:=wd0;
        _Data[1]:=FTmpWD0;
        if FHighWordFirst then WordsFromDWord(FTmpDW1,FTmpWD0,wd0)
        else WordsFromDWord(FTmpDW1,wd0,FTmpWD0);
        if not FHighByteFirst then begin
          wd0:=(wd0 shr 8)or(wd0 shl 8);
          FTmpWD0:=(FTmpWD0 shr 8)or(FTmpWD0 shl 8);
        end;
        _Data[2]:=wd0;
        _Data[3]:=FTmpWD0;
        _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,_WCallBack);
      end;
    end;
  end;

begin
  inherited SetValue_(Value);
  if not _mbc.Active then begin
    FError:=-mbmeClientNotActive;
    inherited DoError(false);
    exit;
  end;

  case FMCVarType of
    MCT_BYTE: begin
      bt0:=Byte(Value);
      FTmpVal:=bt0;
      wd0:=(word(hiByte) shl 8) or bt0;
      sendWord;
    end;
    MCT_WORD: begin
      wd0:=Word(Value);
      FTmpVal:=wd0;
      sendWord;
    end;
    MCT_SMALLINT: begin
      sI:=SMALLINT(Value);
      FTmpVal:=sI;
      p:=@sI;
      wd0:=Word(p^);
      sendWord;
    end;
    MCT_DWORD: begin
      dw0:=Value;
      FTmpVal:=dw0;
      sendDWord;
    end;
    MCT_INT: begin
      lI:=Value;
      FTmpVal:=lI;
      p:=@lI;
      dw0:=DWORD(p^);
      sendDWord;
    end;
    MCT_SINGLE: begin
      sg:=Single(Value);
      FTmpVal:=sg;
      p:=@sg;
      dw0:=DWORD(p^);
      sendDWord;
    end;
    MCT_UINT64: begin
      ui64:=Value;
      FTmpVal:=ui64;
      sendUInt64;
    end;
    MCT_INT64: begin
      i64:=Value;
      FTmpVal:=i64;
      p:=@i64;
      ui64:=uint64(p^);
      sendUInt64;
    end;
    MCT_DOUBLE: begin
      db:=Value;
      FTmpVal:=db;
      p:=@db;
      ui64:=uint64(p^);
      sendUInt64;
    end;
    MCT_STRING: begin
      FTmpVal:=String(Value);
      FString:=WideStringToString(FTmpVal,1251);
      case FWriteFunction of
        mbwfWriteHoldingRegister: begin
          if(Length(FString)>0)then begin
            bt0:=Ord(FString[1]);
          if(Length(FString)>1)then bt1:=Ord(FString[2]) else bt1:=0;
          end else begin
            bt0:=0;
            bt1:=0;
          end;
          FString:=AnsiRightStr(FString,Length(FString)-2);
          FStringLen:=FStrLen-2;
          FStringIndex:=FIndex;
          _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,BytesToWord(bt0,bt1),_StrCallBack);
        end;
        mbwfWriteHoldingRegisters: begin
          SetLength(_Data,((FStrLen-1) div 2)+1);
          for i := 0 to Length(_Data)-1 do begin
            if(Length(FString)>i*2)then begin
              bt0:=Ord(FString[i*2+1]);
              if(Length(FString)>i*2+1)then bt1:=Ord(FString[i*2+2]) else bt1:=0;
            end else begin
              bt0:=0;
              bt1:=0;
            end;
            _Data[i]:=BytesToWord(bt0,bt1);
          end;
          _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,_WCallBack);
        end;
      end;
    end;
    MCT_ARRAY: begin
      for I := 0 to PKRLBuffer(NativeInt(Value))^.len-1 do
        FTmpArray.buf[i]:=PKRLBuffer(NativeInt(Value))^.buf[i];
      FTmpArray.len:=PKRLBuffer(NativeInt(Value))^.len;
      case FWriteFunction of
        mbwfWriteHoldingRegister: begin
          bt0:=0;bt1:=0;
          if FTmpArray.len>0 then bt1:=FTmpArray.buf[0];
          if FTmpArray.len>1 then bt0:=FTmpArray.buf[1];
          FTmpArrayI:=0;
          _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,BytesToWord(bt0,bt1),_ArrayWCallBack);
        end;
        mbwfWriteHoldingRegisters: begin
          SetLength(_Data,((FTmpArray.len-1) div 2)+1);
          for i := 0 to Length(_Data)-1 do
            _Data[i]:=BytesToWord(FTmpArray.buf[i*2+1],FTmpArray.buf[i*2]);
          _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,_WCallBack);
        end;
      end;
    end;
    MCT_DWSTR: begin
      FTmpVal:=String(Value);
      FDWSTR:=WideStringToString(FTmpVal,1251);
      FDWSTR_end:=false;
      DWSTR_wr(1);
    end;
    MCT_WD_STREAM: begin
      FTmpVal:=Word(Value);
      _mbc.WDStreamSet(FUpIndex,WD_STREAM_wr,FTmpVal);
    end;
    MCT_DW_STREAM: begin
      FTmpVal:=Value;
      _mbc.DWStreamSet(FUpIndex,DW_STREAM_wr,FTmpVal);
    end;
    MCT_BT_STREAM: begin
      FTmpVal:=Byte(Value);
      _mbc.BTStreamSet(FUpIndex,BT_STREAM_wr,FTmpVal);
    end;
    MCT_SINT_STREAM: begin
      FTmpVal:=SmallInt(Value);
      _mbc.SINTStreamSet(FUpIndex,SINT_STREAM_wr,FTmpVal);
    end;
  end;
end;

procedure TKRMBRegister.SetValue_c(AValue: Variant);
begin
  inherited SetValue_c(AValue);
end;

procedure TKRMBRegister.SetWriteFunction(const Value: TMBWriteFunction);
begin
  if FMCVarType=MCT_FILE then FWriteFunction:=mbwfWriteHoldingRegisters
  else FWriteFunction := Value;
  if Assigned(FCfgWriteFunc) then
    if FWriteFunction=mbwfWriteHoldingRegister then FCfgWriteFunc.Value:=0 else FCfgWriteFunc.Value:=1;
end;

procedure TKRMBRegister.SINT_STREAM_up;
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
  dw:=WordsToDWord(0,BytesToWord(FStreamIndex,1));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,SINT_STREAM_up_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,SINT_STREAM_up_wcb);
    end;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_up_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,SINT_STREAM_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.SINT_STREAM_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      UpdateValue_c(Smallint(dw));
      _mbc.SINTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then SINT_STREAM_up_r else begin
        _mbc.SINTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(true);
      end;
    end;
  end else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.SINT_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then SINT_STREAM_up_r
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.SINT_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,SINT_STREAM_up_wcb)
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr(AValue: SmallInt);
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
  dw:=WordsToDWord(Word(AValue),BytesToWord(FStreamIndex,2));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,SINT_STREAM_wr_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,SINT_STREAM_wr_wcb);
    end;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,SINT_STREAM_wr_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.SINT_STREAM_wr_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      SetValue_c(FTmpVal);
      _mbc.SINTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then SINT_STREAM_wr_r else begin
        _mbc.SINTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(false);
      end;
    end;
  end else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then SINT_STREAM_wr_r
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,SINT_STREAM_wr_wcb)
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.UpdateReadPack;
begin
  if Assigned(_mb)and Assigned(_mbc) then
    if ReadFunction=mbrfReadHoldingRegisters then begin
      FReadPackLen:=_mb.ReadHoldingRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex,FCount,@FReadPack,FReadPackRLen);
      FReadPackFunc:=mbfReadHoldingRegisters;
      if FMCVarType=MCT_FILE then begin
        FReadPackLen2:=_mb.ReadHoldingRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex,3,@FReadPack2,FReadPackRLen2);
        FReadPackLen3:=_mb.ReadHoldingRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex+1,FCount-1,@FReadPack3,FReadPackRLen3);
      end;
    end else begin
      FReadPackLen:=_mb.ReadInputRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex,FCount,@FReadPack,FReadPackRLen);
      FReadPackFunc:=mbfReadInputRegisters;
      if FMCVarType=MCT_FILE then begin
        FReadPackLen2:=_mb.ReadInputRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex,3,@FReadPack2,FReadPackRLen2);
        FReadPackLen3:=_mb.ReadInputRegistersPack(_mbc.FMBType,_mbc.Addres,FIndex+1,FCount-1,@FReadPack3,FReadPackRLen3);
      end;
    end
  else FReadPackLen:=0;
end;

procedure TKRMBRegister.UpdateValueAsync;
begin
  FAUpdated:=false;
  UpdateValue;
  while(not FAUpdated)and(not Application.Terminated)do begin
    Application.ProcessMessages;
    sleep(1);
  end;
end;

procedure TKRMBRegister.UpdateValue_;
begin

//tLog:=tLog+#13#10+'UpdateValue_ wait';
  inherited UpdateValue_;

  if not _mbc.Active then begin
    FError:=-mbmeClientNotActive;
    inherited DoError(true);
    if FMCVarType=MCT_FILE then begin
      FFileProcess:=2;
      if Assigned(FOnFile) then FOnFile(Self);
    end;
    exit;
  end;
  FTmpArray.len:=0;
//tLog:=tLog+#13#10+'UpdateValue_ in';
//  if Terminate then VarUpCS_Leave else
    case FMCVarType of
      MCT_DWSTR: DWSTR_up($3000000);
      MCT_WD_STREAM: _mbc.WDStreamUp(FUpIndex,WD_STREAM_up);
      MCT_DW_STREAM: _mbc.DWStreamUp(FUpIndex,DW_STREAM_up);
      MCT_BT_STREAM: _mbc.BTStreamUp(FUpIndex,BT_STREAM_up);
      MCT_SINT_STREAM: _mbc.SINTStreamUp(FUpIndex,SINT_STREAM_up);
      MCT_FILE: FileSendName;
      else _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,_RCallBack,FReadPackRLen);
    end;
end;

procedure TKRMBRegister.UpdateValue_c(AVal: Variant);
begin
  inherited UpdateValue_c(AVal);
end;

procedure TKRMBRegister.WD_STREAM_up;
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
//REAddLog(Name+' WD_STREAM_up');
  dw:=WordsToDWord(0,BytesToWord(FStreamIndex,1));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,WD_STREAM_up_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,WD_STREAM_up_wcb);
    end;
  end;
end;

procedure TKRMBRegister.WD_STREAM_up_r;
begin
//REAddLog(Name+' WD_STREAM_up_r');
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,WD_STREAM_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.WD_STREAM_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
//REAddLog(Name+' WD_STREAM_up_rcb');
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      UpdateValue_c(Word(dw));
      _mbc.WDStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then WD_STREAM_up_r else begin
        _mbc.WDStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(true);
      end;
    end;
  end else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.WD_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
//REAddLog(Name+' WD_STREAM_up_wcb');
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then WD_STREAM_up_r
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.WD_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
//REAddLog(Name+' WD_STREAM_up_wcb0');
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,WD_STREAM_up_wcb)
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(true);
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr(AValue: Word);
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
  dw:=WordsToDWord(AValue,BytesToWord(FStreamIndex,2));
  case FWriteFunction of
    mbwfWriteHoldingRegister: begin
      WordsFromDWord(dw,wd0,FTmpWD0);
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex,wd0,WD_STREAM_wr_wcb0);
    end;
    mbwfWriteHoldingRegisters: begin
      SetLength(_Data,2);
      WordsFromDWord(dw,_Data[0],_Data[1]);
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,_Data,WD_STREAM_wr_wcb);
    end;
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr_r;
begin
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,WD_STREAM_wr_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.WD_STREAM_wr_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      SetValue_c(FTmpVal);
      _mbc.WDStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then WD_STREAM_wr_r else begin
        _mbc.WDStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError(false);
      end;
    end;
  end else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then WD_STREAM_wr_r
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,WD_STREAM_wr_wcb)
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError(false);
  end;
end;

procedure TKRMBRegister._ArrayWCallBack(AError: integer; AData: Pointer);
var
  bt0,bt1: byte;
begin
  FError:=AError;
  if AError=0 then begin
    inc(FTmpArrayI);
    if FTmpArrayI*2>=FTmpArray.len then SetValue_c(FTmpVal) else begin
      bt0:=0;bt1:=0;
      if FTmpArray.len>FTmpArrayI*2 then bt1:=FTmpArray.buf[FTmpArrayI*2];
      if FTmpArray.len>FTmpArrayI*2+1 then bt0:=FTmpArray.buf[FTmpArrayI*2+1];
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,BytesToWord(bt0,bt1),_ArrayWCallBack)
    end;
  end else DoError(false);
end;

procedure TKRMBRegister._ParamChange(AParam: TKRIniCfgParam);
begin
  inherited;
  if Assigned(CfgReadFunc) and (CfgReadFunc.Name=AParam.Name) then begin
    if CfgReadFunc.Value=0 then FReadFunction:=mbrfReadHoldingRegisters else FReadFunction:=mbrfReadInputRegisters;
  end;
  if Assigned(CfgWriteFunc) and (CfgWriteFunc.Name=AParam.Name) then begin
    if CfgWriteFunc.Value=0 then FWriteFunction:=mbwfWriteHoldingRegister else FWriteFunction:=mbwfWriteHoldingRegisters;
  end;
  if Assigned(CfgRegisterIndex) and (CfgRegisterIndex.Name=AParam.Name) then begin
    FIndex:=CfgRegisterIndex.Value;
  end;
end;

procedure TKRMBRegister._RCallBack(AError: integer; AData: Pointer);
var
  i: integer;
  wd: word;
begin
  FError:=AError;
  if FError=0 then begin
    try
      case FMCVarType of
      MCT_BYTE: begin
        wd:=MBRegsToWORD(PKRRegisters(AData)^,FHighByteFirst);
        hiByte:=wd shr 8;
        UpdateValue_c(Byte(wd and $ff));
      end;
      MCT_WORD: UpdateValue_c(MBRegsToWORD(PKRRegisters(AData)^,FHighByteFirst));
      MCT_SMALLINT: UpdateValue_c(MBRegsToSMALLINT(PKRRegisters(AData)^,FHighByteFirst));
      MCT_DWORD: UpdateValue_c(MBRegsToDWORD(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst));
      MCT_INT: UpdateValue_c(MBRegsToINT(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst));
      MCT_SINGLE: UpdateValue_c(MBRegsToSINGLE(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst));
      MCT_UINT64: UpdateValue_c(MBRegsToUINT64(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst));
      MCT_INT64: UpdateValue_c(MBRegsToINT64(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst));
      MCT_DOUBLE: UpdateValue_c(MBRegsToDouble(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst));
      MCT_STRING: UpdateValue_c(LeftStr(MBRegsToSTRING(PKRRegisters(AData)^,FHighByteFirst), FStrLen));
      MCT_ARRAY: begin
          FTmpArray.len:=Length(PKRRegisters(AData)^)*2;
          if FTmpArray.len-1=FArrayLen then dec(FTmpArray.len);
          for I := 0 to Length(PKRRegisters(AData)^)-1 do begin
            FTmpArray.buf[i*2]:=PKRRegisters(AData)^[i] shr 8;
            FTmpArray.buf[i*2+1]:=PKRRegisters(AData)^[i] and $FF;
          end;
          UpdateValue_c(FTmpVal);
        end;
      end;
    except on E: Exception do begin
        DoRuntimeError('TKRMBRegister[Name="'+
          Name+'"] procedure _RCallBack;',E);
        FError:=-mbmeRealTimeError;
        DoError(false);
      end;
    end;
  end else DoError(true);
  FAUpdated:=true;
//  if not FAUpdated then SetTimer(FWH, 1, 3, nil);
end;

procedure TKRMBRegister._StrCallBack(AError: integer; AData: Pointer);
var
  bt0,bt1:byte;
begin
  if AError<>0 then begin
    FError:=AError;
    DoError(false);
    exit;
  end;
  if FStringLen>0 then begin
    if(Length(FString)>0)then begin
      bt0:=Ord(FString[1]);
      if(Length(FString)>1)then bt1:=Ord(FString[2]) else bt1:=0;
    end else begin
      bt0:=0;
      bt1:=0;
    end;
    FString:=AnsiRightStr(FString,Length(FString)-2);
    FStringLen:=FStringLen-2;
    Inc(FStringIndex);
    _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FStringIndex,BytesToWord(bt0,bt1),_StrCallBack);
  end else SetValue_c(FTmpVal)
end;

procedure TKRMBRegister._WCallBack(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then SetValue_c(FTmpVal) else DoError(false);
end;

procedure TKRMBRegister._WCallBack0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,_WCallBack)
  else DoError(false);
end;

procedure TKRMBRegister._WCBUInt64_1(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,_WCBUInt64_2)
  else DoError(false);
end;

procedure TKRMBRegister._WCBUInt64_2(AError: integer; AData: Pointer);
var wd0: word;
begin
  FError:=AError;
  if FHighWordFirst then WordsFromDWord(FTmpDW1,FTmpWD0,wd0)
  else WordsFromDWord(FTmpDW1,wd0,FTmpWD0);
  if not FHighByteFirst then begin
    wd0:=(wd0 shr 8)or(wd0 shl 8);
    FTmpWD0:=(FTmpWD0 shr 8)or(FTmpWD0 shl 8);
  end;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+2,wd0,_WCBUInt64_3)
  else DoError(false);
end;

procedure TKRMBRegister._WCBUInt64_3(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+3,FTmpWd0,_WCallBack)
  else DoError(false);
end;

end.
