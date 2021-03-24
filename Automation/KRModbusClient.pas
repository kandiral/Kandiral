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

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, System.AnsiStrings, System.Variants,
    System.StrUtils, Vcl.Forms, Winapi.Messages, System.SyncObjs,
  {$ELSE}
    Windows, Classes, SysUtils, AnsiStrings, Variants, StrUtils, Forms, Messages, SyncObjs,
  {$IFEND}
  KRModbusClientLng, KRModbus, KRModbusMaster, KRVariables, KRComponentCollection,
  KRIniConfig, KRTypes, KRThreadQueue, lgop, funcs;

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
    procedure SetValueRequest(var Value: Variant);override;
    function GetErrorMsg: String;override;
    procedure SetStrLen(const Value: Byte);
    procedure UpdateValueRequest;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure _ParamChange(AParam: TKRIniCfgParam);override;
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
    property IntervalMin default 5;
    property UpdateType default vutAfter;
    property UpAftWrite;
    property WaitForUpdates;

    property CfgInterval;
    property CfgValue;
    property OnValUpdated;
    property OnError;
    property UserError;
    property UserErrorMsg;
    property OnRuntimeError;
    property OnFile: TNotifyEvent read FOnFile write FOnFile;
  end;

  TKRMBCBatchUpdates = class;

  TKRMBCBatchUpdate = class(TCollectionItem)
  private
    FWH: HWND;
    FRequestTM: cardinal;
    regs: TKRRegisters;
    FRegsLen: integer;
    FCount: Word;
    FInterval: Cardinal;
    FIndex: Word;
    FIntervalMin: cardinal;
    FReadFunction: TMBReadFunction;
    FReadPack: TKRBuffer;
    FReadPackLen,FReadPackRLen: byte;
    FReadPackFunc: TMBFunc;
    procedure TmWP(var Msg: TMessage);
    procedure SetInterval(const Value: Cardinal);
    procedure SetReadFunction(const Value: TMBReadFunction);
    procedure MBCallBack(AError: integer; AData: Pointer);
    procedure SetCount(const Value: Word);
    procedure SetRegIndex(const Value: Word);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection);override;
    destructor Destroy;override;
    procedure UpdateReadPack;
  published
    property RegisterIndex: Word read FIndex write SetRegIndex default 0;
    property Count: Word read FCount write SetCount default 1;
    property Interval: Cardinal read FInterval write SetInterval default 0;
    property IntervalMin: cardinal read FIntervalMin write FIntervalMin default 5;
    property ReadFunction: TMBReadFunction read FReadFunction write SetReadFunction default mbrfReadHoldingRegisters;
  end;

  TKRMBCBatchUpdates = class(TCollection)
  private
    FMBC: TKRModbusClient;
    function GetBUItem(Index: integer): TKRMBCBatchUpdate;
    procedure SetBUItem(Index: integer; const Value: TKRMBCBatchUpdate);
  public
    constructor Create(AOwner: TComponent);
    function GetOwner: TPersistent;override;
    property Items[Index: integer]: TKRMBCBatchUpdate read GetBUItem write SetBUItem; default;
  end;

  TKRMCStream = record
    i: integer;
    _stack: TKRThreadQueue;
  end;

  TKRMCStream2 = record
    Wait: boolean;
    RegIndex: integer;
    list: TList;
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

    FDWStreams, FBTStreams, FSINTStreams: array of TKRMCStream;
    FDWStreamsW, FBTStreamsW, FSINTStreamsW: array of boolean;
    KRMBC_DW_lock, KRMBC_BT_lock, KRMBC_SINT_lock: TObject;

    WDStreamCS: TCriticalSection;
    WDStreams: array of TKRMCStream2;
    FBatchUpdates: TKRMBCBatchUpdates;

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
    procedure SetBatchUpdates(const Value: TKRMBCBatchUpdates);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function AddVar(AName: String; AType: TMCVarType; AIndex: word; AInterval: Integer = 0):TKRMBRegister;
  published
    property Modbus: TKRModbusMaster read FModbus write SetModbus;
    property Addres: byte read FAddres write SetAddres default 0;
    property MBType: TMBType read FMBType write SetMBType default mbtTCP;
    property BatchUpdates: TKRMBCBatchUpdates read FBatchUpdates write SetBatchUpdates;
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
  WDStreamCS:=TCriticalSection.Create;


  KRMBC_DW_lock := TObject.Create;
  KRMBC_BT_lock := TObject.Create;
  KRMBC_SINT_lock := TObject.Create;

  FAddres:=0;
  FMBType:=mbtTCP;

  FBatchUpdates:=TKRMBCBatchUpdates.Create(Self);

  inherited SetItemClass(TKRMBRegister);
end;

destructor TKRModbusClient.Destroy;
var
  i,j: integer;
  pDW: PKRMCDWStream;
  pBT: PKRMCBTStream;
  pSINT: PKRMCSINTStream;
begin
  KRMBC_DW_lock.Free;
  KRMBC_BT_lock.Free;
  KRMBC_SINT_lock.Free;

  for i := 0 to Length(WDStreams)-1 do begin
    for j:=0 to WDStreams[i].List.Count-1 do begin
      Dispose(WDStreams[i].List[j]);
    end;
    WDStreams[i].List.Free;
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

  WDStreamCS.Free;
  FBatchUpdates.Free;
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

procedure TKRModbusClient.SetAddres(const Value: byte);
var
  i: integer;
begin
  FAddres := Value;
  for i := 0 to ItemsCount-1 do TKRMBRegister(Items[i]).UpdateReadPack;
  for i := 0 to FBatchUpdates.Count-1 do FBatchUpdates.Items[i].UpdateReadPack;
end;

procedure TKRModbusClient.SetBatchUpdates(const Value: TKRMBCBatchUpdates);
begin
  FBatchUpdates.Assign(Value);
end;

procedure TKRModbusClient.SetMBType(const Value: TMBType);
var
  i: integer;
begin
  FMBType := Value;
  for i := 0 to ItemsCount-1 do TKRMBRegister(Items[i]).UpdateReadPack;
  for i := 0 to FBatchUpdates.Count-1 do FBatchUpdates.Items[i].UpdateReadPack;
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
      for i := 0 to FBatchUpdates.Count-1 do FBatchUpdates.Items[i].UpdateReadPack;
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
  i,n: integer;
begin
  Result:=-1;
  WDStreamCS.Enter;
  try
    n:=Length(WDStreams)-1;
    for i := 0 to n do
      if WDStreams[i].RegIndex=ARegIndex then begin
        Result:=i;
        break;
      end;
    if Result=-1 then begin
      Result:=Length(WDStreams);
      SetLength(WDStreams,Result+1);
      WDStreams[Result].Wait:=false;
      WDStreams[Result].RegIndex:=ARegIndex;
      WDStreams[Result].list:=TList.Create;
    end;
  finally
    WDStreamCS.Leave;
  end;
end;

procedure TKRModbusClient.WDStreamDel(AIndex: integer);
var
  _p: PKRMCWDStream;
begin
  _p:=nil;
  WDStreamCS.Enter;
  try
    if WDStreams[AIndex].list.Count>0 then begin
      _p:=WDStreams[AIndex].list[WDStreams[AIndex].list.Count-1];
       WDStreams[AIndex].list.Delete(WDStreams[AIndex].list.Count-1);
    end;
  finally
    WDStreamCS.Leave;
  end;
  if _p=nil then WDStreams[AIndex].Wait:=false else begin
    if Assigned(_p.upProc) then _p.upProc else _p.setProc(_p.val);
    Dispose(_p);
  end;
end;

procedure TKRModbusClient.WDStreamSet(AIndex: integer;
  AProc: TMBSetStreamProcWD; AValue: Word);
var
  p: PKRMCWDStream;
  prc: TMBSetStreamProcWD;
  vl: word;
begin
  prc:=nil;
  WDStreamCS.Enter;
  try
    if WDStreams[AIndex].Wait then begin
      New(p);
      p.upProc:=nil;
      p.setProc:=AProc;
      p.val:=AValue;
      WDStreams[AIndex].list.Insert(0,p);
    end else begin
      WDStreams[AIndex].Wait:=true;
      prc:=AProc;
      vl:=AValue;
    end;
  finally
    WDStreamCS.Leave;
  end;
  if Assigned(prc) then prc(vl);
end;

procedure TKRModbusClient.WDStreamUp(AIndex: integer; AProc: TMBUpStreamProc);
var
  p: PKRMCWDStream;
begin
  WDStreamCS.Enter;
  try
    if WDStreams[AIndex].Wait then begin
      New(p);
      p.upProc:=AProc;
      WDStreams[AIndex].list.Insert(0,p);
    end else begin
      WDStreams[AIndex].Wait:=true;
      AProc;
    end;
  finally
    WDStreamCS.Leave;
  end;
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
      UpdateValueResponse(Byte(wd));
      _mbc.BTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then BT_STREAM_up_r else begin
        _mbc.BTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.BT_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then BT_STREAM_up_r
  else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError;
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
      SetValueResponse(FTmpVal);
      _mbc.BTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then BT_STREAM_wr_r else begin
        _mbc.BTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.BT_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then BT_STREAM_wr_r
  else begin
    _mbc.BTStreamDel(FUpIndex);
    DoError;
  end;
end;

constructor TKRMBRegister.Create(AOwner: TComponent);
begin
  inherited;
  FIndex:=0;
  FError:=mbceNoValue;
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

procedure TKRMBRegister.DWSTR_up(ACmd: Cardinal);
var
  wd0:word;
  _Data:TKRRegisters;
begin
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
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,DWSTR_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.DWSTR_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
  wd0,wd1: word;
  bt0,bt1,bt2,bt3: byte;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
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
            DWSTR_up($4000000);
          end else UpdateValueResponse(StringToWideString(FDWSTR,1251));
        end else UpdateValueResponse(StringToWideString(FDWSTR,1251));
      end else UpdateValueResponse(StringToWideString(FDWSTR,1251));
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DWSTR_up_r else begin
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else DoError;
end;

procedure TKRMBRegister.DWSTR_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DWSTR_up_r
  else DoError;
end;

procedure TKRMBRegister.DWSTR_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DWSTR_up_wcb)
  else DoError;
end;

procedure TKRMBRegister.DWSTR_wr(ACmd: byte);
var
  wd0: word;
  dw: Cardinal;
  bt0,bt1,bt2: byte;
  _Data: TKRRegisters;
begin
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
        SetValueResponse(FTmpVal)
      end else
        DWSTR_wr(2);
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError;
      end else DWSTR_wr_r;
    end;
  end else DoError;
end;

procedure TKRMBRegister.DWSTR_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DWSTR_wr_r
  else DoError;
end;

procedure TKRMBRegister.DWSTR_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DWSTR_wr_wcb)
  else DoError;
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
        UpdateValueResponse(dw);
        _mbc.DWStreamDel(FUpIndex);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DW_STREAM_up_r else begin
        _mbc.DWStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.DW_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DW_STREAM_up_r
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.DW_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DW_STREAM_up_wcb)
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
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
        SetValueResponse(FTmpVal);
        _mbc.DWStreamDel(FUpIndex);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then DW_STREAM_wr_r else begin
        _mbc.DWStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then DW_STREAM_wr_r
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.DW_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,DW_STREAM_wr_wcb)
  else begin
    _mbc.DWStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.FileDelete(AFileName: String);
begin
  while FFileProcess=1 do Application.ProcessMessages;
  FFileProcess:=1;
  FFileName:=AFileName;
  FFileDo:=1;
  UpdateValueRequest;
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
  UpdateValueRequest;
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
		UpdateValueResponse(NULL);
		FFileProcess:=0;
  end else begin
    inc(FAskCnt);
    if FAskCnt>=FAskLimit then begin
      FError:=mbceAskLimit;
      DoError;
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
  UpdateValueRequest;
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
      DoError;
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
        DoError;
        if Assigned(FOnFile) then FOnFile(Self);
      end else begin
        FFileStat:=wd and $FF;
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack3,FReadPackLen3,FileRead_RES_CB2,FReadPackRLen3)
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError;
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileRead_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError;
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
      UpdateValueResponse(NULL);
      FFileProcess:=0;
      if Assigned(FOnFile) then FOnFile(Self);
    end else
      _mb.WriteHoldingRegisters(_mbc.FMBType,_mbc.FAddres,FIndex,__Data,FileRead_CB);
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError;
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
    DoError;
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
      DoError;
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
              UpdateValueResponse(NULL);
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
              UpdateValueResponse(NULL);
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
          DoError;
          if Assigned(FOnFile) then FOnFile(Self);
        end;
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError;
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileSendName_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError;
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
  UpdateValueRequest;
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
  UpdateValueRequest;
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
      DoError;
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
          UpdateValueResponse(NULL);
          FFileProcess:=0;
          if Assigned(FOnFile) then FOnFile(Self);
        end else FileWrite_;
      end else begin
        FError:=mbceFileWriteError;
        FFileProcess:=2;
        DoError;
        if Assigned(FOnFile) then FOnFile(Self);
      end;
    end else begin
      inc(FAskCnt);
      if FAskCnt>=FAskLimit then begin
        FError:=mbceAskLimit;
        DoError;
        FFileProcess:=2;
        if Assigned(FOnFile) then FOnFile(Self);
      end else
        _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack2,FReadPackLen2,FileWrite_RES_CB,FReadPackRLen2)
    end;
  end else begin
    inc(FFileErrors);
    if FFileErrors>4 then begin
      DoError;
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

procedure TKRMBRegister.SetMCVarType(const Value: TMCVarType);
begin
  FMCVarType := Value;
  FAllowUpdate:=true;
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
      inherited SetType(VT_WORD);
      SetWriteFunction(mbwfWriteHoldingRegisters);
      SetFileRegs(FFileRegs);
      FAllowUpdate:=false;
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
    FCount:=Value shr 1;
    if (Value and 1) > 0 then Inc(FCount);
    UpdateReadPack;
  end;
end;

procedure TKRMBRegister.SetType(const Value: TVarType);
begin
  //
end;

procedure TKRMBRegister.SetValueRequest(var Value: Variant);
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
      UpdateValueResponse(Smallint(dw));
      _mbc.SINTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then SINT_STREAM_up_r else begin
        _mbc.SINTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then SINT_STREAM_up_r
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,SINT_STREAM_up_wcb)
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
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
      SetValueResponse(FTmpVal);
      _mbc.SINTStreamDel(FUpIndex);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then SINT_STREAM_wr_r else begin
        _mbc.SINTStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then SINT_STREAM_wr_r
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.SINT_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,SINT_STREAM_wr_wcb)
  else begin
    _mbc.SINTStreamDel(FUpIndex);
    DoError;
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

procedure TKRMBRegister.UpdateValueRequest;
begin
  inherited;
  FTmpArray.len:=0;
  case FMCVarType of
    MCT_DWSTR: DWSTR_up($3000000);
    MCT_WD_STREAM: _mbc.WDStreamUp(FUpIndex,WD_STREAM_up);
    MCT_DW_STREAM: _mbc.DWStreamUp(FUpIndex,DW_STREAM_up);
    MCT_BT_STREAM: _mbc.BTStreamUp(FUpIndex,BT_STREAM_up);
    MCT_SINT_STREAM: _mbc.SINTStreamUp(FUpIndex,SINT_STREAM_up);
    MCT_FILE: FileSendName
    else _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,_RCallBack,FReadPackRLen);
  end;
end;

procedure TKRMBRegister.WD_STREAM_up;
var
  dw: Cardinal;
  _Data: TKRRegisters;
  wd0: Word;
begin
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
  _mb.SendPack(_mbc.FMBType,FReadPackFunc,@FReadPack,FReadPackLen,WD_STREAM_up_rcb,FReadPackRLen)
end;

procedure TKRMBRegister.WD_STREAM_up_rcb(AError: integer; AData: Pointer);
var
  dw: Cardinal;
begin
  FError:=AError;
  if AError=0 then begin
    dw:=MBRegsToDWORD(PKRRegisters(AData)^);
    if dw shr 24 = 0 then begin
      _mbc.WDStreamDel(FUpIndex);
      UpdateValueResponse(Word(dw));
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then WD_STREAM_up_r else begin
        _mbc.WDStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.WD_STREAM_up_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then WD_STREAM_up_r
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.WD_STREAM_up_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,WD_STREAM_up_wcb)
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
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
      _mbc.WDStreamDel(FUpIndex);
      SetValueResponse(FTmpVal);
    end else begin
      inc(FAskCnt);
      if FAskCnt<FAskLimit then WD_STREAM_wr_r else begin
        _mbc.WDStreamDel(FUpIndex);
        FError:=mbceAskLimit;
        DoError;
      end;
    end;
  end else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr_wcb(AError: integer; AData: Pointer);
begin
  FError:=AError;
  FAskCnt:=0;
  if AError=0 then WD_STREAM_wr_r
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister.WD_STREAM_wr_wcb0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,WD_STREAM_wr_wcb)
  else begin
    _mbc.WDStreamDel(FUpIndex);
    DoError;
  end;
end;

procedure TKRMBRegister._ArrayWCallBack(AError: integer; AData: Pointer);
var
  bt0,bt1: byte;
begin
  FError:=AError;
  if AError=0 then begin
    inc(FTmpArrayI);
    if FTmpArrayI*2>=FTmpArray.len then SetValueResponse(FTmpVal) else begin
      bt0:=0;bt1:=0;
      if FTmpArray.len>FTmpArrayI*2 then bt1:=FTmpArray.buf[FTmpArrayI*2];
      if FTmpArray.len>FTmpArrayI*2+1 then bt0:=FTmpArray.buf[FTmpArrayI*2+1];
      _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,BytesToWord(bt0,bt1),_ArrayWCallBack)
    end;
  end else DoError;
end;

procedure TKRMBRegister._ParamChange(AParam: TKRIniCfgParam);
begin
  inherited;
  if Assigned(CfgReadFunc) and (CfgReadFunc.Name=AParam.Name) then begin
    if CfgReadFunc.Value=0 then ReadFunction:=mbrfReadHoldingRegisters else ReadFunction:=mbrfReadInputRegisters;
  end;
  if Assigned(CfgWriteFunc) and (CfgWriteFunc.Name=AParam.Name) then begin
    if CfgWriteFunc.Value=0 then WriteFunction:=mbwfWriteHoldingRegister else WriteFunction:=mbwfWriteHoldingRegisters;
  end;
  if Assigned(CfgRegisterIndex) and (CfgRegisterIndex.Name=AParam.Name) then begin
    RegisterIndex:=CfgRegisterIndex.Value;
  end;
end;

procedure TKRMBRegister._RCallBack(AError: integer; AData: Pointer);
var
  i: integer;
  wd: word;
  async: boolean;
begin
  if AError=-2147483648 then begin
    FError:=0;
    async:=true;
  end else begin
    FError:=AError;
    async:=false;
  end;

  if FError=0 then begin
    try
      case FMCVarType of
      MCT_BYTE: begin
        wd:=MBRegsToWORD(PKRRegisters(AData)^,FHighByteFirst);
        hiByte:=wd shr 8;
        UpdateValueResponse(Byte(wd and $ff));
      end;
      MCT_WORD: UpdateValueResponse(MBRegsToWORD(PKRRegisters(AData)^,FHighByteFirst),async);
      MCT_SMALLINT: UpdateValueResponse(MBRegsToSMALLINT(PKRRegisters(AData)^,FHighByteFirst),async);
      MCT_DWORD: UpdateValueResponse(MBRegsToDWORD(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst),async);
      MCT_INT: UpdateValueResponse(MBRegsToINT(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst),async);
      MCT_SINGLE: UpdateValueResponse(MBRegsToSINGLE(PKRRegisters(AData)^,FHighWordFirst,FHighByteFirst),async);
      MCT_UINT64: UpdateValueResponse(MBRegsToUINT64(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst),async);
      MCT_INT64: UpdateValueResponse(MBRegsToINT64(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst),async);
      MCT_DOUBLE: UpdateValueResponse(MBRegsToDouble(PKRRegisters(AData)^,FHighDWordFirst,FHighWordFirst,FHighByteFirst),async);
      MCT_STRING: UpdateValueResponse(LeftStr(MBRegsToSTRING(PKRRegisters(AData)^,FHighByteFirst), FStrLen),async);
      MCT_ARRAY: begin
          FTmpArray.len:=Length(PKRRegisters(AData)^)*2;
          if FTmpArray.len-1=FArrayLen then dec(FTmpArray.len);
          for I := 0 to Length(PKRRegisters(AData)^)-1 do begin
            FTmpArray.buf[i*2]:=PKRRegisters(AData)^[i] shr 8;
            FTmpArray.buf[i*2+1]:=PKRRegisters(AData)^[i] and $FF;
          end;
          UpdateValueResponse(FTmpVal);
        end;
      end;
    except on E: Exception do begin
        DoRuntimeError('TKRMBRegister[Name="'+
          Name+'"] procedure _RCallBack;',E);
        FError:=mbceRealTimeError;
        DoError;
      end;
    end;
  end else DoError;
  FAUpdated:=true;
//  if not FAUpdated then SetTimer(FWH, 1, 3, nil);
end;

procedure TKRMBRegister._StrCallBack(AError: integer; AData: Pointer);
var
  bt0,bt1:byte;
begin
  if AError<>0 then begin
    FError:=AError;
    DoError;
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
  end else SetValueResponse(FTmpVal)
end;

procedure TKRMBRegister._WCallBack(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then SetValueResponse(FTmpVal) else DoError;
end;

procedure TKRMBRegister._WCallBack0(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,_WCallBack)
  else DoError;
end;

procedure TKRMBRegister._WCBUInt64_1(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+1,FTmpWd0,_WCBUInt64_2)
  else DoError;
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
  else DoError;
end;

procedure TKRMBRegister._WCBUInt64_3(AError: integer; AData: Pointer);
begin
  FError:=AError;
  if AError=0 then _mb.WriteHoldingRegister(_mbc.FMBType,_mbc.FAddres,FIndex+3,FTmpWd0,_WCallBack)
  else DoError;
end;

{ TKRMBCBatchUpdates }

constructor TKRMBCBatchUpdates.Create(AOwner: TComponent);
begin
  inherited Create(TKRMBCBatchUpdate);
  FMBC:=TKRModbusClient(AOwner);
end;

function TKRMBCBatchUpdates.GetBUItem(Index: integer): TKRMBCBatchUpdate;
begin
  Result:=TKRMBCBatchUpdate(inherited Items[Index]);
end;

function TKRMBCBatchUpdates.GetOwner: TPersistent;
begin
  Result:=FMBC;
end;

procedure TKRMBCBatchUpdates.SetBUItem(Index: integer;
  const Value: TKRMBCBatchUpdate);
begin
  TKRMBCBatchUpdate(inherited Items[Index]).Assign(Value);
end;

{ TKRMBCBatchUpdate }

procedure TKRMBCBatchUpdate.AssignTo(Dest: TPersistent);
begin
  if Dest is TKRMBCBatchUpdate then begin
    TKRMBCBatchUpdate(Dest).FIndex:=FIndex;
    TKRMBCBatchUpdate(Dest).FCount:=FIndex;
    TKRMBCBatchUpdate(Dest).FIntervalMin:=FIntervalMin;
    TKRMBCBatchUpdate(Dest).FReadFunction:=FReadFunction;
    TKRMBCBatchUpdate(Dest).UpdateReadPack;
    TKRMBCBatchUpdate(Dest).Interval:=FInterval;
  end;
end;

constructor TKRMBCBatchUpdate.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWH := AllocateHWnd(tmWP);
  FRegsLen:=4;
  SetLength(regs,FRegsLen);

  FIndex:=0;
  FCount:=1;
  FInterval:=0;
  FIntervalMin:=5;
  FReadFunction:=mbrfReadHoldingRegisters;

  UpdateReadPack;
end;

destructor TKRMBCBatchUpdate.Destroy;
begin
  KillTimer(FWH, 1);
  DeallocateHWnd(FWH);

  inherited;
end;

procedure TKRMBCBatchUpdate.MBCallBack(AError: integer; AData: Pointer);
var
  i, n, k, j, m: integer;
  data: PKRRegisters;
  reg: TKRMBRegister;
  tm: cardinal;
begin
  if AError=0 then begin
    data:=AData;
    if Length(data^)=FCount then begin

      n:=TKRMBCBatchUpdates(Collection).FMBC.ItemsCount-1;
      for i := 0 to n do begin
        reg:=TKRMBRegister(TKRMBCBatchUpdates(Collection).FMBC.Items[i]);
        if(reg.FIndex<FIndex)or(reg.FIndex+reg.FCount>FIndex+FCount)then continue;
        case reg.FMCVarType of
          MCT_BYTE, MCT_WORD, MCT_SMALLINT:  begin
            regs[0]:=data^[reg.FIndex-FIndex];
            reg._RCallBack(-2147483648,@regs);
          end;
          MCT_DWORD, MCT_INT, MCT_SINGLE: begin
            k:=reg.FIndex-FIndex;
            regs[0]:=data^[k];
            inc(k);
            regs[1]:=data^[k];
            reg._RCallBack(-2147483648,@regs);
          end;
          MCT_UINT64, MCT_INT64, MCT_DOUBLE: begin
            k:=reg.FIndex-FIndex;
            regs[0]:=data^[k];
            inc(k);
            regs[1]:=data^[k];
            inc(k);
            regs[2]:=data^[k];
            inc(k);
            regs[3]:=data^[k];
            reg._RCallBack(-2147483648,@regs);
          end;
          MCT_STRING: begin
            if FRegsLen<reg.FCount then begin
              FRegsLen:=reg.FCount;
              SetLength(regs,FRegsLen);
            end;
            k:=reg.FIndex-FIndex;
            m:=reg.FCount-1;
            for j := 0 to m do begin
              regs[j]:=data^[k];
              inc(k);
            end;
            reg._RCallBack(-2147483648,@regs);
          end;
        end;
      end;

    end else AError:=mbceNoValue;
  end;
  if AError<>0 then begin
    n:=TKRMBCBatchUpdates(Collection).FMBC.ItemsCount-1;
    for i := 0 to n do begin
      reg:=TKRMBRegister(TKRMBCBatchUpdates(Collection).FMBC.Items[i]);
      if(reg.FIndex<FIndex)or(reg.FIndex+reg.FCount>FIndex+FCount)then continue;
      reg.FError:=AError;
      reg.DoError(true);
    end;
  end;

  if FInterval>0 then begin
    tm:=getTickCount-FRequestTM;
    if(tm>FInterval)or(FInterval-tm<FIntervalMin)then tm:=FIntervalMin else tm:=FInterval-tm;
    SetTimer(FWH, 1, tm, nil);
  end;
end;

procedure TKRMBCBatchUpdate.SetCount(const Value: Word);
begin
  FCount := Value;
  UpdateReadPack;
end;

procedure TKRMBCBatchUpdate.SetInterval(const Value: Cardinal);
begin
  if FInterval<>Value then begin
    KillTimer(FWH, 1);
    FInterval := Value;
    if (FInterval>0)then SetTimer(FWH, 1, FInterval, nil);
  end;
end;

procedure TKRMBCBatchUpdate.SetReadFunction(const Value: TMBReadFunction);
begin
  FReadFunction := Value;
  UpdateReadPack;
end;

procedure TKRMBCBatchUpdate.SetRegIndex(const Value: Word);
begin
  FIndex := Value;
  UpdateReadPack;
end;

procedure TKRMBCBatchUpdate.TmWP(var Msg: TMessage);
begin
  if(Msg.Msg=WM_TIMER)and(Msg.WParam=1)then begin
    KillTimer(FWH, 1);
    if Assigned(Collection)and Assigned(TKRMBCBatchUpdates(Collection).FMBC.FModbus) then
      if FReadPackLen=0 then begin
        UpdateReadPack;
        if FInterval>0 then SetTimer(FWH, 1, FInterval, nil);
      end else begin
        FRequestTM:=getTickCount;
        TKRMBCBatchUpdates(Collection).FMBC.FModbus.SendPack(
          TKRMBCBatchUpdates(Collection).FMBC.FMBType,
          FReadPackFunc,
          @FReadPack,
          FReadPackLen,
          MBCallBack,
          FReadPackRLen
        )
      end
    else if FInterval>0 then SetTimer(FWH, 1, FInterval, nil);
  end else Msg.Result := DefWindowProc(FWH, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TKRMBCBatchUpdate.UpdateReadPack;
begin
  if Assigned(Collection)and Assigned(TKRMBCBatchUpdates(Collection).FMBC.FModbus) then begin
    if FReadFunction=mbrfReadHoldingRegisters then begin
      FReadPackLen:=TKRMBCBatchUpdates(Collection).FMBC.FModbus.ReadHoldingRegistersPack(
        TKRMBCBatchUpdates(Collection).FMBC.FMBType,
        TKRMBCBatchUpdates(Collection).FMBC.Addres,
        FIndex,
        FCount,
        @FReadPack,
        FReadPackRLen
      );
      FReadPackFunc:=mbfReadHoldingRegisters;
    end else begin
      FReadPackLen:=TKRMBCBatchUpdates(Collection).FMBC.FModbus.ReadInputRegistersPack(
        TKRMBCBatchUpdates(Collection).FMBC.FMBType,
        TKRMBCBatchUpdates(Collection).FMBC.Addres,
        FIndex,
        FCount,
        @FReadPack,
        FReadPackRLen
      );
      FReadPackFunc:=mbfReadInputRegisters;
    end;
  end else FReadPackLen:=0;
end;

end.
