(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVariables                                                               *)
(*  Ver.: 14.07.2020                                                          *)
(*  https://kandiral.ru/delphi/krvariables.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRVariables;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Vcl.ExtCtrls, Vcl.Forms,
    System.Variants, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Windows, Messages, Classes, ExtCtrls, Forms, Variants, SysUtils, SyncObjs,
  {$IFEND}
  KRComponentCollection, KRIniConfig, KRRuntimeErrors, KRVariants;

const
  VARTHREAD_QUEUE_MAX_EVENTS = 16;
    
type
  TKRVariabels = class;
  TKRVariable = class;
  TKRVarThread = class;

  TVarUpdateType = (vutBefore, vutAfter, vutAftUpdate);
  TVarType = (VT_BYTE, VT_WORD, VT_DWORD, VT_SMALLINT, VT_INT, VT_SINGLE,
    VT_STRING, VT_INT64, VT_DOUBLE, VT_UINT64);
  TValUpdate = procedure (Sender: TObject; Variable: TKRVariable) of object;

  IKRVarUp = interface
    ['{D8C73BEF-8E1C-4A36-AA2D-EDB104E1C646}']
    procedure VarUp(AVar: TKRVariable); stdcall;
    procedure VarErr(AVar: TKRVariable); stdcall;
  end;

  TKRVarUpEvent = record
    FEvent: integer;
    FValue: variant;
  end;

  TKRVariable = class(TKRComponentCollectionItem, IKRCfgParam)
  private
    FVariabels: TKRVariabels;    
    FRuntimeErrorEv: TKRRuntimeErrorEv;    
    FList: TList;
    FMonCount: integer;
    FValue: Variant;
    FType: TVarType;    
    FUpdateType: TVarUpdateType;
    FUpAftWrite: boolean;
    FWaitForUpdates: boolean;
    
    FUserError: boolean;
    FUserErrorMsg: String;
    
    FCfgInterval: TKRIniCfgParam;
    FCfgValue: TKRIniCfgParam;
    FCfgValueChange: boolean;
    FCfgValOnlySave: boolean;



    CS: TCriticalSection;
    FWH: HWND;
    FIntervalMin, FRequestTM: cardinal;
    FThread: TKRVarThread;
    FInterval: Cardinal;
    FEError: TValUpdate;
    FUpdate: TValUpdate;

    
    function GetUpAftWrite: boolean;
    procedure CfgParamChange(AParam: TKRIniCfgParam); stdcall;
    procedure SetUserError(const AValue: boolean);
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const AValue: TKRRuntimeErrorEv);
    procedure SetCfgValue(const AValue: TKRIniCfgParam);


    
    procedure TmWP(var Msg: TMessage);
    procedure SetInterval(const Value: Cardinal);
    procedure SetValue(const AValue: Variant);virtual;
  protected
    FAllowUpdate: boolean;


    procedure DoRuntimeError(ADesc: String; E: Exception);
    procedure _ParamChange(AParam: TKRIniCfgParam);virtual;
    procedure SetType(const AValue: TVarType);virtual;
    function GetError: integer;virtual;
    function GetErrorMsg: String;virtual;abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCfgInterval(const AValue: TKRIniCfgParam);


    procedure Activate;
    procedure Deactivate;
    procedure DoError(AAsync: boolean = false);virtual;
    procedure UpdateValueRequest; virtual;
    procedure UpdateValueResponse(AVal: Variant; AAsync: boolean = false);virtual;
    procedure SetValueRequest(var AValue: Variant);virtual;abstract;
    procedure SetValueResponse(var AValue: Variant);virtual;
    procedure SetValueCS(var AValue: Variant);
    procedure SetValueNoCS(var AValue: Variant);
    function GetValue: Variant;virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Value: Variant read GetValue write SetValue;


    //----- 
    property ErrorMsg: String read GetErrorMsg;
    property Error: integer read GetError;
    procedure AddMon(AMon: IKRVarUp);
    procedure DelMon(AMon: IKRVarUp);
    property CfgInterval: TKRIniCfgParam read FCfgInterval write SetCfgInterval;
    property CfgValue: TKRIniCfgParam read FCfgValue write SetCfgValue;
    property CfgValOnlySave: boolean read FCfgValOnlySave write FCfgValOnlySave default true;
    property UserError: boolean read FUserError write SetUserError default false;
    property UserErrorMsg: String read FUserErrorMsg write FUserErrorMsg;
    // ----------

    procedure UpdateValue;
    
    // Pubblished Propery
    property VarType: TVarType read FType write SetType default VT_WORD;
    property Interval: Cardinal read FInterval write SetInterval default 0;
    property IntervalMin: cardinal read FIntervalMin write FIntervalMin default 5;
    property UpdateType: TVarUpdateType read FUpdateType write FUpdateType default vutAfter;
    property UpAftWrite: boolean read GetUpAftWrite write FUpAftWrite default true;
    property WaitForUpdates: boolean read FWaitForUpdates write FWaitForUpdates default false;

    // Pubblished Events
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
    property OnValUpdated: TValUpdate read FUpdate write FUpdate;
    property OnError: TValUpdate read FEError write FEError;
    
  end;

  TKRVarThread = class(TThread)
  private
    CS: TCriticalSection;
    FCurValue: Variant;
    FWaiting: boolean;
    FVariable: TKRVariable;
    FQueue: array[0..VARTHREAD_QUEUE_MAX_EVENTS-1] of TKRVarUpEvent;
    FQueueCount, FQueuePosIn, FQueuePosOut: integer;

    FEvents: array [0..6] of THandle;
  protected
    procedure Execute; override;
    procedure evUpdate; virtual;
    procedure evSetValue; virtual;
    procedure evError; virtual;
    procedure evUpdated; virtual;
  public
    constructor CreateTh;
    destructor Destroy; override;
    procedure Deactivate;
    procedure push(var AValue: Variant);overload;        
    procedure push;overload;
  end;

  TKRVariabels = class(TKRComponentCollection)
  private
    CS: TCriticalSection;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FActive: Boolean;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    procedure SetActive(const Value: boolean);virtual;
    function GetActive: boolean; virtual;
  protected
    procedure DoRuntimeError(ADesc: String; E: Exception);
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);override;

  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure updateAllVariabels;
    procedure updateAllWaitingVariabels;

    property Active: boolean read GetActive write SetActive;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
  end;

implementation

var
  KRVariableID: Cardinal;

{ TKRVariable }

procedure TKRVariable.Activate;
begin
  CS.Enter;
  try
    FThread:=TKRVarThread.CreateTh;
    FThread.FVariable:=Self;    
    if FInterval>0 then FThread.push;    
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.AddMon(AMon: IKRVarUp);
var
  i: integer;
begin
  if FList.Count>0 then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]=Pointer(AMon) then exit;
  FList.Add(Pointer(AMon));
  FMonCount:=FList.Count;
end;

procedure TKRVariable.CfgParamChange(AParam: TKRIniCfgParam);
begin
  _ParamChange(AParam);
end;

constructor TKRVariable.Create(AOwner: TComponent);
begin
  inherited;

  CS:=TCriticalSection.Create;
  FAllowUpdate:=true;
  FInterval:=0;
  FIntervalMin:=5;
  FWH := AllocateHWnd(tmWP);
  FMonCount:=0;
  FList:=TList.Create;


  FWaitForUpdates:=false;
  Inc(KRVariableID);
  FUserError:=false;
  FUpdateType:=vutAfter;
  FType:=VT_WORD;
  FUpAftWrite:=true;
  FCfgValueChange:=false;
  FCfgValOnlySave:=true;
end;

procedure TKRVariable.Deactivate;
begin
  CS.Enter;
  try
    if FThread=nil then exit;
    KillTimer(FWH, 1);
    FThread.Deactivate;
    FreeAndNil(FThread);
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.DelMon(AMon: IKRVarUp);
begin
  FList.Remove(Pointer(AMon));
  FMonCount:=FList.Count;
end;

destructor TKRVariable.Destroy;
begin
  KillTimer(FWH, 1);
  Deactivate;


  if Assigned(FCfgInterval) then FCfgInterval.DelMon(Self);
  if Assigned(FCfgValue) then FCfgValue.DelMon(Self);
  FList.Free;



  DeallocateHWnd(FWH);
  CS.Free;

  inherited;
end;

procedure TKRVariable.DoError(AAsync: boolean = false);
var
  tm: cardinal;
begin
  CS.Enter;
  try
    if FThread=nil then exit;
    if AAsync then SetEvent(FThread.FEvents[6]) else begin
      SetEvent(FThread.FEvents[2]);
      if FInterval>0 then begin
        tm:=getTickCount-FRequestTM;
        if(tm>FInterval)or(FInterval-tm<FIntervalMin)then tm:=FIntervalMin else tm:=FInterval-tm;
        SetTimer(FWH, 1, tm, nil);
      end else if FWaitForUpdates then FThread.push;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRVariable.GetError: integer;
begin
  Result:=0;
end;

function TKRVariable.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRVariable.GetUpAftWrite: boolean;
begin
  Result := FUpAftWrite;
end;

function TKRVariable.GetValue: Variant;
begin
  CS.Enter;
  try
    Result:=FValue;
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgInterval)then FCfgInterval:=nil else
    if (AComponent = FCfgValue)then FCfgValue:=nil;
end;

procedure TKRVariable.SetCfgInterval(const AValue: TKRIniCfgParam);
begin
  if FCfgInterval<>AValue then begin
    if Assigned(FCfgInterval) then FCfgInterval.DelMon(Self);
    FCfgInterval := AValue;
    if Assigned(FCfgInterval) then begin
      FCfgInterval.FreeNotification(self);
      FCfgInterval.AddMon(Self);
      _ParamChange(FCfgInterval);
    end;
  end;
end;

procedure TKRVariable.SetCfgValue(const AValue: TKRIniCfgParam);
begin
  if FCfgValue<>AValue then begin
    if Assigned(FCfgValue) then FCfgValue.DelMon(Self);
    FCfgValue := AValue;
    if Assigned(FCfgValue) then begin
      FCfgValue.FreeNotification(self);
      FCfgValue.AddMon(Self);
      _ParamChange(FCfgValue);
    end;
  end;
end;

procedure TKRVariable.SetInterval(const Value: Cardinal);
var
  oldVl: integer;
begin
  if FInterval=Value then exit;
  oldVl:=FInterval;
  FInterval:=Value;  
  CS.Enter;
  try
    if FThread=nil then exit;  
    if oldVl=0 then FThread.push;    
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.SetRuntimeErrorEv(const AValue: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=AValue;
  RECS_Leave;
end;

procedure TKRVariable.SetType(const AValue: TVarType);
var
  bt: byte;
  wd: word;
  dw: cardinal;
  ddw: uint64;
  i0: Smallint;
  i1: integer;
  i2: int64;
  f0: Single;
  f1: double;
  s: String;
  vr: variant;
begin
  if FType<>AValue then begin
    CS.Enter;
    try
      FType := AValue;
      case FType of
        VT_BYTE: begin bt:=0;vr:=bt;SetValueNoCS(vr);end;
        VT_WORD: begin wd:=0;vr:=wd;SetValueNoCS(vr);end;
        VT_DWORD: begin dw:=0;vr:=dw;SetValueNoCS(vr);end;
        VT_UINT64: begin ddw:=0;vr:=ddw;SetValueNoCS(vr);end;
        VT_SMALLINT: begin i0:=0;vr:=i0;SetValueNoCS(vr);end;
        VT_INT: begin i1:=0;vr:=i1;SetValueNoCS(vr);end;
        VT_INT64: begin i2:=0;vr:=i2;SetValueNoCS(vr);end;
        VT_SINGLE: begin f0:=0;vr:=f0;SetValueNoCS(vr);end;
        VT_DOUBLE: begin f1:=0;vr:=f1;SetValueNoCS(vr);end;
        VT_STRING: begin s:='';vr:=s;SetValueNoCS(vr);end;
      end;
    finally
      CS.Leave;
    end;
  end;
end;

procedure TKRVariable.SetUserError(const AValue: boolean);
begin
  FUserError := AValue;
end;

procedure TKRVariable.SetValue(const AValue: Variant);
var
  vr: Variant;
begin
  CS.Enter;
  try
    if FThread=nil then exit;
    vr:=AValue;
    if FUpdateType=vutBefore then SetValueNoCS(vr);
    FThread.push(vr);
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.SetValueCS(var AValue: Variant);
begin
  CS.Enter;
  try
    SetValueNoCS(AValue);
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.SetValueNoCS(var AValue: Variant);
begin
  if KRVarIsEqually(AValue, FValue) then exit;
  FValue:=AValue;
  if assigned(FCfgValue)then begin
    FCfgValueChange:=true;
    FCfgValue.Value:=FValue;
    FCfgValueChange:=false;
  end;
end;

procedure TKRVariable.SetValueResponse(var AValue: Variant);
begin
  CS.Enter;
  try
    if FThread=nil then exit;
    if FUpdateType=vutAfter then SetValueNoCS(AValue);
    SetEvent(FThread.FEvents[4]);
  finally
    CS.Leave;
  end;
  if FUpAftWrite then UpdateValue;
end;

procedure TKRVariable.TmWP(var Msg: TMessage);
begin
  if(Msg.Msg=WM_TIMER)and(Msg.WParam=1)then begin
    KillTimer(FWH, 1);
    CS.Enter;
    try
      if FThread=nil then exit;
      FThread.push;
    finally
      CS.Leave;
    end;
  end else Msg.Result := DefWindowProc(FWH, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TKRVariable.UpdateValue;
begin
  CS.Enter;
  try
    if FThread=nil then exit;
    if(FInterval>0)or(not FAllowUpdate)then exit;
    FThread.push;
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable.UpdateValueRequest;
begin
  FRequestTM := getTickCount;
end;

procedure TKRVariable.UpdateValueResponse(AVal: Variant; AAsync: boolean);
var
  tm: Cardinal;
begin
  CS.Enter;
  try
    SetValueNoCS(AVal);
    if FThread=nil then exit;
    if AAsync then SetEvent(FThread.FEvents[5]) else begin
      SetEvent(FThread.FEvents[3]);
      if FInterval>0 then begin
        tm:=getTickCount-FRequestTM;
        if(tm>FInterval)or(FInterval-tm<FIntervalMin)then tm:=FIntervalMin else tm:=FInterval-tm;
        SetTimer(FWH, 1, tm, nil);
      end;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TKRVariable._ParamChange(AParam: TKRIniCfgParam);
var
  _prm: TKRIniCfgParam;
begin
  if Assigned(FCfgInterval) and (FCfgInterval.Name=AParam.Name) then begin
    _prm:=FCfgInterval;
    FCfgInterval:=nil;
    Interval:=_prm.Value;
    FCfgInterval:=_prm;
  end else
  if Assigned(FCfgValue) and (FCfgValue.Name=AParam.Name)and(not FCfgValueChange)and(not FCfgValOnlySave)then begin
    SetValue(FCfgValue.Value);
  end;
end;

{ TKRVarThread }

constructor TKRVarThread.CreateTh;
begin
  FWaiting:=false;
  CS:=TCriticalSection.Create;
  FEvents[0]:=CreateEvent(nil, true, false, nil);
  FEvents[1]:=CreateEvent(nil, true, false, nil);
  FEvents[2]:=CreateEvent(nil, true, false, nil);
  FEvents[3]:=CreateEvent(nil, true, false, nil);
  FEvents[4]:=CreateEvent(nil, true, false, nil);
  FEvents[5]:=CreateEvent(nil, true, false, nil);
  FEvents[6]:=CreateEvent(nil, true, false, nil);
  inherited Create(false);
end;

procedure TKRVarThread.Deactivate;
begin
  Terminate;
  SetEvent(FEvents[0]);
end;

destructor TKRVarThread.Destroy;
begin
  Deactivate;
  inherited;
end;

procedure TKRVarThread.evError;
var
  i,n: integer;
begin
  if Assigned(FVariable.FEError) then FVariable.FEError(FVariable,FVariable);
  n:=FVariable.FList.Count-1;
  for i := 0 to n do IKRVarUp(FVariable.FList.Items[i]).VarErr(FVariable);
end;

procedure TKRVarThread.evSetValue;
begin
  FVariable.SetValueRequest(FCurValue);
end;

procedure TKRVarThread.evUpdate;
begin
  FVariable.UpdateValueRequest;
end;

procedure TKRVarThread.evUpdated;
var
  i,n: integer;
begin
  if Assigned(FVariable.FUpdate) then FVariable.FUpdate(FVariable,FVariable);
  n:=FVariable.FList.Count-1;
  for i := 0 to n do IKRVarUp(FVariable.FList.Items[i]).VarUp(FVariable);
end;

procedure TKRVarThread.Execute;
var
  Signal: Cardinal;
  FCurEvent: integer;
begin
  while not Terminated do begin
    Signal:=WaitForMultipleObjects(7, @FEvents, False, INFINITE);
    case Signal of
      WAIT_OBJECT_0 + 1: begin // QueueEvent
        CS.Enter;
        FCurEvent:=FQueue[FQueuePosOut].FEvent;
        FCurValue:=FQueue[FQueuePosOut].FValue;
        if FQueuePosOut=VARTHREAD_QUEUE_MAX_EVENTS-1 then FQueuePosOut:=0 else inc(FQueuePosOut);
        dec(FQueueCount);
        FWaiting:=true;
        ResetEvent(FEvents[1]);
        CS.Leave;
        if FCurEvent=0 then
          Synchronize(evUpdate)
        else
          Synchronize(evSetValue);
      end;
      WAIT_OBJECT_0 + 2: begin // ErrorEvent
        ResetEvent(FEvents[2]);
        if Assigned(FVariable.FEError) or (FVariable.FMonCount>0) then Synchronize(evError);
        CS.Enter;
        FWaiting:=false;
        if FQueueCount>0 then SetEvent(FEvents[1]);
        CS.Leave;
      end;
      WAIT_OBJECT_0 + 3: begin // UpdatedEvent
        ResetEvent(FEvents[3]);
        if Assigned(FVariable.FUpdate) or (FVariable.FMonCount>0) then Synchronize(evUpdated);
        CS.Enter;
        FWaiting:=false;
        if FQueueCount>0 then SetEvent(FEvents[1]);
        CS.Leave;
      end;
      WAIT_OBJECT_0 + 4: begin // SetValue completed
        ResetEvent(FEvents[4]);
        CS.Enter;
        FWaiting:=false;
        if FQueueCount>0 then SetEvent(FEvents[1]);
        CS.Leave;
      end;
      WAIT_OBJECT_0 + 5: begin // UpdatedEvent Async
        ResetEvent(FEvents[5]);
        if Assigned(FVariable.FUpdate) or (FVariable.FMonCount>0) then Synchronize(evUpdated);
      end;
      WAIT_OBJECT_0 + 6: begin // ErrorEvent Async
        ResetEvent(FEvents[6]);
        if Assigned(FVariable.FEError) or (FVariable.FMonCount>0) then Synchronize(evError);
      end
      else break;
    end;
  end;

  CloseHandle(FEvents[0]);
  CloseHandle(FEvents[1]);
  CloseHandle(FEvents[2]);
  CloseHandle(FEvents[3]);
  CloseHandle(FEvents[4]);
  CloseHandle(FEvents[5]);
  CloseHandle(FEvents[6]);
  CS.Free;
end;

procedure TKRVarThread.push;
begin
  if FQueueCount=VARTHREAD_QUEUE_MAX_EVENTS then begin
    //  Overflow. I hope this never happens :)
  end else begin
    CS.Enter;
    FQueue[FQueuePosIn].FEvent:=0;
    if FQueuePosIn=VARTHREAD_QUEUE_MAX_EVENTS-1 then FQueuePosIn:=0 else inc(FQueuePosIn);
    inc(FQueueCount);
    if not FWaiting then SetEvent(FEvents[1]);
    CS.Leave;
  end;
end;

procedure TKRVarThread.push(var AValue: Variant);
begin
  if FQueueCount=VARTHREAD_QUEUE_MAX_EVENTS then begin
    //  Overflow. I hope this never happens :)
  end else begin
    CS.Enter;
    FQueue[FQueuePosIn].FEvent:=1;
    FQueue[FQueuePosIn].FValue:=AValue;
    if FQueuePosIn=VARTHREAD_QUEUE_MAX_EVENTS-1 then FQueuePosIn:=0 else inc(FQueuePosIn);
    inc(FQueueCount);
    if not FWaiting then SetEvent(FEvents[1]);
    CS.Leave;
  end;
end;

{ TKRVariabels }

procedure TKRVariabels.AftAddItem(var AItem: TKRComponentCollectionItem);
begin
  TKRVariable(AItem).FVariabels:=self;
end;

constructor TKRVariabels.Create(AOwner: TComponent);
begin
  CS:=TCriticalSection.Create;
  FActive:=false;
  inherited;
end;

destructor TKRVariabels.Destroy;
begin
  CS.Free;
  inherited;
end;

procedure TKRVariabels.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRVariabels.GetActive: boolean;
begin
  CS.Enter;
  Result:=FActive;
  CS.Leave;
end;

function TKRVariabels.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

procedure TKRVariabels.SetActive(const Value: boolean);
var
  i: integer;
begin
  if Value=FActive then exit;
  CS.Enter;
  FActive:=Value;
  try
    if FActive then 
      for i := 0 to ItemsCount-1 do TKRVariable(Items[i]).Activate
    else
      for i := 0 to ItemsCount-1 do TKRVariable(Items[i]).Deactivate;
  finally
    CS.Leave;
  end;
end;

procedure TKRVariabels.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

procedure TKRVariabels.updateAllVariabels;
var
  i: integer;
begin
  for i := 0 to ItemsCount-1 do
    TKRVariable(Items[i]).UpdateValue;
end;

procedure TKRVariabels.updateAllWaitingVariabels;
var
  i: integer;
  vr: TKRVariable;
begin
  for i := 0 to ItemsCount-1 do begin
    vr:=TKRVariable(Items[i]);
    if vr.WaitForUpdates then vr.UpdateValue;
  end;
end;

begin
  KRVariableID:=0;
end.
