(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVariables                                                               *)
(*  Ver.: 08.10.2019                                                          *)
(*  https://kandiral.ru/delphi/krvariables.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRVariables;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Vcl.ExtCtrls, Vcl.Forms,
    System.Variants, System.SysUtils,
  {$ELSE}
    Windows, Messages, Classes, ExtCtrls, Forms, Variants, SysUtils,
  {$IFEND}
  KRComponentCollection, KRIniConfig, KRRuntimeErrors;

type
  TKRVariabels = class;
  TKRVariable = class;

  TVarUpdateType = (vutBefore, vutAfter, vutAftUpdate);
  TVarType = (VT_BYTE, VT_WORD, VT_DWORD, VT_SMALLINT, VT_INT, VT_SINGLE,
    VT_STRING, VT_INT64, VT_DOUBLE, VT_UINT64);
  TValUpdate = procedure (Sender: TObject; Variable: TKRVariable) of object;

  IKRVarUp = interface
    ['{D8C73BEF-8E1C-4A36-AA2D-EDB104E1C646}']
    procedure VarUp(AVar: TKRVariable); stdcall;
    procedure VarErr(AVar: TKRVariable); stdcall;
  end;

  TKRVariable = class(TKRComponentCollectionItem, IKRCfgParam)
  private
    FVariabels: TKRVariabels;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FValue: Variant;
    FType: TVarType;
    FUpdate: TValUpdate;
    FUpdateType: TVarUpdateType;
    FEError: TValUpdate;
    FUpAftWrite: boolean;
    FCfgInterval: TKRIniCfgParam;
    FList: TList;
    FUserError: boolean;
    FUserErrorMsg: String;
    FInterval: Integer;
    FCfgValue: TKRIniCfgParam;
    FCfgValueChange: boolean;
    FCfgValOnlySave: boolean;
    FVarUpLock: boolean;
    FVarLock: TObject;
    FWH: HWND;
    FValUpdated: boolean;
    FWaitForUpdates: boolean;
    FErrorCounter: integer;
    FErrorCount: integer;
    procedure TmWP(var Msg: TMessage);
    function GetInterval: Integer;
    function GetUpAftWrite: boolean;
    procedure CfgParamChange(AParam: TKRIniCfgParam); stdcall;
    procedure SetUserError(const AValue: boolean);
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const AValue: TKRRuntimeErrorEv);
    procedure SetCfgValue(const AValue: TKRIniCfgParam);
    procedure SetEError(const Value: TValUpdate);
    procedure SetUpdate(const Value: TValUpdate);
    procedure VarCSEnter;
    procedure VarCSLeave;
    function GetError_: integer;
    procedure SetErrorCount(const Value: integer);
  protected
    procedure DoRuntimeError(ADesc: String; E: Exception);
    procedure _ParamChange(AParam: TKRIniCfgParam);virtual;
    function GetValue: Variant;virtual;
    procedure SetValueF(const AValue: Variant);
    procedure SetValue(const AValue: Variant);virtual;
    procedure SetValue_(const AValue: Variant);virtual;
    procedure SetValue_c(AValue: Variant);virtual;
    procedure SetType(const AValue: TVarType);virtual;
    function GetError: integer;virtual;
    procedure DoError(isUp: boolean);
    function GetErrorMsg: String;virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateValue_;virtual;
    procedure UpdateValue_c(AVal: Variant);virtual;
    procedure VarUpCS_Enter;
    procedure VarUpCS_Leave;
    procedure SetInterval(const AValue: Integer);
    procedure SetCfgInterval(const AValue: TKRIniCfgParam);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure UpdateValue;
    property ErrorMsg: String read GetErrorMsg;
    property Error: integer read GetError_;
    procedure AddMon(AMon: IKRVarUp);
    procedure DelMon(AMon: IKRVarUp);
    property Value: Variant read GetValue write SetValue;
    property VarType: TVarType read FType write SetType default VT_WORD;
    property Interval: Integer read GetInterval write SetInterval default 0;
    property CfgInterval: TKRIniCfgParam read FCfgInterval write SetCfgInterval;
    property CfgValue: TKRIniCfgParam read FCfgValue write SetCfgValue;
    property CfgValOnlySave: boolean read FCfgValOnlySave write FCfgValOnlySave default true;
    property UpdateType: TVarUpdateType read FUpdateType write FUpdateType default vutAfter;
    property OnValUpdated: TValUpdate read FUpdate write SetUpdate;
    property OnError: TValUpdate read FEError write SetEError;
    property UpAftWrite: boolean read GetUpAftWrite write FUpAftWrite default true;
    property UserError: boolean read FUserError write SetUserError default false;
    property UserErrorMsg: String read FUserErrorMsg write FUserErrorMsg;
    property ValUpdated: boolean read FValUpdated;
    property WaitForUpdates: boolean read FWaitForUpdates write FWaitForUpdates default false;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
  published
    property ErrorCount: integer read FErrorCount write SetErrorCount default 1;
  end;

  TKRVariabels = class(TKRComponentCollection)
  private
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FActive: Boolean;
    FReactionTime: word;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
  protected
    procedure DoRuntimeError(ADesc: String; E: Exception);
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);override;
    procedure SetActive(const Value: boolean);virtual;
  public
    constructor Create(AOwner: TComponent);override;
    property Active: boolean read FActive write SetActive;
    procedure updateAllVariabels;
    procedure updateAllWaitingVariabels;
  published
    property ReactionTime: word read FReactionTime write FReactionTime default 5;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
  end;

implementation

uses Funcs, Math;

var
  KRVariableID: Cardinal;

{ TKRVariable }

procedure TKRVariable.AddMon(AMon: IKRVarUp);
var
  i: integer;
begin
  if FList.Count>0 then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]=Pointer(AMon) then exit;
  FList.Add(Pointer(AMon));
end;

procedure TKRVariable.CfgParamChange(AParam: TKRIniCfgParam);
begin
  _ParamChange(AParam);
end;

constructor TKRVariable.Create(AOwner: TComponent);
begin
  inherited;
  FErrorCount:=1;
  FErrorCounter:=0;
  FValUpdated:=false;
  FWaitForUpdates:=false;
  Inc(KRVariableID);
  FVarLock:=TObject.Create;
  FUserError:=false;
  FList:=TList.Create;
  FUpdateType:=vutAfter;
  FType:=VT_WORD;
  FUpAftWrite:=true;
  FCfgValueChange:=false;
  FCfgValOnlySave:=true;
  FWH := AllocateHWnd(tmWP);
  FVarUpLock:=false;
end;

procedure TKRVariable.DelMon(AMon: IKRVarUp);
begin
  FList.Remove(Pointer(AMon));
end;

destructor TKRVariable.Destroy;
begin
  KillTimer(FWH, 1);
  KillTimer(FWH, 2);
  KillTimer(FWH, 3);
  DeallocateHWnd(FWH);
  if Assigned(FCfgInterval) then FCfgInterval.DelMon(Self);
  if Assigned(FCfgValue) then FCfgValue.DelMon(Self);
  FList.Free;
  FVarLock.Free;
  inherited;
end;

procedure TKRVariable.DoError(isUp: boolean);
begin
  inc(FErrorCounter);
  KillTimer(FWH, 1);
  if FInterval>0 then begin
    {if isUp then }SetTimer(FWH, 1, FInterval, nil);
  end else if FWaitForUpdates and (not FValUpdated) then SetTimer(FWH, 1, 1000, nil);
  if FErrorCounter=FErrorCount then begin
    inc(FErrorCounter);
    KillTimer(FWH, 3);
    SetTimer(FWH, 3, FVariabels.FReactionTime, nil);
    //FErrorCounter:=0;
  end;
  VarUpCS_Leave;
end;

procedure TKRVariable.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRVariable.GetError: integer;
begin
  Result:=0;
end;

function TKRVariable.GetErrorMsg: String;
begin

end;

function TKRVariable.GetError_: integer;
begin
  if FErrorCounter>FErrorCount then Result:=GetError else Result:=0;
end;

function TKRVariable.GetInterval: Integer;
begin
  Result:=FInterval;
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
  VarCSEnter;
  Result:=FValue;
  VarCSLeave;
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

procedure TKRVariable.SetEError(const Value: TValUpdate);
begin
  FEError:=Value;
end;

procedure TKRVariable.SetErrorCount(const Value: integer);
begin
  FErrorCount := Value;
  if FErrorCount<1 then FErrorCount:=1;
  if FErrorCount>100 then FErrorCount:=100;  
end;

procedure TKRVariable.SetInterval(const AValue: Integer);
begin
  if FInterval<>AValue then begin
    KillTimer(FWH, 1);
    FInterval:=AValue;
    if FInterval<1 then FInterval:=0 else
      SetTimer(FWH, 1, FInterval, nil);
    if Assigned(CfgInterval) then CfgInterval.Value:=FInterval;
  end;
end;

procedure TKRVariable.SetRuntimeErrorEv(const AValue: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FRuntimeErrorEv:=AValue;
  RECS_Leave;
end;

procedure TKRVariable.SetType(const AValue: TVarType);
begin
  if FType<>AValue then begin
    FType := AValue;
    case FType of
      VT_STRING: SetValueF('')
      else SetValueF(0);
    end;
  end;
end;

procedure TKRVariable.SetUpdate(const Value: TValUpdate);
begin
  FUpdate := Value;
end;

procedure TKRVariable.SetUserError(const AValue: boolean);
begin
  FUserError := AValue;
end;

procedure TKRVariable.SetValue(const AValue: Variant);
begin
  if FUpdateType=vutBefore then SetValueF(AValue);
  SetValue_(AValue);
end;

procedure TKRVariable.SetValueF(const AValue: Variant);
begin
  VarCSEnter;
  FValue:=AValue;
  if assigned(FCfgValue)and(not IsVariantsEqual(FValue,FCfgValue.Value))then begin
    FCfgValueChange:=true;
    FCfgValue.Value:=FValue;
    FCfgValueChange:=false;
  end;
  VarCSLeave;
end;

procedure TKRVariable.SetValue_(const AValue: Variant);
begin
  VarUpCS_Enter;
end;

procedure TKRVariable.SetValue_c(AValue: Variant);
begin
  FErrorCounter:=0;
  if FUpdateType=vutAfter then SetValueF(AValue);
  VarUpCS_Leave;
  if FUpAftWrite then UpdateValue;
end;

procedure TKRVariable.TmWP(var Msg: TMessage);
var i: integer;
begin
  if Msg.Msg=WM_TIMER then begin
    case Msg.WParam of
    1: begin
        KillTimer(FWH, 1);
        if FVariabels.FActive then UpdateValue_ else
          if FInterval>0 then SetTimer(FWH, 1, FInterval, nil);
      end;
    2: begin
        KillTimer(FWH, 2);
        if Assigned(FUpdate) then FUpdate(self,Self);
        for I := 0 to FList.Count-1 do IKRVarUp(FList.Items[i]).VarUp(Self);
      end;
    3: begin
        KillTimer(FWH, 3);
        if Assigned(FEError) then FEError(self,Self);
        for I := 0 to FList.Count-1 do IKRVarUp(FList.Items[i]).VarErr(Self);
      end;
    end;
  end else Msg.Result := DefWindowProc(FWH, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TKRVariable.UpdateValue;
begin
  if FInterval=0 then begin
    FValUpdated:=false;
    KillTimer(FWH, 1);
    SetTimer(FWH, 1, FVariabels.FReactionTime, nil);
  end;
end;

procedure TKRVariable.UpdateValue_;
begin
  VarUpCS_Enter;
end;

procedure TKRVariable.UpdateValue_c(AVal: Variant);
begin
  FErrorCounter:=0;
  FValUpdated:=true;
  SetValueF(AVal);
  KillTimer(FWH, 1);
  if FInterval>0 then SetTimer(FWH, 1, FInterval, nil);
  KillTimer(FWH, 2);
  SetTimer(FWH, 2, FVariabels.FReactionTime, nil);
  VarUpCS_Leave;
end;

procedure TKRVariable.VarCSEnter;
begin
  System.TMonitor.Enter(FVarLock);
end;

procedure TKRVariable.VarCSLeave;
begin
  System.TMonitor.Exit(FVarLock);
end;

procedure TKRVariable.VarUpCS_Enter;
begin
  while FVarUpLock do Application.ProcessMessages;
  FVarUpLock:=true;
end;

procedure TKRVariable.VarUpCS_Leave;
begin
  FVarUpLock:=false;
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

{ TKRVariabels }

procedure TKRVariabels.AftAddItem(var AItem: TKRComponentCollectionItem);
begin
  TKRVariable(AItem).FVariabels:=self;
end;

constructor TKRVariabels.Create(AOwner: TComponent);
begin
  FReactionTime:=5;
  inherited;
end;

procedure TKRVariabels.DoRuntimeError(ADesc: String; E: Exception);
begin
  REEvent(FRuntimeErrorEv,Self,ADesc,E);
end;

function TKRVariabels.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FRuntimeErrorEv;
  RECS_Leave;
end;

procedure TKRVariabels.SetActive(const Value: boolean);
begin
  FActive := Value;
  if not FActive then Sleep(1000);
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
    if vr.WaitForUpdates then
      vr.UpdateValue;
  end;
end;

begin
  KRVariableID:=0;
end.
