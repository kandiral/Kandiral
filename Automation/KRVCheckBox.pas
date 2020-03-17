(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVCheckBox                                                               *)
(*  Ver.: 31.08.2017                                                          *)
(*  https://kandiral.ru/delphi/krvcheckbox.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRVCheckBox;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.StdCtrls, Winapi.MMSystem, Winapi.Messages, Vcl.Controls,
    Vcl.ExtCtrls, Vcl.Forms,
  {$ELSE}
    Classes, StdCtrls, MMSystem, Messages, Controls, ExtCtrls, Forms,
  {$IFEND}
  KRTimer, KRVariables;

type
  TKRVCBAType = (vcbatSetBit, vcbatSetValue);
  TKRVCBSType = (vcbstByBit, vcbstByValue);

  TKRVCheckBox = class(TCustomCheckBox, IKRTimer)
  private
    FAction: TKRVCBAType;
    FVarIn: TKRVariable;
    FVarOut: TKRVariable;
    FTimer: TKRTimer;
    FClick: TNotifyEvent;
    FState: TKRVCBSType;
    FStatBit: byte;
    FActBit: byte;
    FOnActValue: Variant;
    FOnStatValue: Variant;
    FOffActValue: Variant;
    procedure SetAction(const Value: TKRVCBAType);
    procedure _Click;
    procedure DoTimer; stdcall;
    procedure SetVarIn(const Value: TKRVariable);
    procedure SetVarOut(const Value: TKRVariable);
    procedure SetTimer(const Value: TKRTimer);
    procedure SetState(const Value: TKRVCBSType);
    procedure SetOffActValue(const Value: Variant);
    procedure SetOnActValue(const Value: Variant);
    procedure SetOnStatValue(const Value: Variant);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Toggle; override;
  public
    constructor Create(AOvner: TComponent);override;
    destructor Destroy;override;
  published
    property Action: TKRVCBAType read FAction write SetAction default vcbatSetBit;
    property State: TKRVCBSType read FState write SetState default vcbstByBit;
    property VarIn: TKRVariable read FVarIn write SetVarIn;
    property VarOut: TKRVariable read FVarOut write SetVarOut;
    property ActBit: byte read FActBit write FActBit;
    property StatBit: byte read FStatBit write FStatBit;
    property OnActValue: Variant read FOnActValue write SetOnActValue;
    property OffActValue: Variant read FOffActValue write SetOffActValue;
    property OnStatValue: Variant read FOnStatValue write SetOnStatValue;
    property Timer: TKRTimer read FTimer write SetTimer;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property OnClick: TNotifyEvent read FClick write FClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses lgop, funcs;

{ TKRVCheckBox }

constructor TKRVCheckBox.Create(AOvner: TComponent);
begin
  inherited;
  FAction:=vcbatSetBit;
  FState:=vcbstByBit;
  FActBit:=0;
  FStatBit:=0;
end;

destructor TKRVCheckBox.Destroy;
begin
  if(Assigned(FTimer))then FTimer.DelMon(Self);
  inherited;
end;

procedure TKRVCheckBox.DoTimer;
var
  ch: boolean;
begin
  if not assigned(FVarIn) then exit;
  ch:=false;
  case FState of
    vcbstByBit: ch:=getBit(FVarIn.Value,FStatBit);
    vcbstByValue: ch:=IsVariantsEqual(FVarIn.Value,FOnStatValue);
  end;
  if ch<>GetChecked then SetChecked(ch);
end;

procedure TKRVCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVarIn)then FVarIn:= nil else
    if (AComponent = FVarOut)then FVarOut:= nil else
    if (AComponent = FTimer)then FTimer:= nil;
end;

procedure TKRVCheckBox.SetAction(const Value: TKRVCBAType);
begin
  FAction := Value;
end;

procedure TKRVCheckBox.SetOffActValue(const Value: Variant);
begin
  FOffActValue := Value;
  if Assigned(FVarOut) then begin
    case FVarOut.VarType of
      VT_BYTE: FOffActValue:=Byte(Value);
      VT_WORD: FOffActValue:=Word(Value);
      VT_DWORD: FOffActValue:=Cardinal(Value);
      VT_SMALLINT: FOffActValue:=SmallInt(Value);
      VT_INT: FOffActValue:=Integer(Value);
      VT_SINGLE: FOffActValue:=Single(Value);
      VT_STRING: FOffActValue:=String(Value);
    end;
  end;
end;

procedure TKRVCheckBox.SetOnActValue(const Value: Variant);
begin
  FOnActValue := Value;
  if Assigned(FVarOut) then begin
    case FVarOut.VarType of
      VT_BYTE: FOnActValue:=Byte(Value);
      VT_WORD: FOnActValue:=Word(Value);
      VT_DWORD: FOnActValue:=Cardinal(Value);
      VT_SMALLINT: FOnActValue:=SmallInt(Value);
      VT_INT: FOnActValue:=Integer(Value);
      VT_SINGLE: FOnActValue:=Single(Value);
      VT_STRING: FOnActValue:=String(Value);
    end;
  end;
end;

procedure TKRVCheckBox.SetOnStatValue(const Value: Variant);
begin
  FOnStatValue := Value;
  if Assigned(FVarIn) then begin
    case FVarIn.VarType of
      VT_BYTE: FOnStatValue:=Byte(Value);
      VT_WORD: FOnStatValue:=Word(Value);
      VT_DWORD: FOnStatValue:=Cardinal(Value);
      VT_SMALLINT: FOnStatValue:=SmallInt(Value);
      VT_INT: FOnStatValue:=Integer(Value);
      VT_SINGLE: FOnStatValue:=Single(Value);
      VT_STRING: FOnStatValue:=String(Value);
    end;
  end;
end;

procedure TKRVCheckBox.SetState(const Value: TKRVCBSType);
begin
  FState := Value;
end;

procedure TKRVCheckBox.SetTimer(const Value: TKRTimer);
begin
  if FTimer<>Value then begin
    if(Assigned(FTimer))then begin
      while FTimer.Working do Application.ProcessMessages;
      FTimer.DelMon(Self);
    end;
    FTimer := Value;
    if(Assigned(FTimer))then begin
      FTimer.FreeNotification(Self);
      while FTimer.Working do Application.ProcessMessages;
      FTimer.AddMon(Self);
    end;
  end;
end;

procedure TKRVCheckBox.SetVarIn(const Value: TKRVariable);
begin
  if FVarIn<>Value then begin
    FVarIn := Value;
    if Assigned(FVarIn) then begin
      FVarIn.FreeNotification(Self);
      SetOnStatValue(FOnStatValue);
    end;
  end;
end;

procedure TKRVCheckBox.SetVarOut(const Value: TKRVariable);
begin
  if FVarOut<>Value then begin
    FVarOut := Value;
    if Assigned(FVarOut) then begin
      FVarOut.FreeNotification(Self);
      SetOffActValue(FOffActValue);
      SetOnActValue(FOnActValue);
    end;
  end;
end;

procedure TKRVCheckBox.Toggle;
begin
  case inherited State of
    cbUnchecked:
      if AllowGrayed then inherited State := cbGrayed else _Click;
    cbChecked: _Click;
    cbGrayed: _Click;
  end;
end;

procedure TKRVCheckBox._Click;
begin
  if Assigned(FClick) then FClick(Self);
  if Assigned(FVarOut) then begin
    case FAction of
      vcbatSetBit: FVarOut.Value:=_is(inherited State = cbChecked,ClearBit(FVarOut.Value,FActBit),SetBit(FVarOut.Value,FActBit));
      vcbatSetValue: FVarOut.Value:=_is(inherited State = cbChecked,FOnActValue,FOffActValue);
    end;
  end;
end;

end.
