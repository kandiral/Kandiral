(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRIndicator                                                               *)
(*  Ver.: 31.08.2017                                                          *)
(*  https://kandiral.ru/delphi/krindicator.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRIndicator;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Controls, Winapi.Messages, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Forms,
  {$ELSE}
    Classes, Controls, Messages, Graphics, ExtCtrls, Forms,
  {$IFEND}
  KRVariables, KRBoundLabel, KRTimer;

type
  TKRIndicatorState = (istOn, istOff, istError);
  TKRIndicatorType = (itpOnHi, itpOnLow, itpBit);

  TKRIndicator = class(TShape, IKRTimer)
  private
    FPenOff: TPen;
    FBrushOff: TBrush;
    FPenOn: TPen;
    FBrushOn: TBrush;
    FIndicatorState: TKRIndicatorState;
    FPenErr: TPen;
    FBrushErr: TBrush;
    FVariable: TKRVariable;
    FBit: byte;
    FIndicatorType: TKRIndicatorType;
    FLimit: Variant;
    FStateChanged: TNotifyEvent;
    FTimer: TKRTimer;
    procedure SetBrushOff(const Value: TBrush);
    procedure SetBrushOn(const Value: TBrush);
    procedure SetPenOff(const Value: TPen);
    procedure SetPenOn(const Value: TPen);
    procedure SetBrushErr(const Value: TBrush);
    procedure SetPenErr(const Value: TPen);
    procedure SetVariable(const Value: TKRVariable);
    procedure SetBit(const Value: byte);
    procedure SetIndicatorType(const Value: TKRIndicatorType);
    procedure SetLimit(const Value: Variant);
    procedure SetTimer(const Value: TKRTimer);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoTimer; stdcall;
    function upState: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
  published
    property Variable: TKRVariable read FVariable write SetVariable;
    property IndicatorState: TKRIndicatorState read FIndicatorState;
    property IndicatorType: TKRIndicatorType read FIndicatorType write SetIndicatorType;
    property Bit: byte read FBit write SetBit;
    property Limit: Variant read FLimit write SetLimit;
    property OnStateChanged: TNotifyEvent read FStateChanged write FStateChanged;
    property Timer: TKRTimer read FTimer write SetTimer;
    property Align;
    property Anchors;
    property BrushOn: TBrush read FBrushOn write SetBrushOn;
    property PenOn: TPen read FPenOn write SetPenOn;
    property BrushOff: TBrush read FBrushOff write SetBrushOff;
    property PenOff: TPen read FPenOff write SetPenOff;
    property BrushErr: TBrush read FBrushErr write SetBrushErr;
    property PenErr: TPen read FPenErr write SetPenErr;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property Shape;
    property ShowHint;
    property Touch;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
  end;

  TKRBLIndicator = class(TKRIndicator)
  private
    FLabel: TKRBoundLabel;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property BLabel: TKRBoundLabel read FLabel;
  end;

implementation

uses lgop;

{ TKRIndicator }

constructor TKRIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FIndicatorState:=istError;
  FPenOn := TPen.Create;
  FPenOn.Color:=clBlack;
  FPenOn.OnChange := StyleChanged;
  FBrushOn := TBrush.Create;
  FBrushOn.Color:=clGreen;
  FBrushOn.OnChange := StyleChanged;
  FPenOff := TPen.Create;
  FPenOff.Color:=clBlack;
  FPenOff.OnChange := StyleChanged;
  FBrushOff := TBrush.Create;
  FBrushOff.Color:=clRed;
  FBrushOff.OnChange := StyleChanged;
  FPenErr := TPen.Create;
  FPenErr.Color:=clBlack;
  FPenErr.OnChange := StyleChanged;
  FBrushErr := TBrush.Create;
  FBrushErr.Color:=clGray;
  FBrushErr.OnChange := StyleChanged;
end;

destructor TKRIndicator.Destroy;
begin
  if(Assigned(FTimer))then begin
    while FTimer.Working do Application.ProcessMessages;
    FTimer.DelMon(Self);
  end;
  FPenOn.Free;
  FBrushOn.Free;
  FPenOff.Free;
  FBrushOff.Free;
  FPenErr.Free;
  FBrushErr.Free;
  inherited;
end;

procedure TKRIndicator.DoTimer;
begin
  if upState then Invalidate;
end;

procedure TKRIndicator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil else
    if(AComponent=FTimer)then begin
      FTimer:=nil;
    end;
end;

procedure TKRIndicator.Paint;
begin
  case FIndicatorState of
    istOn: begin
      Brush.Assign(BrushOn);
      Pen.Assign(PenOn);
    end;
    istOff: begin
      Brush.Assign(BrushOff);
      Pen.Assign(PenOff);
    end;
    istError: begin
      Brush.Assign(BrushErr);
      Pen.Assign(PenErr);
    end;
  end;
  inherited;
end;

procedure TKRIndicator.SetBit(const Value: byte);
begin
  FBit := Value;
  Invalidate;
end;

procedure TKRIndicator.SetBrushErr(const Value: TBrush);
begin
  FBrushErr.Assign(Value);
end;

procedure TKRIndicator.SetBrushOff(const Value: TBrush);
begin
  FBrushOff.Assign(Value);
end;

procedure TKRIndicator.SetBrushOn(const Value: TBrush);
begin
  FBrushOn.Assign(Value);
end;

procedure TKRIndicator.SetIndicatorType(const Value: TKRIndicatorType);
begin
  FIndicatorType := Value;
  Invalidate;
end;

procedure TKRIndicator.SetLimit(const Value: Variant);
begin
  FLimit := Value;
  Invalidate;
end;

procedure TKRIndicator.SetPenErr(const Value: TPen);
begin
  FPenErr.Assign(Value);
end;

procedure TKRIndicator.SetPenOff(const Value: TPen);
begin
  FPenOff.Assign(Value);
end;

procedure TKRIndicator.SetPenOn(const Value: TPen);
begin
  FPenOn.Assign(Value);
end;

procedure TKRIndicator.SetTimer(const Value: TKRTimer);
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

procedure TKRIndicator.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
    end;
  end;
end;

procedure TKRIndicator.Update;
begin
  if upState then Invalidate;
  inherited;
end;

function TKRIndicator.upState: boolean;
var
  _st: TKRIndicatorState;
begin
  _st:=istError;
  if(Assigned(FVariable))and(FVariable.Error=0)then case FIndicatorType of
    itpOnHi: if FVariable.Value>FLimit then _st:=istOn else _st:=istOff;
    itpOnLow: if FVariable.Value<FLimit then _st:=istOn else _st:=istOff;
    itpBit: if GetBit(FVariable.Value,FBit) then _st:=istOn else _st:=istOff;
  end;
  result:=_st<>FIndicatorState;
  if result then begin
    FIndicatorState:=_st;
    if Assigned(FStateChanged) then FStateChanged(Self);
  end;
end;

{ TKRBLIndicator }

procedure TKRBLIndicator.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLIndicator.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLIndicator.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLIndicator.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLIndicator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLIndicator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLIndicator.SetName(const Value: TComponentName);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=Value;
end;

procedure TKRBLIndicator.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLIndicator.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
