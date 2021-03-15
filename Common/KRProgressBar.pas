(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRProgressBar                                                             *)
(*  Ver.: 14.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRProgressBar;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Controls, Winapi.Messages, Vcl.ComCtrls, System.SysUtils,
  {$ELSE}
    Classes, Controls, Messages, ComCtrls, SysUtils,
  {$IFEND}
    KRBoundLabel;

type
  TKRPBLStyle = (lbsProcent, lbsNombers);

  TKRProgressBar = class(TProgressBar)
  private
    FLabel: TKRBoundLabel;
    FLabelStyle: TKRPBLStyle;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure SetLabelStyle(const Value: TKRPBLStyle);
    function GetMax: Integer;
    function GetMin: Integer;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
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
    property LabelStyle: TKRPBLStyle read FLabelStyle write SetLabelStyle default lbsProcent;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Min: Integer read GetMin write SetMin default 0;
    property Max: Integer read GetMax write SetMax default 100;
    property Align;
    property Anchors;
    property BorderWidth;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Constraints;
    property Orientation;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property Smooth;
    property Style;
    property MarqueeInterval;
    property BarColor;
    property BackgroundColor;
    property SmoothReverse;
    property Step;
    property State;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    {$IF CompilerVersion >= 24}
      property StyleElements;
    {$IFEND}
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
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

{ TKRProgressBar }

procedure TKRProgressBar.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRProgressBar.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRProgressBar.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
  FLabel.Position:=blpBottomCenter;
  FLabelStyle:=lbsProcent;
end;

function TKRProgressBar.GetMax: Integer;
begin
  Result:=inherited Max;
end;

function TKRProgressBar.GetMin: Integer;
begin
  Result:=inherited Min;
end;

function TKRProgressBar.GetPosition: Integer;
begin
  Result:=inherited Position;
end;

procedure TKRProgressBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRProgressBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Assigned(FLabel) then SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetLabelStyle(const Value: TKRPBLStyle);
begin
  FLabelStyle := Value;
  SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetMax(const Value: Integer);
begin
  inherited Max:=Value;
  SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetMin(const Value: Integer);
begin
  inherited Min:=Value;
  SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
  SetPosition(inherited Position);
end;

procedure TKRProgressBar.SetPosition(const Value: Integer);
begin
  inherited Position:=Value;
  if not Assigned(FLabel) then exit;
  case FLabelStyle of
    lbsProcent: FLabel.Caption:=IntToStr(Round((inherited Position - Self.Min)/(Self.Max-Self.Min)*100))+'%';
    lbsNombers: FLabel.Caption:=IntToStr(inherited Position - Self.Min)+'/'+IntToStr(Self.Max-Self.Min);
  end;
end;

procedure TKRProgressBar.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
