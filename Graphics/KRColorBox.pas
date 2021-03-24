(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRColorBox                                                                *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRColorBox;

interface

uses Windows, Types, Controls, Classes, Messages, ExtCtrls, StdCtrls, Graphics,
  KRIniConfig, KRBoundLabel, SysUtils;

type
  TKRColorBox = class(TCustomColorBox, IKRCfgParam, IKRCfgParamEditor)
  private
    FSetByOk: boolean;
    FCfgParamChange, FChanging: boolean;
    FCfgParam: TKRIniCfgParam;
    FChange: TNotifyEvent;
    procedure _Change(Sender: TObject);
    procedure SetCfgParam(const Value: TKRIniCfgParam);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CfgParamChange(AParam: TKRIniCfgParam);stdcall;
  public
    constructor Create (AOwner: TComponent); override;
    procedure OK;
    procedure Cancel;
  published
    property CfgParam: TKRIniCfgParam read FCfgParam write SetCfgParam;
    property SetByOk: boolean read FSetByOk write FSetByOk default false;
    property Align;
    property AutoComplete;
    property AutoDropDown;
    property DefaultColorColor;
    property NoneColorColor;
    property Selected;
    property Style;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
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
    property Touch;
    property Visible;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnCloseUp;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetColors;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  TKRBLColorBox = class(TKRColorBox)
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

{ TKRColorBox }

procedure TKRColorBox.Cancel;
begin
  if Assigned(FCfgParam) and FSetByOk then CfgParamChange(FCfgParam);
end;

procedure TKRColorBox.CfgParamChange(AParam: TKRIniCfgParam);
begin
  FCfgParamChange:=true;
  Selected:=AParam.Value;
  FCfgParamChange:=false;
end;

constructor TKRColorBox.Create(AOwner: TComponent);
begin
  inherited;
  inherited OnChange:=_Change;
  FCfgParamChange:=false;
  FChanging:=false;
  FSetByOk:=false;
end;

procedure TKRColorBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

  function ColorToBorderColor(AColor: TColor; State: TOwnerDrawState): TColor;
  begin
    if (Byte(AColor) > 192) or          {Red}
       (Byte(AColor shr 8) > 192) or    {Green}
      (Byte(AColor shr 16) > 192) then  {Blue}
      Result := clBlack
    else if odSelected in State then
      Result := clWhite
    else
      Result := AColor;
  end;

begin
  Canvas.FillRect (Rect);

  Canvas.Brush.Color := Colors[Index];
  Inc(Rect.Left,2);
  Inc(Rect.Top,2);
  Dec(Rect.Right,2);
  Dec(Rect.Bottom,2);

  Canvas.Brush.Color := Colors[Index];
  if Canvas.Brush.Color = clDefault then
    Canvas.Brush.Color := DefaultColorColor
  else if Canvas.Brush.Color = clNone then
    Canvas.Brush.Color := NoneColorColor;
  Canvas.FillRect(Rect);
  Canvas.Brush.Color := ColorToBorderColor(ColorToRGB(Canvas.Brush.Color), State);
  Canvas.FrameRect(Rect);

end;

procedure TKRColorBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgParam)then begin
      FCfgParam.DelMon(Self);
      FCfgParam:= nil;
    end;
end;

procedure TKRColorBox.OK;
begin
  if Assigned(FCfgParam)and FSetByOk then begin
    FChanging:=true;
    FCfgParam.Value:=Selected;
    FChanging:=false;
  end;
end;

procedure TKRColorBox.SetCfgParam(const Value: TKRIniCfgParam);
begin
  if FCfgParam<>Value then begin
    if Assigned(FCfgParam) then begin
      FCfgParam.DelMon(self);
      FCfgParam.RemoveFreeNotification(Self);
    end;
    FCfgParam:=Value;
    if Assigned(FCfgParam) then begin
      FCfgParam.FreeNotification(Self);
      FCfgParam.AddMon(Self);
      CfgParamChange(FCfgParam);
    end;
  end;
end;

procedure TKRColorBox._Change(Sender: TObject);
begin
  if FChanging then exit;
  if Assigned(FCfgParam)and(not FCfgParamChange)and(not FSetByOk)then begin
    FChanging:=true;
    FCfgParam.Value:=Selected;
    FChanging:=false;
  end;
  if Assigned(FChange) then FChange(Self);
end;

{ TKRBLColorBox }

procedure TKRBLColorBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLColorBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLColorBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLColorBox.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLColorBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLColorBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLColorBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FLabel) and
     ((FLabel.GetTextLen = 0) or (CompareText(FLabel.Caption, Name) = 0)) then
    FLabel.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRBLColorBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLColorBox.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
