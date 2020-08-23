(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRPenStyle                                                                *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRPenStyle;

interface

uses Messages, Types, Classes, Controls, StdCtrls, Graphics, SysUtils, KRIniConfig,
  KRBoundLabel;

type
  TKRPenStyle = class(TCustomComboBox, IKRCfgParam, IKRCfgParamEditor)
  private
    FNeedToPopulate: Boolean;
    FSetByOk: boolean;
    FCfgParam: TKRIniCfgParam;
    FChange: TNotifyEvent;
    FCfgParamChange, FChanging: boolean;
    procedure PopulateList;
    procedure AddItems;
    function GetPenStyle: TPenStyle;
    procedure SetPenStyle(const Value: TPenStyle);
    procedure _Change(Sender: TObject);
    procedure SetCfgParam(const Value: TKRIniCfgParam);
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CfgParamChange(AParam: TKRIniCfgParam);stdcall;
  public
    constructor Create (AOwner: TComponent); override;
    procedure OK;
    procedure Cancel;
  published
    property CfgParam: TKRIniCfgParam read FCfgParam write SetCfgParam;
    property SetByOk: boolean read FSetByOk write FSetByOk default false;
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property Align;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property ImeMode;
    property ImeName;
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
    property TextHint;
    property Touch;
    property Visible;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  TKRBLPenStyle = class(TKRPenStyle)
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

{ TKRPenStyle }

procedure TKRPenStyle.AddItems;
var
  I: Integer;
begin
  Items.Clear;
  for I := Integer(psSolid) to Integer(psInsideFrame) do Items.Add('');
  if ItemIndex = -1 then ItemIndex := 0;
end;

procedure TKRPenStyle.Cancel;
begin
  if Assigned(FCfgParam) and FSetByOk then CfgParamChange(FCfgParam);
end;

procedure TKRPenStyle.CfgParamChange(AParam: TKRIniCfgParam);
begin
  FCfgParamChange:=true;
  ItemIndex:=AParam.Value;
  FCfgParamChange:=false;
end;

constructor TKRPenStyle.Create(AOwner: TComponent);
begin
  inherited;
  inherited Style := csOwnerDrawFixed;
  inherited OnChange:=_Change;
  FCfgParamChange:=false;
  FChanging:=false;
  FSetByOk:=false;
  PopulateList;
  if HandleAllocated then ItemIndex:=0;
end;

procedure TKRPenStyle.CreateWnd;
begin
  inherited;
  if FNeedToPopulate then PopulateList;
end;

procedure TKRPenStyle.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  APenStyle: TPenStyle;
begin
  APenStyle := TPenStyle(Index);
  Canvas.FillRect (Rect);

  Canvas.Pen.Color := clWindowText;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := APenStyle;

  Canvas.MoveTo(Rect.Left+3,Rect.Bottom-(ItemHeight div 2));
  Canvas.LineTo (Rect.Right-3,Rect.Bottom-(ItemHeight div 2));
end;

function TKRPenStyle.GetPenStyle: TPenStyle;
begin
   if ItemIndex = -1 then Result := psSolid
   else Result := TPenStyle (ItemIndex);
end;

procedure TKRPenStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgParam)then begin
      FCfgParam.DelMon(Self);
      FCfgParam:= nil;
    end;
end;

procedure TKRPenStyle.OK;
begin
  if Assigned(FCfgParam)and FSetByOk then begin
    FChanging:=true;
    FCfgParam.Value:=ItemIndex;
    FChanging:=false;
  end;
end;

procedure TKRPenStyle.PopulateList;
begin
  if HandleAllocated then begin
    Items.BeginUpdate;
    AddItems;
    Items.EndUpdate;
    FNeedToPopulate := False;
  end else FNeedToPopulate := True;
end;

procedure TKRPenStyle.SetCfgParam(const Value: TKRIniCfgParam);
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

procedure TKRPenStyle.SetPenStyle(const Value: TPenStyle);
begin
  ItemIndex := Integer(Value);
end;

procedure TKRPenStyle._Change(Sender: TObject);
begin
  if FChanging then exit;
  if Assigned(FCfgParam)and(not FCfgParamChange)and(not FSetByOk)then begin
    FChanging:=true;
    FCfgParam.Value:=ItemIndex;
    FChanging:=false;
  end;
  if Assigned(FChange) then FChange(Self);
end;

{ TKRBLPenStyle }

procedure TKRBLPenStyle.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLPenStyle.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLPenStyle.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLPenStyle.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLPenStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLPenStyle.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLPenStyle.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FLabel) and
     ((FLabel.GetTextLen = 0) or (CompareText(FLabel.Caption, Name) = 0)) then
    FLabel.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRBLPenStyle.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLPenStyle.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
