(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCheckBox                                                                *)
(*  Ver.: 03.05.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCheckBox;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, Vcl.Controls, Vcl.StdCtrls, Winapi.Messages,
    System.SysUtils, Vcl.Graphics, System.Math,
  {$ELSE}
    Windows, Classes, Controls, StdCtrls, Messages, SysUtils, Graphics, Math,
  {$IFEND}
    KRIniConfig, KRBoundLabel;

type
  TKRCheckBox = class(TCustomCheckBox, IKRCfgParam, IKRCfgParamEditor)
  private
    FCfgParam: TKRIniCfgParam;
    FClick: TNotifyEvent;
    FAutoSize: boolean;
    FSetByOk: boolean;
    procedure SetCfgParam(const Value: TKRIniCfgParam);
    procedure _Click(Sender: TObject);
    procedure SetAutoSize_(const Value: boolean);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CfgParamChange(AParam: TKRIniCfgParam);stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure OK;
    procedure Cancel;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize_ default false;
    property CfgParam: TKRIniCfgParam read FCfgParam write SetCfgParam;
    property SetByOk: boolean read FSetByOk write FSetByOk default false;
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption: TCaption read GetCaption write SetCaption;
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
    property State;
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

  TKRBLCheckBox = class(TKRCheckBox)
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

{ TKRCheckBox }

procedure TKRCheckBox.Cancel;
begin
  if Assigned(FCfgParam) and FSetByOk then CfgParamChange(FCfgParam);
end;

procedure TKRCheckBox.CfgParamChange(AParam: TKRIniCfgParam);
var
  _prm: TKRIniCfgParam;
begin
  _prm:=FCfgParam;
  FCfgParam:=nil;
  Checked:=_prm.Value;
  FCfgParam:=_prm;
end;

constructor TKRCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize:=false;
  FSetByOk:=false;
  inherited OnClick:=_Click;
end;

function TKRCheckBox.GetCaption: TCaption;
begin
  Result:=inherited Caption;
end;

procedure TKRCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgParam)then begin
      FCfgParam.DelMon(Self);
      FCfgParam:= nil;
    end;
end;

procedure TKRCheckBox.OK;
begin
  if Assigned(FCfgParam)and FSetByOk then begin
    FCfgParam.DelMon(Self);
    FCfgParam.Value:=Checked;
    FCfgParam.AddMon(Self);
  end;
end;

procedure TKRCheckBox.SetAutoSize_(const Value: boolean);
begin
  FAutoSize:=Value;
  SetBounds(Left,Top,Width,Height);
end;

procedure TKRCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  FCanvas: TCanvas;
begin
  if FAutoSize then begin
    FCanvas:=TCanvas.Create;
    FCanvas.Handle := GetDC(Handle);
    FCanvas.Font.Assign(Self.Font);
    AHeight:=Max(FCanvas.TextHeight('0'),GetSystemMetrics(SM_CYMENUCHECK));
    AWidth:=
      GetSystemMetrics(SM_CXMENUCHECK)+
      GetSystemMetrics(SM_CXEDGE) * 2 + //GetSystemMetrics(SM_CXBORDER) * 4
      FCanvas.TextWidth(Caption);

    ReleaseDC(Handle, FCanvas.Handle);
    FCanvas.Free;
  end;
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

procedure TKRCheckBox.SetCaption(const Value: TCaption);
begin
  inherited Caption:=Value;
  SetBounds(Left,Top,Width,Height);
end;

procedure TKRCheckBox.SetCfgParam(const Value: TKRIniCfgParam);
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

procedure TKRCheckBox._Click(Sender: TObject);
begin
  if Assigned(FCfgParam)and(not FSetByOk) then begin
    FCfgParam.DelMon(Self);
    FCfgParam.Value:=Checked;
    FCfgParam.AddMon(Self);
  end;
  if Assigned(FClick) then FClick(Self);
end;

{ TKRBLCheckBox }

procedure TKRBLCheckBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLCheckBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLCheckBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLCheckBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FLabel) and
     ((FLabel.GetTextLen = 0) or (CompareText(FLabel.Caption, Name) = 0)) then
    FLabel.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRBLCheckBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLCheckBox.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
