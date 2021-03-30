(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCheckGroupBox                                                           *)
(*  Ver.: 06.02.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCheckGroupBox;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.Graphics,
    Vcl.Forms, Vcl.Themes, Vcl.StdCtrls, System.SysUtils, System.Math,
  {$ELSE}
    Windows, Messages, Classes, Controls, Graphics, Forms, Themes, StdCtrls,
    SysUtils, Math,
  {$IFEND}
    KRCheckBox;

type
  TGBCheckBox = class(TKRCheckBox)
  private
    FControl: TControl;
    FPosUpdating: boolean;
    procedure UpPosition;
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property AutoSize;
    property CfgParam;
    property SetByOk;
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
    property OnClick;
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

  TKRCheckGroupBox = class(TCustomControl)
  private
    FCheckBox: TGBCheckBox;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    function GetChecked: boolean;
    procedure SetChecked(const Value: boolean);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
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
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property CheckBox: TGBCheckBox read FCheckBox;
    property Checked:boolean read GetChecked write SetChecked;
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Padding;
    property ParentBackground default True;
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
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TKRCheckGroupBox }

procedure TKRCheckGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  Inc(Rect.Top, Canvas.TextHeight('0'));
  InflateRect(Rect, -1, -1);
  if Ctl3d then InflateRect(Rect, -1, -1);
end;

procedure TKRCheckGroupBox.CMBidimodechanged(var Message: TMessage);
begin
  if Assigned(FCheckBox) then FCheckBox.BiDiMode := BiDiMode;
end;

procedure TKRCheckGroupBox.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Realign;
end;

procedure TKRCheckGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end else
      inherited;
end;

procedure TKRCheckGroupBox.CMEnabledchanged(var Message: TMessage);
begin
  if Assigned(FCheckBox) then FCheckBox.Enabled := Enabled;
end;

procedure TKRCheckGroupBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  Realign;
end;

procedure TKRCheckGroupBox.CMVisiblechanged(var Message: TMessage);
begin
  if Assigned(FCheckBox) then FCheckBox.Visible := Visible;
end;

constructor TKRCheckGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csReplicatable, csParentBackground, csGestures];
  Width := 185;
  Height := 105;
  inherited Caption:='';
  FCheckBox:=TGBCheckBox.Create(Self);
  FCheckBox.FreeNotification(Self);
  FCheckBox.FControl:=Self;
end;

procedure TKRCheckGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

function TKRCheckGroupBox.GetCaption: TCaption;
begin
  if Assigned(FCheckBox) then Result:=FCheckBox.Caption;
end;

function TKRCheckGroupBox.GetChecked: boolean;
begin
  Result:=false;
  if Assigned(FCheckBox) then Result:=FCheckBox.Checked;
end;

procedure TKRCheckGroupBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FCheckBox then FCheckBox := nil;
end;

procedure TKRCheckGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  CaptionRect,
  OuterRect: TRect;
  Box: TThemedButton;
  Details: TThemedElementDetails;
begin
  with Canvas do
  begin
    Font := Self.Font;

    if ThemeControl(Self) then begin
      CaptionRect := Rect(0, 0, 0, 0);
      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do ExcludeClipRect(Handle, Left, Top, Right, Bottom);
      if Enabled then Box := tbGroupBoxNormal
      else Box := tbGroupBoxDisabled;
      {$IF CompilerVersion >= 23}
      Details := StyleServices.GetElementDetails(Box);
      StyleServices.DrawElement(Handle, Details, OuterRect);
      SelectClipRgn(Handle, 0);
      Brush.Style := bsClear;
      {$ELSE}
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(Handle, Details, OuterRect);
      SelectClipRgn(Handle, 0);
      {$IFEND}
    end else begin
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end else
        Brush.Color := clWindowFrame;
      FrameRect(R);
    end;
  end;
end;

procedure TKRCheckGroupBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FCheckBox) then FCheckBox.UpPosition;
end;

procedure TKRCheckGroupBox.SetCaption(const Value: TCaption);
begin
  if Assigned(FCheckBox) then FCheckBox.Caption:=Value;
end;

procedure TKRCheckGroupBox.SetChecked(const Value: boolean);
begin
  if Assigned(FCheckBox) then FCheckBox.Checked:=Value;
end;

procedure TKRCheckGroupBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FCheckBox) and
     ((FCheckBox.GetTextLen = 0) or (CompareText(FCheckBox.Caption, Name) = 0)) then
    FCheckBox.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRCheckGroupBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FCheckBox = nil then exit;
  FCheckBox.Parent := AParent;
  FCheckBox.Visible := True;
  FCheckBox.UpPosition;
end;

procedure TKRCheckGroupBox.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

{ TGBCheckBox }

constructor TGBCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  Name := 'SubCheckBox';  { do not localize }
  SetSubComponent(True);
  if Assigned(AOwner) then
    Caption := AOwner.Name;
end;

function TGBCheckBox.GetCaption: TCaption;
begin
  Result:=inherited Caption;
end;

procedure TGBCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    if AComponent = FControl then FControl := nil;
end;

procedure TGBCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not FPosUpdating then UpPosition;
end;

procedure TGBCheckBox.SetCaption(const Value: TCaption);
begin
  inherited Caption:=Value;
  UpPosition;
end;

procedure TGBCheckBox.UpPosition;
var
  FCanvas: TCanvas;
begin
  if Assigned(Parent) and Assigned(FControl) then begin
    FPosUpdating:=true;
    FCanvas:=TCanvas.Create;
    FCanvas.Handle := GetDC(Handle);
    FCanvas.Font.Assign(Self.Font);

    Height:=Max(FCanvas.TextHeight('0'),GetSystemMetrics(SM_CYMENUCHECK));
    Width:=
      GetSystemMetrics(SM_CXMENUCHECK)+
      GetSystemMetrics(SM_CXEDGE) * 2 + //GetSystemMetrics(SM_CXBORDER) * 4
      FCanvas.TextWidth(Caption);

    ReleaseDC(Handle, FCanvas.Handle);
    FCanvas.Free;
    if ThemeControl(Self) then begin
      Left:=FControl.Left+8;
      Top:=FControl.Top-7;
    end else begin
      Left:=FControl.Left+7;
      Top:=FControl.Top-2;
    end;
    FPosUpdating:=false;
  end;
end;

end.
