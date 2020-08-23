(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRadioButton                                                             *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRadioButton;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.StdCtrls,
    Vcl.Forms, Vcl.ActnList, Vcl.Graphics, System.Math,
  {$ELSE}
    Windows, Messages, Classes, Controls, StdCtrls, Forms, ActnList, Graphics, Math,
  {$IFEND}



  KRIniConfig;

type
  TKRRadioButtonCfgSet = (rbcsByChecked, rbcsByTag);

  TKRRadioButton = class(TButtonControl, IKRCfgParam, IKRCfgParamEditor)
  private
    FAlignment: TLeftRight;
    FChecked: Boolean;
    FAutoSize: boolean;
    FCfgParam: TKRIniCfgParam;
    FCfgParamChanging, FChecking: boolean;
    FCfgSet: TKRRadioButtonCfgSet;
    FSetByOk: boolean;
    procedure SetAlignment(Value: TLeftRight);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    function GetCaption: TCaption;
    procedure SetAutoSize_(const Value: boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetCfgParam(const Value: TKRIniCfgParam);
    procedure SetCfgSet(const Value: TKRRadioButtonCfgSet);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CfgParamChange(AParam: TKRIniCfgParam);stdcall;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetControlsAlignment: TAlignment; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure OK;
    procedure Cancel;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize_ default false;
    property CfgSet: TKRRadioButtonCfgSet read FCfgSet write SetCfgSet default rbcsByChecked;
    property CfgParam: TKRIniCfgParam read FCfgParam write SetCfgParam;
    property SetByOk: boolean read FSetByOk write FSetByOk default false;
    property Action;
    property Align;
    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property Anchors;
    property BiDiMode;
    property Caption: TCaption read GetCaption write SetCaption;
    property Checked;
    property Color;
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
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
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

{ TKRRadioButton }

constructor TKRRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 113;
  Height := 17;
  ControlStyle := [csSetCaption, csDoubleClicks];
  FAlignment := taRightJustify;
  FCfgParamChanging:=false;
  FChecking:=false;
  FCfgSet:=rbcsByChecked;
  FSetByOk:=false;
end;

function TKRRadioButton.GetCaption: TCaption;
begin
  Result:=inherited Caption;
end;

function TKRRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TKRRadioButton.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := taRightJustify
  else
    if FAlignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;

procedure TKRRadioButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgParam)then begin
      FCfgParam.DelMon(Self);
      FCfgParam:= nil;
    end;
end;

procedure TKRRadioButton.OK;
begin
  if Assigned(FCfgParam)and FSetByOk then begin
    case FCfgSet of
      rbcsByChecked: FCfgParam.Value:=Checked;
      rbcsByTag: if Checked then FCfgParam.Value:=Tag;
    end;
  end;
end;

procedure TKRRadioButton.SetAlignment(Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TKRRadioButton.SetAutoSize_(const Value: boolean);
begin
  FAutoSize:=Value;
  SetBounds(Left,Top,Width,Height);
end;

procedure TKRRadioButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  FCanvas: TCanvas;
begin
  if FAutoSize then begin
    FCanvas:=TCanvas.Create;
    FCanvas.Font.Assign(Self.Font);
    FCanvas.Handle := GetDC(Handle);
    AHeight:=Max(FCanvas.TextHeight('0'),GetSystemMetrics(SM_CYMENUCHECK));
    AWidth:=
      GetSystemMetrics(SM_CXMENUCHECK)+
      GetSystemMetrics(SM_CXEDGE) * 3 + //GetSystemMetrics(SM_CXBORDER) * 4
      FCanvas.TextWidth(Caption);

    ReleaseDC(Handle, FCanvas.Handle);
    FCanvas.Free;
  end;
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

procedure TKRRadioButton.SetCaption(const Value: TCaption);
begin
  inherited Caption:=Value;
  SetBounds(Left,Top,Width,Height);
end;

procedure TKRRadioButton.SetCfgParam(const Value: TKRIniCfgParam);
begin
  if FCfgParam<>Value then begin
    if Assigned(FCfgParam) then FCfgParam.DelMon(self);
    FCfgParam:=Value;
    if Assigned(FCfgParam) then begin
      FCfgParam.FreeNotification(Self);
      FCfgParam.AddMon(Self);
      CfgParamChange(FCfgParam);
    end;
  end;
end;

procedure TKRRadioButton.SetCfgSet(const Value: TKRRadioButtonCfgSet);
begin
  if FCfgSet<>Value then begin
    FCfgSet := Value;
    if Assigned(FCfgParam) then begin
      case FCfgSet of
        rbcsByChecked: FCfgParam.Value:=Checked;
        rbcsByTag: if Checked then FCfgParam.Value:=Tag;
      end;
    end;
  end;
end;

procedure TKRRadioButton.SetChecked(Value: Boolean);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do
        begin
          Sibling := Controls[I];
          if (Sibling <> Self) and (Sibling is TKRRadioButton) then
            with TKRRadioButton(Sibling) do
            begin
              if Assigned(Action) and
                 (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
        end;
  end;

begin
  if FChecking then exit;
  FChecking:=true;

  if FChecked <> Value then begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
    if Assigned(FCfgParam)and(not FSetByOk)then
      case FCfgSet of
        rbcsByChecked: FCfgParam.Value:=Checked;
        rbcsByTag: if Value then FCfgParam.Value:=Tag;
      end;
    if Value then begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then Click;
    end;
  end;

  FChecking:=false;
end;

procedure TKRRadioButton.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TLeftRight] of DWORD =
    ((BS_LEFTTEXT, 0), (0, BS_LEFTTEXT));
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  with Params do
    Style := Style or BS_RADIOBUTTON or
      Alignments[UseRightToLeftAlignment, FAlignment];
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TKRRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

procedure TKRRadioButton.Cancel;
begin
  if Assigned(FCfgParam) and FSetByOk then CfgParamChange(FCfgParam);
end;

procedure TKRRadioButton.CfgParamChange(AParam: TKRIniCfgParam);
begin
  if FCfgParamChanging then exit;
  FCfgParamChanging:=true;
  case FCfgSet of
    rbcsByChecked: Checked:=FCfgParam.Value;
    rbcsByTag: Checked:=FCfgParam.Value=Tag;
  end;
  //FAutoSize:=false;
  FCfgParamChanging:=false;
end;

procedure TKRRadioButton.CMCtl3DChanged(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TKRRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end else
      inherited;
end;

procedure TKRRadioButton.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    BN_CLICKED: SetChecked(True);
    BN_DOUBLECLICKED: DblClick;
  end;
end;

end.
