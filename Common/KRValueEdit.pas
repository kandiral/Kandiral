(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRValueEdit                                                               *)
(*  Ver.: 06.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRValueEdit;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.Graphics, System.SysUtils, Winapi.Messages, Vcl.Forms, System.StrUtils,
    System.Variants,
  {$ELSE}
    Windows, Classes, Controls, ExtCtrls, StdCtrls, Graphics, SysUtils, Messages,
    Forms, StrUtils, Variants,
  {$IFEND}
    KRIniConfig, KRBoundLabel;

type

  TKRValType = (vtString, vtInteger, vtFloat);

  TKRValueEdit = class(TCustomEdit, IKRCfgParam, IKRCfgParamEditor)
  private
    FAskBeforeInput: boolean;
    FInputMin: Variant;
    FChangeFontColor: TColor;
    FColor: TColor;
    FErrorFontColor: TColor;
    FFormat: String;
    FInputMax: Variant;
    FFontColor: TColor;
    FAsk: boolean;
    FCfgParam: TKRIniCfgParam;
    FOnKeyPress: TKeyPressEvent;
    FOnEnter: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FEditing: boolean;
    FValueType: TKRValType;
    FValue: Variant;
    FChange: TNotifyEvent;
    FEnterVal: TNotifyEvent;
    FEnterAftExit: boolean;
    FSetByOk: boolean;
    procedure SetChangeFontColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetErrorFontColor(const Value: TColor);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoEnter_(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure SetValue(const Value: Variant);
    function GetText: String;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
  protected
    procedure SetCfgParam(const Value: TKRIniCfgParam);virtual;
    procedure SetInputMax(const Value: Variant);virtual;
    procedure SetInputMin(const Value: Variant);virtual;
    procedure SetValueType(const Value: TKRValType);virtual;
    procedure SetText_(AText: String);virtual;
    procedure SetFormat(const Value: String);virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CfgParamChange(AParam: TKRIniCfgParam);stdcall;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure OK;
    procedure Cancel;
  published
    property CfgParam: TKRIniCfgParam read FCfgParam write SetCfgParam;
    property Color: TColor read FColor write SetColor;
    property ErrorFontColor: TColor read FErrorFontColor write SetErrorFontColor default clOlive;
    property ChangeFontColor: TColor read FChangeFontColor write SetChangeFontColor default clTeal;
    property InputMax: Variant read FInputMax write SetInputMax;
    property InputMin: Variant read FInputMin write SetInputMin;
    property AskBeforeInput: boolean read FAskBeforeInput write FAskBeforeInput default false;
    property Format: String read FFormat write SetFormat;
    property ValueType: TKRValType read FValueType write SetValueType default vtString;
    property Value: Variant read FValue write SetValue;
    property EnterAftExit: boolean read FEnterAftExit write FEnterAftExit default false;
    property SetByOk: boolean read FSetByOk write FSetByOk default false;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text: String read GetText;
    property TextHint;
    property Touch;
    property Visible;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnEnterVal: TNotifyEvent read FEnterVal write FEnterVal;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
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

  TKRBLValueEdit = class(TKRValueEdit)
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

var
  KRVEMsgMaximumLimit: String;
  KRVEMsgMinimumLimit: String;
  KRVEMsgAskBeforeInput: String;
  KRVEMsgIncorrectValue: String;

implementation

uses Funcs, lgop;

{ TKRValueEdit }

procedure TKRValueEdit.Cancel;
begin
  if Assigned(FCfgParam) and FSetByOk then CfgParamChange(FCfgParam);
end;

procedure TKRValueEdit.CfgParamChange(AParam: TKRIniCfgParam);
begin
  if AParam.ValueType=icvtBool then SetValue(Integer(AParam.Value))
  else SetValue(AParam.Value);
end;

constructor TKRValueEdit.Create(AOwner: TComponent);
begin
  inherited;
  FFormat:='';
  FColor:=inherited Color;
  FErrorFontColor:=clOlive;
  FFontColor:=Font.Color;
  FAskBeforeInput:=false;
  FAsk:=false;
  FValueType:=vtString;
  FValue:='';
  FChangeFontColor:=clTeal;
  FEnterAftExit:=false;
  FSetByOk:=false;
  inherited OnKeyPress:=DoKeyPress;
  inherited OnEnter:=DoEnter_;
  inherited OnClick:=DoClick;
end;

destructor TKRValueEdit.Destroy;
begin
  if Assigned(FCfgParam) then FCfgParam.DelMon(self);
  inherited;
end;

procedure TKRValueEdit.DoClick(Sender: TObject);
begin
  Self.SelectAll;
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TKRValueEdit.DoEnter_(Sender: TObject);
begin
  Self.SelectAll;
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

procedure TKRValueEdit.DoKeyPress(Sender: TObject; var Key: Char);
var
  _val: Variant;
  sval:String;
  n,e: Integer;
  st: integer;
  s: String;
  fl: Extended;
begin
  if Key=#27 then begin
    Key:=#0;
    SetValue(FValue);
  end else if Key=#13 then begin
    Key:=#0;
    st:=0;
    case FValueType of
      vtString: begin
        sval:=inherited Text;
        _val:=sval;
      end;
      vtInteger: if(Assigned(FCfgParam))and(FCfgParam.ValueType=icvtBool)then begin
      end else begin
        s:=Trim(inherited Text);
        if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
          n:=HexToInt(LeftStr(s,Length(s)-1));
          e:=0;
        end else Val(s,n,e);
        if e<>0 then st:=1 else
        if(n<FInputMin)then st:=2 else
        if(n>FInputMax)then st:=3 else
        _val:=Word(n);
        sval:=IntToStr(_val);
      end;
      vtFloat: begin
        s:=Trim(inherited Text);
        if FormatSettings.DecimalSeparator=#44
        then s:=ReplaceStr(s,'.',FormatSettings.DecimalSeparator)
        else s:=ReplaceStr(s,',',FormatSettings.DecimalSeparator);
        if not TextToFloat(PChar(S), fl, fvExtended, FormatSettings) then st:=1 else
        if(fl<FInputMin)then st:=2 else
        if(fl>FInputMax)then st:=3 else
        _val:=fl;
        if FFormat='' then sval:=FloatToStr(_val)
        else sval:=FormatFloat(FFormat,_val);
      end;
    end;
    case st of
      0: if FAskBeforeInput then begin
        FAsk:=true;
        if AppMsgBox(ReplaceStr(KRVEMsgAskBeforeInput,'[#Value]',sval),MB_YESNOCANCEL or MB_ICONQUESTION)=IDYES then begin
          if Assigned(FCfgParam)and(not FSetByOk)then FCfgParam.Value:=_val else SetValue(_val);
          if Assigned(FEnterVal) then FEnterVal(Self);
        end;
        FAsk:=false;
      end else begin
        if Assigned(FCfgParam)and(not FSetByOk)then FCfgParam.Value:=_val else SetValue(_val);
        if Assigned(FEnterVal) then FEnterVal(Self);
      end;
      1: AppMsgBox(ReplaceStr(KRVEMsgIncorrectValue,'[#Value]',sval),MB_OKCANCEL or MB_ICONERROR);
      2: begin
        if FValueType=vtInteger then s:=IntToStr(FInputMin) else
          if FFormat='' then s:=FloatToStr(FInputMin) else s:=FormatFloat(FFormat,FInputMin);
        AppMsgBox(ReplaceStr(KRVEMsgMinimumLimit,'[#Value]',s),MB_OKCANCEL or MB_ICONERROR);
      end;
      3: begin
        if FValueType=vtInteger then s:=IntToStr(FInputMax) else
          if FFormat='' then s:=FloatToStr(FInputMax) else s:=FormatFloat(FFormat,FInputMax);
        AppMsgBox(ReplaceStr(KRVEMsgMaximumLimit,'[#Value]',s),MB_OKCANCEL or MB_ICONERROR);
      end;
    end;
  end;
  if Assigned(FOnKeyPress) then FOnKeyPress(Self,Key);
end;

function TKRValueEdit.GetReadOnly: boolean;
begin
  Result:=inherited ReadOnly;
end;

function TKRValueEdit.GetText: String;
begin
  Result:=inherited Text;
end;

procedure TKRValueEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FCfgParam)then begin
      FCfgParam.DelMon(Self);
      FCfgParam:= nil;
    end;
end;

procedure TKRValueEdit.OK;
begin
  if Assigned(FCfgParam)and FSetByOK then FCfgParam.Value:=FValue;
end;

procedure TKRValueEdit.SetCfgParam(const Value: TKRIniCfgParam);
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
      case FCfgParam.ValueType of
        icvtString: FValueType:=vtString;
        icvtFloat: FValueType:=vtFloat
        else FValueType:=vtInteger;
      end;
      CfgParamChange(FCfgParam);
    end;
  end;
end;

procedure TKRValueEdit.SetChangeFontColor(const Value: TColor);
begin
  if FChangeFontColor <> Value then begin
    FChangeFontColor := Value;
    if FEditing then Font.Color:=FChangeFontColor;
  end;
end;

procedure TKRValueEdit.SetColor(const Value: TColor);
begin
  if FColor<>Value then begin
    FColor:=Value;
    if not FEditing then inherited Color:=FColor;
  end;
end;

procedure TKRValueEdit.SetErrorFontColor(const Value: TColor);
begin
  if FErrorFontColor<>Value then begin
    FErrorFontColor := Value;
//    if then Font.Color:=FErrorFontColor;
  end;
end;

procedure TKRValueEdit.SetFormat(const Value: String);
begin
  FFormat := Value;
end;

procedure TKRValueEdit.SetInputMax(const Value: Variant);
begin
  FInputMax := Value;
end;

procedure TKRValueEdit.SetInputMin(const Value: Variant);
begin
  FInputMin := Value;
end;

procedure TKRValueEdit.SetReadOnly(const Value: boolean);
begin
  inherited ReadOnly:=Value;
  if inherited ReadOnly then begin
    Cursor:=crArrow;
  end else begin
    Cursor:=crDefault;
  end;
end;

procedure TKRValueEdit.SetText_(AText: String);
begin
  inherited Text:=AText;
end;

procedure TKRValueEdit.SetValue(const Value: Variant);
var
  Form: TCustomForm;
  s: string;
begin
  if(not IsVariantsEqual(FValue,Value))then begin
    FValue := Value;
    if Assigned(FChange) then FChange(Self);
  end;
  case FValueType of
    vtString: s:=FValue;
    vtInteger: if FFormat='' then s:=IntToStr(FValue) else begin
      if(FFormat[1]='h') then s:=IntToHex(FValue,Length(FFormat))+'h'
      else s:=FormatFloat(FFormat, FValue);
    end;
    vtFloat: if FFormat='' then s:=FloatToStr(FValue)
      else s:=FormatFloat(FFormat, FValue);
  end;
  if inherited Text<>s then inherited Text:=s;
  FEditing:=false;
  Font.Color:=FFontColor;
  if Focused then begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.DefocusControl(Self, true);
  end;
end;

procedure TKRValueEdit.SetValueType(const Value: TKRValType);
begin
  if Assigned(FCfgParam) then exit;
  if FValueType<>Value then begin
    FValueType := Value;
    case FValueType of
      vtString: SetValue('');
      else SetValue(0);
    end;
  end;
end;

procedure TKRValueEdit.WMFontChange(var Message: TMessage);
begin
  FFontColor:=Font.Color;
  if FEditing then Font.Color:=FChangeFontColor;
end;

procedure TKRValueEdit.WMKillFocus(var Message: TWMKillFocus);
var key: char;
begin
  if FEnterAftExit and not FAsk then begin
    key:=#13;
    DoKeyPress(nil,key);
  end;
  SetValue(FValue);
  inherited;
end;

procedure TKRValueEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  if Not ReadOnly then begin
    Font.Color:=FChangeFontColor;
    FEditing:=true;
    inherited;
  end;
end;

{ TKRBLValueEdit }

procedure TKRBLValueEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLValueEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLValueEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLValueEdit.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLValueEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLValueEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLValueEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FLabel) and
     ((FLabel.GetTextLen = 0) or (CompareText(FLabel.Caption, Name) = 0)) then
    FLabel.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRBLValueEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLValueEdit.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

initialization
  KRVEMsgMaximumLimit:='Значение не должно быть больше [#Value]';
  KRVEMsgMinimumLimit:='Значение не должно быть меньше [#Value]';
  KRVEMsgAskBeforeInput:='Установить значение «[#Value]»?';
  KRVEMsgIncorrectValue:='Неверно введено значение!';

end.
