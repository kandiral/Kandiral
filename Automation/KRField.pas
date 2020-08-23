(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRField                                                                   *)
(*  Ver.: 14.07.2020                                                          *)
(*  https://kandiral.ru/delphi/krfield.pas.html                               *)
(*                                                                            *)
(******************************************************************************)
unit KRField;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.Variants, Vcl.Controls, Vcl.ExtCtrls,
    Vcl.StdCtrls, Vcl.Graphics, System.SysUtils, Winapi.Messages, Vcl.Forms, System.Math,
  {$ELSE}
    Windows, Classes, Variants, Controls, ExtCtrls, StdCtrls, Graphics, SysUtils,
    Messages, Forms, Math,
  {$IFEND}
  KRBoundLabel, StrUtils, KRVariables, KRTimer;

type
  TKRFieldDateTime = (fdtNone, fdtTime, fdtDate, fdtDateTime);
  TKRFieldValueEvent = procedure (Sender: TObject; var AValue: Variant) of object;

  TKRField = class(TCustomEdit, IKRTimer)
  private
    FVariable: TKRVariable;
    FHint: String;
    FColor: TColor;
    FFormat: String;
    FErrorColor: TColor;
    FErrorFontColor: TColor;
    FChangeFontColor: TColor;
    FFontColor: TColor;
    FAsk: boolean;
    FErrorToHint: boolean;
    FEditing: boolean;
    FOnKeyPress: TKeyPressEvent;
    FInputMin: Variant;
    FInputMax: Variant;
    FAskBeforeInput: boolean;
    FOnEnter: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FDateTime: TKRFieldDateTime;
    FVarSet: TKRVariable;
    FSkipUserError: boolean;
    FTimer: TKRTimer;
    FOldValue: Variant;
    FOldChange: boolean;
    FOldError: integer;
    FEnterAftExit: boolean;
    FShowValue: TKRFieldValueEvent;
    FValType: TVarType;
    FSetValue: TKRFieldValueEvent;
    procedure SetVariable(const Value: TKRVariable);
    procedure SetColor(const Value: TColor);
    procedure SetHint(const Value: String);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure SetErrorColor(const Value: TColor);
    procedure SetErrorFontColor(const Value: TColor);
    procedure SetChangeFontColor(const Value: TColor);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    function GetText: String;
    procedure SetErrorToHint(const Value: boolean);
    procedure SetInputMax(const Value: Variant);
    procedure SetInputMin(const Value: Variant);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoEnter_(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure SetFontColor(const Value: TColor);
    procedure SetVarSet(const Value: TKRVariable);
    procedure SetTimer(const Value: TKRTimer);
    procedure SetFormat(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoTimer; stdcall;
    procedure DoTimer_;virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Text: String read GetText;
  published
    property Variable: TKRVariable read FVariable write SetVariable;
    property VarSet: TKRVariable read FVarSet write SetVarSet;
    property ValType: TVarType read FValType write FValType;
    property Format: String read FFormat write SetFormat;
    property FontColor: TColor read FFontColor write SetFontColor;
    property SkipUserError: boolean read FSkipUserError write FSkipUserError;
    property Timer: TKRTimer read FTimer write SetTimer;
    property EnterAftExit: boolean read FEnterAftExit write FEnterAftExit default false;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color: TColor read FColor write SetColor;
    property ErrorColor: TColor read FErrorColor write SetErrorColor default $2F2F2F;
    property ErrorFontColor: TColor read FErrorFontColor write SetErrorFontColor default clOlive;
    property ChangeFontColor: TColor read FChangeFontColor write SetChangeFontColor default clTeal;
    property InputMax: Variant read FInputMax write SetInputMax;
    property InputMin: Variant read FInputMin write SetInputMin;
    property AskBeforeInput: boolean read FAskBeforeInput write FAskBeforeInput default false;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint: String read FHint write SetHint;
    property ErrorToHint: boolean read FErrorToHint write SetErrorToHint;
    property DateTime: TKRFieldDateTime read FDateTime write FDateTime default fdtNone;
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
    property TextHint;
    property Touch;
    property Visible;
    property OnChange;
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
    property OnSetValue: TKRFieldValueEvent read FSetValue write FSetValue;
    property OnShowValue: TKRFieldValueEvent read FShowValue write FShowValue;
    property OnStartDock;
    property OnStartDrag;
  end;

  TKRBLField = class(TKRField)
  private
    FLabel: TKRBoundLabel;
    FErrorToBoundLabel: boolean;
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
    procedure DoTimer_;override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property ErrorToBoundLabel: boolean read FErrorToBoundLabel write FErrorToBoundLabel;
    property BLabel: TKRBoundLabel read FLabel;
  end;

var
  KRFldMsgMaximumLimit: String;
  KRFldMsgMinimumLimit: String;
  KRFldMsgAskBeforeInput: String;
  KRFldMsgIncorrectValue: String;

implementation

uses funcs, lgop;

{ TKRField }

constructor TKRField.Create(AOwner: TComponent);
begin
  inherited;
  FFormat:='';
  FEnterAftExit:=false;
  FOldValue:=null;
  FOldChange:=true;
  FOldError:=-12698139;
  FColor:=inherited Color;
  FErrorColor:=$2F2F2F;
  FErrorFontColor:=clOlive;
  FFontColor:=Font.Color;
  FAskBeforeInput:=false;
  FAsk:=false;
  FHint:=inherited Hint;
  FVariable:=nil;
  FChangeFontColor:=clTeal;
  inherited ReadOnly:=true;
  Cursor:=crArrow;
  FErrorToHint:=true;
  FEditing:=false;
  FDateTime:=fdtNone;
  inherited OnKeyPress:=DoKeyPress;
  inherited OnEnter:=DoEnter_;
  inherited OnClick:=DoClick;
end;

destructor TKRField.Destroy;
begin
  if(Assigned(FTimer))then FTimer.DelMon(Self);
  inherited;
end;

procedure TKRField.DoClick(Sender: TObject);
begin
  Self.SelectAll;
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TKRField.DoEnter_(Sender: TObject);
begin
  Self.SelectAll;
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

procedure TKRField.DoKeyPress(Sender: TObject; var Key: Char);
var
  Form: TCustomForm;
  st: integer;
  n,e: Integer;
  dw: Cardinal;
  _val: Variant;
  sval,s:String;
  b: boolean;
  ext: Extended;
  sg: Single;
  i64: int64;
  ui64: uint64;
  db: double;
begin
  Form := GetParentForm(Self);
  if Key=#27 then begin
    FEditing:=false;
    Key:=#0;
    if Form <> nil then Form.DefocusControl(Self, true);
  end else if Key=#13 then begin
    if Assigned(FVariable) then begin
      st:=0;
      case FValType of
        VT_BYTE: begin
          s:=Trim(inherited Text);
          if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
            n:=HexToInt(LeftStr(s,Length(s)-1));
            e:=0;
          end else Val(s,n,e);
          if e<>0 then st:=1 else
          if(n<FInputMin)then st:=2 else
          if(n>FInputMax)then st:=3 else begin
            _val:=Byte(n);
            sval:=IntToStr(_val);
          end;
        end;
        VT_WORD: begin
          s:=Trim(inherited Text);
          if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
            n:=HexToInt(LeftStr(s,Length(s)-1));
            e:=0;
          end else Val(s,n,e);
          if e<>0 then st:=1 else
          if(n<FInputMin)then st:=2 else
          if(n>FInputMax)then st:=3 else begin
            _val:=Word(n);
            sval:=IntToStr(_val);
          end;
        end;
        VT_DWORD: begin
          s:=Trim(inherited Text);
          if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
            dw:=HexToInt(LeftStr(s,Length(s)-1));
            b:=true;
          end else b:=StrToDWord(s,dw);
          if b then begin
            if(dw<FInputMin)then st:=2 else
            if(dw>FInputMax)then st:=3 else begin
              _val:=dw;
              sval:=DWordToStr(_val);
            end;
          end else st:=1;
        end;
        VT_SMALLINT: begin
          s:=Trim(inherited Text);
          if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
            n:=HexToInt(LeftStr(s,Length(s)-1));
            e:=0;
          end else Val(s,n,e);
          if e<>0 then st:=1 else
          if(n<FInputMin)then st:=2 else
          if(n>FInputMax)then st:=3 else begin
            _val:=Smallint(n);
            sval:=IntToStr(_val);
          end;
        end;
        VT_INT: begin
          s:=Trim(inherited Text);
          if(Length(s)>0)and(LowerCase(s[Length(s)])='h')then begin
            n:=HexToInt(LeftStr(s,Length(s)-1));
            e:=0;
          end else Val(s,n,e);
          if e<>0 then st:=1 else
          if(n<FInputMin)then st:=2 else
          if(n>FInputMax)then st:=3 else begin
            _val:=n;
            sval:=IntToStr(_val);
          end;
        end;
        VT_INT64: begin
          s:=Trim(inherited Text);
          Val(s,i64,e);
          if e<>0 then st:=1 else
          if(i64<FInputMin)then st:=2 else
          if(i64>FInputMax)then st:=3 else begin
            _val:=i64;
            sval:=IntToStr(_val);
          end;
        end;
        VT_UINT64: begin
          s:=Trim(inherited Text);
          Val(s,ui64,e);
          if e<>0 then st:=1 else
          if(ui64<FInputMin)then st:=2 else
          if(ui64>FInputMax)then st:=3 else begin
            _val:=ui64;
            sval:=IntToStr(_val);
          end;
        end;
        VT_SINGLE: begin
          s:=Trim(inherited Text);
          if FormatSettings.DecimalSeparator=#44
          then s:=ReplaceStr(s,'.',FormatSettings.DecimalSeparator)
          else s:=ReplaceStr(s,',',FormatSettings.DecimalSeparator);
          if TextToFloat(PChar(S), ext, fvExtended, FormatSettings) then begin
            sg:=ext;
            if(sg<FInputMin)then st:=2 else
            if(sg>FInputMax)then st:=3 else begin
              _val:=sg;
              if FFormat='' then sval:=FloatToStr(_val)
              else sval:=FormatFloat(FFormat,_val);
            end;
          end else st:=1;
        end;
        VT_DOUBLE: begin
          s:=Trim(inherited Text);
          if FormatSettings.DecimalSeparator=#44
          then s:=ReplaceStr(s,'.',FormatSettings.DecimalSeparator)
          else s:=ReplaceStr(s,',',FormatSettings.DecimalSeparator);
          if TextToFloat(PChar(S), ext, fvExtended, FormatSettings) then begin
            db:=ext;
            if(db<FInputMin)then st:=2 else
            if(db>FInputMax)then st:=3 else begin
              _val:=db;
              if FFormat='' then sval:=FloatToStr(_val)
              else FormatFloat(FFormat,_val);
            end;
          end else st:=1;
        end;
        VT_STRING: begin
          _val:=inherited Text;
          sval:=inherited Text;
        end;
      end;
      case st of
        0: if FAskBeforeInput then begin
          FAsk:=true;
          if AppMsgBox(ReplaceStr(KRFldMsgAskBeforeInput,'[#Value]',sval),MB_YESNOCANCEL or MB_ICONQUESTION)=IDYES then begin
            if Assigned(FSetValue) then FSetValue(Self,_val);
            if Assigned(FVarSet) then FVarSet.Value:=_val else FVariable.Value:=_val;
          end;
          FAsk:=false;
        end else begin
          if Assigned(FSetValue) then FSetValue(Self,_val);
          if Assigned(FVarSet) then FVarSet.Value:=_val else FVariable.Value:=_val;
        end;
        1: AppMsgBox(ReplaceStr(KRFldMsgIncorrectValue,'[#Value]',sval),MB_OKCANCEL or MB_ICONERROR);
        2: begin
          if (FValType<>VT_SINGLE)and(FValType<>VT_DOUBLE) then s:=IntToStr(FInputMin) else
            if FFormat='' then s:=FloatToStr(FInputMin) else s:=FormatFloat(FFormat,FInputMin);
          AppMsgBox(ReplaceStr(KRFldMsgMinimumLimit,'[#Value]',s),MB_OKCANCEL or MB_ICONERROR);
        end;
        3: begin
          if (FValType<>VT_SINGLE)and(FValType<>VT_DOUBLE) then s:=IntToStr(FInputMax) else
            if FFormat='' then s:=FloatToStr(FInputMax) else s:=FormatFloat(FFormat,FInputMax);
          AppMsgBox(ReplaceStr(KRFldMsgMaximumLimit,'[#Value]',s),MB_OKCANCEL or MB_ICONERROR);
        end;
      end;
    end;
    FEditing:=false;
    Key:=#0;
    if Form <> nil then Form.DefocusControl(Self, true);
  end;
  if Assigned(FOnKeyPress) then FOnKeyPress(Self,Key);
end;

procedure TKRField.DoTimer;
var
  val: Variant;
  i64: int64;
  ui64: uint64;

  function VarSameValue(const A, B: Variant): Boolean;
  var
    LA, LB: TVarData;
  begin
    LA := FindVarData(A)^;
    LB := FindVarData(B)^;
    if LA.VType = varEmpty then
      Result := LB.VType = varEmpty
    else if LA.VType = varNull then
      Result := LB.VType = varNull
    else if LB.VType in [varEmpty, varNull] then
      Result := False
    else if VarIsFloat(A) and IsNaN(A) then
      Result := VarIsFloat(B) and IsNaN(B)
    else if VarIsFloat(B) and IsNaN(B) then
      Result := VarIsFloat(A) and IsNaN(A)
    else Result := A = B;
  end;

begin
  if FEditing then Exit;

  if Assigned(FVariable) then begin
    if(not VarSameValue(FVariable.Value,FOldValue))or(FOldChange)or(FOldError<>FVariable.Error)then begin


      FOldChange:=false;
      FOldValue:=FVariable.Value;
      FOldError:=FVariable.Error;

      if(FVariable.Error=0)or((FVariable.Error=-2147483648)and(FSkipUserError))then begin
        inherited Hint:=FHint;
        inherited Color:=FColor;
        if not FEditing then Font.Color:=FFontColor;
      end else begin
        if FErrorToHint then inherited Hint:=FVariable.ErrorMsg;
        inherited Color:=FErrorColor;
        Font.Color:=FErrorFontColor;
      end;

      val:=FOldValue;
      if Assigned(FShowValue) then FShowValue(Self,val);

      if(FDateTime<>fdtNone)and(FValType in [VT_BYTE,VT_WORD,VT_DWORD,VT_SMALLINT,VT_INT,VT_INT64,VT_UINT64])then begin
        case FDateTime of
          fdtTime: if FFormat='' then inherited Text:=FormatDateTime('h:nn:ss',MKTimeToDateTime(val))
            else inherited Text:=FormatDateTime(FFormat,MKTimeToDateTime(val));
          fdtDate: if FFormat='' then inherited Text:=FormatDateTime('dd.mm.yyyy',MKTimeToDateTime(val))
            else inherited Text:=FormatDateTime(FFormat,MKTimeToDateTime(val));
          fdtDateTime: if FFormat='' then inherited Text:=FormatDateTime('dd.mm.yyyy h:nn:ss',MKTimeToDateTime(val))
            else inherited Text:=FormatDateTime(FFormat,MKTimeToDateTime(val));
        end;
      end else if(FDateTime<>fdtNone)and(FValType in [VT_SINGLE,VT_DOUBLE])then begin
        case FDateTime of
          fdtTime: if FFormat='' then inherited Text:=FormatDateTime('h:nn:ss',val)
            else inherited Text:=FormatDateTime(FFormat,val);
          fdtDate: if FFormat='' then inherited Text:=FormatDateTime('dd.mm.yyyy',val)
            else inherited Text:=FormatDateTime(FFormat,val);
          fdtDateTime: if FFormat='' then inherited Text:=FormatDateTime('dd.mm.yyyy h:nn:ss',val)
            else inherited Text:=FormatDateTime(FFormat,val);
        end;
      end else case FValType of
        VT_BYTE: if FFormat='' then inherited Text:=IntToStr(val) else
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[Byte(val)]);
        VT_WORD: if FFormat='' then inherited Text:=IntToStr(val) else
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[Word(val)]);
        VT_DWORD: if FFormat='' then inherited Text:=IntToStr(val)else
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[cardinal(val)]);
        VT_SMALLINT: if FFormat='' then inherited Text:=IntToStr(val) else
         inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[integer(val)]);
        VT_INT: if FFormat='' then inherited Text:=IntToStr(val) else
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[integer(val)]);
        VT_INT64: if FFormat='' then inherited Text:=IntToStr(val) else begin
          i64:=val;
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[i64]);
        end;
        VT_UINT64: if FFormat='' then inherited Text:=IntToStr(val) else begin
          ui64:=val;
          inherited Text:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[ui64]);
        end;
        VT_SINGLE, VT_DOUBLE: if FFormat='' then inherited Text:=FloatToStr(val)else
          inherited Text:=FormatFloat(FFormat,val);
        VT_STRING: inherited Text:=val;
        end;
    end;
  end;
  DoTimer_;
end;

procedure TKRField.DoTimer_;
begin

end;

function TKRField.GetReadOnly: boolean;
begin
  Result:=inherited ReadOnly;
end;

function TKRField.GetText: String;
begin
  Result:=inherited Text;
end;

procedure TKRField.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil else
    if (AComponent = FVarSet)then FVarSet:= nil else
    if (AComponent = FTimer)then begin
      FTimer:=nil;
    end;
end;

procedure TKRField.SetChangeFontColor(const Value: TColor);
begin
  if FChangeFontColor <> Value then begin
    FChangeFontColor := Value;
    if FEditing then Font.Color:=FChangeFontColor;
    FOldChange:=true;
  end;
end;

procedure TKRField.SetColor(const Value: TColor);
begin
  if FColor<>Value then begin
    FColor:=Value;
    if(not Assigned(FVariable))or(FVariable.Error=0)then inherited Color:=FColor;
    FOldChange:=true;
  end;
end;

procedure TKRField.SetErrorColor(const Value: TColor);
begin
  if FErrorColor<>Value then begin
    FErrorColor:=Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then inherited Color:=FErrorColor;
    FOldChange:=true;
  end;
end;

procedure TKRField.SetErrorFontColor(const Value: TColor);
begin
  if FErrorFontColor<>Value then begin
    FErrorFontColor := Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
    FOldChange:=true;
  end;
end;

procedure TKRField.SetErrorToHint(const Value: boolean);
begin
  if FErrorToHint<>Value then begin
    FErrorToHint:=Value;
    if(FErrorToHint)and(Assigned(FVariable))and(FVariable.Error<>0)then inherited Hint:=FVariable.ErrorMsg;
  end;
end;

procedure TKRField.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
  if(Assigned(FVariable))and(FVariable.Error=0)then Font.Color:=FFontColor;
  if FEditing then Font.Color:=FChangeFontColor;
  FOldChange:=true;
end;

procedure TKRField.SetFormat(const Value: String);
begin
  FFormat := Value;
  FOldChange:=true;
end;

procedure TKRField.SetHint(const Value: String);
begin
  if FHint<>Value then begin
    FHint:=Value;
    if(not FErrorToHint)or(not Assigned(FVariable))or(FVariable.Error=0)then inherited Hint:=FHint;
  end;
end;

procedure TKRField.SetInputMax(const Value: Variant);
begin
  FInputMax := Value;
end;

procedure TKRField.SetInputMin(const Value: Variant);
begin
  FInputMin := Value;
end;

procedure TKRField.SetReadOnly(const Value: boolean);
begin
  inherited ReadOnly:=value;
  if inherited ReadOnly then begin
    Cursor:=crArrow;
    FEditing:=false;
  end else Cursor:=crDefault;
  FOldChange:=true;
end;

procedure TKRField.SetTimer(const Value: TKRTimer);
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
  FOldChange:=true;
end;

procedure TKRField.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    if Assigned(FVariable) then FVariable.RemoveFreeNotification(Self);
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      if(csDesigning in ComponentState)and(not(csLoading  in ComponentState))then
        FValType:=FVariable.VarType;
    end;
  end;
  FOldChange:=true;
end;

procedure TKRField.SetVarSet(const Value: TKRVariable);
begin
  if FVarSet<>Value then begin
    FVarSet := Value;
    if Assigned(FVarSet) then begin
      FVarSet.FreeNotification(Self);
    end;
  end;
end;

procedure TKRField.WMFontChange(var Message: TMessage);
begin
  FFontColor:=Font.Color;
  if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
  if FEditing then Font.Color:=FChangeFontColor;
  FOldChange:=true;
end;

procedure TKRField.WMKillFocus(var Message: TWMKillFocus);
var key: char;
begin
  if FAsk then exit;
  if FEnterAftExit then begin
    key:=#13;
    DoKeyPress(nil,key);
  end else begin
    FEditing:=false;
    FOldChange:=true;
    DoTimer;
  end;
  inherited;
end;

procedure TKRField.WMSetFocus(var Message: TWMSetFocus);
begin
  if
    (Not ReadOnly)and
    (Assigned(FVariable))and
    ((FVariable.Error=0)or((FVariable.Error=-2147483648)and(FSkipUserError)))then begin
    FEditing:=true;
    FOldChange:=true;
    Font.Color:=FChangeFontColor;
    inherited;
  end;
end;

{ TKRBLField }

procedure TKRBLField.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLField.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLField.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLField.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
  FErrorToBoundLabel:=false;
end;

procedure TKRBLField.DoTimer_;
begin
  inherited;
  if Assigned(Variable)then
    if FErrorToBoundLabel then if Variable.Error=0 then FLabel.Caption:='' else FLabel.Caption:=Variable.ErrorMsg;
end;

procedure TKRBLField.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLField.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLField.SetName(const Value: TComponentName);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=Value;
end;

procedure TKRBLField.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLField.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

initialization
  KRFldMsgMaximumLimit:='Значение не должно быть больше [#Value]';
  KRFldMsgMinimumLimit:='Значение не должно быть меньше [#Value]';
  KRFldMsgAskBeforeInput:='Установить значение «[#Value]»?';
  KRFldMsgIncorrectValue:='Неверно введено значение!';
end.
