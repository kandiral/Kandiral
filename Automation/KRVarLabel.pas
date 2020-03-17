(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVarLabel                                                                *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRVarLabel;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Graphics,
    System.SysUtils, Winapi.Messages, System.Variants, Vcl.Forms,
  {$ELSE}
    Classes, Controls, ExtCtrls, StdCtrls, Graphics, SysUtils, Messages, Variants,
    Forms,
  {$IFEND}
  KRField, KRTimer, KRVariables;

type
  TKRVarLabel = class(TCustomLabel, IKRTimer)
  private
    FVariable: TKRVariable;
    FHint: String;
    FErrorColor: TColor;
    FColor: TColor;
    FErrorFontColor: TColor;
    FFormat: String;
    FDateTime: TKRFieldDateTime;
    FErrorToHint: boolean;
    FFontColor: TColor;
    FVarUnit: String;
    FTimer: TKRTimer;
    FOldValue: Variant;
    FOldChange: boolean;
    FOldError: integer;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    function GetCaption_: TCaption;
    procedure SetColor(const Value: TColor);
    procedure SetErrorColor(const Value: TColor);
    procedure SetErrorFontColor(const Value: TColor);
    procedure SetErrorToHint(const Value: boolean);
    procedure SetHint(const Value: String);
    procedure SetVariable(const Value: TKRVariable);
    procedure SetFontColor(const Value: TColor);
    procedure SetTimer(const Value: TKRTimer);
    procedure SetFormat(const Value: String);
    procedure SetVarUnit(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoTimer; stdcall;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Caption: TCaption read GetCaption_;
  published
    property Variable: TKRVariable read FVariable write SetVariable;
    property Format: String read FFormat write SetFormat;
    property Color: TColor read FColor write SetColor;
    property ErrorColor: TColor read FErrorColor write SetErrorColor default $2F2F2F;
    property ErrorFontColor: TColor read FErrorFontColor write SetErrorFontColor default clOlive;
    property Hint: String read FHint write SetHint;
    property ErrorToHint: boolean read FErrorToHint write SetErrorToHint;
    property DateTime: TKRFieldDateTime read FDateTime write FDateTime default fdtNone;
    property VarUnit: String read FVarUnit write SetVarUnit;
    property FontColor: TColor read FFontColor write SetFontColor;
    property Timer: TKRTimer read FTimer write SetTimer;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property GlowSize; // Windows Vista only
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Touch;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses lgop, funcs;

{ TKRVarLabel }

constructor TKRVarLabel.Create(AOwner: TComponent);
begin
  inherited;
  FOldValue:=null;
  FOldChange:=true;
  FOldError:=-8723982;
  FFormat:='';
  FColor:=inherited Color;
  FFontColor:=Font.Color;
  FErrorColor:=$2F2F2F;
  FErrorFontColor:=clOlive;
  FHint:=inherited Hint;
  FVariable:=nil;
  FErrorToHint:=true;
  FDateTime:=fdtNone;
  FVarUnit:='';
end;

destructor TKRVarLabel.Destroy;
begin
  if(Assigned(FTimer))then begin
    while FTimer.Working do Application.ProcessMessages;
    FTimer.DelMon(Self);
  end;
  inherited;
end;

procedure TKRVarLabel.DoTimer;
begin
  if Assigned(FVariable) then begin
    if(not IsVariantsEqual(FVariable.Value,FOldValue))or(FOldChange)or(FOldError<>FVariable.Error)then begin

      FOldChange:=false;
      FOldValue:=FVariable.Value;
      FOldError:=FVariable.Error;

      if FVariable.Error=0 then begin
        inherited Hint:=FHint;
        inherited Color:=FColor;
        Font.Color:=FFontColor;
      end else begin
        if FErrorToHint then inherited Hint:=FVariable.ErrorMsg;
        inherited Color:=FErrorColor;
        Font.Color:=FErrorFontColor;
      end;
      if(FDateTime<>fdtNone)and(Variable.VarType in [VT_BYTE,VT_WORD,VT_DWORD,VT_SMALLINT,VT_INT,VT_INT64])then begin
        case FDateTime of
          fdtTime: inherited Caption:=FormatDateTime(_is(FFormat='','h:nn:ss',FFormat),MKTimeToDateTime(FOldValue));
          fdtDate: inherited Caption:=FormatDateTime(_is(FFormat='','dd.mm.yyyy',FFormat),MKTimeToDateTime(FOldValue));
          fdtDateTime: inherited Caption:=FormatDateTime(_is(FFormat='','dd.mm.yyyy h:nn:ss',FFormat),MKTimeToDateTime(FOldValue));
        end;
      end else if(FDateTime<>fdtNone)and(Variable.VarType in [VT_SINGLE,VT_DOUBLE])then begin
        case FDateTime of
          fdtTime: inherited Caption:=FormatDateTime(_is(FFormat='','h:nn:ss',FFormat),FOldValue);
          fdtDate: inherited Caption:=FormatDateTime(_is(FFormat='','dd.mm.yyyy',FFormat),FOldValue);
          fdtDateTime: inherited Caption:=FormatDateTime(_is(FFormat='','dd.mm.yyyy h:nn:ss',FFormat),FOldValue);
        end;
      end else case Variable.VarType of
        VT_BYTE: inherited Caption:=_is(FFormat='',IntToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[Byte(FOldValue)]));
        VT_WORD: inherited Caption:=_is(FFormat='',IntToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[Word(FOldValue)]));
        VT_DWORD: inherited Caption:=_is(FFormat='',DWordToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[cardinal(FOldValue)]));
        VT_SMALLINT: inherited Caption:=_is(FFormat='',IntToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[integer(FOldValue)]));
        VT_INT: inherited Caption:=_is(FFormat='',IntToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[integer(FOldValue)]));
        VT_INT64: inherited Caption:=_is(FFormat='',IntToStr(FOldValue),{$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[integer(FOldValue)]));
        VT_SINGLE, VT_DOUBLE: inherited Caption:=_is(FFormat='',FloatToStr(FOldValue),FormatFloat(FFormat,FOldValue));
        VT_STRING: inherited Caption:=FOldValue;
        end;
      if FVarUnit<>'' then inherited Caption:=Caption+' '+FVarUnit;
    end;
  end;
end;

function TKRVarLabel.GetCaption_: TCaption;
begin
  Result:=inherited Caption;
end;

procedure TKRVarLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if(AComponent=FVariable)then FVariable:= nil else
    if(AComponent=FTimer)then begin
      FTimer:=nil;
    end;
end;

procedure TKRVarLabel.SetColor(const Value: TColor);
begin
  if FColor<>Value then begin
    FColor:=Value;
    if(not Assigned(FVariable))or(FVariable.Error=0)then inherited Color:=FColor;
    FOldChange:=true;
  end;
end;

procedure TKRVarLabel.SetErrorColor(const Value: TColor);
begin
  if FErrorColor<>Value then begin
    FErrorColor:=Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then inherited Color:=FErrorColor;
    FOldChange:=true;
  end;
end;

procedure TKRVarLabel.SetErrorFontColor(const Value: TColor);
begin
  if FErrorFontColor<>Value then begin
    FErrorFontColor := Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
    FOldChange:=true;
  end;
end;

procedure TKRVarLabel.SetErrorToHint(const Value: boolean);
begin
  if FErrorToHint<>Value then begin
    FErrorToHint:=Value;
    if(FErrorToHint)and(Assigned(FVariable))and(FVariable.Error<>0)then inherited Hint:=FVariable.ErrorMsg;
  end;
end;

procedure TKRVarLabel.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
  if(Assigned(FVariable))and(FVariable.Error=0)then Font.Color:=FFontColor;
  FOldChange:=true;
end;

procedure TKRVarLabel.SetFormat(const Value: String);
begin
  FFormat := Value;
  FOldChange:=true;
end;

procedure TKRVarLabel.SetHint(const Value: String);
begin
  if FHint<>Value then begin
    FHint:=Value;
    if(not FErrorToHint)or(not Assigned(FVariable))or(FVariable.Error=0)then inherited Hint:=FHint;
  end;
end;

procedure TKRVarLabel.SetTimer(const Value: TKRTimer);
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

procedure TKRVarLabel.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
    end;
  end;
  FOldChange:=true;
end;

procedure TKRVarLabel.SetVarUnit(const Value: String);
begin
  FVarUnit := Value;
  FOldChange:=true;
end;

procedure TKRVarLabel.WMFontChange(var Message: TMessage);
begin
  FFontColor:=Font.Color;
  if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
  FOldChange:=true;
end;

end.
