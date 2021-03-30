(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVarLabel                                                                *)
(*  Ver.: 06.02.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRVarLabel;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Graphics,
    System.SysUtils, Winapi.Messages, System.Variants, Vcl.Forms, System.Math,
  {$ELSE}
    Classes, Controls, ExtCtrls, StdCtrls, Graphics, SysUtils, Messages, Variants,
    Forms, Math,
  {$IFEND}
  KRTypes, KRField, KRVariables, KRDateTime, KRVariants;

type
  TKRVarLabel = class(TCustomLabel, IKRVarUp)
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
    FOldValue: Variant;
    FOldError: integer;
    FOutFunction: TKREmptyEvent;
    FShowValue: TKRFieldValueEvent;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    function GetCaption_: TCaption;
    procedure SetColor(const Value: TColor);
    procedure SetErrorColor(const Value: TColor);
    procedure SetErrorFontColor(const Value: TColor);
    procedure SetErrorToHint(const Value: boolean);
    procedure SetHint(const Value: String);
    procedure SetVariable(const Value: TKRVariable);
    procedure SetFontColor(const Value: TColor);
    procedure SetFormat(const Value: String);
    procedure SetVarUnit(const Value: String);
    procedure SetDateTime(const Value: TKRFieldDateTime);
    procedure VarUp(AVar: TKRVariable); stdcall;
    procedure VarErr(AVar: TKRVariable); stdcall;

    // Out Functions
    procedure UpOutFunction;
    
    procedure OF_String;
    
    procedure OF_Integer;
    procedure OF_Integer_F;
    procedure OF_DWORD_F;
    procedure OF_Int64_F;
    procedure OF_UInt64_F;

    procedure OF_Float;
    procedure OF_Float_F;
    
    procedure OF_Time;
    procedure OF_Date;
    procedure OF_DateTime;
    procedure OF_DateTime_F;
    procedure OF_TimeD;
    procedure OF_DateD;
    procedure OF_DateTimeD;
    procedure OF_DateTimeD_F;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoTimer(AChanged: boolean = false);
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
    property DateTime: TKRFieldDateTime read FDateTime write SetDateTime default fdtNone;
    property VarUnit: String read FVarUnit write SetVarUnit;
    property FontColor: TColor read FFontColor write SetFontColor;
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
    property OnShowValue: TKRFieldValueEvent read FShowValue write FShowValue;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

{ TKRVarLabel }

constructor TKRVarLabel.Create(AOwner: TComponent);
begin
  inherited;
  FOldValue:=null;
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
  UpOutFunction;
end;

destructor TKRVarLabel.Destroy;
begin
  if(Assigned(FVariable))then FVariable.DelMon(Self);
  inherited;
end;

procedure TKRVarLabel.DoTimer(AChanged: boolean = false);
begin
  if not Assigned(FVariable) then exit;

  if AChanged or(not KRVarIsEqually(FVariable.Value,FOldValue))or(FOldError<>FVariable.Error)then begin

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

    if Assigned(FShowValue) then FShowValue(Self,FOldValue);

    FOutFunction;
    if FVarUnit<>'' then inherited Caption:=Caption+' '+FVarUnit;
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
    if(AComponent=FVariable)then FVariable:=nil;
end;

procedure TKRVarLabel.OF_Date;
begin  
  inherited Caption:=DateToStr(KRUnixToDateTime(FOldValue));
end;

procedure TKRVarLabel.OF_DateD;
begin
  inherited Caption:=DateToStr(FOldValue);
end;

procedure TKRVarLabel.OF_DateTime;
begin
  inherited Caption:=DateTimeToStr(KRUnixToDateTime(FOldValue));
end;

procedure TKRVarLabel.OF_DateTimeD;
begin
  inherited Caption:=DateTimeToStr(FOldValue);
end;

procedure TKRVarLabel.OF_DateTimeD_F;
begin
  inherited Caption:=FormatDateTime(FFormat,FOldValue);
end;

procedure TKRVarLabel.OF_DateTime_F;
begin
  inherited Caption:=FormatDateTime(FFormat,KRUnixToDateTime(FOldValue));
end;

procedure TKRVarLabel.OF_DWORD_F;
begin
  inherited Caption:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[cardinal(FOldValue)]);
end;

procedure TKRVarLabel.OF_Float;
begin
  inherited Caption:=FloatToStr(FOldValue);
end;

procedure TKRVarLabel.OF_Float_F;
begin
  inherited Caption:=FormatFloat(FFormat,FOldValue);
end;

procedure TKRVarLabel.OF_Int64_F;
var i: int64;
begin
  i:=FOldValue;
  inherited Caption:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[i]);
end;

procedure TKRVarLabel.OF_Integer;
begin
  inherited Caption:=IntToStr(FOldValue);
end;

procedure TKRVarLabel.OF_Integer_F;
var i: integer;
begin
  i:=FOldValue;
  inherited Caption:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[i]);
end;

procedure TKRVarLabel.OF_String;
begin
  inherited Caption:=FOldValue;
end;

procedure TKRVarLabel.OF_Time;
begin
  inherited Caption:=TimeToStr(KRUnixToDateTime(FOldValue));
end;

procedure TKRVarLabel.OF_TimeD;
begin
  inherited Caption:=TimeToStr(FOldValue);
end;

procedure TKRVarLabel.OF_UInt64_F;
var i: uint64;
begin
  i:=FOldValue;
  inherited Caption:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.Format(FFormat,[i]);
end;

procedure TKRVarLabel.SetColor(const Value: TColor);
begin
  if FColor<>Value then begin
    FColor:=Value;
    if(not Assigned(FVariable))or(FVariable.Error=0)then inherited Color:=FColor;
    DoTimer(true);
  end;
end;

procedure TKRVarLabel.SetDateTime(const Value: TKRFieldDateTime);
begin
  FDateTime := Value;
  UpOutFunction;
end;

procedure TKRVarLabel.SetErrorColor(const Value: TColor);
begin
  if FErrorColor<>Value then begin
    FErrorColor:=Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then inherited Color:=FErrorColor;
    DoTimer(true);
  end;
end;

procedure TKRVarLabel.SetErrorFontColor(const Value: TColor);
begin
  if FErrorFontColor<>Value then begin
    FErrorFontColor := Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
    DoTimer(true);
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
  DoTimer(true);
end;

procedure TKRVarLabel.SetFormat(const Value: String);
begin
  FFormat := Value;
  UpOutFunction;
end;

procedure TKRVarLabel.SetHint(const Value: String);
begin
  if FHint<>Value then begin
    FHint:=Value;
    if(not FErrorToHint)or(not Assigned(FVariable))or(FVariable.Error=0)then inherited Hint:=FHint;
  end;
end;

procedure TKRVarLabel.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    if Assigned(FVariable) then begin
      FVariable.DelMon(Self);
      FVariable.RemoveFreeNotification(Self);
    end;
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      FVariable.AddMon(Self);
    end;
  end;
  UpOutFunction;
end;

procedure TKRVarLabel.SetVarUnit(const Value: String);
begin
  FVarUnit := Value;
  DoTimer(true);
end;

procedure TKRVarLabel.UpOutFunction;
begin
  if Not Assigned(FVariable) then exit;
  
  if(FDateTime<>fdtNone)and(Variable.VarType in [VT_BYTE,VT_WORD,VT_DWORD,VT_SMALLINT,VT_INT,VT_INT64])then begin
    case FDateTime of
      fdtTime: if FFormat='' then FOutFunction:=OF_Time else FOutFunction:=OF_DateTime_F;
      fdtDate: if FFormat='' then FOutFunction:=OF_Date else FOutFunction:=OF_DateTime_F;
      fdtDateTime: if FFormat='' then FOutFunction:=OF_DateTime else FOutFunction:=OF_DateTime_F;
    end;
  end else if(FDateTime<>fdtNone)and(Variable.VarType in [VT_SINGLE,VT_DOUBLE])then begin
    case FDateTime of
      fdtTime: if FFormat='' then FOutFunction:=OF_TimeD else FOutFunction:=OF_DateTimeD_F;
      fdtDate: if FFormat='' then FOutFunction:=OF_DateD else FOutFunction:=OF_DateTimeD_F;
      fdtDateTime: if FFormat='' then FOutFunction:=OF_DateTimeD else FOutFunction:=OF_DateTimeD_F;
    end;
  end else case Variable.VarType of
    VT_BYTE, VT_WORD, VT_SMALLINT, VT_INT: if FFormat='' then FOutFunction:=OF_Integer else FOutFunction:=OF_Integer_F;
    VT_DWORD: if FFormat='' then FOutFunction:=OF_Integer else FOutFunction:=OF_DWORD_F;
    VT_INT64: if FFormat='' then FOutFunction:=OF_Integer else FOutFunction:=OF_Int64_F;
    VT_UINT64: if FFormat='' then FOutFunction:=OF_Integer else FOutFunction:=OF_UInt64_F;
    VT_SINGLE, VT_DOUBLE: if FFormat='' then FOutFunction:=OF_Float else FOutFunction:=OF_Float_F;
    VT_STRING: FOutFunction:=OF_String;
  end;

  DoTimer(true);
end;

procedure TKRVarLabel.VarErr(AVar: TKRVariable);
begin
  DoTimer(true);
end;

procedure TKRVarLabel.VarUp(AVar: TKRVariable);
begin
  DoTimer;
end;

procedure TKRVarLabel.WMFontChange(var Message: TMessage);
begin
  FFontColor:=Font.Color;
  if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
  DoTimer(true);
end;

end.
