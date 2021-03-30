(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVComboBox                                                               *)
(*  Ver.: 17.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRVComboBox;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Winapi.Messages, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls,
    System.SysUtils,
  {$ELSE}
    Classes, Messages, Controls, Graphics, StdCtrls, SysUtils,
  {$IFEND}
  KRComboBox, KRVariables, KRBoundLabel;

type
  TKRVCBCheckValue = (cvByIndex, cvByData);

  TKRVComboBox = class(TKRComboBox, IKRVarUp)
  private
    FVariable: TKRVariable;
    FChange: TNotifyEvent;
    FFontColor: TColor;
    FErrorColor: TColor;
    FColor: TColor;
    FErrorFontColor: TColor;
    FVarChange, FChanging: boolean;
    FAfterChange: TNotifyEvent;
    FCheckValue: TKRVCBCheckValue;
    procedure SetVariable(const Value: TKRVariable);
    procedure _Change(Sender: TObject);
    procedure VarUp(AVar: TKRVariable); stdcall;
    procedure VarErr(AVar: TKRVariable); stdcall;
    procedure SetColor(const Value: TColor);
    procedure SetErrorColor(const Value: TColor);
    procedure SetErrorFontColor(const Value: TColor);
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Variable: TKRVariable read FVariable write SetVariable;
    property CheckValue: TKRVCBCheckValue read FCheckValue write FCheckValue default cvByIndex;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnAfterChange: TNotifyEvent read FAfterChange write FAfterChange;
    property Color: TColor read FColor write SetColor;
    property ErrorColor: TColor read FErrorColor write SetErrorColor default $2F2F2F;
    property ErrorFontColor: TColor read FErrorFontColor write SetErrorFontColor default clOlive;
  end;

  TKRBLVComboBox = class(TKRVComboBox)
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

{ TKRVComboBox }

constructor TKRVComboBox.Create(AOwner: TComponent);
begin
  inherited;
  inherited OnChange:=_Change;
  FColor:=inherited Color;
  FErrorColor:=$2F2F2F;
  FErrorFontColor:=clOlive;
  FFontColor:=Font.Color;
  FVarChange:=false;
  FChanging:=false;
  FCheckValue:=cvByIndex;
end;

procedure TKRVComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRVComboBox.SetColor(const Value: TColor);
begin
  if FColor<>Value then begin
    FColor:=Value;
    if(not Assigned(FVariable))or(FVariable.Error=0)then inherited Color:=FColor;
  end;
end;

procedure TKRVComboBox.SetErrorColor(const Value: TColor);
begin
  if FErrorColor<>Value then begin
    FErrorColor:=Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then inherited Color:=FErrorColor;
  end;
end;

procedure TKRVComboBox.SetErrorFontColor(const Value: TColor);
begin
  if FErrorFontColor<>Value then begin
    FErrorFontColor := Value;
    if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
  end;
end;

procedure TKRVComboBox.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    if Assigned(FVariable) then FVariable.DelMon(Self);
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      FVariable.AddMon(Self);
      VarUp(FVariable);
    end;
  end;
end;

procedure TKRVComboBox.VarErr(AVar: TKRVariable);
begin
  inherited Color:=FErrorColor;
  Font.Color:=FErrorFontColor;
end;

procedure TKRVComboBox.VarUp(AVar: TKRVariable);
var
  i,n: integer;
begin
  inherited Color:=FColor;
  Font.Color:=FFontColor;
  FVarChange:=true;
  if Style=csDropDownList then begin
    case FCheckValue of
      cvByIndex: if(ItemIndex<>FVariable.Value)then
        if(FVariable.Value>=-1)and(ItemCount>FVariable.Value)then ItemIndex:=FVariable.Value
        else ItemIndex:=-1;
      cvByData: begin
        n:=-1;
        for i := 0 to ItemCount-1 do
          if NativeInt(Items.Objects[i])=FVariable.Value then begin
            n:=i;
            break;
          end;
        ItemIndex:=n;
      end;
    end;

  end else begin
    if Text<>FVariable.Value then Text:=FVariable.Value;
  end;
  FVarChange:=false;
end;

procedure TKRVComboBox.WMFontChange(var Message: TMessage);
begin
  FFontColor:=Font.Color;
  if(Assigned(FVariable))and(FVariable.Error<>0)then Font.Color:=FErrorFontColor;
end;

procedure TKRVComboBox._Change(Sender: TObject);
begin
  if FChanging then exit;
  if Assigned(FChange) then FChange(Self);
  if Assigned(FVariable)and(not FVarChange) then begin
    FChanging:=true;
    if Style=csDropDownList then begin
    case FCheckValue of
      cvByIndex: FVariable.Value:=ItemIndex;
      cvByData: if ItemIndex=-1 then FVariable.Value:=0 else
        FVariable.Value:=NativeInt(Items.Objects[ItemIndex]);
    end;
    end else FVariable.Value:=Text;
    FChanging:=false;
  end;
  if Assigned(FAfterChange) then FAfterChange(Self);
end;

{ TKRBLVComboBox }

procedure TKRBLVComboBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.BiDiMode := BiDiMode;
end;

procedure TKRBLVComboBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Enabled := Enabled;
end;

procedure TKRBLVComboBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) then FLabel.Visible := Visible;
end;

constructor TKRBLVComboBox.Create(AOwner: TComponent);
begin
  inherited;
  SetupInternalLabel;
end;

procedure TKRBLVComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FLabel then FLabel := nil;
end;

procedure TKRBLVComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if assigned(FLabel) then FLabel.Caption:=FLabel.Caption;
end;

procedure TKRBLVComboBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and assigned(FLabel) and
     ((FLabel.GetTextLen = 0) or (CompareText(FLabel.Caption, Name) = 0)) then
    FLabel.Caption := Value;
  inherited SetName(Value);
end;

procedure TKRBLVComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLabel = nil then exit;
  FLabel.Parent := AParent;
  FLabel.Visible := True;
end;

procedure TKRBLVComboBox.SetupInternalLabel;
begin
  if Assigned(FLabel) then exit;
  FLabel:=TKRBoundLabel.Create(Self);
  FLabel.FreeNotification(Self);
end;

end.
