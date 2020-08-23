(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBRegIndex                                                              *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBRegIndex;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Winapi.Messages, System.SysUtils,
  {$ELSE}
    Classes, Messages, SysUtils,
  {$IFEND}
  KRValueEdit, KRIniConfig, KRModbusClient;

type
  TKRMBRegIndex = class(TKRValueEdit)
  private
    FChange: TNotifyEvent;
    FVariable: TKRMBRegister;
    procedure SetVariable(const AValue: TKRMBRegister);
    procedure Change_(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInputMax(const Value: Variant);override;
    procedure SetInputMin(const Value: Variant);override;
    procedure SetValueType(const Value: TKRValType);override;
    procedure SetFormat(const Value: String);override;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property Variable: TKRMBRegister read FVariable write SetVariable;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

implementation

uses Funcs;

{ TKRMBRegIndex }

procedure TKRMBRegIndex.Change_(Sender: TObject);
begin
  if Assigned(FVariable) then FVariable.RegisterIndex:=Value;
  if Assigned(FChange) then FChange(Self);
end;

constructor TKRMBRegIndex.Create(AOwner: TComponent);
begin
  inherited;
  ValueType:=vtInteger;
  Value:=0;
  InputMax:=65535;
  InputMin:=0;
  Format:='hhhh';
  inherited OnChange:=Change_;
end;

procedure TKRMBRegIndex.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRMBRegIndex.SetFormat(const Value: String);
begin
  if Value='hhhh' then inherited SetFormat(Value);
end;

procedure TKRMBRegIndex.SetInputMax(const Value: Variant);
begin
  if Value=65535 then inherited SetInputMax(Value);
end;

procedure TKRMBRegIndex.SetInputMin(const Value: Variant);
begin
  if Value=0 then inherited SetInputMin(Value);
end;

procedure TKRMBRegIndex.SetValueType(const Value: TKRValType);
begin
  if Value=vtInteger then inherited SetValueType(Value);
end;

procedure TKRMBRegIndex.SetVariable(const AValue: TKRMBRegister);
begin
  if FVariable<>AValue then begin
    FVariable := AValue;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      Self.Value:=FVariable.RegisterIndex;
    end;
  end;
end;

end.
