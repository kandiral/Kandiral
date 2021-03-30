(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBRegInterval                                                           *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBRegInterval;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Winapi.Messages, System.SysUtils,
  {$ELSE}
    Classes, Messages, SysUtils,
  {$IFEND}
  KRValueEdit, KRIniConfig, KRModbusClient;

type
  TKRMBRegInterval = class(TKRValueEdit)
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
  public
    constructor Create(AOwner: TComponent);override;
  published
    property Variable: TKRMBRegister read FVariable write SetVariable;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

implementation

{ TKRMBRegInterval }

procedure TKRMBRegInterval.Change_(Sender: TObject);
begin
  if Assigned(FVariable) then FVariable.Interval:=Value;
  if Assigned(FChange) then FChange(Self);
end;

constructor TKRMBRegInterval.Create(AOwner: TComponent);
begin
  inherited;
  ValueType:=vtInteger;
  Value:=0;
  InputMax:=65535;
  InputMin:=0;
  inherited OnChange:=Change_;
end;

procedure TKRMBRegInterval.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRMBRegInterval.SetInputMax(const Value: Variant);
begin
  if Value=65535 then inherited SetInputMax(Value);
end;

procedure TKRMBRegInterval.SetInputMin(const Value: Variant);
begin
  if Value=0 then inherited SetInputMin(Value);
end;

procedure TKRMBRegInterval.SetValueType(const Value: TKRValType);
begin
  if Value=vtInteger then inherited SetValueType(Value);
end;

procedure TKRMBRegInterval.SetVariable(const AValue: TKRMBRegister);
begin
  if FVariable<>AValue then begin
    FVariable := AValue;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      Self.Value:=FVariable.Interval;
    end;
  end;
end;

end.
