(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBQueueBar                                                              *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBQueueBar;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Forms, Vcl.ComCtrls,
  {$ELSE}
    Classes, Forms, ComCtrls,
  {$IFEND}
  KRProgressBar, KRTimer, KRModbus, KRModbusMaster, KRBoundLabel;

type
  TKRMBQueueBar = class(TKRProgressBar, IKRTimer)
  private
    FModbus: TKRModbusMaster;
    FTimer: TKRTimer;
    procedure SetModbus(const Value: TKRModbusMaster);
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPosition: Integer;
    procedure SetTimer(const Value: TKRTimer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoTimer; stdcall;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Modbus: TKRModbusMaster read FModbus write SetModbus;
    property Position: Integer read GetPosition;
    property Timer: TKRTimer read FTimer write SetTimer;
    property Min: Integer read GetMin;
    property Max: Integer read GetMax;
  end;

implementation


{ TKRMBQueueBar }

constructor TKRMBQueueBar.Create(AOwner: TComponent);
begin
  inherited;
  Self.Orientation:=pbVertical;
  Height:=57;
  Width:=25;
  SetupInternalLabel;
  inherited Max:=MB_QUEUE_MAX_ITEMS;
  Enabled:=false;
  BLabel.Position:=blpRightBottom;
end;

destructor TKRMBQueueBar.Destroy;
begin
  if(Assigned(FTimer))then begin
    while FTimer.Working do Application.ProcessMessages;
    FTimer.DelMon(Self);
  end;
  inherited;
end;

procedure TKRMBQueueBar.DoTimer;
begin
  if Assigned(FModbus) then begin
    inherited Position:=FModbus.QueueCount;
    Enabled:=true;
  end else begin
    inherited Position:=0;
    Enabled:=false;
  end;
end;

function TKRMBQueueBar.GetMax: Integer;
begin
  Result:=inherited Max;
end;

function TKRMBQueueBar.GetMin: Integer;
begin
  Result:=inherited Min;
end;

function TKRMBQueueBar.GetPosition: Integer;
begin
  Result:=inherited Position;
end;

procedure TKRMBQueueBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if AComponent = FModbus then begin FModbus:=nil;Enabled:=false;end else
    if (AComponent = FTimer)then begin
      FTimer:=nil;
    end;
  end;
end;

procedure TKRMBQueueBar.SetModbus(const Value: TKRModbusMaster);
begin
  if FModbus<>Value then begin
    FModbus := Value;
    if Assigned(FModbus) then begin
      FModbus.FreeNotification(Self);
      Enabled:=true;
    end else Enabled:=false;
  end;
end;

procedure TKRMBQueueBar.SetTimer(const Value: TKRTimer);
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
end;

end.
