(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRConnectorQueueBar                                                       *)
(*  Ver.: 14.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRConnectorQueueBar;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.ComCtrls, Vcl.Forms,
  {$ELSE}
    Classes, ComCtrls, Forms,
  {$IFEND}
  KRConnector, KRProgressBar, KRTimer, KRBoundLabel;

type
  TKRConnectorQueueBar = class(TKRProgressBar, IKRTimer)
  private
    FConnector: TKRConnector;
    FTimer: TKRTimer;
    FStackOut: boolean;
    procedure SetConnector(const Value: TKRConnector);
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
    property Connector: TKRConnector read FConnector write SetConnector;
    property Position: Integer read GetPosition;
    property Timer: TKRTimer read FTimer write SetTimer;
    property Min: Integer read GetMin;
    property Max: Integer read GetMax;
    property StackOut: boolean read FStackOut write FStackOut;
    property Align;
    property Anchors;
    property BorderWidth;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Constraints;
    property Orientation;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property Smooth;
    property Style;
    property MarqueeInterval;
    property BarColor;
    property BackgroundColor;
    property SmoothReverse;
    property Step;
    property State;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    {$IF CompilerVersion >= 24}
      property StyleElements;
    {$IFEND}
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
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

{ TKRConnectorQueueBar }

constructor TKRConnectorQueueBar.Create(AOwner: TComponent);
begin
  inherited;
  FStackOut:=false;
  SetupInternalLabel;
  inherited Max:=CE_QUEUE_MAX_ITEMS;
  Enabled:=false;
  BLabel.Position:=blpTopCenter;
end;

destructor TKRConnectorQueueBar.Destroy;
begin
  if(Assigned(FTimer))then begin
    while FTimer.Working do Application.ProcessMessages;
    FTimer.DelMon(Self);
  end;
  inherited;
end;

procedure TKRConnectorQueueBar.DoTimer;
begin
  if Assigned(FConnector) then begin
    if FStackOut then inherited Position:=FConnector.QueueOutCount
    else inherited Position:=FConnector.QueueCount;
    Enabled:=true;
  end else begin
    inherited Position:=0;
    Enabled:=false;
  end;
end;

function TKRConnectorQueueBar.GetMax: Integer;
begin
  Result:=inherited Max;
end;

function TKRConnectorQueueBar.GetMin: Integer;
begin
  Result:=inherited Min;
end;

function TKRConnectorQueueBar.GetPosition: Integer;
begin
  Result:=inherited Position;
end;

procedure TKRConnectorQueueBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent = FConnector then begin FConnector:=nil;Enabled:=false;end else
    if (AComponent = FTimer)then begin
      FTimer:=nil;
    end;
end;

procedure TKRConnectorQueueBar.SetConnector(const Value: TKRConnector);
begin
  if FConnector<>Value then begin
    FConnector := Value;
    if Assigned(FConnector) then begin
      FConnector.FreeNotification(Self);
      Enabled:=true;
    end else Enabled:=false;
  end;
end;

procedure TKRConnectorQueueBar.SetTimer(const Value: TKRTimer);
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
