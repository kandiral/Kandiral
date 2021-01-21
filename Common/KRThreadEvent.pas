unit KRThreadEvent;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes,
  {$ELSE}
    Windows, Classes,
  {$IFEND}
  KRRuntimeErrors;

type

  TKRThreadEvent = class(TThread)
  private
    FWaitEvent, FStopEvent: THandle;
  protected
    procedure Execute; override;
    procedure KRExecute; virtual; abstract;
    procedure KRExecuteFirst; virtual;
    procedure KRExecuteLast; virtual;
  public
    constructor Create(AWaitEvent: THandle);
    destructor Destroy; override;
    procedure Terminate;
  end;

implementation

{ TKRThreadEvent }

constructor TKRThreadEvent.Create(AWaitEvent: THandle);
begin
  FWaitEvent:=AWaitEvent;
  FStopEvent:=CreateEvent(nil, true, false, nil);
  inherited Create(false);
end;

destructor TKRThreadEvent.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TKRThreadEvent.Execute;
var
  Events: array [0..1] of THandle;
begin
  Events[0]:=FStopEvent;
  Events[1]:=FWaitEvent;
  KRExecuteFirst;
  while not Terminated do
    if WaitForMultipleObjects(2, @Events, False, INFINITE)=(WAIT_OBJECT_0 + 1)
    then KRExecute else break;
  KRExecuteLast;
  CloseHandle(FStopEvent);
end;

procedure TKRThreadEvent.KRExecuteFirst; begin end;

procedure TKRThreadEvent.KRExecuteLast; begin end;

procedure TKRThreadEvent.Terminate;
begin
  inherited;
  SetEvent(FStopEvent);
end;

end.
