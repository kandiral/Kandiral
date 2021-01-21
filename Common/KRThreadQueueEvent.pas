unit KRThreadQueueEvent;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes;
  {$ELSE}
    Windows, Classes;
  {$IFEND}

type
  TKRThreadQueueEvent = class(TObject)
  private
    FLock: TObject;
    FList: TList;
    FSignalEvent: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    procedure Push(AItem: Pointer);
    function Pop: Pointer;
    function Peek: Pointer;
    procedure LockQueue;
    procedure UnlockQueue;
    property SignalEvent: THandle read FSignalEvent;
  end;

implementation

{ TKRThreadQueueEvent }

function TKRThreadQueueEvent.AtLeast(ACount: Integer): Boolean;
begin
  TMonitor.Enter(FLock);
  Result:=FList.Count>=ACount;
  TMonitor.Exit(FLock);
end;

procedure TKRThreadQueueEvent.Clear;
begin
  TMonitor.Enter(FLock);
  try
    FList.Clear;
    ResetEvent(FSignalEvent);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TKRThreadQueueEvent.Count: Integer;
begin
  TMonitor.Enter(FLock);
  Result:=FList.Count;
  TMonitor.Exit(FLock);
end;

constructor TKRThreadQueueEvent.Create;
begin
  inherited Create;
  FLock:=TObject.Create;
  FList:=TList.Create;
  FSignalEvent:=CreateEvent(nil, true, false, nil);
end;

destructor TKRThreadQueueEvent.Destroy;
begin
  FList.Free;
  FLock.Free;
  CloseHandle(FSignalEvent);
  inherited;
end;

procedure TKRThreadQueueEvent.LockQueue;
begin
  TMonitor.Enter(FLock);
end;

function TKRThreadQueueEvent.Peek: Pointer;
begin
  TMonitor.Enter(FLock);
  Result:=FList[FList.Count-1];
  TMonitor.Exit(FLock);
end;

function TKRThreadQueueEvent.Pop: Pointer;
begin
  TMonitor.Enter(FLock);
  try
    Result:=FList[FList.Count-1];
    FList.Delete(FList.Count-1);
    if FList.Count>0 then
      ResetEvent(FSignalEvent);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TKRThreadQueueEvent.Push(AItem: Pointer);
begin
  TMonitor.Enter(FLock);
  try
    FList.Insert(0, AItem);
    SetEvent(FSignalEvent);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TKRThreadQueueEvent.UnlockQueue;
begin
  TMonitor.Exit(FLock);
end;

end.
