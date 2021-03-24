(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRThreadQueue                                                             *)
(*  Ver.: 31.08.2017                                                          *)
(*  https://kandiral.ru/delphi/krthreadqueue.pas.html                         *)
(*                                                                            *)
(******************************************************************************)
unit KRThreadQueue;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes;
  {$ELSE}
    Classes;
  {$IFEND}

type
  TKRThreadQueue = class(TObject)
  private
    FLock: TObject;
    FList: TList;
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
  end;

implementation

{ TKRThreadQueue }

function TKRThreadQueue.AtLeast(ACount: Integer): Boolean;
begin
  TMonitor.Enter(FLock);
  Result:=FList.Count>=ACount;
  TMonitor.Exit(FLock);
end;

procedure TKRThreadQueue.Clear;
begin
  TMonitor.Enter(FLock);
  FList.Clear;
  TMonitor.Exit(FLock);
end;

function TKRThreadQueue.Count: Integer;
begin
  TMonitor.Enter(FLock);
  Result:=FList.Count;
  TMonitor.Exit(FLock);
end;

constructor TKRThreadQueue.Create;
begin
  inherited Create;
  FLock:=TObject.Create;
  FList:=TList.Create;
end;

destructor TKRThreadQueue.Destroy;
begin
  FList.Free;
  FLock.Free;
  inherited;
end;

procedure TKRThreadQueue.LockQueue;
begin
  TMonitor.Enter(FLock);
end;

function TKRThreadQueue.Peek: Pointer;
begin
  TMonitor.Enter(FLock);
  Result := FList[FList.Count-1];
  TMonitor.Exit(FLock);
end;

function TKRThreadQueue.Pop: Pointer;
begin
  TMonitor.Enter(FLock);
  Result := FList[FList.Count-1];
  FList.Delete(FList.Count-1);
  TMonitor.Exit(FLock);
end;

procedure TKRThreadQueue.Push(AItem: Pointer);
begin
  TMonitor.Enter(FLock);
  FList.Insert(0, AItem);
  TMonitor.Exit(FLock);
end;

procedure TKRThreadQueue.UnlockQueue;
begin
  TMonitor.Exit(FLock);
end;

end.
