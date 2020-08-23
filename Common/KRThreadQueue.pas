(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRThreadQueue                                                             *)
(*  Ver.: 14.07.2020                                                          *)
(*  https://kandiral.ru/delphi/krthreadqueue.pas.html                         *)
(*                                                                            *)
(******************************************************************************)
unit KRThreadQueue;

interface

uses Contnrs;

type
  TKRThreadQueue = class(TOrderedList)
  private
    FLock: TObject;
  protected
    procedure PushItem(AItem: Pointer); override;
    function PopItem: Pointer; override;
    function PeekItem: Pointer; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LockQueue;
    procedure UnlockQueue;
    procedure Clear;
  end;

implementation

{ TKRThreadQueue }

procedure TKRThreadQueue.Clear;
begin
  LockQueue;
  List.Clear;
  UnlockQueue;
end;

constructor TKRThreadQueue.Create;
begin
  inherited;
  FLock:=TObject.Create;
end;

destructor TKRThreadQueue.Destroy;
begin
  LockQueue;
  inherited Destroy;
  UnlockQueue;
  FLock.Free;
end;

procedure TKRThreadQueue.LockQueue;
begin
  TMonitor.Enter(FLock);
end;

function TKRThreadQueue.PeekItem: Pointer;
begin
  LockQueue;
  Result:=inherited PeekItem;
  UnlockQueue;
end;

function TKRThreadQueue.PopItem: Pointer;
begin
  LockQueue;
  Result:=inherited PopItem;
  UnlockQueue;
end;

procedure TKRThreadQueue.PushItem(AItem: Pointer);
begin
  LockQueue;
  List.Insert(0, AItem);
  UnlockQueue;
end;

procedure TKRThreadQueue.UnlockQueue;
begin
  TMonitor.Exit(FLock);
end;

end.
