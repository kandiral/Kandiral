(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTPClient                                                                *)
(*  Ver.: 26.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTPClient;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Forms,
  {$ELSE}
    Windows, Messages, Classes, Forms,
  {$IFEND}
  KRConnector, KRThread, KRTypes, KRCRC, lgop;

type
  TKRTPClientPack = record
    Pack: TKRBuffer;
    PackLen: byte;
    RecvLen: byte;
    ID: byte;
    Func: byte;
    CallBack: TKRConnectorCallBack;
  end;
  PKRTPClientPack = ^TKRTPClientPack;

  TKRTPClientThread = class;

  TKRTPClient = class(TComponent)
  private
    FConnector: TKRConnector;
    FThread: TKRTPClientThread;
    FEchoInterval: integer;
    FTPName: String;
    FConnected: boolean;
    FStack: TThreadList;
    FEchoTm: HWND;
    FID: byte;
    procedure SetConnector(const Value: TKRConnector);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetEchoInterval(const Value: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Active: boolean read GetActive write SetActive;
    property Connected: boolean read FConnected;
    function CheckPack(ABuffer: PKRBuffer; ALength: byte; APack: PKRTPClientPack): integer;
    procedure SendPack(APack: PKRTPClientPack);
    function GetTPName: String;
  published
    property Connector: TKRConnector read FConnector write SetConnector;
    property EchoInterval: integer read FEchoInterval write SetEchoInterval default 1000;
    property TPName: String read FTPName;
  end;

  TKRTPClientThread = class(TKRThread)
  private
    FClient: TKRTPClient;
    FEchoInProgress: boolean;
    FGetNameInProgress: boolean;
    procedure Echo;
    procedure EchoCB(AError: integer; APack: PKRBuffer; ALength: integer; AData: Pointer);
    procedure EchoTmExec(var Msg: TMessage);
    procedure GetName;
    procedure GetNameCB(AError: integer; APack: PKRBuffer; ALength: integer; AData: Pointer);
  protected
    procedure KRExecute; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(AClient: TKRTPClient);
  end;

implementation

{ TKRTPClient }

function TKRTPClient.CheckPack(ABuffer: PKRBuffer; ALength: byte;
  APack: PKRTPClientPack): integer;
begin
  Result:=0;
  if(ALength<6)then begin
    Result:=1;
    exit;
  end;
  if(ABuffer^[0]<>126)or(ABuffer^[ALength-3]<>35)then begin
    Result:=2;
    exit;
  end;
  if KRCRC16(ABuffer,ALength-2)<>BytesToWord(ABuffer^[ALength-2],ABuffer^[ALength-1]) then begin
    Result:=3;
    exit;
  end;
  if APack.ID<>ABuffer^[1] then begin
    Result:=4;
    exit;
  end;
  if APack.Func<>ABuffer^[2] then begin
    Result:=5;
    exit;
  end;
end;

constructor TKRTPClient.Create(AOwner: TComponent);
begin
  inherited;
  FID:=0;
  FTPName:='';
  FConnected:=false;
  FStack:=TThreadList.Create;
  FThread:=TKRTPClientThread.CreateTh(Self);
  FEchoTm:=AllocateHWnd(FThread.EchoTmExec);
  SetEchoInterval(1000);
end;

destructor TKRTPClient.Destroy;
begin
  FThread.Free;
  KillTimer(FEchoTm, 1);
  DeallocateHWnd(FEchoTm);
  FStack.Free;
  inherited;
end;

function TKRTPClient.GetActive: boolean;
begin
  Result:=FThread.Active;
end;

function TKRTPClient.GetTPName: String;
begin
  FThread.GetName;
  while FThread.FGetNameInProgress do Application.ProcessMessages;
  Result:=FTPName;
end;

procedure TKRTPClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FConnector)then begin
      SetActive(false);
      FConnector:= nil;
    end;
end;

procedure TKRTPClient.SendPack(APack: PKRTPClientPack);
begin
  APack^.Pack[1]:=FID;
  APack^.ID:=FID;
  inc(FID);
  KRCRC16(@(APack^.Pack),4,APack^.Pack[5],APack^.Pack[4]);
  FStack.LockList.Add(APack);
  FStack.UnlockList;
end;

procedure TKRTPClient.SetActive(const Value: boolean);
var b: boolean;
begin
  b:=FThread.Active;
  FThread.Active:=Value and Assigned(FConnector);
  if(not b)and FThread.Active then FThread.Echo;
end;

procedure TKRTPClient.SetConnector(const Value: TKRConnector);
begin
  if FConnector<>Value then begin
    SetActive(false);
    if Assigned(FConnector) then FConnector.RemoveFreeNotification(Self);
    FConnector := Value;
    if Assigned(FConnector) then begin
      SetActive(false);
      FConnector.FreeNotification(Self);
    end;
  end;
end;

procedure TKRTPClient.SetEchoInterval(const Value: integer);
begin
  if(FEchoInterval=0)and Active then FThread.Echo;
  FEchoInterval := Value;
end;

{ TKRTPClientThread }

constructor TKRTPClientThread.CreateTh(AClient: TKRTPClient);
begin
  FEchoInProgress:=false;
  FGetNameInProgress:=false;
  FClient:=AClient;
  inherited Create;
end;

procedure TKRTPClientThread.Echo;
var
  pk: PKRTPClientPack;
begin
  KillTimer(FClient.FEchoTm, 1);
  if (not Active)or FEchoInProgress then exit;
  FEchoInProgress:=true;
  New(pk);
  pk^.PackLen:=6;
  pk^.RecvLen:=6;
  pk^.Func:=0;
  pk^.Pack[0]:=126;
  pk^.Pack[2]:=pk^.Func;
  pk^.Pack[3]:=64;
  pk^.CallBack:=EchoCB;
  FClient.SendPack(pk);
end;

procedure TKRTPClientThread.EchoCB(AError: integer; APack: PKRBuffer;
  ALength: integer; AData: Pointer);
var
  pk: PKRTPClientPack;
begin
  pk:=AData;
  if AError=0 then begin
    FClient.FConnected:=FClient.CheckPack(APack,ALength,pk)=0;
  end;
  Dispose(pk);
  if Active and (FClient.FEchoInterval>0) then
    SetTimer(FClient.FEchoTm, 1, FClient.FEchoInterval, nil);
  FEchoInProgress:=false;
end;

procedure TKRTPClientThread.EchoTmExec(var Msg: TMessage);
begin
  if(Msg.Msg=WM_TIMER)and(Msg.WParam=1)then Echo;
end;

procedure TKRTPClientThread.GetName;
var
  pk: PKRTPClientPack;
begin
  if (not Active)or FGetNameInProgress then exit;
  FGetNameInProgress:=true;
  New(pk);
  pk^.PackLen:=6;
  pk^.RecvLen:=0;
  pk^.Func:=1;
  pk^.Pack[0]:=126;
  pk^.Pack[2]:=pk^.Func;
  pk^.Pack[3]:=64;
  pk^.CallBack:=GetNameCB;
  FClient.SendPack(pk);
end;

procedure TKRTPClientThread.GetNameCB(AError: integer; APack: PKRBuffer;
  ALength: integer; AData: Pointer);
var
  pk: PKRTPClientPack;
  s: String;
  i: integer;
begin
  pk:=AData;
  if AError=0 then begin
    if FClient.CheckPack(APack,ALength,pk)=0 then begin
      s:='';
      for i := 3 to ALength-4 do s:=s+Chr(APack^[i]);
      FClient.FTPName:=s;
    end;
  end;
  Dispose(pk);
  FGetNameInProgress:=false;
end;

procedure TKRTPClientThread.KRExecute;
var
  List: TList;
  pk: PKRTPClientPack;
begin
  List:=FClient.FStack.LockList;
  if List.Count>0 then begin
    pk:=List.First;
    List.Delete(0);
    FClient.FConnector.Send(@(pk^.Pack),pk^.PackLen,pk^.CallBack,pk,true,pk^.RecvLen);
  end;
  FClient.FStack.UnlockList;
end;

procedure TKRTPClientThread.KRExecutePausedFirst;
var
  List: TList;
  pk: PKRTPClientPack;
begin
  List:=FClient.FStack.LockList;
  while List.Count>0 do begin
    pk:=List.First;
    List.Delete(0);
    if Assigned(pk^.CallBack) then pk^.CallBack(-1,nil,0,pk);
  end;
  FClient.FStack.UnlockList;
end;

end.
