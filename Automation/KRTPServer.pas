(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTPServer                                                                *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTPServer;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes,
  {$ELSE}
    Windows, Classes,
  {$IFEND}
  KRServer, KRTypes, KRCRC, KRWindows;

type
  TKRTPServerData = procedure (Sender: TObject; ABuffer: PKRBuffer; var ALength: integer) of object;

  TKRTPServer = class(TComponent)
  private
    FServer: TKRServer;
    FTPName: String;
    FData: TKRTPServerData;
    procedure SetServer(const Value: TKRServer);
    procedure cb(Sender: TObject; APack: PKRBuffer; var ALength: integer);
    function BuildResp(Sender: TObject; APack: PKRBuffer; ALength: integer): integer;
    function funcGetName(APack: PKRBuffer): integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property TPName: String read FTPName write FTPName;
  published
    property Server: TKRServer read FServer write SetServer;
    property OnData: TKRTPServerData read FData write FData;
  end;

implementation

{ TKRTPServer }

function TKRTPServer.BuildResp(Sender: TObject; APack: PKRBuffer; ALength: integer): integer;
begin
  Result:=0;
  case APack^[2] of
    0: result:=6;   // Echo
    1: result:=funcGetName(APack)
    else if Assigned(FData) then begin
      Result:=ALength;
      FData(Sender, APack, Result);
    end;
  end;
  APack^[Result-3]:=35;
  KRCRC16(APack,Result-2,APack^[Result-2],APack^[Result-1]);
end;

procedure TKRTPServer.cb(Sender: TObject; APack: PKRBuffer; var ALength: integer);
begin
  if(ALength>5)then begin
    if(APack^[0]=126)and(APack^[ALength-3]=64)then begin
      if KRCRC16(APack,ALength-2)=MakeWord(APack^[ALength-1],APack^[ALength-2]) then begin
        ALength:=buildResp(Sender,APack,ALength);
        exit;
      end;
    end;
  end;
  ALength:=0;
end;

constructor TKRTPServer.Create(AOwner: TComponent);
begin
  inherited;
  FTPName:=KRWindows.KRGetComputerName;
end;

destructor TKRTPServer.Destroy;
begin
  if Assigned(FServer) then begin
    FServer.OnEvent:=nil;
    FServer.RemoveFreeNotification(Self);
  end;
  inherited;
end;

function TKRTPServer.funcGetName(APack: PKRBuffer): integer;
var i,n: integer;
s: AnsiString;
begin
  s:=AnsiString(FTPName);
  n:=Length(s);
  if n>127 then n:=127;
  for i := 1 to n do APack^[2+i]:=ord(s[i]);
  Result:=6+n;
end;

procedure TKRTPServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FServer)then FServer:= nil;
end;

procedure TKRTPServer.SetServer(const Value: TKRServer);
begin
  if FServer<>Value then begin
    if Assigned(FServer) then begin
      FServer.OnEvent:=nil;
      FServer.RemoveFreeNotification(Self);
    end;
    FServer := Value;
    if Assigned(FServer) then begin
      FServer.FreeNotification(Self);
      FServer.OnEvent:=cb;
    end;
  end;
end;

end.
