(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTCPServer                                                               *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTCPServer;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes,
  {$ELSE}
    Classes,
  {$IFEND}
  KRServer, KRTCPSocketServer;

type
  TKRTCPServer = class(TKRServer)
  private
    function GetPort: Word;
    procedure SetPort(const Value: Word);
    function GetAddr: AnsiString;
    procedure SetAddr(const Value: AnsiString);
  published
    constructor Create(AOwner: TComponent);override;
    property Port: Word read GetPort write SetPort;
    property Addr: AnsiString read GetAddr write SetAddr;
  end;

implementation


{ TKRTCPServer }

constructor TKRTCPServer.Create(AOwner: TComponent);
begin
  FServer:=TKRTCPSocketServer.Create;
  inherited;
  WaitRespTime:=15;
end;

function TKRTCPServer.GetAddr: AnsiString;
begin
  Result:=TKRTCPSocketServer(FServer).Addr;
end;

function TKRTCPServer.GetPort: Word;
begin
  Result:=TKRTCPSocketServer(FServer).Port;
end;

procedure TKRTCPServer.SetAddr(const Value: AnsiString);
begin
  TKRTCPSocketServer(FServer).Addr:=Value;
end;

procedure TKRTCPServer.SetPort(const Value: Word);
begin
  TKRTCPSocketServer(FServer).Port:=Value;
end;

end.
