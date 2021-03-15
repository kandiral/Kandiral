(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBTServer                                                                *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBTServer;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes,
  {$ELSE}
    Classes,
  {$IFEND}
  KRServer, KRBTSocketServer;

type
  TKRBTServer = class(TKRServer)
  private
    function GetPort: Cardinal;
    procedure SetPort(const Value: Cardinal);
  published
    constructor Create(AOwner: TComponent);override;
    property Port: Cardinal read GetPort write SetPort;
  end;

implementation


{ TKRBTServer }

constructor TKRBTServer.Create(AOwner: TComponent);
begin
  FServer:=TKRBTSocketServer.Create;
  inherited;
  WaitRespTime:=35;
end;

function TKRBTServer.GetPort: Cardinal;
begin
  Result:=TKRBTSocketServer(FServer).Port;
end;

procedure TKRBTServer.SetPort(const Value: Cardinal);
begin
  TKRBTSocketServer(FServer).Port:=Value;
end;

end.
