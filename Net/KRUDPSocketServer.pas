(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRUDPSocketServer                                                         *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRUDPSocketServer;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Winsock2,
  {$ELSE}
    Winsock2,
  {$IFEND}
  KRSockets;

type
  TKRUDPSocketServer = class(TKRSocketClient)
  private
    FPort: Word;
    FAddr: AnsiString;
  public
    constructor Create;override;
    procedure Open;override;
    property Port: Word read FPort write FPort;
    property Addr: AnsiString read FAddr write FAddr;
  end;

implementation

{ TKRUDPSocketServer }

constructor TKRUDPSocketServer.Create;
begin
  inherited;
  FAddr:='127.0.0.1';
  FPort := 0;
  FAF := AF_INET;
  FPROTO := IPPROTO_UDP;
  FSType := SOCK_DGRAM;
end;

procedure TKRUDPSocketServer.Open;
var
  addr: TSockAddr;
  AddrSize: Cardinal;
begin
  inherited;
  if Active and not FConnected then begin
    AddrSize := SizeOf(Addr);
    FillChar(Addr, AddrSize, 0);
    TSockAddrIn(addr).sin_family := AF_INET;
    if FAddr='' then
      TSockAddrIn(addr).sin_addr.s_addr := htonl(INADDR_ANY)
    else
      TSockAddrIn(addr).sin_addr.s_addr := inet_addr(PAnsiChar(FAddr));
    TSockAddrIn(addr).sin_port := htons(FPort);

    if ErrorCheck(bind(FSocket, addr, sizeof(addr))) = 0 then
      FConnected:=true;
  end;
end;

end.
