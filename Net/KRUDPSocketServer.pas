(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRUDPSocketServer                                                         *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRUDPSocketServer;

interface

uses KRSockets, JwaWinsock2;

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
  addr: sockaddr_in;
  AddrSize: Cardinal;
begin
  inherited;
  if Active and not FConnected then begin
    AddrSize := SizeOf(Addr);
    FillChar(Addr, AddrSize, 0);
    addr.sin_family := AF_INET;
    if FAddr='' then
      addr.sin_addr.s_addr := htonl(INADDR_ANY)
    else
      addr.sin_addr.s_addr := inet_addr(PAnsiChar(FAddr));
    addr.sin_port := htons(FPort);

    if ErrorCheck(JwaWinSock2.bind(FSocket, @addr, sizeof(addr))) = 0 then
      FConnected:=true;
  end;
end;

end.
