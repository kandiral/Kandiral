(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTCPSocketServer                                                         *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTCPSocketServer;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Winsock2,
  {$ELSE}
    Winsock2,
  {$IFEND}
  KRSockets;

type
  TKRTCPSocketServer = class;

  TKRTCPSocketSrvClient = class(TKRSocketSrvClient)
  private
    FPort: Word;
    FAddr: AnsiString;
  public
    constructor CreateClient(AServer: TKRSocketServer; ASocket: Cardinal;
      AAddr: AnsiString; APort: Word);
    property Port: Word read FPort write FPort;
    property Addr: AnsiString read FAddr write FAddr;
  end;

  TKRTCPSocketServer = class(TKRSocketServer)
  private
    FPort: Word;
    FAddr: AnsiString;
  public
    constructor Create;override;
    procedure Open;override;
    property Port: Word read FPort write FPort;
    property Addr: AnsiString read FAddr write FAddr;
    function Accept: TKRSocketSrvClient;override;
  end;

implementation

{ TKRTCPSocketSrvClient }

constructor TKRTCPSocketSrvClient.CreateClient(AServer: TKRSocketServer;
  ASocket: Cardinal; AAddr: AnsiString; APort: Word);
begin
  FAddr:=AAddr;
  FPort:=APort;
  inherited CreateClient(AServer, ASocket);
end;

{ TKRTCPSocketServer }

function TKRTCPSocketServer.Accept: TKRSocketSrvClient;
var
  sock: TSocket;
  addr: TSockAddr;
  len: Integer;
begin
  Result := nil;
  len := sizeof(addr);
  Fillchar(addr, sizeof(addr), 0);
  try
    Sock := ErrorCheck( {$IF CompilerVersion >= 23}Winapi.{$IFEND}Winsock2.accept(
      FSocket,
      @addr,
      @len));
  except
    Sock := INVALID_SOCKET;
  end;
  if Sock <> INVALID_SOCKET then begin
    Result := TKRTCPSocketSrvClient.CreateClient(Self,Sock,
      inet_ntoa(TSockAddrIn(addr).sin_addr),ntohs(TSockAddrIn(addr).sin_port));
    FClients.Add(Result);
  end;
end;

constructor TKRTCPSocketServer.Create;
begin
  inherited;
  FAddr:='127.0.0.1';
  FPort := 0;
  FAF := AF_INET;
  FPROTO := IPPROTO_IP;
end;

procedure TKRTCPSocketServer.Open;
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
      if FSType=SOCK_DGRAM then FConnected:=true
      else FConnected:=ErrorCheck(listen(FSocket, SOMAXCONN)) = 0;
  end;
end;

end.
