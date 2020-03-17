(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBTSocketServer                                                          *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBTSocketServer;

interface

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils,
  {$ELSE}
    SysUtils,
  {$IFEND}
  KRSockets, JwaWinsock2, JwaWs2Bth;

type
  TKRBTSocketServer = class;

  TKRBTSocketSrvClient = class(TKRSocketSrvClient)
  private
    FPort: Cardinal;
    FAddr: int64;
  public
    constructor CreateClient(AServer: TKRSocketServer; ASocket: Cardinal;
      AAddr: int64; APort: Cardinal);
    property Port: Cardinal read FPort write FPort;
    property Addr: int64 read FAddr write FAddr;
  end;

  TKRBTSocketServer = class(TKRSocketServer)
  private
    FPort: Cardinal;
  public
    constructor Create;override;
    procedure Open;override;
    property Port: Cardinal read FPort write FPort;
    function Accept: TKRSocketSrvClient;override;
  end;


implementation

{ TKRBTSocketSrvClient }

constructor TKRBTSocketSrvClient.CreateClient(AServer: TKRSocketServer;
  ASocket: Cardinal; AAddr: int64; APort: Cardinal);
begin
  FAddr:=AAddr;
  FPort:=APort;
  inherited CreateClient(AServer, ASocket);
end;

{ TKRBTSocketServer }

function TKRBTSocketServer.Accept: TKRSocketSrvClient;
var
  sock: TSocket;
  addr: SOCKADDR_BTH;
  len: Integer;
begin
  Result := nil;
  len := sizeof(addr);
  Fillchar(addr, sizeof(addr), 0);
  try
    Sock := ErrorCheck(JwaWinSock2.accept(FSocket, @addr, @len));
  except
    Sock := INVALID_SOCKET;
  end;
  if Sock <> INVALID_SOCKET then begin
    Result := TKRBTSocketSrvClient.CreateClient(Self,Sock,addr.btAddr,addr.port);
    FClients.Add(Result);
  end;
end;

constructor TKRBTSocketServer.Create;
begin
  inherited;
  FPort := Cardinal(BT_PORT_ANY);;
  FAF := AF_BTH;
  FPROTO := BTHPROTO_RFCOMM;
end;

procedure TKRBTSocketServer.Open;
var
  addr: SOCKADDR_BTH;
  AddrSize: Cardinal;
begin
  inherited;
  if Active and not FConnected then begin
    AddrSize := SizeOf(Addr);
    FillChar(Addr, AddrSize, 0);
    addr.addressFamily := AF_BTH;
    addr.btAddr := 0;
    addr.port := FPort;
    addr.serviceClassId := StringToGUID('{00001101-0000-1000-8000-00805F9B34FB}');
    if ErrorCheck(JwaWinSock2.bind(FSocket, @addr, sizeof(addr))) = 0 then
      FConnected:=ErrorCheck(JwaWinSock2.listen(FSocket, SOMAXCONN)) = 0;
  end;

end;

end.
