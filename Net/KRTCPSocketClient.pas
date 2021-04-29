(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTCPSocketClient                                                         *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTCPSocketClient;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Winsock2,
  {$ELSE}
    Winsock2,
  {$IFEND}
  KRSockets;

type
  TKRTCPSocketClient = class(TKRSocketClient)
  private
    FPort{, FLPort}: Word;
    FAddr: AnsiString;
    FLocalPort: word;
    FLocalAddr: AnsiString;
    FConnectTimeout: integer;
  public
    constructor Create;override;
    property ConnectTimeout: integer read FConnectTimeout write FConnectTimeout;
    property Addr: AnsiString read FAddr write FAddr;
    property Port: Word read FPort write FPort;
    procedure Open;override;
    property LocalAddr: AnsiString read FLocalAddr write FLocalAddr;
    property LocalPort: word read FLocalPort write FLocalPort;
    procedure GetLocalData;
    procedure Activate;
  end;

implementation

{ TKRTCPSocketClient }

procedure TKRTCPSocketClient.Activate;
begin
  inherited Open;
end;

constructor TKRTCPSocketClient.Create;
begin
  inherited;
  FAddr := '0.0.0.0';
  FPort := 0;
  FAF := AF_INET;
  FPROTO := IPPROTO_IP;
  FConnectTimeout := 1000;
end;

procedure TKRTCPSocketClient.GetLocalData;
var
  addr: TSockAddr;
  AddrSize: integer;
begin
  FLocalAddr := '0.0.0.0';
  FLocalPort:=0;
  if Active and FConnected then begin
    AddrSize := SizeOf(Addr);
    if ErrorCheck(getsockname(FSocket, addr, AddrSize))=0 then
      if(TSockAddrIn(Addr).sin_family=AF_INET)and(AddrSize=SizeOf(Addr)) then begin
        FLocalAddr:=inet_ntoa(TSockAddrIn(addr).sin_addr);
        FLocalPort:=ntohs(TSockAddrIn(addr).sin_port);
      end;
  end;
end;

procedure TKRTCPSocketClient.Open;
var
  addr: TSockAddr;
  AddrSize: Cardinal;
  writeReady, exceptFlag: Boolean;
begin
  inherited;
  if Active and not FConnected then begin
    AddrSize := SizeOf(Addr);
    FillChar(Addr, AddrSize, 0);
    TSockAddrIn(addr).sin_family := AF_INET;
    TSockAddrIn(addr).sin_addr.s_addr := inet_addr(PAnsiChar(FAddr));
    TSockAddrIn(addr).sin_port := htons(FPort);

    FConnected := ErrorCheck(connect(FSocket, addr, AddrSize)) = 0;
    if not FConnected then begin
      Select(FSocket, nil, @writeReady, @exceptFlag, FConnectTimeout);
      FConnected := writeReady and not exceptFlag;
    end;
  end;
end;

end.
