(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBTSocketClient                                                          *)
(*  Ver.: 18.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBTSocketClient;

interface

uses
  {$IF CompilerVersion >= 23}
    {$IF CompilerVersion >= 28}Winapi.Bluetooth, {$IFEND}Winapi.Winsock2, System.SysUtils,
  {$ELSE}
    Winsock2, SysUtils,
  {$IFEND}
  KRSockets

{$IF CompilerVersion <= 27}
  , JwaWs2Bth
{$IFEND}
;

type
  TKRBTSocketClient = class(TKRSocketClient)
  private
    FAddr: int64;
    FConnectTimeout: integer;
  public
    constructor Create;override;
    property ConnectTimeout: integer read FConnectTimeout write FConnectTimeout;
    property Addr: int64 read FAddr write FAddr;
    procedure Open;override;
  end;

implementation

{ TKRBTSocketClient }

constructor TKRBTSocketClient.Create;
begin
  inherited;
  FAddr:=0;
  FAF:=AF_BTH;
  FPROTO:=BTHPROTO_RFCOMM;
  FConnectTimeout := 1000;
end;

procedure TKRBTSocketClient.Open;
var
//  addr: TSockAddr;
  addr: TSockAddrBth;
  AddrSize: cardinal;
  writeReady, exceptFlag: Boolean;
begin
  inherited Open;
  if Active and not FConnected then begin
    AddrSize := SizeOf(Addr);
    FillChar(Addr, AddrSize, 0);
    addr.addressFamily := AF_BTH;
    addr.btAddr := FAddr;
    addr.port := Cardinal(BT_PORT_ANY);
    addr.serviceClassId := StringToGUID('{00001101-0000-1000-8000-00805F9B34FB}');
    FConnected := ErrorCheck(connect(FSocket, PSockAddr(@addr)^, AddrSize)) = 0;
    if not FConnected then begin
      Select(FSocket, nil, @writeReady, @exceptFlag, FConnectTimeout);
      FConnected := writeReady and not exceptFlag;
    end;
  end;
end;

end.
