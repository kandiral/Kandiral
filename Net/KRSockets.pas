(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRSockets                                                                 *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRSockets;

interface

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils, System.Classes, System.Types,
  {$ELSE}
    SysUtils, Classes, Types,
  {$IFEND}
  JwaWinsock2;

type
  EKRSocketError = class(Exception);
  TSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;

  TKRSocket = class
  private
    FActive: boolean;
    FOnError: TSocketErrorEvent;
    FID: Cardinal;
  protected
    FAF, FPROTO: integer;
    FSocket: cardinal;
    FSType: integer;
    function ErrorCheck(rc: Integer): Integer;
  public
    function Select(ASocket:cardinal; ReadReady, WriteReady, ExceptFlag: PBoolean;
      TimeOut: Integer): Boolean;
    constructor Create;virtual;
    destructor Destroy;override;
    procedure Open;virtual;
    procedure Close;virtual;
    property Active: boolean read FActive;
    property Handle: cardinal read FSocket;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property ID: Cardinal read FID;
    property AF: integer read FAF write FAF;
    property PROTO: integer read FPROTO write FPROTO;
    property SocketType: integer read FSType write FSType;
  end;

  TKRSocketClient = class(TKRSocket)
  protected
    FConnected: boolean;
  public
    constructor Create;override;
    procedure Open;override;
    procedure Close;override;
    property Connected: boolean read FConnected;
    function SendBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function SendBufTo(var AFrom: TSockAddr; var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function WaitForData(ATimeOut: Integer = 0): Boolean;
    function WaitForWrite(ATimeOut: Integer = 0): Boolean;
    function WaitForDataFrom(var AFrom: TSockAddr; ATimeOut: Integer = 0): Boolean;
    function ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function ReceiveBufFrom(var AFrom: TSockAddr; var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
  end;

  TKRSocketServer = class;

  TKRSocketSrvClient = class(TKRSocketClient)
  private
    FServer: TKRSocketServer;
    FConnected: boolean;
  public
    constructor CreateClient(AServer: TKRSocketServer; ASocket: Cardinal);
    destructor Destroy;override;
    property Server: TKRSocketServer read FServer;
    procedure CheckClose;
  end;

  TKRSocketServer = class(TKRSocket)
  protected
    FConnected: boolean;
    FClients: TThreadList;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Open;override;
    procedure Close;override;
    property Connected: boolean read FConnected;
    function Accept: TKRSocketSrvClient;virtual;
  end;

implementation

var
  WSAData: TWSAData;
  SocketID: Cardinal;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0202, WSAData);
  if ErrorCode <> 0 then
    raise EKRSocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise EKRSocketError.Create('WSACleanup');
end;

{ TKRSocket }

procedure TKRSocket.Close;
begin
  if FActive then begin
    ErrorCheck(closesocket(FSocket));
    FSocket := INVALID_SOCKET;
    FActive := False;
  end;
end;

constructor TKRSocket.Create;
begin
  FActive  := False;
  FSocket := INVALID_SOCKET;
  FOnError := nil;
  Inc(SocketID);
  FID:=SocketID;
  FSType:=SOCK_STREAM;
end;

destructor TKRSocket.Destroy;
begin
  Close;
  inherited;
end;

function TKRSocket.ErrorCheck(rc: Integer): Integer;
var
  SocketError: Integer;
begin
  Result := rc;
  if rc = SOCKET_ERROR then begin
    SocketError := WSAGetLastError;
    if Assigned(FOnError) then OnError(Self, SocketError);
  end;
end;

procedure TKRSocket.Open;
var
  NonBlock: Cardinal;
begin
  if not FActive then begin
    FSocket := ErrorCheck(socket(FAF, FSType, FPROTO));
    FActive := FSocket <> INVALID_SOCKET;
    if FActive then begin
      NonBlock := 1;
      ErrorCheck(ioctlsocket(FSocket, Integer(FIONBIO), NonBlock));
    end;
  end;
end;

function TKRSocket.Select(ASocket: cardinal; ReadReady, WriteReady,
  ExceptFlag: PBoolean; TimeOut: Integer): Boolean;

  procedure FD_SET(Socket: Cardinal; var FDSet: TFDSet);
  begin
    if FDSet.fd_count < FD_SETSIZE then begin
      FDSet.fd_array[FDSet.fd_count] := Socket;
      Inc(FDSet.fd_count);
    end;
  end;

var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if Assigned(ReadReady) then begin
    ReadFdsptr := @ReadFds;
    ReadFds.fd_count:=0;
    FD_SET(ASocket, ReadFds);
  end else ReadFdsptr := nil;
  if Assigned(WriteReady) then begin
    WriteFdsptr := @WriteFds;
    WriteFds.fd_count:=0;
    FD_SET(ASocket, WriteFds);
  end else WriteFdsptr := nil;
  if Assigned(ExceptFlag) then begin
    ExceptFdsptr := @ExceptFds;
    ExceptFds.fd_count:=0;
    FD_SET(ASocket, ExceptFds);
  end else ExceptFdsptr := nil;
  if TimeOut >= 0 then begin
    tv.tv_sec := TimeOut div 1000;
    tv.tv_usec :=  1000 * (TimeOut mod 1000);
    Timeptr := @tv;
  end else Timeptr := nil;
  try
    Result := JwaWinSock2.select(ASocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr) > 0;
  except
    Result := False;
  end;
  if Assigned(ReadReady) then ReadReady^ := FD_ISSET(ASocket, ReadFds);
  if Assigned(WriteReady) then WriteReady^ := FD_ISSET(ASocket, WriteFds);
  if Assigned(ExceptFlag) then ExceptFlag^ := FD_ISSET(ASocket, ExceptFds);
end;


{ TKRSocketClient }

procedure TKRSocketClient.Close;
begin
  if FConnected then begin
    ErrorCheck(shutdown(FSocket, SD_BOTH));
    FConnected := False;
  end;
  inherited;
end;

constructor TKRSocketClient.Create;
begin
  inherited;
  FConnected := false;
end;

procedure TKRSocketClient.Open;
begin
  inherited Open;
end;

function TKRSocketClient.ReceiveBuf(var Buf; BufSize, Flags: Integer): Integer;
begin
  Result := ErrorCheck(recv(FSocket, Buf, BufSize, Flags));
end;

function TKRSocketClient.ReceiveBufFrom(var AFrom: TSockAddr; var Buf; BufSize,
  Flags: Integer): Integer;
var
  i: integer;
begin
  i:=SizeOf(AFrom);
  Result := ErrorCheck(recvfrom(FSocket, Buf, BufSize, Flags, @AFrom, i));
end;

function TKRSocketClient.SendBuf(var Buf; BufSize, Flags: Integer): Integer;
begin
  Result := ErrorCheck(Send(FSocket, Buf, BufSize, Flags));
end;

function TKRSocketClient.SendBufTo(var AFrom: TSockAddr; var Buf; BufSize,
  Flags: Integer): Integer;
var
  i: integer;
begin
  i:=SizeOf(AFrom);
  Result := ErrorCheck(SendTo(FSocket, Buf, BufSize, Flags, @AFrom, i));
end;

function TKRSocketClient.WaitForData(ATimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  DataByte: Byte;
begin
  Result := False;
  if Select(FSocket, @ReadReady, nil, @ExceptFlag, ATimeOut) then
    Result := ReadReady and not ExceptFlag and
    (ErrorCheck(recv(FSocket, DataByte, 1, MSG_PEEK))= 1);
end;

function TKRSocketClient.WaitForDataFrom(var AFrom: TSockAddr;
  ATimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  DataByte: Byte;
  i: integer;
begin
  Result := False;
  i:=SizeOf(AFrom);
  if Select(FSocket, @ReadReady, nil, @ExceptFlag, ATimeOut) then
    Result := ReadReady and not ExceptFlag and
    (ErrorCheck(recvfrom(FSocket, DataByte, 1, MSG_PEEK, @AFrom, i))= 1);
end;

function TKRSocketClient.WaitForWrite(ATimeOut: Integer): Boolean;
var
  WriteReady, ExceptFlag: Boolean;
begin
  Result := False;
  if Select(FSocket,  nil, @WriteReady, @ExceptFlag, ATimeOut) then
    Result := WriteReady and not ExceptFlag;
end;

{ TKRSocketSrvClient }

procedure TKRSocketSrvClient.CheckClose;
var
  ReadReady: Boolean;
begin
  if not Select(FSocket, @ReadReady, nil, nil, 1000) then Close;
end;

constructor TKRSocketSrvClient.CreateClient(AServer: TKRSocketServer;
  ASocket: Cardinal);
begin
  inherited Create;
  FServer:=AServer;
  FSocket:=ASocket;
  FConnected:=true;
  FActive:=true;
end;

destructor TKRSocketSrvClient.Destroy;
begin
  FServer.FClients.Remove(Self);
  inherited;
end;

{ TKRSocketServer }

function TKRSocketServer.Accept: TKRSocketSrvClient;
begin
  Result:=nil;
end;

procedure TKRSocketServer.Close;
var
  list: TList;
  client: TKRSocketSrvClient;
begin
  list:=FClients.LockList;
  while list.Count>0 do begin
    client:=TKRSocketSrvClient(list[0]);
    list.Delete(0);
    client.Free;
  end;
  FClients.UnlockList;
  if FConnected then begin
    ErrorCheck(shutdown(FSocket, SD_BOTH));
    FConnected := False;
  end;
  inherited;
end;

constructor TKRSocketServer.Create;
begin
  inherited;
  FConnected := false;
  FClients:=TThreadList.Create;
end;

destructor TKRSocketServer.Destroy;
begin
  FClients.Destroy;
  inherited;
end;

procedure TKRSocketServer.Open;
begin
  inherited;
end;

initialization
  SocketID:=0;
  Startup;

finalization
  Cleanup;

end.
