(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRIdHTTP                                                                  *)
(*  Ver.: 03.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRIdHTTP;

interface

uses IdHTTP, IdSSL, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdCTypes, IdIOHandler;

type
  TKRIdHTTP = class( TIdHTTP )
  private
    FHandleTLSHostName: boolean;
    procedure SetHandleTLSHostName(const Value: boolean);
    procedure OpenSSLStatusInfo(ASender : TObject; const AsslSocket: PSSL;
      const AWhere, Aret: TIdC_INT; const AType, AMsg : String );
  protected
    procedure SetIOHandler(AValue: TIdIOHandler); override;
  published
    property HandleTLSHostName: boolean read FHandleTLSHostName write SetHandleTLSHostName default false;
  end;

implementation

{ TKRIdHTTP }

procedure TKRIdHTTP.OpenSSLStatusInfo(ASender: TObject; const AsslSocket: PSSL;
  const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, Request.Host);
end;

procedure TKRIdHTTP.SetHandleTLSHostName(const Value: boolean);
begin
  FHandleTLSHostName := Value;
  if Assigned( IOHandler ) then
    TIdSSLIOHandlerSocketOpenSSL( IOHandler ).OnStatusInfoEx := OpenSSLStatusInfo;

end;

procedure TKRIdHTTP.SetIOHandler(AValue: TIdIOHandler);
begin
  inherited;
  if Assigned( IOHandler ) and FHandleTLSHostName then
    TIdSSLIOHandlerSocketOpenSSL( IOHandler ).OnStatusInfoEx := OpenSSLStatusInfo;
end;

end.
