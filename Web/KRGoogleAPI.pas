(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleAPI                                                               *)
(*  Ver.: 17.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleAPI;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes,
  {$ELSE}
    Classes,
  {$IFEND}

  KRIdHTTP, KRGoogleCommon, KRGoogleAuth;

type
  TKRGoogleAPI = class( TKRGoogleCommon )
  private
    procedure SetGoogleAuth(const Value: TKRGoogleAuth);
  protected
    FAPIKey: String;
    FProjectID: String;
    FGoogleAuth: TKRGoogleAuth;
    FNeedScopes: TKRGoogleAuthScope;
    function getParams: String; override;
    function HTTPCreate: TKRIdHTTP; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property GoogleAuth: TKRGoogleAuth read FGoogleAuth write SetGoogleAuth;
    property ProjectID: String read FProjectID write FProjectID;
    property APIKey: String read FAPIKey write FAPIKey;
  end;

implementation

{ TKRGoogleAPI }

function TKRGoogleAPI.getParams: String;
begin
  if FAPIKey<>'' then FParams.Add( 'key', FAPIKey);
  Result := inherited getParams;
end;

function TKRGoogleAPI.HTTPCreate: TKRIdHTTP;
var
  token: String;
begin
  Result:=inherited HTTPCreate;
  if Assigned( FGoogleAuth )  then begin
    token := FGoogleAuth.Token;
    if token <> '' then
      Result.Request.CustomHeaders.AddValue('Authorization', 'Bearer ' + token);
  end;
end;

procedure TKRGoogleAPI.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FGoogleAuth)then FGoogleAuth:= nil;
end;

procedure TKRGoogleAPI.SetGoogleAuth(const Value: TKRGoogleAuth);
begin
  if FGoogleAuth<>Value then begin
    if Assigned(FGoogleAuth) then FGoogleAuth.RemoveFreeNotification(Self);
    FGoogleAuth := Value;
    if Assigned(FGoogleAuth) then begin
      FGoogleAuth.FreeNotification(Self);
      FGoogleAuth.Scope:=FGoogleAuth.Scope + FNeedScopes;
    end;
  end;
end;

end.
