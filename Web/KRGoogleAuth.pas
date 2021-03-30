(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleAuth                                                              *)
(*  Ver.: 18.03.2021                                                          *)
(*  https://kandiral.ru/delphi/krgoogleauth.pas.html                          *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleAuth;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.ShellAPI, System.Classes, System.SysUtils, System.IniFiles,
    System.StrUtils, Vcl.Graphics, Vcl.Imaging.pngimage,
  {$ELSE}
    Windows, ShellAPI, Classes, SysUtils, IniFiles, StrUtils, Graphics, pngimage,
  {$IFEND}

  KRIdHTTP, KRSockets, KRTypes, KRStrUtils, KRTCPServer, KRThread, KRWebCommon,
  KRGoogleCommon, KRWebDownload, KRJSON,

  IdHTTP;

type
  TKRGoogleAuthScopes = (gscEMail, gscContactsRead, gscContacts, gscCalendarRead,
    gscCalendar, gscSheets, gscSheetsRead, gscWebmaster, gscWebmasterRead, gscCloudPlatform,
    gscCloudTranslation );
  TKRGoogleAuthScope = set of TKRGoogleAuthScopes;
  TKRGoogleAuth = class;

  TKRGoogleAuthThread = class(TThread)
  private
    FAuth: TKRGoogleAuth;
    FToken, FRefreshToken: String;
    FExpiresIn: TDateTime;
    procedure GetTokenInfo;
    procedure RefreshToken;
  protected
    procedure Execute; override;
  public
    constructor CreateTh(AAuth: TKRGoogleAuth);
  end;

  TKRGoogleAuth = class( TKRGoogleCommon )
  private
    FThread: TKRGoogleAuthThread;
    FAppID: String;
    FSecretKey: String;
    FScope: TKRGoogleAuthScope;
    FServer: TKRTCPServer;
    FLoginTimeout: Cardinal;
    FPort: word;
    FDataFolder: TFileName;
    FState: Cardinal;
    FCode: String;
    FOnError: TNotifyEvent;
    FExpiresIn: TDateTime;
    FWaitWebBrowser: boolean;

    FUserID: string;
    FUserIcon: TBitmap;
    FUserName: String;
    FUserEmail: String;
    FUserFamilyName: String;
    FUserGivenName: String;
    FToken: string;
    FRefreshToken: String;


    FOnLogin: TNotifyEvent;
    FOnLogout: TNotifyEvent;
    FCancelLogin: boolean;
    FResultPage: TStringList;
    FResultPageStr: String;

    procedure srvEvent(Sender: TObject; APack: PKRBuffer2k; var ALength: integer);
    function GetExpiresIn: TDateTime;
    procedure SetDataFolder(const Value: TFileName);

    function thGetProfile(AThread: TThread; AData: Pointer): Pointer;
    function thRefreshToken(AThread: TThread; AData: Pointer): Pointer;
    function thLogout(AThread: TThread; AData: Pointer): Pointer;
    function thWaitWebBrowser(AThread: TThread; AData: Pointer): Pointer;
    function thAuth(AThread: TThread; AData: Pointer): Pointer;
    function thLoadIcon(AThread: TThread; AData: Pointer): Pointer;
    procedure SetResultPage(const Value: TStrings);
    function GetResultPage: TStrings;
    function GetUserIcon( AURL: String ): TBitmap;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    procedure Clear;
    function Login: boolean;
    procedure CancelLogin;
    function Refresh: boolean;
    function GetProfile: boolean;
    procedure Logout;
    function RefreshToken: boolean;
    function GetRefreshToken: String;
    procedure SetRefreshToken(ARefreshToken: String);
    property Token: string read FToken;
    property UserID: string read FUserID;
    property UserName: String read FUserName;
    property UserGivenName: String read FUserGivenName;
    property UserFamilyName: String read FUserFamilyName;
    property UserEmail: String read FUserEmail;
    property UserIcon: TBitmap read FUserIcon;
    property ExpiresIn: TDateTime read GetExpiresIn;
  published
    property AppID: String read FAppID write FAppID;
    property SecretKey: String read FSecretKey write FSecretKey;
    property Scope: TKRGoogleAuthScope read FScope write FScope;
    property Port: word read FPort write FPort;
    property ResultPage: TStrings read GetResultPage write SetResultPage;
    property LoginTimeout: Cardinal read FLoginTimeout write FLoginTimeout default 60;
    property DataFolder: TFileName read FDataFolder write SetDataFolder;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
    property OnLogout: TNotifyEvent read FOnLogout write FOnLogout;
  end;

implementation

{ TKRGoogleAuth }

procedure TKRGoogleAuth.CancelLogin;
begin
  FCancelLogin:=true;
end;

procedure TKRGoogleAuth.Clear;
var
  ini: TIniFile;
begin
  FRefreshToken:='';
  FToken:='';
  FUserID:='';
  FUserName:='';
  FUserEmail:='';
  FUserFamilyName:='';
  FUserGivenName:='';
  if Assigned(FUserIcon) then FreeAndNil(FUserIcon);
  ini:=TIniFile.Create(FDataFolder+'google.ini');
  ini.WriteString('OAuth',Name+'_RefreshToken','');
  ini.Free;
end;

constructor TKRGoogleAuth.Create(AOwner: TComponent);
begin
  inherited;

  FLoginTimeout:=60;
  FThread:=TKRGoogleAuthThread.CreateTh(Self);
  FResultPage:=TStringList.Create;
  FResultPage.Add( '<!DOCTYPE html>' );
  FResultPage.Add( '<html>' );
  FResultPage.Add( '<head>' );
  FResultPage.Add( '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' );
  FResultPage.Add( '  <title>KROAuth</title>' );
  FResultPage.Add( '</head>' );
  FResultPage.Add( '<body>' );
  FResultPage.Add( '  <h1>You can close this page.</h1>' );
  FResultPage.Add( '</body>' );
  FResultPage.Add( '</html>' );
  FScope := [gscEMail];
end;

destructor TKRGoogleAuth.Destroy;
begin
  if assigned(FServer) then FServer.Free;
  FThread.Free;
  if Assigned(FUserIcon) then FUserIcon.Free;
  FResultPage.Free;
  inherited;
end;

function TKRGoogleAuth.GetExpiresIn: TDateTime;
begin
  Result:=FExpiresIn;
end;

function TKRGoogleAuth.GetProfile: boolean;
var
  js: TKRJSONObject;
  i,n: integer;
  fname: String;
  data: TKRGoogleRESTRequestData;
begin
  Result := false;
  FParams.Clear;
  FParams.Add( 'access_token', FToken );
  data.url := 'https://www.googleapis.com/oauth2/v1/userinfo' + getParams;

  js := TKRJSONObject( KRRunInThread( @data, thGetProfile ) );
  if data.isOk then try
    if js.StringOf('id',i) then FUserID:=js.Strings[ i ].Value else FUserID:='';
    if js.StringOf('name',i) then FUserName:=js.Strings[ i ].Value else FUserName:='';
    if js.StringOf('given_name',i) then FUserGivenName:=js.Strings[ i ].Value else FUserGivenName:='';
    if js.StringOf('family_name',i) then FUserFamilyName:=js.Strings[ i ].Value else FUserFamilyName:='';
    if js.StringOf('email',i) then FUserEmail:=js.Strings[ i ].Value else FUserEmail:='';

    if FUserName='' then begin
      if FUserGivenName<>'' then FUserName:=FUserGivenName;
      if FUserFamilyName<>'' then fname:=FUserFamilyName;
      if FUserName='' then FUserName:=fname
      else if fname<>'' then FUserName:=FUserName+' '+fname;
    end;

    if Assigned(FUserIcon) then FreeAndNil(FUserIcon);
    if js.StringOf('picture',i) then FUserIcon:=GetUserIcon( js.Strings[ i ].Value );

    Result:=true;
  finally
    js.Free;
  end;
end;

function TKRGoogleAuth.GetRefreshToken: String;
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(FDataFolder+'google.ini');
  Result:=ini.ReadString('OAuth',Name+'_RefreshToken','');
  ini.Free;
  FRefreshToken:=Result;
end;

function TKRGoogleAuth.GetResultPage: TStrings;
begin
  Result := FResultPage;
end;

function TKRGoogleAuth.GetUserIcon( AURL: String ): TBitmap;
var
  ini: TIniFile;
  i, n: integer;
  s : String;
  png: TPngImage;

  function mkPath(APath: String): String;
  begin
    if not DirectoryExists(APath) then mkDir(APath);
    Result:=APath+'\';
  end;

begin
  Result := nil;
  ini := TIniFile.Create(FDataFolder+'google.ini');
  try

    n := ini.ReadInteger('UserIcons','Count',0) - 1;
    for I := 0 to n do
      if ini.ReadString('UserIcons','URL'+IntToStr(i),'') = AURL then begin
        png:=TPngImage.Create;
        Result:=TBitmap.Create;
        try
          png.LoadFromFile( ini.ReadString('UserIcons','PATH'+IntToStr(i),'') );
          Result.Assign(png);
        except
          FreeAndNil( Result );
        end;
        png.Free;
        exit;
      end;

    s:=AURL;
    Result := TBitmap( KRRunInThread( Pointer(PChar(s)), thLoadIcon) );
    if Result<>nil then begin
      inc(n);
      randomize;
      s:=FDataFolder;

      i:=100000000 + Random(899999999);
      while fileExists( s + IntToStr( i div 10000000 ) + '\' + IntToStr( i div 10000 ) + '\' + IntToStr( i ) + '.png' ) do  i:=Random(99999999);
      s := mkPath( mkPath( s + IntToStr( i div 10000000 ) ) + IntToStr( i div 10000 ) ) + IntToStr( i ) + '.png';
      png := TPngImage.Create;
      png.Assign( Result );
      png.SaveToFile( s );
      png.Free;
      ini.WriteString( 'UserIcons', 'URL' + IntToStr( n ), AURL );
      ini.WriteString( 'UserIcons', 'PATH' + IntToStr( n ), s );
      ini.WriteInteger( 'UserIcons', 'Count', n + 1 );
    end;

  finally
    ini.Free;
  end;
end;

function TKRGoogleAuth.Login: boolean;
var
  js: TKRJSONObject;
  i,n: integer;
  url: String;
  data: TKRGoogleRESTRequestData;
  s: AnsiString;
begin
  Result:=Refresh;
  if Result then exit;

  Clear;

  if not Assigned(FServer) then begin
    FServer:=TKRTCPServer.Create(self);
    FServer.OnEvent:=srvEvent;
  end;

  FServer.Port:=FPort;
  FServer.Active:=true;

  FState:=getTickCount;
  FParams.Clear;

  FParams.Add( 'scope', 'https://www.googleapis.com/auth/userinfo.profile' );
  if gscEMail in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/userinfo.email' );
  if gscContacts in FScope then FParams.Add( 'scope', 'https://www.google.com/m8/feeds/' ) else
  if gscContactsRead in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/contacts.readonly' );
  if gscCalendar in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/calendar' ) else
  if gscCalendarRead in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/calendar.readonly' );
  if gscSheets in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/spreadsheets' );
  if gscSheetsRead in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/spreadsheets.readonly' );
  if gscWebmaster in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/webmasters' ) else
  if gscWebmasterRead in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/webmasters.readonly' );
  if gscCloudPlatform in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/cloud-platform' );
  if gscCloudTranslation in FScope then FParams.Add( 'scope', 'https://www.googleapis.com/auth/cloud-translation' );

  FParams.Add( 'access_type', 'offline' );
  FParams.Add( 'include_granted_scopes', 'true' );
  FParams.Add( 'response_type', 'code' );
  FParams.Add( 'state', IntToStr(FState) );
  FParams.Add( 'redirect_uri', 'http://127.0.0.1:'+IntToStr(FPort) );
  FParams.Add( 'client_id', FAppID );
  url := 'https://accounts.google.com/o/oauth2/v2/auth' + getParams;

  FCode:='';
  FWaitWebBrowser := true;
  FCancelLogin := false;
  FResultPageStr := FResultPage.Text;
  ShellExecute( 0, 'open', PChar(url), nil, nil, SW_RESTORE);
  KRRunInThread( nil, thWaitWebBrowser );
  Sleep(1000);
  FServer.Active:=false;

  if FCode='' then begin
    if Assigned(FOnError) then FOnError(Self);
    exit;
  end;

  data.url := 'https://www.googleapis.com/oauth2/v4/token';
  s:=AnsiString( 'code='+FCode+'&'+
    'client_id='+FAppID+'&'+
    'client_secret='+FSecretKey+'&'+
    'redirect_uri='+'http://127.0.0.1:'+IntToStr(FPort)+'&'+
    'grant_type=authorization_code' );
  data.sData := TMemoryStream.Create;
  data.sData.Write( s[1], Length(s) );
  js := TKRJSONObject( KRRunInThread( @data, thAuth ) );
  data.sData.Free;
  if not data.isOk then begin
    if Assigned(FOnError) then FOnError(Self);
    exit;
  end;
  try
    if js.StringOf('access_token',i) then begin
      FToken:=js.Strings[ i ].Value;
      if js.IntegerOf('expires_in',i) then n:=js.Integers[ i ].Value else n:=3600;
      FExpiresIn:=now+n*1.157407407407407e-5;
      if js.StringOf('refresh_token',i) then SetRefreshToken( js.Strings[ i ].Value );
    end else begin
      if Assigned(FOnError) then FOnError(Self);
      exit
    end;
  finally
    js.Free;
  end;

  Result:=GetProfile;

  if Result then
    if Assigned(FOnLogin) then FOnLogin(Self);

end;

procedure TKRGoogleAuth.Logout;
var
  data: TKRGoogleRESTRequestData;
begin
  FParams.Clear;
  FParams.Add( 'token', FToken );
  data.url := 'https://accounts.google.com/o/oauth2/revoke' + getParams;
  KRRunInThread( @data, thLogout );
  Clear;
  if Assigned(FOnLogout) then FOnLogout( Self );
end;


function TKRGoogleAuth.Refresh: boolean;
begin
  Result:=false;
  if GetRefreshToken='' then exit;
  if RefreshToken then
    Result:=GetProfile
  else if FGoogleError then SetRefreshToken( '' );

  if Result then
    if Assigned(FOnLogin) then FOnLogin(Self);

end;

function TKRGoogleAuth.RefreshToken: boolean;
var
  js: TKRJSONObject;
  i, n: integer;
  data: TKRGoogleRESTRequestData;
  s: AnsiString;
begin
  Result := false;
  data.url := 'https://www.googleapis.com/oauth2/v4/token';
  s := AnsiString('refresh_token='+FRefreshToken+'&'+
    'client_id='+FAppID+'&'+
    'client_secret='+FSecretKey+'&'+
    'grant_type=refresh_token');
  data.sData := TMemoryStream.Create;
  data.sData.Write( s[1], Length(s) );
  js := TKRJSONObject( KRRunInThread( @data, thRefreshToken ) );
  data.sData.Free;
  if data.isOk then try
    if js.StringOf('access_token',i) then begin
      FToken:=js.Strings[ i ].Value;
      if js.IntegerOf('expires_in',i) then n:=js.Integers[i].Value else n:=3600;
      FExpiresIn:=now+n*1.157407407407407e-5;
      if js.StringOf('refresh_token',i) then SetRefreshToken( js.Strings[ i ].Value );
      Result:=true;
    end;
  finally
    js.Free;
  end;
end;

procedure TKRGoogleAuth.SetDataFolder(const Value: TFileName);
begin
  if (csDesigning in ComponentState) then
    FDataFolder:=ExtractFilePath(Value)
  else FDataFolder:=Value;
end;

procedure TKRGoogleAuth.SetRefreshToken(ARefreshToken: String);
var
  ini: TIniFile;
begin
  FRefreshToken:=ARefreshToken;
  ini:=TIniFile.Create(FDataFolder+'google.ini');
  ini.WriteString('OAuth',Name+'_RefreshToken',FRefreshToken);
  ini.Free;
end;

procedure TKRGoogleAuth.SetResultPage(const Value: TStrings);
begin
  FResultPage.Assign( Value );
end;

procedure TKRGoogleAuth.srvEvent(Sender: TObject; APack: PKRBuffer2k;
  var ALength: integer);
var
  s: String;
  i,n: integer;
  sl: TStringList;
  stat: cardinal;
  error: boolean;
  Content: TStringStream;
begin
  stat:=0;
  s:='';
  for I := 0 to ALength-1 do s:=s+Chr(APack^[i]);
  if LeftStr(s,6) = 'GET /?' then begin
    n:=Pos(' HTTP/1.1',s);
    if n>0 then begin
      s:=Copy(s,7,n-7);
      sl:=TStringList.Create;
      try
        KRSplitStr(s,'&',sl);
        error:=false;
        for I := 0 to sl.Count-1 do
          if SameText(LeftStr(sl[i],5),'code=')then begin
            FCode:=RightStr(sl[i],Length(sl[i])-5);
          end else if SameText(LeftStr(sl[i],6),'state=')then begin
            stat:=StrToIntDef(RightStr(sl[i],Length(sl[i])-6),0);
          end;
      finally
        sl.Free;
      end;
    end;
  end;

  Content:=TStringStream.Create(
    'HTTP/1.0 200 OK' + CKR_CRLF +
    'Content-Type: text/html; charset=UTF-8' + CKR_CRLF +
    'Connection: close' + CKR_CRLF + CKR_CRLF + FResultPageStr,
     TEncoding.UTF8 );

  Content.Position := 0;
  ALength := Content.Size;
  Content.Read( APack^,  ALength );

  FWaitWebBrowser := ( stat <> FState );
end;

function TKRGoogleAuth.thAuth(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TMemoryStream;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  Http := HTTPCreate;

  Response:=POST( Http, Data, 'application/x-www-form-urlencoded' );

  if data.isOk then begin
    Result := Pointer(TKRJSON.Parse(Response.Memory,Response.Size));
    data.isOk := Result <> nil;
    if data.isOk and ( TKRJSON(Result).ValueType<>jstObject ) then begin
      TKRJSON(Result).Free;
      data.isOk:=false;
    end;
  end;

  HTTPDestroy( http );
  Response.Free;
end;

function TKRGoogleAuth.thGetProfile(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TMemoryStream;
  data: PKRGoogleRESTRequestData;
begin
  result := nil;
  data := AData;
  http := HTTPCreate;
  Response := GET( http, data );

  if data.isOk then begin
    Result := Pointer(TKRJSON.Parse(Response.Memory,Response.Size));
    data.isOk := Result <> nil;
    if data.isOk and ( TKRJSON(Result).ValueType<>jstObject ) then begin
      TKRJSON(Result).Free;
      data.isOk:=false;
    end;
  end;

  HTTPDestroy( http );
  response.Free;
end;

function TKRGoogleAuth.thLoadIcon(AThread: TThread; AData: Pointer): Pointer;
var
  url: String;
  http: TKRIdHTTP;
  bmp: TBitmap;
begin
  Http:=HTTPCreate;
  Http.Request.Accept := '';
  url := String( PChar( AData ) );

  bmp:=TBitmap.Create;
  if not KRDownloadImageToBMP( url, Http, bmp) then FreeAndNil( bmp );

  Result:=Pointer( bmp );
  HTTPDestroy( http );
end;

function TKRGoogleAuth.thLogout(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  Http := HTTPCreate;
  GET( Http, data ).Free;
  HTTPDestroy( Http );
end;

function TKRGoogleAuth.thRefreshToken(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TMemoryStream;
  data: PKRGoogleRESTRequestData;
begin
  Result:=nil;
  data := AData;
  Http:=HTTPCreate;
  Response := POST( Http, data, 'application/x-www-form-urlencoded' );

  if data.isOk then begin
    Result := Pointer(TKRJSON.Parse(Response.Memory,Response.Size));
    data.isOk := Result <> nil;
    if data.isOk and ( TKRJSON(Result).ValueType<>jstObject ) then begin
      TKRJSON(Result).Free;
      data.isOk:=false;
    end;
  end;

  HTTPDestroy( http );
  Response.Free;
end;

function TKRGoogleAuth.thWaitWebBrowser(AThread: TThread;
  AData: Pointer): Pointer;
var
  tm: Cardinal;
begin
  Result:=nil;
  tm:=getTickCount;
  while FWaitWebBrowser and not(FCancelLogin) do begin
    if FLoginTimeout>0 then begin
      if (getTickCount-tm) div 1000 > FLoginTimeout then begin
        {$IFDEF GOOGLE_API_LOGS} AddLog('-- LoginTimeout');{$ENDIF}
        break;
      end;
    end;
    sleep(100);
  end;
end;

{ TKRGoogleAuthThread }

constructor TKRGoogleAuthThread.CreateTh(AAuth: TKRGoogleAuth);
begin
  FAuth:=AAuth;
  inherited Create(false);
end;

procedure TKRGoogleAuthThread.Execute;
begin
  while not Terminated do begin
    Synchronize(GetTokenInfo);
    if(FToken<>'')and(FRefreshToken<>'')and((FExpiresIn-now)<0.0034722222222222) then begin
      Synchronize(RefreshToken);
    end;
    sleep(5000);
  end;
end;

procedure TKRGoogleAuthThread.GetTokenInfo;
begin
  FRefreshToken:=FAuth.GetRefreshToken;
  FToken:=FAuth.FToken;
  FExpiresIn:=FAuth.FExpiresIn;
end;

procedure TKRGoogleAuthThread.RefreshToken;
begin
  FAuth.RefreshToken;
end;

end.

