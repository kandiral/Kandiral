(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleAuth                                                              *)
(*  Ver.: 16.09.2019                                                          *)
(*  https://kandiral.ru/delphi/krgoogleauth.pas.html                          *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleAuth;

interface

//{$DEFINE GOOGLE_API_LOGS}

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils, System.StrUtils,
    Winapi.MMSystem, Vcl.Forms, System.Variants, Vcl.Graphics, Vcl.Controls,
    Vcl.Dialogs, Vcl.OleCtrls, System.IniFiles, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
    Vcl.Imaging.GIFImg, Winapi.ShLwApi,
  {$ELSE}
    Windows, Messages, Classes, SysUtils, StrUtils, MMSystem, Forms, Variants,
    Graphics, Controls, Dialogs, OleCtrls, IniFiles, JPEG, PngImage, GIFImg,
    ShLwApi,
  {$IFEND}
  SHDocVw, HTTPApp,
  IdHTTP, IdSSL, IdSSLOpenSSL,
  KRSockets, KRTypes, KRTCPServer, KRThread, KRRuntimeErrors, IdLogEvent, funcs,
  uLkJSON;

const
  WM_GOOGLE_OAUTH = WM_USER + 1;
  WM_GOOGLE_ERROR = WM_USER + 2;

type
  TKRGoogleAuthScopes = (gscEMail, gscContactsRead, gscContacts, gscCalendarRead,
    gscCalendar, gscSheets, gscSheetsRead, gscWebmaster, gscWebmasterRead);
  TKRGoogleAuthScope = set of TKRGoogleAuthScopes;
  TKRGoogleAuth = class;

  TKRGoogleAuthForm = class(TForm)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure WebBrowser1NavigateError(ASender: TObject; const pDisp: IDispatch;
      var URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
    procedure FormHide(Sender: TObject);
    procedure WebBrowser1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
  private
    autoClose: boolean;
  protected
    oauth: TKRGoogleAuth;
    procedure CreateParams(var Params: TCreateParams);override;
    procedure WMKRGoogleAuth(var Message: TMessage); message WM_GOOGLE_OAUTH;
    procedure WMGoogleError(var Message: TMessage); message WM_GOOGLE_ERROR;
  public
  end;

  TKRGoogleAuthThread = class(TKRThread)
  private
    FAuth: TKRGoogleAuth;
    FToken, FRefreshToken: String;
    FExpiresIn: TDateTime;
    procedure GetTokenInfo;
    procedure RefreshToken;
  protected
    procedure KRExecute; override;
  end;

  TKRGoogleAuth = class(TComponent)
  private
    FThread: TKRGoogleAuthThread;
    FAuthForm: TKRGoogleAuthForm;
    FAppID: String;
    FSecretKey: String;
    FScope: TKRGoogleAuthScope;
    FToken, FTmpToken: string;
    FServer: TKRTCPServer;
    FPort: word;
    FState: Cardinal;
    FCode: String;
    FOnError: TNotifyEvent;
    FProccesed: boolean;
    FUserID: string;
    FUserIcon: TBitmap;
    FUserName: String;
    FDataFolder, FDataFolder_: String;
    FRefreshToken: String;
    FExpiresIn: TDateTime;
    Fjs: TlkJSONobject;
    FOnLogin: TNotifyEvent;
    FOnLogout: TNotifyEvent;
    FIdLogs: TIdLogEvent;
    FLoginTimeout: Cardinal;
    FAuthError, FGetprofileError: String;
    FUserEmail: String;
    FUserFamilyName: String;
    FUserGivenName: String;
    function oauth(AThread: TThread; AData: Pointer): Pointer;
    function getprofile(AThread: TThread; AData: Pointer): Pointer;
    function refreshtokendo(AThread: TThread; AData: Pointer): Pointer;
    function process(AThread: TThread; AData: Pointer): Pointer;
    function logoutdo(AThread: TThread; AData: Pointer): Pointer;
    function loadusericon(AThread: TThread; AData: Pointer): Pointer;
    procedure srvEvent(Sender: TObject; APack: PKRBuffer; var ALength: integer);
    function getScope: string;
    function GetExpiresIn: TDateTime;
    procedure logRecv(ASender: TComponent; const AText, AData: string);
    procedure logSend(ASender: TComponent; const AText, AData: string);
    procedure logStat(ASender: TComponent; const AText: string);
    procedure AddLog(AText: STring);
    function getErrorMsg(E: EIdHTTPProtocolException): String;
    function GetDataFolder: String;
    procedure SetDataFolder(const Value: String);
  protected
    function OAuthURL: String;
    procedure StartOAuth(ACode: String);
    procedure DoError;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function Login: boolean;
    function Logout: boolean;
    function RefreshToken: boolean;
    function GetRefreshToken: String;
    function GetUserIcon(AURL: String; AETag: String = ''; AAccessToken: boolean = false): TBitmap;
    property Token: string read FToken;
    property UserID: string read FUserID;
    property UserName: String read FUserName;
    property UserGivenName: String read FUserGivenName;
    property UserFamilyName: String read FUserFamilyName;
    property UserEmail: String read FUserEmail;
    property UserIcon: TBitmap read FUserIcon;
    property ExpiresIn: TDateTime read GetExpiresIn;
    property LoginTimeout: Cardinal read FLoginTimeout write FLoginTimeout;
  published
    property AppID: String read FAppID write FAppID;
    property SecretKey: String read FSecretKey write FSecretKey;
    property Scope: TKRGoogleAuthScope read FScope write FScope;
    property Port: word read FPort write FPort;
    property DataFolder: String read GetDataFolder write SetDataFolder;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
    property OnLogout: TNotifyEvent read FOnLogout write FOnLogout;
  end;

  EKRGoogleAuth = class(Exception)
  end;

implementation

{$R '..\dfm\KRGoogleAuth.dfm'}

function AbsToRel(const AbsPath, BasePath: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  result := Path;
end;

function RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array[0..MAX_PATH-1] of char;
begin
  PathCanonicalize(@Dst[0], PChar(IncludeTrailingBackslash(BasePath) + RelPath));
  result := Dst;
end;

{ TKRGoogleAuth }

procedure TKRGoogleAuth.AddLog(AText: STring);
begin
  REAddLog(AText);
end;

constructor TKRGoogleAuth.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF GOOGLE_API_LOGS}
  FIdLogs:=TIdLogEvent.Create(Self);
  FIdLogs.OnReceived:=logRecv;
  FIdLogs.OnSent:=logSend;
  FIdLogs.OnStatus:=logStat;
  FIdLogs.LogTime:=false;
  FIdLogs.ReplaceCRLF:=false;
  FIdLogs.Active:=true;
{$ELSE}
  FIdLogs:=nil;
{$ENDIF}
  FThread:=TKRGoogleAuthThread.Create;
  FThread.FAuth:=self;
  FLoginTimeout:=0;
end;

destructor TKRGoogleAuth.Destroy;
begin
  if assigned(FServer) then FServer.Free;
  FThread.Free;
  if Assigned(FIdLogs) then
    FIdLogs.Free;
  inherited;
end;

procedure TKRGoogleAuth.DoError;
begin
  FProccesed:=false;
end;

function TKRGoogleAuth.GetDataFolder: String;
begin
  Result:=FDataFolder_;
end;

function TKRGoogleAuth.getErrorMsg(E: EIdHTTPProtocolException): String;
var
  js0,js: TlkJSONobject;
begin
  Result:='';
  js0:=TlkJSON.ParseText(E.ErrorMessage) as TlkJSONobject;
  if js0<>nil then begin
    if js0.Field['error'] is TlkJSONobject then begin
      js:=js0.Field['error'] as TlkJSONobject;
      Result:='['+IntToStr(js.Field['code'].value)+'] '+js.Field['status'].value+' '+js.Field['message'].value;
    end;
    js0.Free;
  end;
  if Result='' then Result:='['+IntToStr(E.ErrorCode)+'] '+E.Message;
end;

function TKRGoogleAuth.GetExpiresIn: TDateTime;
begin
  Result:=FExpiresIn;
end;

function TKRGoogleAuth.getprofile(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
  Response: TStringStream;
  s: String;
begin
  FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Mode :=  sslmUnassigned;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 2;

  Http:=TIdHTTP.Create(nil);
  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.IOHandler:=FSSL;
  Http.AllowCookies:=false;

  Http.Request.Accept:='application/json';
  Http.Request.host := 'www.googleapis.com';
  Http.Request.CharSet:='utf8';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  FGetprofileError:='';
  Response:=TStringStream.Create;

  try
    Http.Get('https://www.googleapis.com/oauth2/v1/userinfo?access_token='+FTmpToken,Response);
  except
    on E: EIdHTTPProtocolException do begin
      FGetprofileError:=getErrorMsg(E);
    end;
    on E: Exception do begin
      FGetprofileError:=E.Message;
    end;
  end;

  if FGetprofileError='' then begin
    s:=Response.DataString;
    fjs := TlkJSON.ParseText(s) as TlkJSONobject;
  end else begin
    fjs:=nil;
{$IFDEF GOOGLE_API_LOGS}
    REAddLog('FGetprofileError: '+FGetprofileError);
{$ENDIF}
  end;

  response.Free;
  Http.Free;
  FSSL.Free;
  Result:=nil;
end;

function TKRGoogleAuth.GetRefreshToken: String;
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(FDataFolder+'google.ini');
  Result:=ini.ReadString('OAuth',Name+'_RefreshToken','');
  ini.Free;
end;

function TKRGoogleAuth.getScope: string;
begin
  result:=String(HTTPEncode('https://www.googleapis.com/auth/userinfo.profile'));
  if gscEMail in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/userinfo.email'));
  if gscContacts in FScope then result:=result+'+'+String(HTTPEncode('https://www.google.com/m8/feeds/')) else
  if gscContactsRead in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/contacts.readonly'));
  if gscCalendar in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/calendar')) else
  if gscCalendarRead in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/calendar.readonly'));
  if gscSheets in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/spreadsheets'));
  if gscSheetsRead in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/spreadsheets.readonly'));
  if gscWebmaster in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/webmasters')) else
  if gscWebmasterRead in FScope then result:=result+'+'+String(HTTPEncode('https://www.googleapis.com/auth/webmasters.readonly'));

end;

function TKRGoogleAuth.GetUserIcon(AURL: String; AETag: String = '';
  AAccessToken: boolean = false): TBitmap;
var
  ini: TIniFile;
  i, n: integer;
  path{,ext},ms,s,s0: String;
  p:PChar;
  jpeg: TJPEGImage;
  png: TPngImage;
  gif: TGIFImage;
begin
  Result:=nil;
  ini:=TIniFile.Create(FDataFolder+'google.ini');
  path:='';
  n:=ini.ReadInteger('UserIcons','Count',0)-1;
  ms:=AURL;
  if AETag<>'' then ms:=ms+'!'+AETag;
  for I := 0 to n do
    if SameText(ini.ReadString('UserIcons','URL'+IntToStr(i),''),ms) then begin
      path:=ini.ReadString('UserIcons','PATH'+IntToStr(i),'');
      break;
    end;
  s0:=ExtractFilePath(Application.ExeName);
  if(Length(s0)>3)and(s0[Length(s0)]='\') then s0:=copy(s0,1,Length(s0)-1);
  if SameText(path,'') then begin
    {ext:=LowerCase(ExtractFileExt(AURL));
    if SameText(ext,'.jpeg') or SameText(ext,'.jpg') then ext:='.jpg'
    else if(not SameText(ext,'.gif'))and(not SameText(ext,'.png'))then ext:='';
    if not SameText(ext,'') then begin  }
      s:=AURL;
      if AAccessToken then s:=s+'?access_token='+FToken;
      p:=KRRunInThread(Pointer(PChar(s)),loadusericon);
      if p<>nil then begin
        path:=p;
        inc(n);
        ini.WriteString('UserIcons','URL'+IntToStr(n),ms);
        ini.WriteString('UserIcons','PATH'+IntToStr(n),AbsToRel(path,s0));
        ini.WriteInteger('UserIcons','Count',n+1);
        FreeMem(p);
      end;
    //end;
  end else path:=RelToAbs(path,s0);
  if not SameText(path,'') then begin
    if SameText(ExtractFileExt(path),'.gif') then begin
      Result:=TBitmap.Create;
      gif:=TGIFImage.Create;
      gif.LoadFromFile(path);
      Result.Assign(gif);
      gif.Free;
    end else if SameText(ExtractFileExt(path),'.png') then begin
      Result:=TBitmap.Create;
      png:=TPngImage.Create;
      png.LoadFromFile(path);
      Result.Assign(png);
      png.Free;
    end else if SameText(ExtractFileExt(path),'.jpg') then begin
      Result:=TBitmap.Create;
      jpeg := TJPEGImage.Create;
      jpeg.LoadFromFile(path);
      Result.Assign(jpeg);
      jpeg.Free;
    end;
  end;
  ini.Free;
end;

function TKRGoogleAuth.loadusericon(AThread: TThread; AData: Pointer): Pointer;
var
  pth,ext,url,f: String;
  n,i: integer;
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
  Response: TMemoryStream;
  p: PChar;
  buf: array[0..3] of byte;
begin
  Result:=nil;
  randomize;
  f:=FDataFolder;
  url:=String(PChar(AData));

  Http:=TIdHTTP.Create(nil);
  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.AllowCookies:=false;
  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  if SameText(LeftStr(url,6),'https:') then begin
    FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    FSSL.SSLOptions.Mode :=  sslmUnassigned;
    FSSL.SSLOptions.VerifyMode := [];
    FSSL.SSLOptions.VerifyDepth := 2;
    Http.IOHandler:=FSSL;
  end else FSSL:=nil;

  Response:=TMemoryStream.Create;

  try
    try
      Http.Get(url,Response);
    except on E: EIdHTTPProtocolException do
      if e.ErrorCode=404 then begin
        pth:='404';
        n:=Length(pth);
        GetMem(p, n * SizeOf(Char) + SizeOf(Char));
        for I := 1 to n do p[i-1]:=pth[i];
        p[n]:=#0;
        result:=p;
      end;
    end;
  except end;

  if Response.Size>10 then begin
    Response.Position:=0;
    Response.Read(buf,4);
    ext:='';
    if(buf[0]=$FF)and(buf[1]=$D8)and(buf[2]=$FF)and(buf[3] shr 4 = $E)then ext:='.jpg'
    else if(buf[1]=$50)and(buf[2]=$4E)and(buf[3]=$47)then ext:='.png'
    else if(buf[0]=$47)and(buf[1]=$49)and(buf[2]=$46)then ext:='.gif';
    if not SameText(ext,'') then begin
      n:=Random(99999999);
      while fileExists(f+IntToStr(n)+ext) do n:=Random(99999999);
      pth:=f+IntToStr(n)+ext;
      Response.SaveToFile(pth);
      n:=Length(pth);
      GetMem(p, n * SizeOf(Char) + SizeOf(Char));
      for I := 1 to n do p[i-1]:=pth[i];
      p[n]:=#0;
      result:=p;
    end;
  end;

  Response.Free;
  http.Free;
  if Assigned(FSSL) then FSSL.Free;

end;

function TKRGoogleAuth.Login: boolean;
var
  ini: TIniFile;
  fname, pic: string;
begin
  Logout;

  fjs:=nil;

  ini:=TIniFile.Create(FDataFolder+'google.ini');
  FRefreshToken:=ini.ReadString('OAuth',Name+'_RefreshToken','');
  if FRefreshToken<>'' then begin
    if RefreshToken then begin
      FToken:=FTmpToken;
      KRRunInThread(nil,getprofile);
    end;
  end;

  if FToken='' then begin

    if not Assigned(FAuthForm) then begin
      Application.CreateForm(TKRGoogleAuthForm, FAuthForm);
      FAuthForm.oauth:=self;
    end;
    FAuthForm.Hide;

    if not assigned(FServer) then
      FServer:=TKRTCPServer.Create(self);

    FServer.Port:=FPort;
    FServer.Active:=true;
    FServer.OnEvent:=srvEvent;

    FAuthForm.Show;
    SetActiveWindow(FAuthForm.Handle);
    ShowWindow(FAuthForm.Handle, SW_RESTORE);
    SetForegroundWindow(FAuthForm.Handle);

    FProccesed:=true;
    FTmpToken:='';
    KRRunInThread(nil,process);
    FServer.Active:=false;

    FToken:=FTmpToken;
  end;

  result:=FToken<>'';

  if not result then begin
    //FRefreshToken:='';
    if Assigned(FOnError) then FOnError(self);
  end else begin
    FUserID:='';
    FUserName:='';
    FUserGivenName:='';
    FUserFamilyName:='';
    FUserEmail:='';
    if assigned(FUserIcon) then FreeAndNil(FUserIcon);    
    if assigned(fjs) then begin
      if fjs.Field['id'] is TlkJSONstring then
        FUserID:=(fjs.Field['id'] as TlkJSONstring).Value;
      if fjs.Field['name'] is TlkJSONstring then
        FUserName:=(fjs.Field['name'] as TlkJSONstring).Value;
      if fjs.Field['given_name'] is TlkJSONstring then
        FUserGivenName:=(fjs.Field['given_name'] as TlkJSONstring).Value;
      if fjs.Field['family_name'] is TlkJSONstring then
        FUserFamilyName:=(fjs.Field['family_name'] as TlkJSONstring).Value;
      if fjs.Field['email'] is TlkJSONstring then
        FUserEmail:=(fjs.Field['email'] as TlkJSONstring).Value;

      if SameText(FUserName,'') then begin
        if FUserGivenName<>'' then FUserName:=FUserGivenName;
        if FUserFamilyName<>'' then fname:=FUserFamilyName;
        if FUserName='' then FUserName:=fname
        else if fname<>'' then FUserName:=FUserName+' '+fname;
      end;

      if fjs.Field['picture'] is TlkJSONstring then begin
        pic:=(fjs.Field['picture'] as TlkJSONstring).Value;
        FUserIcon:=GetUserIcon(pic);
      end;
      FreeAndNil(fjs);
    end;
    if Assigned(FOnLogin) then FOnLogin(Self);
    FThread.Active:=FRefreshToken<>'';
  end;

  ini.WriteString('OAuth',Name+'_RefreshToken',FRefreshToken);
  ini.Free;

end;

function TKRGoogleAuth.Logout: boolean;
var
  ini: TIniFile;
begin
  Result:=false;
  FThread.Active:=false;
  if FToken='' then exit;

  FTmpToken:=FToken;
  KRRunInThread(nil,logoutdo);

  ini:=TIniFile.Create(FDataFolder+'google.ini');
  FRefreshToken:='';
  ini.WriteString('OAuth',Name+'_RefreshToken',FRefreshToken);
  ini.Free;
  FToken:='';
  FUserID:='';
  FUserName:='';
  if Assigned(FUserIcon) then FUserIcon.Free;
  FUserIcon:=nil;
  if Assigned(FOnLogout) then FOnLogout(Self);
  Result:=true;
end;

function TKRGoogleAuth.logoutdo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
  Response: TStringStream;
  s: String;
begin
  FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Mode :=  sslmUnassigned;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 2;

  Http:=TIdHTTP.Create(nil);
  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.IOHandler:=FSSL;
  Http.AllowCookies:=false;

//  Http.Request.Accept:='text/xml';
  Http.Request.ContentType:='application/x-www-form-urlencoded';
  Http.Request.CharSet:='utf8';


  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  Response:=TStringStream.Create;

  try
    Http.Get('https://accounts.google.com/o/oauth2/revoke?token='+FTmpToken,Response);
  except end;

  s:=Response.DataString;

  response.Free;
  Http.Free;
  FSSL.Free;
  Result:=nil;
end;

procedure TKRGoogleAuth.logRecv(ASender: TComponent; const AText, AData: string);
begin
  AddLog('HTTP_RECV');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleAuth.logSend(ASender: TComponent; const AText, AData: string);
begin
  AddLog('HTTP_SEND');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleAuth.logStat(ASender: TComponent; const AText: string);
begin
  AddLog('HTTP_STAT: '+AText);
end;

function TKRGoogleAuth.oauth(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
  Parameters, Response: TStringStream;
  s: String;
  js: TlkJSONobject;
  n: integer;
begin
  FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Mode :=  sslmUnassigned;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 2;

  Http:=TIdHTTP.Create(nil);
  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.IOHandler:=FSSL;
  Http.AllowCookies:=false;

  Http.Request.Accept:='application/json';
  Http.Request.host := 'www.googleapis.com';
  Http.Request.ContentType:='application/x-www-form-urlencoded';
  Http.Request.CharSet:='utf8';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  Parameters:=TStringStream.Create(
    'code='+fcode+'&'+
    'client_id='+FAppID+'&'+
    'client_secret='+FSecretKey+'&'+
    'redirect_uri='+'http://127.0.0.1:'+IntToStr(FPort)+'&'+
    'grant_type=authorization_code'
  , TEncoding.UTF8);

  FAuthError:='';
  Response:=TStringStream.Create;

  try
    Http.Post('https://www.googleapis.com/oauth2/v4/token',Parameters,Response);
  except
    on E: EIdHTTPProtocolException do begin
      FAuthError:=getErrorMsg(E);
    end;
    on E: Exception do begin
      FAuthError:=E.Message;
    end;
  end;

  if FAuthError='' then begin
    s:=Response.DataString;
    js := TlkJSON.ParseText(s) as TlkJSONobject;
    if assigned(js) then begin
      if js.Field['access_token'] is TlkJSONstring then begin
        FTmpToken:=(js.Field['access_token'] as TlkJSONstring).Value;
        if js.Field['refresh_token'] is TlkJSONstring then
          FRefreshToken:=(js.Field['refresh_token'] as TlkJSONstring).Value;
        n:=(js.Field['expires_in'] as TlkJSONnumber).Value;
        FExpiresIn:=now+n*1.157407407407407e-5;
        getprofile(nil,nil);
        FAuthError:=FGetprofileError;
      end;
    end;
  end else begin
{$IFDEF GOOGLE_API_LOGS}

    REAddLog('FAuthError: '+FAuthError);
{$ENDIF}
  end;

  response.Free;
  Parameters.Free;
  Http.Free;
  FSSL.Free;
  Result:=nil;
end;

function TKRGoogleAuth.OAuthURL: String;
begin
    FState:=getTickCount;
    Result:='https://accounts.google.com/o/oauth2/v2/auth?'+
    'client_id='+FAppID+'&'+
    'access_type=offline&include_granted_scopes=true&'+
    'response_type=code&'+
    'redirect_uri='+String(HTTPEncode(AnsiString('http://127.0.0.1:'+IntToStr(FPort))))+'&'+
    'scope='+getScope+'&'+
    'state='+IntToStr(FState);
end;

function TKRGoogleAuth.process(AThread: TThread; AData: Pointer): Pointer;
var
  tm: Cardinal;
begin
  Result:=Pointer(0);
  tm:=timeGetTime;
  while FProccesed do begin
    if FLoginTimeout>0 then begin
      if (timeGetTime-tm) div 1000 > FLoginTimeout then begin
{$IFDEF GOOGLE_API_LOGS}
        AddLog('-- LoginTimeout');
{$ENDIF}
        sendmessage(FAuthForm.Handle,WM_CLOSE,0,0);
      end;
    end;
    sleep(100);
  end;
end;

function TKRGoogleAuth.RefreshToken: boolean;
begin
  FTmpToken:='';
  KRRunInThread(nil,refreshtokendo);
  result:=FTmpToken<>'';
end;

function TKRGoogleAuth.refreshtokendo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
  Parameters, Response: TStringStream;
  s, FErrorMsg: String;
  js: TlkJSONobject;
  n: integer;
begin
  FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Mode :=  sslmUnassigned;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 2;

  Http:=TIdHTTP.Create(nil);
  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.IOHandler:=FSSL;
  Http.AllowCookies:=false;

  Http.Request.Accept:='application/json';
  Http.Request.host := 'www.googleapis.com';
  Http.Request.ContentType:='application/x-www-form-urlencoded';
  Http.Request.CharSet:='utf8';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  Parameters:=TStringStream.Create(
    'refresh_token='+FRefreshToken+'&'+
    'client_id='+FAppID+'&'+
    'client_secret='+FSecretKey+'&'+
    'grant_type=refresh_token'
  , TEncoding.UTF8);

  FErrorMsg:='';
  Response:=TStringStream.Create;

  try
    Http.Post('https://www.googleapis.com/oauth2/v4/token',Parameters,Response);
  except
    on E: EIdHTTPProtocolException do begin
      FErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      FErrorMsg:=E.Message;
    end;
  end;

  if FErrorMsg='' then begin
    s:=Response.DataString;
    js := TlkJSON.ParseText(s) as TlkJSONobject;
    if assigned(js) then begin
      if js.Field['access_token'] is TlkJSONstring then begin
        FTmpToken:=(js.Field['access_token'] as TlkJSONstring).Value;
        n:=(js.Field['expires_in'] as TlkJSONnumber).Value;
        FExpiresIn:=now+n*1.157407407407407e-5;
      end;
    end;
  end else begin
{$IFDEF GOOGLE_API_LOGS}
    REAddLog('FRefreshTockenError: '+FErrorMsg);
{$ENDIF}
  end;

  response.Free;
  Parameters.Free;
  Http.Free;
  FSSL.Free;
  Result:=nil;
end;

procedure TKRGoogleAuth.SetDataFolder(const Value: String);
begin
  FDataFolder_:=Value;
  FDataFolder:=RelToAbs(FDataFolder_,ExtractFilePath(Application.ExeName));
  if(Length(FDataFolder)>0)and(RightStr(FDataFolder,1)<>'\')then
    FDataFolder:=FDataFolder+'\';
end;

procedure TKRGoogleAuth.srvEvent(Sender: TObject; APack: PKRBuffer;
  var ALength: integer);
var
  s,code: String;
  i,n: integer;
  sl: TStringList;
  stat: cardinal;
  error: boolean;
begin
{$IFDEF GOOGLE_API_LOGS}
  REAddLog('TKRGoogleAuth.srvEvent APack-------------------');
{$ENDIF}
  s:='';
  for I := 0 to ALength-1 do s:=s+Chr(APack^[i]);
{$IFDEF GOOGLE_API_LOGS}
  REAddLog(s);
  REAddLog('---------------------/APack-------------------');
{$ENDIF}
  if SameText(LeftStr(s,6),'GET /?')then begin
    n:=Pos(' HTTP/1.1',s);
    if n>0 then begin
      s:=Copy(s,7,n-7);
      sl:=Explode('&',s);
      n:=0;
      error:=false;
      stat:=0;
      for I := 0 to sl.Count-1 do
        if SameText(LeftStr(sl[i],5),'code=')then begin
          inc(n);
          code:=RightStr(sl[i],Length(sl[i])-5);
        end else if SameText(LeftStr(sl[i],6),'state=')then begin
          inc(n);
          stat:=StrToDWordDef(RightStr(sl[i],Length(sl[i])-6),0);
        end else if SameText(LeftStr(sl[i],6),'error=')then begin
          error:=true;
        end;
      if Assigned(FAuthForm) then begin
        if error then
          SendMessage(FAuthForm.Handle,WM_GOOGLE_ERROR,0,Integer(stat))
        else if n=2 then
          SendMessage(FAuthForm.Handle,WM_GOOGLE_OAUTH,Integer(@code),Integer(stat))
        else
          SendMessage(FAuthForm.Handle,WM_GOOGLE_ERROR,0,Integer(stat));
      end;
      TKRSocketSrvClient(Sender).Close;
      sl.Free;
    end;
  end;
end;

procedure TKRGoogleAuth.StartOAuth(ACode: String);
begin
  FCode:=ACode;
  KRRunInThread(nil,oauth);
  FProccesed:=false;
  if FAuthError<>'' then raise EKRGoogleAuth.Create(FAuthError);
end;

{ TKRGoogleAuthForm }

procedure TKRGoogleAuthForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle:=Params.ExStyle or WS_Ex_AppWindow;
  Params.WndParent:=0;
end;

procedure TKRGoogleAuthForm.FormHide(Sender: TObject);
begin
  if not autoClose then begin
    oauth.DoError;
  end;
end;

procedure TKRGoogleAuthForm.FormShow(Sender: TObject);
begin
  autoClose:=false;
  WebBrowser1.Navigate('about:blank');
  WebBrowser1.Navigate(oauth.OAuthURL);
end;

procedure TKRGoogleAuthForm.WebBrowser1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
{$IFDEF GOOGLE_API_LOGS}
  REAddLog('Navigate: '+URL);
{$ENDIF}
end;

procedure TKRGoogleAuthForm.WebBrowser1NavigateError(ASender: TObject;
  const pDisp: IDispatch; var URL, Frame, StatusCode: OleVariant;
  var Cancel: WordBool);
begin
{$IFDEF GOOGLE_API_LOGS}
  REAddLog('Navigate Error['+VarToStr(StatusCode)+']: '+URL);
{$ENDIF}
  WebBrowser1.Navigate('about:blank');
  autoClose:=true;
  Hide;
  oauth.DoError;
end;

procedure TKRGoogleAuthForm.WMGoogleError(var Message: TMessage);
var
  stat: Cardinal;
begin
  stat:=Cardinal(Message.LParam);
{$IFDEF GOOGLE_API_LOGS}
  REAddLog('WMGoogleError stat='+IntToSTr(stat));
{$ENDIF}
  if stat=oauth.FState then begin
    WebBrowser1.Navigate('about:blank');
    autoClose:=true;
    Hide;
    oauth.DoError;
  end;
end;

procedure TKRGoogleAuthForm.WMKRGoogleAuth(var Message: TMessage);
var
  stat: Cardinal;
  code: String;
begin
  code:=String(Pointer(Message.WParam)^);
  stat:=Cardinal(Message.LParam);
{$IFDEF GOOGLE_API_LOGS}
  REAddLog('WMKRGoogleAuth stat='+IntToSTr(stat)+'; code='+code);
{$ENDIF}
  if stat=oauth.FState then begin
    WebBrowser1.Navigate('about:blank');
    autoClose:=true;
    Hide;
    oauth.StartOAuth(code);
  end;
end;

{ TKRGoogleAuthThread }

procedure TKRGoogleAuthThread.GetTokenInfo;
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(FAuth.FDataFolder+'google.ini');
  FRefreshToken:=ini.ReadString('OAuth',FAuth.Name+'_RefreshToken','');
  FToken:=FAuth.FToken;
  FExpiresIn:=FAuth.FExpiresIn;
end;

procedure TKRGoogleAuthThread.KRExecute;
begin
  sleep(5000);
  Synchronize(GetTokenInfo);
  if(FToken<>'')and(FRefreshToken<>'')and((FExpiresIn-now)<0.0034722222222222) then begin
    Synchronize(RefreshToken);
  end;
end;

procedure TKRGoogleAuthThread.RefreshToken;
begin
  FAuth.RefreshToken;
end;

end.
