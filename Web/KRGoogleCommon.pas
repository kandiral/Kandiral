(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleCommon                                                            *)
(*  Ver.: 23.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleCommon;

interface

//{$DEFINE GOOGLE_API_LOGS}

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$IFEND}

  KRRuntimeErrors, KRJSON, KRWebCommon,

  idHTTP, KRIdHTTP, IdSSL, IdSSLOpenSSL, IdLogEvent, IdCompressorZLib;

type
  TKRGoogleRESTRequestData = record
    url, ErrorMsg, res, res1: String;
    sData: TStream;
    isOk: boolean;
  end;
  PKRGoogleRESTRequestData = ^TKRGoogleRESTRequestData;

  TKRGoogleCommon = class( TComponent )
  private
    FIdLogs: TIdLogEvent;
    FReadTimeout: integer;
    FConnectTimeout: integer;
    {$IFDEF GOOGLE_API_LOGS}
      procedure logRecv(ASender: TComponent; const AText, AData: string);
      procedure logSend(ASender: TComponent; const AText, AData: string);
      procedure logStat(ASender: TComponent; const AText: string);
    {$ENDIF}
  protected
    FLastErrorMsg: String;
    FGoogleError: boolean;
    FParams: TKRWebParams;
    procedure AddLog( AText: String );
    function getParams: String; virtual;
    function HTTPCreate: TKRIdHTTP; virtual;
    procedure HTTPDestroy( AHTTP: TKRIdHTTP ); virtual;
    function getErrorMsg(E: EIdHTTPProtocolException): String;
    function GET( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData ): TMemoryStream;
    procedure DEL( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData );
    function POST( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData; AContentType: String ): TMemoryStream;
    function PUT( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData; AContentType: String ): TMemoryStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LastErrorMessage: String read FLastErrorMsg;
    property ConnectTimeout: integer read FConnectTimeout write FConnectTimeout default 5000;
    property ReadTimeout: integer read FReadTimeout write FReadTimeout default 0;
  end;

implementation

{ TKRGoogleCommon }

procedure TKRGoogleCommon.AddLog(AText: String);
begin
  REAddLogW(AText);
end;

constructor TKRGoogleCommon.Create(AOwner: TComponent);
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
  FReadTimeout := 0;
  FConnectTimeout := 5000;
  FParams:=TKRWebParams.Create;
end;

procedure TKRGoogleCommon.DEL(AHttp: TKRIdHTTP;
  AData: PKRGoogleRESTRequestData);
begin
  AData.isOk:=false;
  FGoogleError:=false;
  try
    AHttp.Delete( AData.url );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
end;

destructor TKRGoogleCommon.Destroy;
begin
  {$IFDEF GOOGLE_API_LOGS}
    FIdLogs.Free;
  {$ENDIF}
  FParams.Free;
  inherited;
end;

function TKRGoogleCommon.GET(AHttp: TKRIdHTTP;
  AData: PKRGoogleRESTRequestData): TMemoryStream;
begin
  Result:=TMemoryStream.Create;
  AData.isOk:=false;
  FGoogleError:=false;
  try
    AHttp.Get( AData.url, Result );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
  if not AData.isOk then FreeAndNil( Result );
end;

function TKRGoogleCommon.getErrorMsg(E: EIdHTTPProtocolException): String;
var
  js0: TKRJSON;
  js: TKRJSONObject;
  s: AnsiString;
  i,i1,i2,i3: integer;
begin
  Result:='';
  s:=AnsiString(E.ErrorMessage);
  js0:=TKRJSON.Parse(@s[1], Length(s));
  if Assigned(js0) then begin
    try
      if ( js0.ValueType = jstObject ) then begin
        js:=TKRJSONObject(js0);
        i:=js.IndexOf('error');
        if i>-1 then begin
          FGoogleError := true;
          if js[i].ValueType = jstString then begin
            if js.StringOf('error_description',i1) then Result := '['+js.Strings[i].Value+'] '+js.Strings[i1].Value
            else Result := TKRJSONString(js[i]).Value;
          end else if js[i].ValueType = jstObject then begin
            js:=TKRJSONObject( js[i] );
            if js.IntegerOf('code',i1) and js.IntegerOf('status',i2) and js.IntegerOf('message',i3) then
              Result:='['+IntToStr(js.Integers[i1].Value)+'] '+js.Strings[i2].value+' '+js.Strings[i3].value;
          end;
        end;
      end;
    except end;
    js0.Free;
  end;
  if Result='' then Result:='['+IntToStr(E.ErrorCode)+'] '+E.Message;
end;

function TKRGoogleCommon.getParams: String;
begin
  Result := FParams.GetParams;
end;

function TKRGoogleCommon.HTTPCreate: TKRIdHTTP;
begin
  Result:=TKRIdHTTP.Create(nil);
  Result.Intercept:=FIdLogs;
  Result.HandleRedirects:=true;
  Result.AllowCookies:=false;

  Result.IOHandler:=TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  TIdSSLIOHandlerSocketOpenSSL(Result.IOHandler).SSLOptions.Mode:=sslmUnassigned;
  TIdSSLIOHandlerSocketOpenSSL(Result.IOHandler).SSLOptions.VerifyMode := [];
  TIdSSLIOHandlerSocketOpenSSL(Result.IOHandler).SSLOptions.VerifyDepth := 2;

  {$IFNDEF GOOGLE_API_LOGS}
    Result.Compressor:=TIdCompressorZLib.Create(Result);
  {$ENDIF}

  Result.Request.CharSet:='utf8';
  Result.Request.Accept:='application/json';

  Result.ConnectTimeout:=FConnectTimeout;
  Result.ReadTimeout:=FReadTimeout;
end;

procedure TKRGoogleCommon.HTTPDestroy(AHTTP: TKRIdHTTP);
begin
  AHTTP.IOHandler.Free;
  {$IFNDEF GOOGLE_API_LOGS}
    AHttp.Compressor.Free;
  {$ENDIF}
  AHTTP.Free;
end;

{$IFDEF GOOGLE_API_LOGS}
  procedure TKRGoogleCommon.logRecv(ASender: TComponent; const AText,
    AData: string);
  begin
    AddLog('HTTP_RECV');
    if AText<>'' then AddLog(AText);
    if AData<>'' then AddLog(AData);
  end;
{$ENDIF}

{$IFDEF GOOGLE_API_LOGS}
  procedure TKRGoogleCommon.logSend(ASender: TComponent; const AText,
    AData: string);
  begin
    AddLog('HTTP_SEND');
    if AText<>'' then AddLog(AText);
    if AData<>'' then AddLog(AData);
  end;
{$ENDIF}

{$IFDEF GOOGLE_API_LOGS}
  procedure TKRGoogleCommon.logStat(ASender: TComponent; const AText: string);
  begin
    AddLog('HTTP_STAT: '+AText);
  end;
{$ENDIF}

function TKRGoogleCommon.POST(AHttp: TKRIdHTTP;
  AData: PKRGoogleRESTRequestData; AContentType: String): TMemoryStream;
begin
  Result:=TMemoryStream.Create;

  AHttp.Request.ContentType:=AContentType;
  FGoogleError:=false;
  AData.isOk:=false;
  try
    AHttp.Post( AData.url, AData.sData, Result );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
  if not AData.isOk then FreeAndNil( Result );
end;

function TKRGoogleCommon.PUT(AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData;
  AContentType: String): TMemoryStream;
begin
  Result:=TMemoryStream.Create;

  AHttp.Request.ContentType:=AContentType;
  FGoogleError:=false;
  AData.isOk:=false;
  try
    AHttp.Put( AData.url, AData.sData, Result );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
  if not AData.isOk then FreeAndNil( Result );
end;

end.
