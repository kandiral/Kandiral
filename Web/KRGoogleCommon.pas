(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleCommon                                                            *)
(*  Ver.: 14.03.2021                                                          *)
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

  KRRuntimeErrors, KRWebCommon,

  idHTTP, KRIdHTTP, IdSSL, IdSSLOpenSSL, IdLogEvent, IdCompressorZLib,

  uLkJSON;

type
  TKRGoogleRESTRequestData = record
    url, data, ErrorMsg, res: String;
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
    function GET( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData ): TStringStream;
    procedure DEL( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData );
    function POST( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData; AContentType: String ): TStringStream;
    function PUT( AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData; AContentType: String ): TStringStream;
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
  AData: PKRGoogleRESTRequestData): TStringStream;
begin
  Result:=TStringStream.Create;
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
  js1,js: TlkJSONobject;
  i: integer;
begin
  Result:='';
  js:=TlkJSON.ParseText(E.ErrorMessage) as TlkJSONobject;
  if Assigned(js) then begin
    try
    i:=js.IndexOfName('error');
    if i>-1 then begin
      FGoogleError := true;
      if js.Field['error'].SelfType = jsString then begin
        i:=js.IndexOfName('error_description');
        if i>-1 then Result := '['+js.Field['error'].Value+'] '+js.Field['error_description'].Value
        else Result := js.Field['error'].Value;
      end else begin
        js1:=TlkJSONobject( js.Field['error'] );
        Result:='['+IntToStr(js1.Field['code'].value)+'] '+js1.Field['status'].value+' '+js1.Field['message'].value;
      end;
    end;
    except end;
    js.Free;
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
  AData: PKRGoogleRESTRequestData; AContentType: String): TStringStream;
var
  Content: TStream;
begin
  Result:=TStringStream.Create;
  if AData.data<>'' then
    Content := TStringStream.Create(  AData.data, TEncoding.UTF8 )
  else
    Content := AData.sData;

  AHttp.Request.ContentType:=AContentType;
  FGoogleError:=false;
  AData.isOk:=false;
  try
    AHttp.Post( AData.url, Content, Result );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
  if AData.sData=nil then Content.Free;
  if not AData.isOk then FreeAndNil( Result );
end;

function TKRGoogleCommon.PUT(AHttp: TKRIdHTTP; AData: PKRGoogleRESTRequestData;
  AContentType: String): TStringStream;
var
  Content: TStream;
begin
  Result:=TStringStream.Create;
  if AData.data<>'' then
    Content := TStringStream.Create(  AData.data, TEncoding.UTF8 )
  else
    Content := AData.sData;

  AHttp.Request.ContentType:=AContentType;
  FGoogleError:=false;
  AData.isOk:=false;
  try
    AHttp.Put( AData.url, Content, Result );
    AData.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      AData.ErrorMsg:=getErrorMsg(E);
    end;
    on E: Exception do begin
      AData.ErrorMsg:=E.Message;
    end;
  end;
  if AData.sData=nil then Content.Free;
  if not AData.isOk then FreeAndNil( Result );
end;

end.
