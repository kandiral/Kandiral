(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleSearchConsole                                                     *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleSearchConsole;

interface

{$DEFINE GOOGLE_API_LOGS}

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.Imaging.pngimage, Vcl.Graphics,
  {$ELSE}
    Classes, SysUtils, PngImage, Graphics,
  {$IFEND}
  IdHTTP, IdSSL, IdSSLOpenSSL, IdCompressorZLib, IdLogEvent, IdCoderMIME,
  KRGoogleAuth, KRRuntimeErrors, KRThread, lgop, funcs,
  uLkJSON;

type
  TKRMFTTestStatusEnum = (mftsUNSPECIFIED, mftsCOMPLETE, mftsINTERNAL_ERROR,
    mftsPAGE_UNREACHABLE);
  TKRMFTTestStatus = record
    status: TKRMFTTestStatusEnum;
    details: String;
  end;
  TKRMFTMobileFriendlyTestResult = (mftrsUNSPECIFIED, mftrsMOBILE_FRIENDLY,
    mftrsNOT_MOBILE_FRIENDLY);
  TKRMFTMobileFriendlyRule = (mftrUNSPECIFIED, mftrUSES_INCOMPATIBLE_PLUGINS,
    mftrCONFIGURE_VIEWPORT, mftrFIXED_WIDTH_VIEWPORT, mftrSIZE_CONTENT_TO_VIEWPORT,
    mftrUSE_LEGIBLE_FONT_SIZES, mftrTAP_TARGETS_TOO_CLOSE);
  TKRMFTMobileFriendlyIssue = record
    rule: TKRMFTMobileFriendlyRule;
  end;
  TKRMFTBlockedResource = record
    url: String;
  end;
  TKRMFTResourceIssue = record
    blockedResource: TKRMFTBlockedResource;
  end;
  TKRMobileFriendlyTest = record
    testStatus: TKRMFTTestStatus;
    mobileFriendliness: TKRMFTMobileFriendlyTestResult;
    mobileFriendlyIssues: array of TKRMFTMobileFriendlyIssue;
    resourceIssues: array of TKRMFTResourceIssue;
    image: array of byte;
    errMsg: string;
  end;
  PKRMobileFriendlyTest=^TKRMobileFriendlyTest;

  TKRGoogleSearchConsole = class(TComponent)
  private
    FGoogleAuth: TKRGoogleAuth;
    FIdLogs: TIdLogEvent;
    procedure SetGoogleAuth(const Value: TKRGoogleAuth);
    procedure logRecv(ASender: TComponent; const AText, AData: string);
    procedure logSend(ASender: TComponent; const AText, AData: string);
    procedure logStat(ASender: TComponent; const AText: string);
    procedure AddLog(AText: STring);
    function getHTTP: TIdHTTP;
    function checkError(e: EIdHTTPProtocolException): string;
    function MobileFriendlyTestDo(AThread: TThread; AData: Pointer): Pointer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MobileFriendlyTest(AURL: String; ARequestScreenshot: boolean;
      var  AResult: TKRMobileFriendlyTest): boolean;
  published
    property GoogleAuth: TKRGoogleAuth read FGoogleAuth write SetGoogleAuth;
  end;

implementation

type
  TKRMFTData = record
    url, testUrl: String;
    requestScreenshot: boolean;
    res: PKRMobileFriendlyTest;
    isOk: boolean;
  end;
  PKRMFTData=^TKRMFTData;

{ TKRGoogleSearchConsole }

procedure TKRGoogleSearchConsole.AddLog(AText: STring);
begin
  REAddLog(AText);
end;

function TKRGoogleSearchConsole.checkError(e: EIdHTTPProtocolException): string;
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

constructor TKRGoogleSearchConsole.Create(AOwner: TComponent);
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
end;

destructor TKRGoogleSearchConsole.Destroy;
begin
  if Assigned(FIdLogs) then FIdLogs.Free;
  inherited;
end;

function TKRGoogleSearchConsole.getHTTP: TIdHTTP;
var
  http: TIdHTTP;
  FSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  Http:=TIdHTTP.Create(nil);

  FSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(Http);
  FSSL.SSLOptions.Mode :=  sslmUnassigned;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 2;

  Http.Intercept:=FIdLogs;
  Http.HandleRedirects:=true;
  Http.IOHandler:=FSSL;
  Http.AllowCookies:=false;
{$IFNDEF GOOGLE_API_LOGS}
  Http.Compressor:=TIdCompressorZLib.Create(Http);
{$ENDIF}

  Http.Request.UserAgent:='KRGoogleSearchConsole ver. 1.0';
  Http.Request.Accept:='application/json';
  Http.Request.AcceptCharSet:='utf-8';
  Http.Request.CharSet:='utf-8';
  Http.Request.ContentType:='application/json';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  result:=http;
end;

procedure TKRGoogleSearchConsole.logRecv(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_RECV');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleSearchConsole.logSend(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_SEND');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleSearchConsole.logStat(ASender: TComponent;
  const AText: string);
begin
  AddLog('HTTP_STAT: '+AText);
end;

function TKRGoogleSearchConsole.MobileFriendlyTest(AURL: String;
  ARequestScreenshot: boolean; var AResult: TKRMobileFriendlyTest): boolean;
var
  data: TKRMFTData;
begin
  data.url:='https://searchconsole.googleapis.com/v1/urlTestingTools/mobileFriendlyTest:run?alt=json&key=AIzaSyCI9Xwl4G7Alzv7kJahiEK8VMR3-YpWmgc';
  data.testUrl:=AURL;
  data.requestScreenshot:=ARequestScreenshot;
  data.res:=@AResult;
  //if(FGoogleAuth<>nil)and(FGoogleAuth.Token<>'')then
    //data.url:=data.url+'&access_token='+FGoogleAuth.Token;

  KRRunInThread(@data,MobileFriendlyTestDo);
  result:=data.isOk;
end;

function TKRGoogleSearchConsole.MobileFriendlyTestDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response, inData: TStringStream;
  data: PKRMFTData;
  js,js0: TlkJSONobject;
  jsl: TlkJSONlist;
  s: String;
  i,n,j: integer;
  rule: TKRMFTMobileFriendlyRule;
  Decoder: TIdDecoderMIME;
  DestStream, StreamSource: TMemoryStream;
  _s: AnsiString;
  png: TPngImage;
  bmp: TBitmap;
  buf: array[0..1023] of byte;
begin
  http:=getHTTP;
  http.ConnectTimeout:=120000;
  http.ReadTimeout:=120000;
  data:=AData;

  Response:=TStringStream.Create;
  InData:=TStringStream.Create;
  InData.WriteString('{"url":'+TlkJSON.GenerateText(TlkJSONstring.Generate(data.testUrl))+
    ',"requestScreenshot":'+_is(data.requestScreenshot,'true','false')+'}');
  data.isOk:=false;
  try
    Http.Post(data.url,InData,Response);
    data.isOk:=true;
  except
    on E: EIdHTTPProtocolException do begin
      data.res.errMsg:=checkError(E);
    end;
    on E: Exception do begin
      data.res.errMsg:=E.Message;
    end;
  end;
  if data.isOk then begin
    js:=TlkJSON.ParseText(Response.DataString) as TlkJSONobject;
    if js=nil then data.isOk:=false else begin
      data.res.testStatus.status:=mftsUNSPECIFIED;
      data.res.testStatus.details:='';
      if js.Field['testStatus'] is TlkJSONobject then begin
        js0:=js.Field['testStatus'] as TlkJSONobject;
        if js0.Field['status'] is TlkJSONstring then begin
          s:=(js0.Field['status'] as TlkJSONstring).Value;
          if s='COMPLETE' then data.res.testStatus.status:=mftsCOMPLETE else
          if s='INTERNAL_ERROR' then data.res.testStatus.status:=mftsINTERNAL_ERROR else
          if s='PAGE_UNREACHABLE' then data.res.testStatus.status:=mftsPAGE_UNREACHABLE;
        end;
        if js0.Field['details'] is TlkJSONstring then
          data.res.testStatus.details:=(js0.Field['status'] as TlkJSONstring).Value;
      end;
      data.res.mobileFriendliness:=mftrsUNSPECIFIED;
      if js.Field['mobileFriendliness'] is TlkJSONstring then begin
        s:=(js.Field['mobileFriendliness'] as TlkJSONstring).Value;
        if s='MOBILE_FRIENDLY' then data.res.mobileFriendliness:=mftrsMOBILE_FRIENDLY else
        if s='NOT_MOBILE_FRIENDLY' then data.res.mobileFriendliness:=mftrsNOT_MOBILE_FRIENDLY;
      end;
      SetLength(data.res.mobileFriendlyIssues,0);
      if js.Field['mobileFriendlyIssues'] is TlkJSONlist then begin
        jsl:=js.Field['mobileFriendlyIssues'] as TlkJSONlist;
        n:=jsl.Count-1;
        for i := 0 to n do if jsl.Child[0] is TlkJSONobject then begin
          js0:=jsl.Child[0] as TlkJSONobject;
          if js0.Field['rule'] is TlkJSONstring then begin
            s:=(js0.Field['rule'] as TlkJSONstring).Value;
            rule:=mftrUNSPECIFIED;
            if s='USES_INCOMPATIBLE_PLUGINS' then rule:=mftrUSES_INCOMPATIBLE_PLUGINS else
            if s='CONFIGURE_VIEWPORT' then rule:=mftrCONFIGURE_VIEWPORT else
            if s='FIXED_WIDTH_VIEWPORT' then rule:=mftrFIXED_WIDTH_VIEWPORT else
            if s='SIZE_CONTENT_TO_VIEWPORT' then rule:=mftrSIZE_CONTENT_TO_VIEWPORT else
            if s='USE_LEGIBLE_FONT_SIZES' then rule:=mftrUSE_LEGIBLE_FONT_SIZES else
            if s='TAP_TARGETS_TOO_CLOSE' then rule:=mftrTAP_TARGETS_TOO_CLOSE;
            if rule<>mftrUNSPECIFIED then begin
              SetLength(data.res.mobileFriendlyIssues,Length(data.res.mobileFriendlyIssues)+1);
              data.res.mobileFriendlyIssues[Length(data.res.mobileFriendlyIssues)-1].rule:=rule;
            end;
          end;
        end;
      end;
      SetLength(data.res.resourceIssues,0);
      if js.Field['resourceIssues'] is TlkJSONlist then begin
        jsl:=js.Field['resourceIssues'] as TlkJSONlist;
        n:=jsl.Count-1;
        for i := 0 to n do if jsl.Child[0] is TlkJSONobject then begin
          js0:=jsl.Child[0] as TlkJSONobject;
          if js0.Field['blockedResource'] is TlkJSONobject then begin
            js0:=js0.Field['blockedResource'] as TlkJSONobject;
            if js0.Field['url'] is TlkJSONstring then begin
              SetLength(data.res.resourceIssues,Length(data.res.resourceIssues)+1);
              data.res.resourceIssues[Length(data.res.resourceIssues)-1].blockedResource.url:=
                (js0.Field['url'] as TlkJSONstring).Value;
            end;
          end;
        end;
      end;

      if js.Field['screenshot'] is TlkJSONobject then begin
        js0:=js.Field['screenshot'] as TlkJSONobject;
        if js0.Field['data'] is TlkJSONstring then begin
          _s:=AnsiString((js0.Field['data'] as TlkJSONstring).Value);
          Decoder := TIdDecoderMIME.Create(nil);
          try
            DestStream := TMemoryStream.Create;
            StreamSource := TMemoryStream.Create;
            png:=TPngImage.Create;
            bmp:=TBitmap.Create;
            try
              StreamSource.Write(PAnsiChar(_s)^,Length(_s));
              StreamSource.Position:=0;

              Decoder.DecodeBegin(DestStream);
              Decoder.Decode(StreamSource);
              Decoder.DecodeEnd;

              DestStream.Position:=0;
              png.LoadFromStream(DestStream);
              bmp.Assign(png);
              StreamSource.Clear;
              bmp.SaveToStream(StreamSource);
              StreamSource.Position:=0;
              SetLength(data.res.image,StreamSource.Size);
              j:=0;
              repeat
                n:=StreamSource.Read(buf,1024);
                for I := 0 to n-1 do begin
                  data.res.image[j]:=buf[i];
                  inc(j);
                end;
              until n=0;
            finally
              DestStream.Free;
              StreamSource.Free;
              png.Free;
              bmp.Free;
            end;
          finally
            Decoder.Free;
          end;
        end;
      end;
    end;
  end;
  response.Free;
  InData.Free;
  Http.IOHandler.Free;
  Http.Free;
end;

procedure TKRGoogleSearchConsole.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FGoogleAuth)then FGoogleAuth:= nil;
end;

procedure TKRGoogleSearchConsole.SetGoogleAuth(const Value: TKRGoogleAuth);
begin
  if FGoogleAuth<>Value then begin
    if Assigned(FGoogleAuth) then FGoogleAuth.RemoveFreeNotification(Self);
    FGoogleAuth := Value;
    if Assigned(FGoogleAuth) then begin
      FGoogleAuth.FreeNotification(Self);
      FGoogleAuth.Scope:=FGoogleAuth.Scope + [gscWebmaster];
    end;
  end;
end;

end.
