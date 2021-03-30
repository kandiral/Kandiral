(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleTranslate                                                         *)
(*  Ver.: 30.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleTranslate;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$IFEND}

  KRThread, KRGoogleCommon, KRGoogleAuth, KRGoogleAPI,

  KRIdHTTP, KRJSON;

const
  GOOGLE_TRANSLATE_SERVICE_ENDPOINT = 'https://translation.googleapis.com';
  GOOGLE_TRANSLATE_ADVANCED_REST_RESOURCE = '/v3/projects/';


type
  TKRGoogleTranslateTextGlossaryConfig = class
  private
    FIgnoreCase: boolean;
    FGlossary: string;
  public
    constructor Create( AGlossary: String = ''; AIgnoreCase: boolean = false);
    property Glossary: string read FGlossary;
    property IgnoreCase: boolean read FIgnoreCase;
  end;

  TKRGoogleTranslation = class
  private
    FGlossaryConfig: TKRGoogleTranslateTextGlossaryConfig;
    FModel: string;
    FDetectedLanguageCode: string;
    FTranslatedText: string;
  public
    constructor Create( ATranslatedText: String = ''; AModel: String = '';
      ADetectedLanguageCode: String = ''; AGlossaryConfig: TKRGoogleTranslateTextGlossaryConfig = nil );
    destructor Destroy; override;
    property TranslatedText: string read FTranslatedText;
    property Model: string read FModel;
    property DetectedLanguageCode: string read FDetectedLanguageCode;
    property GlossaryConfig: TKRGoogleTranslateTextGlossaryConfig read FGlossaryConfig;
  end;

  TKRGoogleTranslations = class
  private
    list: array of TKRGoogleTranslation;
    FCount: integer;
    function GetItem(Index: integer): TKRGoogleTranslation;
  public
    destructor Destroy; override;
    property Count: integer read FCount;
    property Items[ Index: integer ]: TKRGoogleTranslation read GetItem; default;
  end;

  TKRGoogleSupportedLanguage = class
  private
    FDisplayName: string;
    FSupportSource: boolean;
    FLanguageCode: string;
    FSupportTarget: boolean;
  public
    constructor Create( ALanguageCode: String = ''; ASupportSource: boolean = false;
      ASupportTarget: boolean = false; ADisplayName: String = '' );
    property LanguageCode: string read FLanguageCode;
    property DisplayName: string read FDisplayName;
    property SupportSource: boolean read FSupportSource;
    property SupportTarget: boolean read FSupportTarget;
  end;

  TKRGoogleSupportedLanguages = class
  private
    list: array of TKRGoogleSupportedLanguage;
    FCount: integer;
    function GetItem(Index: integer): TKRGoogleSupportedLanguage;
  public
    destructor Destroy; override;
    property Count: integer read FCount;
    property Items[ Index: integer ]: TKRGoogleSupportedLanguage read GetItem; default;
  end;

  TKRGoogleTranslate = class( TKRGoogleAPI )
  private
    function TranslateTh(AThread: TThread; AData: Pointer): Pointer;
    function GetSupportedLanguagesTh(AThread: TThread; AData: Pointer): Pointer;
  public
    constructor Create(AOwner: TComponent); override;
    function Translate(AContents: TStrings; AMimeType, ASourceLanguage, ATargetLanguage: String;
      var ATranslations: TKRGoogleTranslations): boolean;
    function GetSupportedLanguages( var ASupportedLanguages: TKRGoogleSupportedLanguages;
      ADisplayLanguageCode: String = '' ): boolean;
  published
    property GoogleAuth;
    property ProjectID;
    property APIKey;
  end;

implementation

{ TKRGoogleTranslate }

constructor TKRGoogleTranslate.Create(AOwner: TComponent);
begin
  inherited;
  FNeedScopes := [ gscCloudPlatform, gscCloudTranslation ];
end;

function TKRGoogleTranslate.GetSupportedLanguages( var ASupportedLanguages: TKRGoogleSupportedLanguages;
  ADisplayLanguageCode: String): boolean;
var
  data: TKRGoogleRESTRequestData;

  js,jso: TKRJSONObject;
  jsa: TKRJSONArray;
  i, n, m: integer;

begin
  Result:=false;
  FParams.Clear;

  if ADisplayLanguageCode<>'' then FParams.Add( 'display_language_code',  ADisplayLanguageCode );;
  data.url := GOOGLE_TRANSLATE_SERVICE_ENDPOINT + GOOGLE_TRANSLATE_ADVANCED_REST_RESOURCE +
    FProjectID + '/supportedLanguages' + getParams;

  js:=TKRJSONObject( KRRunInThread( @data, GetSupportedLanguagesTh ) );

  if not data.isOk then begin
    FLastErrorMsg := data.ErrorMsg;
    exit;
  end;

  if not Assigned( js ) then begin
    FLastErrorMsg := '[1]Response JSON Error!';
    exit;
  end;

  try
    i:=js.IndexOf('languages');
    if (i>-1) and (js[i].ValueType=jstArray) then begin
      jsa := TKRJSONArray(js[i]);
      ASupportedLanguages.FCount := jsa.Count;
      m := ASupportedLanguages.FCount - 1;
      SetLength( ASupportedLanguages.list, ASupportedLanguages.FCount );
      n:=0;
      for i := 0 to m do
        if jsa[i].ValueType=jstObject then begin
          jso:=TKRJSONObject(jsa[i]);
          ASupportedLanguages.list[ n ] := TKRGoogleSupportedLanguage.Create(
            jso.GetStringDef('languageCode',''),
            jso.GetBooleanDef('supportSource',false),
            jso.GetBooleanDef('supportTarget',false),
            jso.GetStringDef( 'displayName', '' )
          );
          inc(n);
        end;
      ASupportedLanguages.FCount := n;
      SetLength( ASupportedLanguages.list, ASupportedLanguages.FCount );
      Result := true;
    end;
  except on E: Exception do
    FLastErrorMsg := E.Message;
  end;
  js.Free;
end;

function TKRGoogleTranslate.GetSupportedLanguagesTh(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TMemoryStream;
  data: PKRGoogleRESTRequestData;
begin
  Result:=nil;
  data:=AData;

  Http:=HTTPCreate;
  Response := GET( http, data );

  if data.isOk then begin
    Result := Pointer(TKRJSON.Parse(Response.Memory,Response.Size));
    data.isOk := Result <> nil;
    if data.isOk and ( TKRJSON(Result).ValueType<>jstObject ) then begin
      TKRJSON(Result).Free;
      data.isOk:=false;
    end;
  end;

  HTTPDestroy( Http );
  Response.Free;
end;

function TKRGoogleTranslate.Translate(AContents: TStrings; AMimeType,
  ASourceLanguage, ATargetLanguage: String;
  var ATranslations: TKRGoogleTranslations): boolean;
var
  data: TKRGoogleRESTRequestData;
  js, jso: TKRJSONObject;
  jsa: TKRJSONArray;
  i,n,m: integer;
  s: String;
  sa: AnsiString;
begin
  Result:=false;
  FParams.Clear;

  js:=TKRJSONobject.Create;
  js.AddChild( TKRJSONString.Create('mimeType', AMimeType ));
  js.AddChild( TKRJSONString.Create('targetLanguageCode', ATargetLanguage ));
  if ASourceLanguage<>'' then
    js.AddChild( TKRJSONString.Create('sourceLanguageCode', ASourceLanguage ));

  jsa:=TKRJSONArray.Create;
  jsa.Key:='contents';
  n:=AContents.Count-1;
  for I := 0 to n do begin
    s:=Trim( AContents[i] );
    if s<>'' then begin
      inc(n);
      jsa.AddChild( TKRJSONString.Create( s ) );
    end;
  end;
  js.AddChild(jsa);
  if n=0 then begin
    js.Free;
    FLastErrorMsg := '[1]Empty data!';
    exit;
  end;
  data.sData:=TMemoryStream.Create;
  sa:=js.toString;
  data.sData.Write( sa[1], Length(sa) );
  js.Free;

  data.url := GOOGLE_TRANSLATE_SERVICE_ENDPOINT + GOOGLE_TRANSLATE_ADVANCED_REST_RESOURCE +
    FProjectID + ':translateText' + getParams;

  js:=TKRJSONObject( KRRunInThread( @data, TranslateTh ) );
  data.sData.Free;
  if not data.isOk then begin
    FLastErrorMsg := data.ErrorMsg;
    if FLastErrorMsg='' then
      FLastErrorMsg := '[1]Response JSON Error!';
    exit;
  end;

  try
    i:=js.IndexOf('translations');
    if(i>-1)and(js[i].ValueType=jstArray) then begin
      jsa := TKRJSONArray( js[i] );
      ATranslations.FCount := jsa.Count;
      m := ATranslations.FCount - 1;
      SetLength( ATranslations.list, ATranslations.FCount );
      n:=0;
      for i := 0 to m do
        if jsa[i].ValueType=jstObject then begin
          jso := TKRJSONObject( jsa[ i ] );
          ATranslations.list[ n ] :=
            TKRGoogleTranslation.Create(
              jso.GetStringDef( 'translatedText' , '' ),
              jso.GetStringDef( 'model' , '' ),
              jso.GetStringDef( 'detectedLanguageCode' , '' )
            );
          {if js0.IndexOfName( 'glossaryConfig' ) > -1 then begin

          end;}
          inc( n );
        end;
      ATranslations.FCount := n;
      SetLength( ATranslations.list, ATranslations.FCount );
      Result := true;
    end;
  except on E: Exception do
    FLastErrorMsg := E.Message;
  end;
  js.free;
end;

function TKRGoogleTranslate.TranslateTh(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TMemoryStream;
  data: PKRGoogleRESTRequestData;
begin
  Result:=nil;
  data:=AData;

  Http:=HTTPCreate;

  Response := POST( http, data, 'application/json');

  if data.isOk then begin
    Result := Pointer(TKRJSON.Parse(Response.Memory,Response.Size));
    data.isOk := Result <> nil;
    if data.isOk and ( TKRJSON(Result).ValueType<>jstObject ) then begin
      TKRJSON(Result).Free;
      data.isOk:=false;
    end;
  end;

  HTTPDestroy( Http );
  Response.Free;
end;

{ TKRGoogleTranslateTextGlossaryConfig }

constructor TKRGoogleTranslateTextGlossaryConfig.Create(AGlossary: String;
  AIgnoreCase: boolean);
begin
  FGlossary := AGlossary;
  FIgnoreCase := AIgnoreCase;
end;

{ TKRGoogleTranslation }

constructor TKRGoogleTranslation.Create(ATranslatedText, AModel,
  ADetectedLanguageCode: String;
  AGlossaryConfig: TKRGoogleTranslateTextGlossaryConfig);
begin
  FTranslatedText := ATranslatedText;
  FModel := AModel;
  FDetectedLanguageCode := ADetectedLanguageCode;
  FGlossaryConfig := AGlossaryConfig;
end;

destructor TKRGoogleTranslation.Destroy;
begin
  if Assigned( FGlossaryConfig ) then FGlossaryConfig.Free;
  inherited;
end;

{ TKRGoogleTranslations }

destructor TKRGoogleTranslations.Destroy;
var
  i, n: integer;
begin
  n := FCount - 1;
  for I := 0 to n do list[i].Free;
  inherited;
end;

function TKRGoogleTranslations.GetItem(Index: integer): TKRGoogleTranslation;
begin
  Result:=list[ index ];
end;

{ TKRGoogleSupportedLanguage }

constructor TKRGoogleSupportedLanguage.Create(ALanguageCode: String; ASupportSource,
  ASupportTarget: boolean; ADisplayName: String);
begin
  FLanguageCode := ALanguageCode;
  FDisplayName := ADisplayName;
  FSupportSource := ASupportSource;
  FSupportTarget := ASupportTarget;
end;

{ TKRGoogleSupportedLanguages }

destructor TKRGoogleSupportedLanguages.Destroy;
var
  i, n: integer;
begin
  n := FCount - 1;
  for I := 0 to n do list[i].Free;
  inherited;
end;

function TKRGoogleSupportedLanguages.GetItem(
  Index: integer): TKRGoogleSupportedLanguage;
begin
  Result:=list[ index ];
end;

end.
