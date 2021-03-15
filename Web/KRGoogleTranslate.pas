(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleTranslate                                                         *)
(*  Ver.: 04.03.2021                                                          *)
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

  KRIdHTTP,

  uLkJSON;

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
  js, js0: TlkJSONObject;
  jsl: TlkJSONlist;
  i, n: integer;
  delimiter: Char;

begin
  Result:=false;
  FParams.Clear;

  if ADisplayLanguageCode<>'' then FParams.Add( 'display_language_code',  ADisplayLanguageCode );;
  data.url := GOOGLE_TRANSLATE_SERVICE_ENDPOINT + GOOGLE_TRANSLATE_ADVANCED_REST_RESOURCE +
    FProjectID + '/supportedLanguages' + getParams;

  js:=TlkJSONObject( KRRunInThread( @data, GetSupportedLanguagesTh ) );

  if not data.isOk then begin
    FLastErrorMsg := data.ErrorMsg;
    exit;
  end;

  if not Assigned( js ) then begin
    FLastErrorMsg := '[1]Response JSON Error!';
    exit;
  end;

  try
    jsl := TlkJSONlist( js.Field['languages'] );
    ASupportedLanguages.FCount := jsl.Count;
    n := ASupportedLanguages.FCount - 1;
    SetLength( ASupportedLanguages.list, ASupportedLanguages.FCount );
    for i := 0 to n do begin
      js0 := TlkJSONObject( jsl.Child[ i ] );
      ASupportedLanguages.list[ i ] := TKRGoogleSupportedLanguage.Create(
        js0.Field['languageCode'].Value,
        js0.Field['supportSource'].Value,
        js0.Field['supportTarget'].Value
      );
      if js0.IndexOfName( 'displayName' ) > -1 then
        ASupportedLanguages.list[ i ].FDisplayName := js0.Field['displayName'].Value;
    end;

    Result := true;
  except on E: Exception do
    FLastErrorMsg := E.Message;
  end;
end;

function TKRGoogleTranslate.GetSupportedLanguagesTh(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TStringStream;
  data: PKRGoogleRESTRequestData;
begin
  Result:=nil;
  data:=AData;

  Http:=HTTPCreate;
  Response := GET( http, data );

  if data.isOk then
    result := Pointer(TlkJSON.ParseText(Response.DataString));

  HTTPDestroy( Http );
  Response.Free;
end;

function TKRGoogleTranslate.Translate(AContents: TStrings; AMimeType,
  ASourceLanguage, ATargetLanguage: String;
  var ATranslations: TKRGoogleTranslations): boolean;
var
  data: TKRGoogleRESTRequestData;
  js, js0: TlkJSONobject;
  jsl: TlkJSONlist;
  i,n: integer;

begin
  Result:=false;
  FParams.Clear;
  js:=TlkJSONobject.Create;
  jsl:=TlkJSONlist.Create;
  try
    js.Add( 'mimeType', AMimeType);
    js.Add( 'targetLanguageCode', ATargetLanguage);
    if ASourceLanguage<>'' then
      js.Add( 'sourceLanguageCode', ASourceLanguage);

    n:=AContents.Count-1;
    for I := 0 to n do
      jsl.Add( AContents[i] );

    js.Add( 'contents', jsl);

    data.data := TlkJSON.GenerateText( js );
  finally
    js.Free;
  end;

  data.url := GOOGLE_TRANSLATE_SERVICE_ENDPOINT + GOOGLE_TRANSLATE_ADVANCED_REST_RESOURCE +
    FProjectID + ':translateText' + getParams;

  js:=TlkJSONObject( KRRunInThread( @data, TranslateTh ) );

  if not data.isOk then begin
    FLastErrorMsg := data.ErrorMsg;
    exit;
  end;

  if not Assigned( js ) then begin
    FLastErrorMsg := '[1]Response JSON Error!';
    exit;
  end;

  try
    jsl := TlkJSONlist( js.Field['translations'] );
    ATranslations.FCount := jsl.Count;
    n := ATranslations.FCount - 1;
    SetLength( ATranslations.list, ATranslations.FCount );
    for i := 0 to n do begin
      js0 := TlkJSONobject( jsl.Child[ i ] );

      ATranslations.list[ i ] :=
        TKRGoogleTranslation.Create(
          js0.Field[ 'translatedText' ].Value
        );
      if js0.IndexOfName( 'model' ) > -1 then ATranslations.list[i].FModel := js0.Field[ 'model' ].Value;
      if js0.IndexOfName( 'detectedLanguageCode' ) > -1 then ATranslations.list[i].FDetectedLanguageCode := js0.Field[ 'detectedLanguageCode' ].Value;
      if js0.IndexOfName( 'glossaryConfig' ) > -1 then begin

      end;

    end;

    Result := true;

  except on E: Exception do
    FLastErrorMsg := E.Message;
  end;

end;

function TKRGoogleTranslate.TranslateTh(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  Response: TStringStream;
  data: PKRGoogleRESTRequestData;
begin
  Result:=nil;
  data:=AData;

  Http:=HTTPCreate;

  Response := POST( http, data, 'application/json');

  if data.isOk then
    result := Pointer(TlkJSON.ParseText(Response.DataString));

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
