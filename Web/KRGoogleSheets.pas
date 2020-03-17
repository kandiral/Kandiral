(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleSheets                                                            *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleSheets;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.Forms, Web.HTTPApp, Vcl.Graphics,
    System.Variants,
  {$ELSE}
    Classes, SysUtils, Forms, HTTPApp, Graphics, Variants,
  {$IFEND}
  IdHTTP, IdSSL, IdSSLOpenSSL, IdLogEvent, IdCompressorZLib,
  KRGoogleAuth, KRRuntimeErrors, KRThread,
  uLkJSON;

type
  TGSMajorDimension = (gsmdROWS, gsmdCOLUMNS);
  TGSValueRender = (gsvrFORMATTED, gsvrUNFORMATTED, gsvrFORMULA);
  TGSDateTimeRender = (gsdtrSERIAL_NUMBER, gsdtrFORMATTED_STRING);
  TGSSheetType = (gsstGRID, gsstOBJECT);
  TGSValueInputOption = (gsvioRAW, gsvioUSER_ENTERED);

  TGSData = record
    AStr: String;
    AData: pointer;
  end;

  TGSRow = class
  private
    FCols: array of Variant;
    FRowIndex: integer;
    FSheet: String;
    FSheetID: Int64;
    function getCol(AIndex: integer): Variant;
    function GetCount: integer;
    procedure setCol(AIndex: integer; const Value: Variant);
  public
    constructor Create(ASheet: String; ASheetID: int64;ARowIndex: integer; ACols: array of Variant);
    property Col[AIndex: integer]: Variant read getCol write setCol; default;
    property RowIndex: integer read FRowIndex;
    property Sheet: String read FSheet;
    property SheetID: Int64 read FSheetID;
    property Count: integer read GetCount;
  end;

  TGSSample = class
  private
    FMajorDimension: TGSMajorDimension;
    FSheet: String;
    FRange: String;
    FValues: TList;
    FColCount: integer;
    FSheetID: Int64;
    function GetRowCount: integer;
    function GetValue(AIndex: integer): TGSRow;
  public
    constructor Create(ASheet, ARange: String; AMajorDimension: TGSMajorDimension);
    destructor Destroy;override;
    property Sheet: String read FSheet;
    property SheetID: Int64 read FSheetID;
    property Range: String read FRange;
    property MajorDimension: TGSMajorDimension read FMajorDimension;
    property Values[AIndex: integer]: TGSRow read GetValue; default;
    property RowCount: integer read GetRowCount;
    property ColCount: integer read FColCount;
    procedure DeleteRow(AIndex: integer);
    procedure Sort(Compare: TListSortCompare);
    procedure AddRow(ARow: TGSRow);
    procedure InsertRow(AIndex: integer; ARow: TGSRow);
  end;

  TGSGridProperties = class
  private
    FHideGridlines: boolean;
    FRowCount: integer;
    FColumnCount: integer;
    FFrozenRowCount: integer;
    FFrozenColumnCount: integer;
    FRowGroupControlAfter: boolean;
    FColumnGroupControlAfter: boolean;
  public
    property RowCount: integer read FRowCount;
    property ColumnCount: integer read FColumnCount;
    property FrozenRowCount: integer read FFrozenRowCount;
    property FrozenColumnCount: integer read FFrozenColumnCount;
    property HideGridlines: boolean read FHideGridlines;
    property RowGroupControlAfter: boolean read FRowGroupControlAfter;
    property ColumnGroupControlAfter: boolean read FColumnGroupControlAfter;
  end;

  TGSSheet = class
  private
    FSheetType: TGSSheetType;
    FGridProperties: TGSGridProperties;
    FTabColorAlpha: byte;
    FHidden: boolean;
    FTitle: String;
    FID: int64;
    FTabColor: TColor;
    FIndex: integer;
    FRightToLeft: boolean;
  public
    constructor Create;
    destructor Destroy;override;
    property ID: int64 read FID;
    property Title: String read FTitle;
    property SheetIndex: integer read FIndex;
    property SheetType: TGSSheetType read FSheetType;
    property GridProperties: TGSGridProperties read FGridProperties;
    property Hidden: boolean read FHidden;
    property TabColor: TColor read FTabColor;
    property TabColorAlpha: byte read FTabColorAlpha;
    property RightToLeft: boolean read FRightToLeft;
  end;

  TKRGoogleSheets = class(TComponent)
  private
    FGoogleAuth: TKRGoogleAuth;
    FIdLogs: TIdLogEvent;
    FReadOnly: boolean;
    FSpreadSheetID: String;
    FSheetsList: TList;
    procedure SetGoogleAuth(const Value: TKRGoogleAuth);
    procedure logRecv(ASender: TComponent; const AText, AData: string);
    procedure logSend(ASender: TComponent; const AText, AData: string);
    procedure logStat(ASender: TComponent; const AText: string);
    procedure AddLog(AText: STring);
    procedure SetReadOnly(const Value: boolean);
    function getHTTP: TIdHTTP;
    function HTTPGet(AThread: TThread; AData: Pointer): Pointer;
    function HTTPPost(AThread: TThread; AData: Pointer): Pointer;
    function HTTPPut(AThread: TThread; AData: Pointer): Pointer;
    procedure ClearSheetsList;
    function GetSheet(AIndex: integer): TGSSheet;
    function GetSheetsCount: integer;
    function getErrorMsg(E: EIdHTTPProtocolException): String;
    procedure SampleParse(ASample: TGSSample; AData: TlkJSONobject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Read(ASheet, ARange: String;
      AMajorDimension: TGSMajorDimension = gsmdROWS;
      AValueRender: TGSValueRender = gsvrUNFORMATTED;
      ADateTimeRender: TGSDateTimeRender = gsdtrSERIAL_NUMBER): TGSSample;

    function Write(ASample: TGSSample; AIncludeValuesInResponse: boolean = true;
      AValueInputOption: TGSValueInputOption = gsvioUSER_ENTERED;
      AValueRender: TGSValueRender = gsvrUNFORMATTED;
      ADateTimeRender: TGSDateTimeRender = gsdtrSERIAL_NUMBER): TGSSample;

    function batchUpdate(ARequest: TlkJSONobject): TlkJSONobject;

    function ReadListOfSheets: integer;
    property Sheets[AIndex: integer]: TGSSheet read GetSheet;
    property SheetsCount: integer read GetSheetsCount;

  published
    property GoogleAuth: TKRGoogleAuth read FGoogleAuth write SetGoogleAuth;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default true;
    property SpreadSheetID: String read FSpreadSheetID write FSpreadSheetID;
  end;

  EKRGoogleSheets = class(Exception)
  end;

implementation

uses lgop;

{ TGSRow }

constructor TGSRow.Create(ASheet: String; ASheetID: int64; ARowIndex: integer;
  ACols: array of Variant);
var
  i: integer;
begin
  FSheet:=ASheet;
  FSheetID:=ASheetID;
  FRowIndex:=ARowIndex;
  SetLength(FCols,Length(ACols));
  for I := 0 to Length(ACols)-1 do FCols[i]:=ACols[i];
end;

function TGSRow.getCol(AIndex: integer): Variant;
begin
  Result:=FCols[AIndex];
end;

function TGSRow.GetCount: integer;
begin
  Result:=Length(FCols);
end;

procedure TGSRow.setCol(AIndex: integer; const Value: Variant);
begin
  FCols[AIndex]:=Value;
end;

{ TGSSample }

procedure TGSSample.AddRow(ARow: TGSRow);
begin
  FValues.Add(TGSRow.Create(ARow.FSheet,ARow.SheetID,ARow.FRowIndex,ARow.FCols));
end;

constructor TGSSample.Create(ASheet, ARange: String; AMajorDimension: TGSMajorDimension);
begin
  FValues:=TList.Create;
  FSheet:=ASheet;
  FRange:=ARange;
  FMajorDimension:=AMajorDimension;
end;

procedure TGSSample.DeleteRow(AIndex: integer);
begin
  FValues.Delete(AIndex);
end;

destructor TGSSample.Destroy;
begin
  while FValues.Count>0 do begin
    TGSRow(FValues[0]).free;
    FValues.Delete(0);
  end;
  FValues.Free;
  inherited;
end;

function TGSSample.GetRowCount: integer;
begin
  Result:=FValues.Count;
end;

function TGSSample.GetValue(AIndex: integer): TGSRow;
begin
  Result:=FValues[AIndex];
end;

procedure TGSSample.InsertRow(AIndex: integer; ARow: TGSRow);
begin
  FValues.Insert(AIndex,TGSRow.Create(ARow.FSheet,ARow.SheetID,ARow.FRowIndex,ARow.FCols));
end;

procedure TGSSample.Sort(Compare: TListSortCompare);
begin
  FValues.Sort(Compare);
end;

{ TGSSheet }

constructor TGSSheet.Create;
begin
  FGridProperties:=TGSGridProperties.Create;
  FGridProperties.FHideGridlines:=false;
  FGridProperties.FRowCount:=0;
  FGridProperties.FColumnCount:=0;
  FGridProperties.FFrozenRowCount:=0;
  FGridProperties.FFrozenColumnCount:=0;
  FGridProperties.FRowGroupControlAfter:=false;
  FGridProperties.FColumnGroupControlAfter:=false;
end;

destructor TGSSheet.Destroy;
begin
  FGridProperties.Free;
  inherited;
end;

{ TKRGoogleSheets }

procedure TKRGoogleSheets.AddLog(AText: STring);
begin
  REAddLog(AText);
end;

function TKRGoogleSheets.batchUpdate(ARequest: TlkJSONobject): TlkJSONobject;
var
  data: TGSData;
  str, Response: TStringStream;
begin
  Result:=nil;
  data.AStr:='https://sheets.googleapis.com/v4/spreadsheets/'+FSpreadSheetID+':batchUpdate';
  if Assigned(FGoogleAuth) then data.AStr:=data.AStr+'?access_token='+FGoogleAuth.Token;

  str:=TStringStream.Create;
  str.WriteString(TlkJSON.GenerateText(ARequest));
  data.AData:=str;
  Response:=TStringStream(KRRunInThread(@data,HTTPPost));
  str.Free;

  if(data.AStr='')and(Response.Size>0)then begin
    Response.Position:=0;
    Result:=TlkJSON.ParseText(Response.DataString) as TlkJSONobject;
  end;
  Response.Free;

  if data.AStr<>'' then raise EKRGoogleSheets.Create(data.AStr);

end;

procedure TKRGoogleSheets.ClearSheetsList;
begin
  while FSheetsList.Count>0 do begin
    TGSSheet(FSheetsList[0]).Free;
    FSheetsList.Delete(0);
  end;
end;

constructor TKRGoogleSheets.Create(AOwner: TComponent);
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
  FReadOnly:=true;
  FSheetsList:=TList.Create;
end;

destructor TKRGoogleSheets.Destroy;
begin
  ClearSheetsList;
  FSheetsList.Free;
  inherited;
end;

function TKRGoogleSheets.getErrorMsg(E: EIdHTTPProtocolException): String;
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

function TKRGoogleSheets.getHTTP: TIdHTTP;
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

  Http.Request.UserAgent:='KRKRGoogleSheets ver. 1.0';
  Http.Request.Accept:='application/json';
  Http.Request.AcceptCharSet:='utf-8';
  Http.Request.CharSet:='utf-8';
  Http.Request.ContentType:='application/json';
//  Http.Request.CustomHeaders.Values['GData-Version']:='4.0';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  result:=http;
end;

function TKRGoogleSheets.GetSheet(AIndex: integer): TGSSheet;
begin
  result:=FSheetsList[AIndex];
end;

function TKRGoogleSheets.GetSheetsCount: integer;
begin
  Result:=FSheetsList.Count;
end;

function TKRGoogleSheets.HTTPGet(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
  err: String;
begin
  http:=getHTTP;

  err:='';
  Response:=TStringStream.Create;
  try
    Http.Get(TGSData(AData^).AStr,Response);
  except
    on E: EIdHTTPProtocolException do begin
      err:=getErrorMsg(E);
    end;
    on E: Exception do begin
      err:=E.Message;
    end;
  end;
  TGSData(AData^).AStr:=err;

  Http.IOHandler.Free;
  Http.Free;
  Result:=Response;
end;

function TKRGoogleSheets.HTTPPost(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
  err: STring;
begin
  http:=getHTTP;

  Response:=TStringStream.Create;
  err:='';
  try
    Http.Post(TGSData(AData^).AStr,TStream(TGSData(AData^).AData),Response);
  except
    on E: EIdHTTPProtocolException do begin
      err:=getErrorMsg(E);
    end;
    on E: Exception do begin
      err:=E.Message;
    end;
  end;
  TGSData(AData^).AStr:=err;

  Http.IOHandler.Free;
  Http.Free;
  Result:=Response;
end;

function TKRGoogleSheets.HTTPPut(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
  err: STring;
begin
  http:=getHTTP;

  Response:=TStringStream.Create;
  err:='';
  try
    Http.Put(TGSData(AData^).AStr,TStream(TGSData(AData^).AData),Response);
  except
    on E: EIdHTTPProtocolException do begin
      err:=getErrorMsg(E);
    end;
    on E: Exception do begin
      err:=E.Message;
    end;
  end;
  TGSData(AData^).AStr:=err;

  Http.IOHandler.Free;
  Http.Free;
  Result:=Response;
end;

procedure TKRGoogleSheets.logRecv(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_RECV');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleSheets.logSend(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_SEND');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleSheets.logStat(ASender: TComponent; const AText: string);
begin
  AddLog('HTTP_STAT: '+AText);
end;

procedure TKRGoogleSheets.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FGoogleAuth)then FGoogleAuth:= nil;
end;

function TKRGoogleSheets.Read(ASheet, ARange: String; AMajorDimension: TGSMajorDimension;
  AValueRender: TGSValueRender; ADateTimeRender: TGSDateTimeRender):TGSSample;
var
  res: TlkJSONobject;
  Response: TStringStream;
  data: TGSData;
  I: Integer;
begin
  data.AStr:='https://sheets.googleapis.com/v4/spreadsheets/'+FSpreadSheetID+'/values/';
  if ASheet='' then data.AStr:=data.AStr+ARange else
    data.AStr:=data.AStr+''''+String(HTTPEncode(Utf8Encode(ASheet)))+'''!'+ARange;
  data.AStr:=data.AStr+'?majorDimension='+
    _is(AMajorDimension=gsmdROWS,'ROWS','COLUMNS')+'&valueRenderOption='+
    _sel(Integer(AValueRender),['FORMATTED_VALUE','UNFORMATTED_VALUE','FORMULA']);
  if AValueRender<>gsvrFORMATTED then data.AStr:=data.AStr+'&dateTimeRenderOption='+
    _is(ADateTimeRender=gsdtrSERIAL_NUMBER,'SERIAL_NUMBER','FORMATTED_STRING');
  if Assigned(FGoogleAuth) then data.AStr:=data.AStr+'&access_token='+FGoogleAuth.Token;

  Response:=TStringStream(KRRunInThread(@data,HTTPGet));

  if(data.AStr='')and(Response.Size>0) then begin
    Response.Position:=0;
    res:=TlkJSON.ParseText(Response.DataString) as TlkJSONobject;
  end;
  Response.Free;

  if data.AStr<>'' then raise EKRGoogleSheets.Create(data.AStr);
  if res=nil then  raise EKRGoogleSheets.Create('Result error');

  Result:=TGSSample.Create(ASheet,'',gsmdROWS);
  for I := 0 to FSheetsList.Count-1 do
    if TGSSheet(FSheetsList[i]).FTitle=ASheet then begin
      Result.FSheetID:=TGSSheet(FSheetsList[i]).ID;
      break;
    end;

  SampleParse(Result,res);

  FreeAndNil(res);
end;

function TKRGoogleSheets.ReadListOfSheets: integer;
var
  jsl: TlkJSONcustomlist;
  res, js, js1: TlkJSONobject;
  i: integer;
  b: boolean;
  sheet: TGSSheet;
  Response: TStringStream;
  data: TGSData;
begin
  data.AStr:='https://sheets.googleapis.com/v4/spreadsheets/'+FSpreadSheetID+'?&fields=sheets.properties';
  if Assigned(FGoogleAuth) then data.AStr:=data.AStr+'&access_token='+FGoogleAuth.Token;

  ClearSheetsList;
  Response:=TStringStream(KRRunInThread(@data,HTTPGet));

  if(data.AStr='')and(Response.Size>0)then begin
    Response.Position:=0;
    res:=TlkJSON.ParseText(Response.DataString) as TlkJSONobject;
  end;

  Response.Free;

  if data.AStr<>'' then raise EKRGoogleSheets.Create(data.AStr);
  if res=nil then  raise EKRGoogleSheets.Create('Result error');

  jsl:=res.Field['sheets'] as TlkJSONcustomlist;
  for I := 0 to jsl.Count-1 do begin
    b:=false;
    sheet:=TGSSheet.Create;;
    try
      js:=jsl.Child[i] as TlkJSONobject;
      js:=js.Field['properties'] as TlkJSONobject;
      sheet.FID:=js.Field['sheetId'].Value;
      sheet.FTitle:=js.Field['title'].Value;
      sheet.FIndex:=js.Field['index'].Value;
      sheet.FSheetType:=_is(js.Field['sheetType'].Value='OBJECT', gsstOBJECT, gsstGRID);
      js1:=js.Field['gridProperties'] as TlkJSONobject;
      sheet.FGridProperties.FRowCount:=js1.Field['rowCount'].Value;
      sheet.FGridProperties.FColumnCount:=js1.Field['columnCount'].Value;
      if js1.Field['frozenRowCount']<>nil then
        sheet.FGridProperties.FFrozenRowCount:=js1.Field['frozenRowCount'].Value;
      if js1.Field['frozenColumnCount']<>nil then
        sheet.FGridProperties.ffrozenColumnCount:=js1.Field['frozenColumnCount'].Value;
      if js1.Field['hideGridlines']<>nil then
        sheet.FGridProperties.fhideGridlines:=js1.Field['hideGridlines'].Value;
      if js1.Field['rowGroupControlAfter']<>nil then
        sheet.FGridProperties.frowGroupControlAfter:=js1.Field['rowGroupControlAfter'].Value;
      if js1.Field['columnGroupControlAfter']<>nil then
        sheet.FGridProperties.fcolumnGroupControlAfter:=js1.Field['columnGroupControlAfter'].Value;
      if js.Field['hidden']<>nil then
        sheet.FHidden:=js.Field['hidden'].Value else sheet.FHidden:=false;
      if js.Field['tabColor']<>nil then begin
        js1:=js.Field['tabColor'] as TlkJSONobject;
        sheet.FTabColor:=0;
        if js1.Field['red']<>nil then sheet.FTabColor:=sheet.FTabColor or Round(255*js1.Field['red'].Value);
        if js1.Field['green']<>nil then sheet.FTabColor:=sheet.FTabColor or (Round(255*js1.Field['green'].Value) SHL 8);
        if js1.Field['blue']<>nil then sheet.FTabColor:=sheet.FTabColor or (Round(255*js1.Field['blue'].Value) SHL 16);
        sheet.FTabColorAlpha:=0;
        if js1.Field['alpha']<>nil then sheet.FTabColorAlpha:=Round(255*js1.Field['alpha'].Value);
      end else begin
        sheet.FTabColor:=0;
        sheet.FTabColorAlpha:=0;
      end;
      sheet.FRightToLeft:=false;
      if js.Field['rightToLeft']<>nil then sheet.FRightToLeft:=js.Field['rightToLeft'].Value;
      b:=true;
    finally
      if b then FSheetsList.Add(sheet) else sheet.Free;
    end;
  end;
  FreeAndNil(res);
  Result:=FSheetsList.Count;
end;

procedure TKRGoogleSheets.SampleParse(ASample: TGSSample; AData: TlkJSONobject);
var
  r: TGSRow;
  jsl, jsl1: TlkJSONcustomlist;
  i,j,q: integer;
  s: String;
begin
  s:=AData.Field['range'].Value;
  i:=Pos('!',s);
  if i>0 then begin
    ASample.FSheet:=Copy(s,1,i-1);
    if ASample.FSheet<>'' then
      if ASample.FSheet[1]='''' then ASample.FSheet:=Copy(s,2,i-3);
    ASample.FRange:=Copy(s,i+1,Length(s)-i);
  end else begin
    ASample.FSheet:='';
    ASample.FRange:=s;
  end;
  ASample.FMajorDimension:=_is(AData.Field['majorDimension'].Value='COLUMNS', gsmdCOLUMNS, gsmdROWS);

  ASample.FColCount:=0;
  jsl:=AData.Field['values'] as TlkJSONcustomlist;
  for I := 0 to jsl.Count-1 do begin
    r:=TGSRow.Create(ASample.FSheet,ASample.FSheetID,i+1,[]);
    jsl1:=jsl.Child[i] as TlkJSONcustomlist;

    if ASample.FColCount<jsl1.Count then begin
      for j := 0 to i-1 do begin
        SetLength(ASample.Values[j].FCols, jsl1.Count);
        for q := ASample.FColCount to jsl1.Count-1 do ASample.Values[j].FCols[q]:=null;
      end;
      ASample.FColCount:=jsl1.Count;
    end;

    SetLength(r.FCols,ASample.FColCount);
    for j := 0 to ASample.FColCount-1 do
      if j>=jsl1.Count then r.FCols[j]:=null
      else r.FCols[j]:=jsl1.Child[j].Value;
    ASample.FValues.Add(r);
  end;
end;

procedure TKRGoogleSheets.SetGoogleAuth(const Value: TKRGoogleAuth);
begin
  if FGoogleAuth<>Value then begin
    if Assigned(FGoogleAuth) then FGoogleAuth.RemoveFreeNotification(Self);
    FGoogleAuth := Value;
    if Assigned(FGoogleAuth) then begin
      FGoogleAuth.FreeNotification(Self);
      SetReadOnly(FReadOnly);
    end;
  end;
end;

procedure TKRGoogleSheets.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if Assigned(FGoogleAuth) then begin
    FGoogleAuth.Scope:=FGoogleAuth.Scope - [gscSheets, gscSheetsRead];
    if FReadOnly then
      FGoogleAuth.Scope:=FGoogleAuth.Scope + [gscSheetsRead]
    else
      FGoogleAuth.Scope:=FGoogleAuth.Scope + [gscSheets]
  end;
end;

function TKRGoogleSheets.Write(ASample: TGSSample; AIncludeValuesInResponse: boolean;
  AValueInputOption: TGSValueInputOption; AValueRender: TGSValueRender;
  ADateTimeRender: TGSDateTimeRender): TGSSample;
var
  Response: TStringStream;
  data, res: TlkJSONobject;
  jsl,jsl1: TlkJSONlist;
  i,j: integer;
  s: String;
  gsdata: TGSData;
  str: TStringStream;
begin
  gsdata.AStr:='https://sheets.googleapis.com/v4/spreadsheets/'+FSpreadSheetID+'/values/';
  if ASample.Sheet='' then gsdata.AStr:=gsdata.AStr+ASample.Range else
    gsdata.AStr:=gsdata.AStr+''''+String(HTTPEncode(Utf8Encode(ASample.Sheet)))+'''!'+ASample.Range;
  gsdata.AStr:=gsdata.AStr+'?valueInputOption='+_is(AValueInputOption=gsvioRAW,'RAW','USER_ENTERED')+
    '&includeValuesInResponse='+_is(AIncludeValuesInResponse,'true','false');
  if AIncludeValuesInResponse then gsdata.AStr:=gsdata.AStr+'&responseValueRenderOption='+
    _sel(Integer(AValueRender),['FORMATTED_VALUE','UNFORMATTED_VALUE','FORMULA']);
  if AIncludeValuesInResponse and(AValueRender<>gsvrFORMATTED)then gsdata.AStr:=gsdata.AStr+'&responseDateTimeRenderOption='+
    _is(ADateTimeRender=gsdtrSERIAL_NUMBER,'SERIAL_NUMBER','FORMATTED_STRING');
  if Assigned(FGoogleAuth) then gsdata.AStr:=gsdata.AStr+'&access_token='+FGoogleAuth.Token;

  data:=TlkJSON.ParseText('{}') as TlkJSONobject;
  s:='';
  if ASample.Sheet<>'' then s:=s+''''+ASample.Sheet+'''!';
  s:=s+ASample.Range;
  data.Add('range',s);
  data.Add('majorDimension',String(_is(ASample.MajorDimension=gsmdROWS,'ROWS','COLUMNS')));

  jsl:=TlkJSON.ParseText('[]') as TlkJSONlist;
  for I := 0 to ASample.RowCount-1 do begin
    jsl1:=TlkJSON.ParseText('[]') as TlkJSONlist;
    for j := 0 to ASample.Values[i].Count-1 do
      if VarIsNull(ASample.Values[i][j]) then jsl1.Add(TlkJSONnull.Create) else
      if VarIsStr(ASample.Values[i][j]) then jsl1.Add(String(ASample.Values[i][j])) else
      if varIsType(ASample.Values[i][j], varBoolean) then jsl1.Add(boolean(ASample.Values[i][j])) else
      if VarIsOrdinal(ASample.Values[i][j]) then jsl1.Add(Integer(ASample.Values[i][j])) else
      if VarIsFloat(ASample.Values[i][j]) then jsl1.Add(Double(ASample.Values[i][j]));
    jsl.Add(jsl1);
  end;
  data.add('values',jsl);

  str:=TStringStream.Create;

  str.WriteString(TlkJSON.GenerateText(data));

  gsdata.AData:=str;
  Response:=TStringStream(KRRunInThread(@gsdata,HTTPPut));
  data.Free;
  str.Free;

  if(gsdata.AStr='')and(Response.Size>0) then begin
    Response.Position:=0;
    res:=TlkJSON.ParseText(Response.DataString) as TlkJSONobject;
  end;
  Response.Free;

  if gsdata.AStr<>'' then raise EKRGoogleSheets.Create(gsdata.AStr);
  if res=nil then  raise EKRGoogleSheets.Create('Result error');

  if AIncludeValuesInResponse then begin
    Result:=TGSSample.Create('','',gsmdROWS);
    SampleParse(Result,res.Field['updatedData'] as TlkJSONobject);
  end else Result:=nil;

  FreeAndNil(res);
end;

end.
