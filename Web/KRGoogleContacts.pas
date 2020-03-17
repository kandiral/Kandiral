(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleContacts                                                          *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleContacts;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, Vcl.Dialogs,
    Xml.XMLDoc, Xml.XMLIntf, Winapi.ActiveX, System.Variants, Vcl.Graphics,
    Vcl.Imaging.pngimage,
  {$ELSE}
    Windows, Classes, SysUtils, StrUtils, Dialogs, XMLDoc, XMLIntf, ActiveX,
    Variants, Graphics, PngImage,
  {$IFEND}
    IdHTTP, IdSSL, IdSSLOpenSSL, IdCompressorZLib, IdLogEvent,
    KRGoogleAuth, KRThread, KRRuntimeErrors,
    ISO3166, Funcs;

type
  TKRGoogleContacts = class;
  TGoogleContact = class;

  TGoogleContactPostalAddressType = (gpatWork, gpatHome, gpatOther);
  TGoogleContactPostalAddress = class
  private
    contact: TGoogleContact;
    FLabel: String;
    FRel: TGoogleContactPostalAddressType;
    FAgent: String;
    FStreet: String;
    FHousename: String;
    FCountry: TISO3166;
    FNeighborhood: String;
    FPostcode: String;
    FCity: String;
    FPobox: String;
    FRegion: String;
    procedure SetLabel(const Value: String);
    procedure SetRel(const Value: TGoogleContactPostalAddressType);
    procedure SetAgent(const Value: String);
    procedure SetCity(const Value: String);
    procedure SetCountry(const Value: TISO3166);
    procedure SetHousename(const Value: String);
    procedure SetNeighborhood(const Value: String);
    procedure SetPobox(const Value: String);
    procedure SetPostcode(const Value: String);
    procedure SetRegion(const Value: String);
    procedure SetStreet(const Value: String);
  public
    procedure Assign(APostalAddress: TGoogleContactPostalAddress);
    property rel: TGoogleContactPostalAddressType read FRel write SetRel;
    property AddressLabel: String read FLabel write SetLabel;
    property Agent: String read FAgent write SetAgent;
    property Housename: String read FHousename write SetHousename;
    property Street: String read FStreet write SetStreet;
    property Pobox: String read FPobox write SetPobox;
    property Neighborhood: String read FNeighborhood write SetNeighborhood;
    property City: String read FCity write SetCity;
    property Region: String read FRegion write SetRegion;
    property Postcode: String read FPostcode write SetPostcode;
    property Country: TISO3166 read FCountry write SetCountry;
    procedure SetPrimary;
    function FormattedAddress: String;
  end;

  TGoogleContactOrganizationType = (gotOther, gotWork);
  TGoogleContactOrganization = class
  private
    contact: TGoogleContact;
    FOrgName: String;
    FRel: TGoogleContactOrganizationType;
    FLabel: String;
    FOrgTitle: String;
    procedure SetLabel(const Value: String);
    procedure SetOrgName(const Value: String);
    procedure SetOrgTitle(const Value: String);
    procedure SetRel(const Value: TGoogleContactOrganizationType);
  public
    procedure Assign(AOrganization: TGoogleContactOrganization);
    property rel: TGoogleContactOrganizationType read FRel write SetRel;
    property OrgLabel: String read FLabel write SetLabel;
    property OrgName: String read FOrgName write SetOrgName;
    property OrgTitle: String read FOrgTitle write SetOrgTitle;
    procedure SetPrimary;
    function getFormatted: String;
  end;

  TGoogleContactPhoneType = (gptAssistant, gptCallback, gptCar, gptCompanyMain,
    gptFax, gptHome, gptHomeFax, gptIsdn, gptMain, gptMobile, gptOther, gptOtherFax,
    gptPager, gptRadio, gptTelex, gptTtyTdd, gptWork, gptWorkFax, gptWorkMobile,
    gptWorkPager);
  TGoogleContactPhone = class
  private
    contact: TGoogleContact;
    FPhoneType: TGoogleContactPhoneType;
    FPhoneUri: String;
    FPhoneText: String;
    FPhoneLabel: String;
    procedure SetPhoneLabel(const Value: String);
    procedure SetPhoneText(const Value: String);
    procedure SetPhoneType(const Value: TGoogleContactPhoneType);
  public
    procedure Assign(APhone: TGoogleContactPhone);
    property PhoneText: String read FPhoneText write SetPhoneText;
    property PhoneUri: String read FPhoneUri;
    property PhoneType: TGoogleContactPhoneType read FPhoneType write SetPhoneType;
    property PhoneLable: String read FPhoneLabel write SetPhoneLabel;
    procedure SetPrimary;
  end;

  TGoogleContactEMailType = (gmtHome, gmtOther, gmtWork);
  TGoogleContactEMail = class
  private
    contact: TGoogleContact;
    FDisplayName: String;
    FRel: TGoogleContactEMailType;
    FLabel: String;
    FAddress: String;
    procedure SetAddress(const Value: String);
    procedure SetDisplayName(const Value: String);
    procedure SetLabel(const Value: String);
    procedure SetRel(const Value: TGoogleContactEMailType);
  public
    procedure Assign(AEmail: TGoogleContactEMail);
    property Address: String read FAddress write SetAddress;
    property DisplayName: String read FDisplayName write SetDisplayName;
    property EmailLabel: String read FLabel write SetLabel;
    property Rel: TGoogleContactEMailType read FRel write SetRel;
    procedure SetPrimary;
  end;

  TGoogleContactPhoto = class
  private
    contact: TGoogleContact;
    FETag, Fhref, FURL,_etag: String;
    bmp: TBitmap;
    function getBitmap: TBitmap;
    procedure setBitmap(const Value: TBitmap);
    function DeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function LoadDo(AThread: TThread; AData: Pointer): Pointer;
    function getEtagDo(AThread: TThread; AData: Pointer): Pointer;
  public
    procedure Assign(APhoto: TGoogleContactPhoto);
    destructor Destroy; override;
    property Bitmap: TBitmap read getBitmap write setBitmap;
  end;

  TGoogleContactName = class
  private
    contact: TGoogleContact;
    FNameSuffix: String;
    FFamilyName: String;
    FGivenName: String;
    FAdditionalName: String;
    FNamePrefix: String;
    procedure SetAdditionalName(const Value: String);
    procedure SetFamilyName(const Value: String);
    procedure SetGivenName(const Value: String);
    procedure SetNamePrefix(const Value: String);
    procedure SetNameSuffix(const Value: String);
  public
    procedure Assign(AName: TGoogleContactName);
    property GivenName: String read FGivenName write SetGivenName;
    property AdditionalName: String read FAdditionalName write SetAdditionalName;
    property FamilyName: String read FFamilyName write SetFamilyName;
    property NamePrefix: String read FNamePrefix write SetNamePrefix;
    property NameSuffix: String read FNameSuffix write SetNameSuffix;
    function fullName: String;
  end;

  TGoogleContact = class(TObject)
  private
    contacts: TKRGoogleContacts;
    FID, FID_, FETag, fxml, FURL: String;
    FName: TGoogleContactName;
    FPhoto: TGoogleContactPhoto;
    FUpdated: TDateTime;
    FModified: boolean;

    FEmailList, FPhoneList, FOrganizationList, FPostalAddressList: TList;
    FNotes: String;
    FGroupID: String;
    FDeleted: boolean;
    function GetEmailCount: integer;
    function GetEmail(AIndex: Integer): TGoogleContactEMail;
    function GetPhone(AIndex: Integer): TGoogleContactPhone;
    function GetPhoneCount: integer;
    procedure SetNotes(const Value: String);
    function UpdateDo(AThread: TThread; AData: Pointer): Pointer;
    function CreateDo(AThread: TThread; AData: Pointer): Pointer;
    function RetrieveDo(AThread: TThread; AData: Pointer): Pointer;
    function GetOrganization(AIndex: Integer): TGoogleContactOrganization;
    function GetOrganizationCount: integer;
    function GetPostalAddress(AIndex: Integer): TGoogleContactPostalAddress;
    function GetPostalAddressCount: integer;
    procedure SetGroupID(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AContact: TGoogleContact);
    property ContactsList: TKRGoogleContacts read contacts;
    property isModified: boolean read FModified;
    function Update: boolean;
    function Retrieve: boolean;
    property id: String read FID;
    property isDeleted: boolean read FDeleted;
    property Updated: TDateTime read FUpdated;
    property GroupID: String read FGroupID write SetGroupID;
    property Name: TGoogleContactName read FName;
    property Notes: String read FNotes write SetNotes;
    property Photo: TGoogleContactPhoto read FPhoto;
    property EmailCount: integer read GetEmailCount;
    property Emails[AIndex: Integer]: TGoogleContactEMail read GetEmail;
    function EmailAdd(AAddress: String; ARel: TGoogleContactEMailType;
      ADisplayName: String = ''; ALabel: String = ''): TGoogleContactEMail;
    procedure EmailDelete(AIndex: integer);
    property PhoneCount: integer read GetPhoneCount;
    property Phones[AIndex: Integer]: TGoogleContactPhone read GetPhone;
    function PhoneAdd(APhone: String; ARel: TGoogleContactPhoneType;
      ALabel: String = ''): TGoogleContactPhone;
    procedure PhoneDelete(AIndex: integer);
    property OrganizationCount: integer read GetOrganizationCount;
    property Organizations[AIndex: Integer]: TGoogleContactOrganization read GetOrganization;
    function OrganizationAdd(AOrgName: String; ARel: TGoogleContactOrganizationType;
      AOrgTitle: String = ''; ALabel: String = ''): TGoogleContactOrganization;
    procedure OrganizationDelete(AIndex: integer);
    property PostalAddressCount: integer read GetPostalAddressCount;
    property PostalAddress[AIndex: Integer]: TGoogleContactPostalAddress read GetPostalAddress;
    function PostalAddressAdd(ACountry: TISO3166; ACity: String; ARel: TGoogleContactPostalAddressType;
      AStreet: String = ''; APostcode: String = ''; ALabel: String = ''): TGoogleContactPostalAddress;
    procedure PostalAddressDelete(AIndex: integer);
  end;

  TKRGoogleContactsGroup = class
  private
    contacts: TKRGoogleContacts;
    FID, FID_, FETag, fxml, FURL: String;
    FUpdated: TDateTime;
    FTitle: String;
    FSystemGroup: boolean;
    FModified: boolean;
    FSystemGroupID: String;
    function RetrieveDo(AThread: TThread; AData: Pointer): Pointer;
    procedure SetTitle(const Value: String);
    function UpdateDo(AThread: TThread; AData: Pointer): Pointer;
    function CreateDo(AThread: TThread; AData: Pointer): Pointer;
  public
    property id: String read FID;
    property isModified: boolean read FModified;
    property SystemGroup: boolean read FSystemGroup;
    property SystemGroupID: String read FSystemGroupID;
    property Title: String read FTitle write SetTitle;
    property Updated: TDateTime read FUpdated;
    function Update: boolean;
    function Retrieve: boolean;
  end;

  TKRGoogleContacts = class(TComponent)
  private
    FGoogleAuth: TKRGoogleAuth;
    FURL: String;
    FContacts, FContactsTmp: TList;
    FGroups, FGroupsTmp: TList;
    FIdLogs: TIdLogEvent;
    FEMailID: String;
    FRetrievingDeleted: boolean;
    procedure SetGoogleAuth(const Value: TKRGoogleAuth);
    function GetCount: integer;
    function RetrievingAllDo(AThread: TThread; AData: Pointer): Pointer;
    function RetrievingGroupsDo(AThread: TThread; AData: Pointer): Pointer;
    function DeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function GroupDeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function GetContact(Index: integer): TGoogleContact;
    procedure logRecv(ASender: TComponent; const AText, AData: string);
    procedure logSend(ASender: TComponent; const AText, AData: string);
    procedure logStat(ASender: TComponent; const AText: string);
    procedure AddLog(AText: STring);
    procedure parseUserData(entry: IXMLNode; cntct: TGoogleContact);
    procedure userDataToXml(AXML: String; cntct: TGoogleContact; AStream: TStream);
    procedure parseGroupData(entry: IXMLNode; grp: TKRGoogleContactsGroup);
    procedure groupDataToXml(AXML: String; grp: TKRGoogleContactsGroup; AStream: TStream);
    function getHTTP: TIdHTTP;
    procedure checkError(e: EIdHTTPProtocolException);
    function GetGroup(AIndex: Integer): TKRGoogleContactsGroup;
    function GetGroupCount: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RetrievingDeleted: boolean read FRetrievingDeleted write FRetrievingDeleted;
    function RetrievingAll: integer;
    procedure UpdateModified;
    function Add: TGoogleContact;
    function Delete(AContact: TGoogleContact): boolean;overload;
    function Delete(AIndex: integer): boolean;overload;
    property Count: integer read GetCount;
    property Contacts[Index: integer]: TGoogleContact read GetContact;

    function RetrievingGroups: integer;
    property GroupCount: integer read GetGroupCount;
    property Groups[AIndex: Integer]: TKRGoogleContactsGroup read GetGroup;
    function GroupAdd(ATitle: String): TKRGoogleContactsGroup;
    function GroupDelete(AGroup: TKRGoogleContactsGroup): boolean;overload;
    function GroupDelete(AIndex: integer): boolean;overload;

  published
    property GoogleAuth: TKRGoogleAuth read FGoogleAuth write SetGoogleAuth;
  end;

implementation

function HTTPEncode(const AStr: AnsiString): AnsiString;
// The NoConversion set contains characters as specificed in RFC 1738 and
// should not be modified unless the standard changes.
const
  NoConversion = ['A'..'Z','a'..'z','*'{,'@'},'.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PAnsiChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PAnsiChar(AStr);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      {if Sp^ = ' ' then
        Rp^ := '+'
      else}
      begin
        FormatBuf(Rp^, 3, AnsiString('%%%.2x'), 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

function DateTimeTOgcDate(adt: TDateTime): String;
var
  yy,mm,dd,hh,mn,ss,ms: Word;
  SystemTime: TSystemTime;
  _d: TDateTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    _d:=EncodeDate(wYear, wMonth, wDay)+EncodeTime(wHour, wMinute, wSecond, wMilliseconds)-now;
  DecodeDate(adt+_d,yy,mm,dd);
  DecodeTime(adt+_d,hh,mn,ss,ms);
  Result:=IntToStrL(yy,4)+'-'+IntToStrL(mm,2)+'-'+IntToStrL(dd,2)+'T'+
    IntToStrL(hh,2)+':'+IntToStrL(mn,2)+':'+IntToStrL(ss,2)+'.'+
    IntToStrL(ms,3)+'Z';
end;

function gcDateToDateTime(adt: String): TDateTime;
var
  sl: TStringList;
  dt,tm: STring;
  yy,mm,dd,hh,mn,ss,ms: Word;
  SystemTime: TSystemTime;
  _d: TDateTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    _d:=EncodeDate(wYear, wMonth, wDay)+EncodeTime(wHour, wMinute, wSecond, wMilliseconds)-now;
  sl:=Funcs.Explode('T',adt);
  dt:=sl[0];
  tm:=sl[1];
  sl.Free;
  sl:=Funcs.Explode('-',dt);
  yy:=StrToInt(sl[0]);
  mm:=StrToInt(sl[1]);
  dd:=StrToInt(sl[2]);
  sl.Free;
  sl:=Funcs.Explode(':',tm);
  hh:=StrToInt(sl[0]);
  mn:=StrToInt(sl[1]);
  tm:=sl[2];
  sl.Free;
  if tm[Length(tm)]='Z' then tm:=copy(tm,1,Length(tm)-1);
  sl:=Funcs.Explode('.',tm);
  ss:=StrToInt(sl[0]);
  if sl.Count>1 then ms:=StrToInt(sl[1]) else ms:=0;
  sl.Free;
  Result:=EncodeDate(yy,mm,dd)+EncodeTime(hh,mn,ss,ms)-_d;
end;

function gcStrToEmailType(rel: String): TGoogleContactEMailType;
begin
  result:=gmtOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#home'))then result:=gmtHome
  else if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gmtWork;
end;

function EmailTypeTOgcStr(tp: TGoogleContactEMailType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  case tp of
    gmtHome: result:='http://schemas.google.com/g/2005#home';
    gmtWork: result:='http://schemas.google.com/g/2005#work';
  end;
end;

function gcStrToPhoneType(rel: String): TGoogleContactPhoneType;
begin
  result:=gptOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#assistant'))then result:=gptAssistant
  else if(SameText(rel,'http://schemas.google.com/g/2005#callback'))then result:=gptCallback
  else if(SameText(rel,'http://schemas.google.com/g/2005#car'))then result:=gptCar
  else if(SameText(rel,'http://schemas.google.com/g/2005#company_main'))then result:=gptCompanyMain
  else if(SameText(rel,'http://schemas.google.com/g/2005#fax'))then result:=gptFax
  else if(SameText(rel,'http://schemas.google.com/g/2005#home'))then result:=gptHome
  else if(SameText(rel,'http://schemas.google.com/g/2005#home_fax'))then result:=gptHomeFax
  else if(SameText(rel,'http://schemas.google.com/g/2005#isdn'))then result:=gptIsdn
  else if(SameText(rel,'http://schemas.google.com/g/2005#main'))then result:=gptMain
  else if(SameText(rel,'http://schemas.google.com/g/2005#mobile'))then result:=gptMobile
  else if(SameText(rel,'http://schemas.google.com/g/2005#other_fax'))then result:=gptOtherFax
  else if(SameText(rel,'http://schemas.google.com/g/2005#pager'))then result:=gptPager
  else if(SameText(rel,'http://schemas.google.com/g/2005#radio'))then result:=gptRadio
  else if(SameText(rel,'http://schemas.google.com/g/2005#telex'))then result:=gptTelex
  else if(SameText(rel,'http://schemas.google.com/g/2005#tty_tdd'))then result:=gptTtyTdd
  else if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gptWork
  else if(SameText(rel,'http://schemas.google.com/g/2005#work_fax'))then result:=gptWorkFax
  else if(SameText(rel,'http://schemas.google.com/g/2005#work_mobile'))then result:=gptWorkMobile
  else if(SameText(rel,'http://schemas.google.com/g/2005#work_pager'))then result:=gptWorkPager
end;

function PhoneTypeTOgcStr(tp: TGoogleContactPhoneType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  case tp of
    gptAssistant: result:='http://schemas.google.com/g/2005#assistant';
    gptCallback: result:='http://schemas.google.com/g/2005#callback';
    gptCar: result:='http://schemas.google.com/g/2005#car';
    gptCompanyMain: result:='http://schemas.google.com/g/2005#company_main';
    gptFax: result:='http://schemas.google.com/g/2005#fax';
    gptHome: result:='http://schemas.google.com/g/2005#home';
    gptHomeFax: result:='http://schemas.google.com/g/2005#home_fax';
    gptIsdn: result:='http://schemas.google.com/g/2005#isdn';
    gptMain: result:='http://schemas.google.com/g/2005#main';
    gptMobile: result:='http://schemas.google.com/g/2005#mobile';
    gptOtherFax: result:='http://schemas.google.com/g/2005#other_fax';
    gptPager: result:='http://schemas.google.com/g/2005#pager';
    gptRadio: result:='http://schemas.google.com/g/2005#radio';
    gptTelex: result:='http://schemas.google.com/g/2005#telex';
    gptTtyTdd: result:='http://schemas.google.com/g/2005#tty_tdd';
    gptWork: result:='http://schemas.google.com/g/2005#work';
    gptWorkFax: result:='http://schemas.google.com/g/2005#work_fax';
    gptWorkMobile: result:='http://schemas.google.com/g/2005#work_mobile';
    gptWorkPager: result:='http://schemas.google.com/g/2005#work_pager';
  end;
end;

function gcStrToOrganizationType(rel: String): TGoogleContactOrganizationType;
begin
  result:=gotOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gotWork;
end;

function OrganizationTypeTOgcStr(rel: TGoogleContactOrganizationType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  if(rel=gotWork)then result:='http://schemas.google.com/g/2005#work';
end;

function gcStrToPostalAddressType(rel: String): TGoogleContactPostalAddressType;
begin
  result:=gpatOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gpatWork
  else if(SameText(rel,'http://schemas.google.com/g/2005#home'))then result:=gpatHome;
end;

function PostalAddressTypeTOgcStr(rel: TGoogleContactPostalAddressType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  case rel of
    gpatWork: result:='http://schemas.google.com/g/2005#work';
    gpatHome: result:='http://schemas.google.com/g/2005#home';
  end;
end;

{ TKRGoogleContacts }

function TKRGoogleContacts.Add: TGoogleContact;
begin
  Result:=TGoogleContact.Create;
  Result.FModified:=true;
  Result.contacts:=self;
  Result.FGroupID:='6';
  FContacts.Add(result);
end;

procedure TKRGoogleContacts.AddLog(AText: STring);
begin
  REAddLog(AText);
end;

procedure TKRGoogleContacts.checkError(e: EIdHTTPProtocolException);
begin

end;

constructor TKRGoogleContacts.Create(AOwner: TComponent);
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
  FContacts:=TList.Create;
  FContactsTmp:=TList.Create;
  FGroups:=TList.Create;
  FGroupsTmp:=TList.Create;
end;

function TKRGoogleContacts.Delete(AIndex: integer): boolean;
var s: String;
begin
  result:=true;
  if TGoogleContact(FContacts[AIndex]).FID<>'' then begin
    FURL:='https://www.google.com/m8/feeds/contacts/default/full/'+
      TGoogleContact(FContacts[AIndex]).FID+'?access_token='+FGoogleAuth.Token;
    s:=TGoogleContact(FContacts[AIndex]).FETag;
    result:=Integer(KRRunInThread(Pointer(PChar(s)),DeleteDo))<>0;
  end;
  TGoogleContact(FContacts[AIndex]).Free;
  FContacts.Delete(AIndex);
end;

function TKRGoogleContacts.DeleteDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
begin
  http:=getHTTP;
  Http.Request.CustomHeaders.Values['If-Match']:=String(PChar(AData));

  Result:=Pointer(1);
  try
    Http.Delete(FURL);
  except
    Result:=Pointer(0);
  end;

  Http.IOHandler.Free;
  Http.Free;
end;

function TKRGoogleContacts.Delete(AContact: TGoogleContact): boolean;
var
  i: integer;
begin
  Result:=false;
  for I := 0 to FContacts.Count-1 do
    if Pointer(AContact)=FContacts[i] then begin
      result:=Delete(i);
      break;
    end;
end;

destructor TKRGoogleContacts.Destroy;
var
  i: integer;
begin
  for I := 0 to FContacts.Count-1 do TGoogleContact(FContacts[i]).Free;
  FContacts.Free;
  FContactsTmp.Free;
  for I := 0 to FGroups.Count-1 do TKRGoogleContactsGroup(FGroups[i]).Free;
  FGroups.Free;
  FGroupsTmp.Free;
  if Assigned(FIdLogs) then FIdLogs.Free;
  inherited;
end;

function TKRGoogleContacts.GetContact(Index: integer): TGoogleContact;
begin
  Result:=TGoogleContact(FContacts[Index]);
end;

function TKRGoogleContacts.GetCount: integer;
begin
  Result:=FContacts.Count;
end;

function TKRGoogleContacts.GetGroup(AIndex: Integer): TKRGoogleContactsGroup;
begin
  result:=TKRGoogleContactsGroup(FGroups[AIndex]);
end;

function TKRGoogleContacts.GetGroupCount: integer;
begin
  Result:=FGroups.Count;
end;

function TKRGoogleContacts.getHTTP: TIdHTTP;
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

  Http.Request.UserAgent:='KRKRGoogleContacts ver. 1.0';
  Http.Request.Accept:='application/atom+xml';
  Http.Request.AcceptCharSet:='utf-8';
  Http.Request.CharSet:='utf-8';
  Http.Request.ContentType:='application/atom+xml';
  Http.Request.CustomHeaders.Values['GData-Version']:='3.0';

  Http.ConnectTimeout:=10000;
  Http.ReadTimeout:=0;

  result:=http;
end;

function TKRGoogleContacts.GroupAdd(ATitle: String): TKRGoogleContactsGroup;
begin
  Result:=TKRGoogleContactsGroup.Create;
  Result.FTitle:=ATitle;
  Result.FModified:=true;
  Result.contacts:=self;
  FGroups.Add(result);
end;

procedure TKRGoogleContacts.groupDataToXml(AXML: String;
  grp: TKRGoogleContactsGroup; AStream: TStream);
var
  xml: TXMLDocument;
  RootNode, title, content, appEdited: IXMLNode;
  i: integer;
begin
  CoInitialize(nil);
  xml:=TXMLDocument.Create(self);
  try
    xml.LoadFromXML(AXML);
    xml.Active:=true;
    RootNode:=xml.DocumentElement;
    title:=nil;
    appEdited:=nil;
    content:=nil;

    for i := 0 to RootNode.ChildNodes.Count - 1 do begin
      if SameText(RootNode.ChildNodes[i].NodeName,'title') then
        title:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'content') then
        content:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'app:edited') then
        appEdited:=RootNode.ChildNodes[i]
    end;

    if content<>nil then RootNode.ChildNodes.Remove(content);
    if appEdited<>nil then RootNode.ChildNodes.Remove(appEdited);
    if title=nil then begin
      title:=RootNode.AddChild('title');
    end;
    title.Text:=grp.Title;
    xml.SaveToStream(astream);
  finally
    xml.Free;
    CoUninitialize;
  end;
end;

function TKRGoogleContacts.GroupDelete(AGroup: TKRGoogleContactsGroup): boolean;
var
  i: integer;
begin
  Result:=false;
  for I := 0 to FGroups.Count-1 do
    if Pointer(AGroup)=FGroups[i] then begin
      result:=GroupDelete(i);
      break;
    end;
end;

function TKRGoogleContacts.GroupDelete(AIndex: integer): boolean;
var s: String;
begin
  result:=true;
  if TKRGoogleContactsGroup(FGroups[AIndex]).FID<>'' then begin
    FURL:='https://www.google.com/m8/feeds/groups/default/full/'+
      TKRGoogleContactsGroup(FGroups[AIndex]).FID+'?access_token='+FGoogleAuth.Token;
    s:=TKRGoogleContactsGroup(FGroups[AIndex]).FETag;
    result:=Integer(KRRunInThread(Pointer(PChar(s)),GroupDeleteDo))<>0;
  end;
  TKRGoogleContactsGroup(FGroups[AIndex]).Free;
  FGroups.Delete(AIndex);
end;

function TKRGoogleContacts.GroupDeleteDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
begin
  http:=getHTTP;
  Http.Request.CustomHeaders.Values['If-Match']:=String(PChar(AData));

  Result:=Pointer(1);
  try
    Http.Delete(FURL);
  except
    Result:=Pointer(0);
  end;

  Http.IOHandler.Free;
  Http.Free;
end;

procedure TKRGoogleContacts.logRecv(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_RECV');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleContacts.logSend(ASender: TComponent; const AText,
  AData: string);
begin
  AddLog('HTTP_SEND');
  if AText<>'' then AddLog(AText);
  if AData<>'' then AddLog(AData);
end;

procedure TKRGoogleContacts.logStat(ASender: TComponent; const AText: string);
begin
  AddLog('HTTP_STAT: '+AText);
end;

procedure TKRGoogleContacts.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FGoogleAuth)then FGoogleAuth:= nil;
end;

procedure TKRGoogleContacts.parseGroupData(entry: IXMLNode;
  grp: TKRGoogleContactsGroup);
var
  j: integer;
  sl: TStringList;
begin
  grp.fxml:=entry.XML;
  if entry.HasAttribute('gd:etag') then
    grp.FETag:=entry.Attributes['gd:etag'];
  grp.FID_:=entry.ChildNodes['id'].Text;
  sl:=Funcs.Explode('/',grp.FID_);
  grp.FID:=sl[sl.Count-1];
  grp.FUpdated:=gcDateToDateTime(entry.ChildNodes['updated'].Text);
  grp.FSystemGroup:=false;
  for j := 0 to entry.ChildNodes.Count-1 do begin
    if SameText(entry.ChildNodes[j].NodeName,'title') then begin
      grp.FTitle:=entry.ChildNodes[j].Text;
    end else if SameText(entry.ChildNodes[j].NodeName,'gContact:systemGroup') then begin
      grp.FSystemGroup:=true;
      grp.FSystemGroupID:=entry.ChildNodes[j].Attributes['id'];
    end;
  end;
end;

procedure TKRGoogleContacts.parseUserData(entry: IXMLNode; cntct: TGoogleContact);
var
  i,j,q: integer;
  email: TGoogleContactEmail;
  phone: TGoogleContactPhone;
  organization: TGoogleContactOrganization;
  PostalAddress: TGoogleContactPostalAddress;
  sl: TStringList;
  s,s0,s1,s2,s3,s4: string;
  tmp: IXMLNode;
  Country: TISO3166;
  FAgent: String;
  FHousename: String;
  FNeighborhood: String;
  FPobox: String;
  FRegion: String;
begin
  for I := 0 to cntct.FEmailList.Count-1 do TGoogleContactEMail(cntct.FEmailList[i]).Free;
  cntct.FEmailList.Clear;
  for I := 0 to cntct.FPhoneList.Count-1 do TGoogleContactPhone(cntct.FPhoneList[i]).Free;
  cntct.FPhoneList.Clear;
  for I := 0 to cntct.FOrganizationList.Count-1 do TGoogleContactOrganization(cntct.FOrganizationList[i]).Free;
  cntct.FOrganizationList.Clear;
  for I := 0 to cntct.FPostalAddressList.Count-1 do TGoogleContactPostalAddress(cntct.FPostalAddressList[i]).Free;
  cntct.FPostalAddressList.Clear;
  cntct.fxml:=entry.XML;
  cntct.FETag:=entry.Attributes['gd:etag'];
  cntct.FID_:=entry.ChildNodes['id'].Text;
  sl:=Funcs.Explode('/',cntct.FID_);
  cntct.FID:=sl[sl.Count-1];
  cntct.FDeleted:=false;
  sl.Free;
  cntct.FUpdated:=gcDateToDateTime(entry.ChildNodes['updated'].Text);
  for j := 0 to entry.ChildNodes.Count-1 do begin
    if SameText(entry.ChildNodes[j].NodeName,'gd:name') then begin
      tmp:=entry.ChildNodes[j];
      for q := 0 to tmp.ChildNodes.Count-1 do
        if SameText(tmp.ChildNodes[q].NodeName,'gd:givenName') then
          cntct.Name.fgivenName:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:additionalName') then
          cntct.Name.fadditionalName:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:familyName') then
          cntct.Name.ffamilyName:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:namePrefix') then
          cntct.Name.fnamePrefix:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:nameSuffix') then
          cntct.Name.fnameSuffix:=tmp.ChildNodes[q].Text
    end else if SameText(entry.ChildNodes[j].NodeName,'gd:deleted') then begin
      cntct.FDeleted:=true
    end else if SameText(entry.ChildNodes[j].NodeName,'gContact:groupMembershipInfo') then begin
      sl:=Funcs.Explode('/',entry.ChildNodes[j].Attributes['href']);
      cntct.FGroupID:=sl[sl.Count-1];
      sl.Free;
    end else if SameText(entry.ChildNodes[j].NodeName,'link') then begin
      s:=entry.ChildNodes[j].Attributes['rel'];
      if SameText(s,'http://schemas.google.com/contacts/2008/rel#photo') then begin
        if entry.ChildNodes[j].HasAttribute('gd:etag') then
          cntct.FPhoto.FETag:=entry.ChildNodes[j].Attributes['gd:etag'];
        cntct.FPhoto.Fhref:=entry.ChildNodes[j].Attributes['href'];
      end;
    end else if SameText(entry.ChildNodes[j].NodeName,'gd:email') then begin
      tmp:=entry.ChildNodes[j];
      if tmp.HasAttribute('address')then begin
        if tmp.HasAttribute('rel') then s:=tmp.Attributes['rel'] else s:='';
        email:=cntct.EmailAdd(tmp.Attributes['address'],gcStrToEmailType(s));
        if tmp.HasAttribute('displayName') then email.FDisplayName:=tmp.Attributes['displayName'];
        if tmp.HasAttribute('label') then email.Flabel:=tmp.Attributes['label'];
        if tmp.HasAttribute('primary') then
          if SameText(LowerCase(tmp.Attributes['primary']),'true') then email.SetPrimary;
      end;
    end else if SameText(entry.ChildNodes[j].NodeName,'gd:organization') then begin
      tmp:=entry.ChildNodes[j];
      if tmp.HasAttribute('rel') then s:=tmp.Attributes['rel'] else s:='';
      s0:='';
      s1:='';
      for q := 0 to tmp.ChildNodes.Count-1 do
        if SameText(tmp.ChildNodes[q].NodeName,'gd:orgName') then
          s0:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:orgTitle') then
          s1:=tmp.ChildNodes[q].Text;
      organization:=cntct.OrganizationAdd(s0,gcStrToOrganizationType(s),s1);
      if tmp.HasAttribute('label') then organization.Flabel:=tmp.Attributes['label'];
      if tmp.HasAttribute('primary') then
        if SameText(LowerCase(tmp.Attributes['primary']),'true') then organization.SetPrimary;
    end else if SameText(entry.ChildNodes[j].NodeName,'gd:structuredPostalAddress') then begin
      tmp:=entry.ChildNodes[j];
      if tmp.HasAttribute('rel') then s:=tmp.Attributes['rel'] else s:='';
      s0:='';
      s1:='';
      s2:='';
      FAgent:='';
      FHousename:='';
      FNeighborhood:='';
      FPobox:='';
      FRegion:='';
      Country:=ISO3166List[0];
      for q := 0 to tmp.ChildNodes.Count-1 do
        if SameText(tmp.ChildNodes[q].NodeName,'gd:country') then begin
          if tmp.ChildNodes[q].HasAttribute('code')then
            Country:=ISO3166_byAlpha2StrCode(tmp.ChildNodes[q].Attributes['code']);
        end else if SameText(tmp.ChildNodes[q].NodeName,'gd:city') then
          s0:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:street') then
          s1:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:postcode') then
          s2:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:agent') then
          FAgent:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:housename') then
          FHousename:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:neighborhood') then
          FNeighborhood:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:pobox') then
          FPobox:=tmp.ChildNodes[q].Text
        else if SameText(tmp.ChildNodes[q].NodeName,'gd:region') then
          FRegion:=tmp.ChildNodes[q].Text;
      PostalAddress:=cntct.PostalAddressAdd(Country,s0,gcStrToPostalAddressType(s),s1,s2);
      PostalAddress.FAgent:=FAgent;
      PostalAddress.FHousename:=FHousename;
      PostalAddress.FNeighborhood:=FNeighborhood;
      PostalAddress.FPobox:=FPobox;
      PostalAddress.FRegion:=FRegion;
      if tmp.HasAttribute('label') then PostalAddress.Flabel:=tmp.Attributes['label'];
      if tmp.HasAttribute('primary') then
        if SameText(LowerCase(tmp.Attributes['primary']),'true') then PostalAddress.SetPrimary;
    end else if SameText(entry.ChildNodes[j].NodeName,'gd:phoneNumber') then begin
      tmp:=entry.ChildNodes[j];
      if tmp.HasAttribute('rel') then s:=tmp.Attributes['rel'] else s:='';
      phone:=cntct.PhoneAdd(tmp.Text,gcStrToPhoneType(s));
      if tmp.HasAttribute('uri') then phone.FPhoneUri:=tmp.Attributes['uri'];
      if tmp.HasAttribute('label') then phone.FPhoneLabel:=tmp.Attributes['label'];
      if tmp.HasAttribute('primary') then
        if SameText(LowerCase(tmp.Attributes['primary']),'true') then phone.SetPrimary;
    end else if SameText(entry.ChildNodes[j].NodeName,'content') then begin
      cntct.FNotes:=entry.ChildNodes[j].Text;
    end;
  end;
end;

function TKRGoogleContacts.RetrievingAll: integer;
var
  i: integer;
begin
  Result:=0;
  for I := 0 to FContacts.Count-1 do TGoogleContact(FContacts[i]).Free;
  FContacts.Clear;
  if FGoogleAuth=nil then exit;
  if FGoogleAuth.Token='' then FGoogleAuth.Login;
  if FGoogleAuth.Token='' then exit;
  FURL:='https://www.google.com/m8/feeds/contacts/default/full?';
  if FRetrievingDeleted then FURL:=FURL+'showdeleted=true&';
  FURL:=FURL+'access_token='+FGoogleAuth.Token;

  KRRunInThread(nil,RetrievingAllDo);
  for I := 0 to FContactsTmp.Count-1 do FContacts.Add(FContactsTmp[i]);
  Result:=FContacts.Count;
end;

function TKRGoogleContacts.RetrievingAllDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  i, cnt, totalResults, itemsPerPage: integer;

  procedure prs;
  var
    i: integer;
    entry: IXMLNode;
    cntct: TGoogleContact;
  begin
    for i := 0 to RootNode.ChildNodes.Count - 1 do begin
      if SameText(RootNode.ChildNodes[i].NodeName,'entry') then begin
        entry:=RootNode.ChildNodes[i];
        if SameText(entry.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#contact') then begin
          cntct:=TGoogleContact.Create;
          cntct.contacts:=Self;
          parseUserData(entry,cntct);

          cntct.FModified:=false;
          FContactsTmp.Add(cntct);
        end;
      end;
    end;
  end;

begin
  FContactsTmp.Clear;

  http:=getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Get(FURL,Response);
  except end;

  if Response.Size>0 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(Self);
    try
      cnt:=0;
      Response.Position:=0;
//Response.SaveToFile('d:\'+self.FGoogleAuth.Name+'_'+IntToStr(cnt)+'.xml');
      xml.LoadFromStream(Response);
      xml.Active := true;


      RootNode := xml.DocumentElement;
      totalResults:=0;
      itemsPerPage:=0;

      for i := 0 to RootNode.ChildNodes.Count - 1 do begin
        if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:totalResults') then
          totalResults:=StrToIntDef(RootNode.ChildNodes[i].Text,0)
        else if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:itemsPerPage') then
          itemsPerPage:=StrToIntDef(RootNode.ChildNodes[i].Text,0)
        else if SameText(RootNode.ChildNodes[i].NodeName,'id') then
          if FEMailID='' then FEMailID:=String(HTTPEncode(AnsiString(RootNode.ChildNodes[i].Text)));
      end;

      prs;
      cnt:=cnt+itemsPerPage;
      while cnt<totalResults do begin
        Response.Clear;
        try
          Http.Get(FURL+'&start-index='+IntToStr(cnt+1),Response);
        except end;
        if Response.Size=0 then break;
        xml.Active:=false;
        Response.Position:=0;
//Response.SaveToFile('d:\'+self.FGoogleAuth.Name+'_'+IntToStr(cnt)+'.xml');
        xml.LoadFromStream(Response);
        xml.Active := true;
        RootNode := xml.DocumentElement;
        for i := 0 to RootNode.ChildNodes.Count - 1 do
          if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:itemsPerPage') then
            itemsPerPage:=StrToIntDef(RootNode.ChildNodes[i].Text,0);

        prs;
        cnt:=cnt+itemsPerPage;

      end;

    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
  response.Free;
  Http.IOHandler.Free;
  Http.Free;
  Result:=nil;
end;

function TKRGoogleContacts.RetrievingGroups: integer;
var
  i: integer;
begin
  Result:=0;
  for I := 0 to FGroups.Count-1 do TKRGoogleContactsGroup(FGroups[i]).Free;
  FGroups.Clear;
  if FGoogleAuth=nil then exit;
  if FGoogleAuth.Token='' then FGoogleAuth.Login;
  if FGoogleAuth.Token='' then exit;
  FURL:='https://www.google.com/m8/feeds/groups/default/full?access_token='+
    FGoogleAuth.Token;

  KRRunInThread(nil,RetrievingGroupsDo);
  for I := 0 to FGroupsTmp.Count-1 do FGroups.Add(FGroupsTmp[i]);
  Result:=FGroups.Count;
end;

function TKRGoogleContacts.RetrievingGroupsDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  i, cnt, totalResults, itemsPerPage: integer;

  procedure prs;
  var
    i: integer;
    entry: IXMLNode;
    grp: TKRGoogleContactsGroup;
  begin
    for i := 0 to RootNode.ChildNodes.Count - 1 do begin
      if SameText(RootNode.ChildNodes[i].NodeName,'entry') then begin
        entry:=RootNode.ChildNodes[i];
        if SameText(entry.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#group') then begin
          grp:=TKRGoogleContactsGroup.Create;
          grp.contacts:=Self;
          parseGroupData(entry,grp);

          grp.FModified:=false;
          FGroupsTmp.Add(grp);
        end;
      end;
    end;
  end;

begin
  Result:=nil;

  FGroupsTmp.Clear;

  http:=getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Get(FURL,Response);
  except end;

  if Response.Size>0 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(Self);
    try
      cnt:=0;
      Response.Position:=0;
//Response.SaveToFile('d:\'+self.FGoogleAuth.Name+'_groups_'+IntToStr(cnt)+'.xml');
      xml.LoadFromStream(Response);
      xml.Active := true;


      RootNode := xml.DocumentElement;
      totalResults:=0;
      itemsPerPage:=0;

      for i := 0 to RootNode.ChildNodes.Count - 1 do begin
        if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:totalResults') then
          totalResults:=StrToIntDef(RootNode.ChildNodes[i].Text,0)
        else if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:itemsPerPage') then
          itemsPerPage:=StrToIntDef(RootNode.ChildNodes[i].Text,0)
        else if SameText(RootNode.ChildNodes[i].NodeName,'id') then
          if FEMailID='' then FEMailID:=String(HTTPEncode(AnsiString(RootNode.ChildNodes[i].Text)));
      end;

      prs;
      cnt:=cnt+itemsPerPage;
      while cnt<totalResults do begin
        Response.Clear;
        try
          Http.Get(FURL+'&start-index='+IntToStr(cnt+1),Response);
        except end;
        if Response.Size=0 then break;
        xml.Active:=false;
        Response.Position:=0;
//Response.SaveToFile('d:\'+self.FGoogleAuth.Name+'_groups_'+IntToStr(cnt)+'.xml');
        xml.LoadFromStream(Response);
        xml.Active := true;
        RootNode := xml.DocumentElement;
        for i := 0 to RootNode.ChildNodes.Count - 1 do
          if SameText(RootNode.ChildNodes[i].NodeName,'openSearch:itemsPerPage') then
            itemsPerPage:=StrToIntDef(RootNode.ChildNodes[i].Text,0);

        prs;
        cnt:=cnt+itemsPerPage;

      end;

    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
  response.Free;
  Http.IOHandler.Free;
  Http.Free;
end;

procedure TKRGoogleContacts.SetGoogleAuth(const Value: TKRGoogleAuth);
begin
  if FGoogleAuth<>Value then begin
    if Assigned(FGoogleAuth) then FGoogleAuth.RemoveFreeNotification(Self);
    FGoogleAuth := Value;
    if Assigned(FGoogleAuth) then begin
      FGoogleAuth.FreeNotification(Self);
      FGoogleAuth.Scope:=FGoogleAuth.Scope + [gscContacts];
    end;
  end;
end;

procedure TKRGoogleContacts.UpdateModified;
begin

end;

procedure TKRGoogleContacts.userDataToXml(AXML: String; cntct: TGoogleContact;
  AStream: TStream);
var
  xml: TXMLDocument;
  RootNode, tmp, title, nm, content, appEdited, grp: IXMLNode;
  phns, mls, orgs, adrs: array of IXMLNode;
  i: integer;
begin
  CoInitialize(nil);
  xml:=TXMLDocument.Create(self);
  try
    xml.LoadFromXML(AXML);
    xml.Active:=true;
    RootNode:=xml.DocumentElement;
    title:=nil;
    nm:=nil;
    appEdited:=nil;
    content:=nil;
    grp:=nil;
    for i := 0 to RootNode.ChildNodes.Count - 1 do begin
      if SameText(RootNode.ChildNodes[i].NodeName,'title') then
        title:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'content') then
        content:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'app:edited') then
        appEdited:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'gd:name') then
        nm:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'gContact:groupMembershipInfo') then
        grp:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'gd:email') then begin
        SetLength(mls,Length(mls)+1);
        mls[Length(mls)-1]:=RootNode.ChildNodes[i];
      end else if SameText(RootNode.ChildNodes[i].NodeName,'gd:organization') then begin
        SetLength(orgs,Length(orgs)+1);
        orgs[Length(orgs)-1]:=RootNode.ChildNodes[i];
      end else if SameText(RootNode.ChildNodes[i].NodeName,'gd:structuredPostalAddress') then begin
        SetLength(adrs,Length(adrs)+1);
        adrs[Length(adrs)-1]:=RootNode.ChildNodes[i];
      end else if SameText(RootNode.ChildNodes[i].NodeName,'gd:phoneNumber') then begin
        SetLength(phns,Length(phns)+1);
        phns[Length(phns)-1]:=RootNode.ChildNodes[i];
      end;
    end;

    if title<>nil then RootNode.ChildNodes.Remove(title);
    if appEdited<>nil then RootNode.ChildNodes.Remove(appEdited);
    if cntct.Name.fullName<>'' then begin
      if nm=nil then nm:=RootNode.AddChild('gd:name') else nm.ChildNodes.Clear;
      if cntct.Name.FgivenName<>'' then nm.AddChild('gd:givenName').Text:=cntct.Name.FgivenName;
      if cntct.Name.FAdditionalName<>'' then nm.AddChild('gd:additionalName').Text:=cntct.Name.FadditionalName;
      if cntct.Name.FFamilyName<>'' then nm.AddChild('gd:familyName').Text:=cntct.Name.FfamilyName;
      if cntct.Name.FnamePrefix<>'' then nm.AddChild('gd:namePrefix').Text:=cntct.Name.FnamePrefix;
      if cntct.Name.FnameSuffix<>'' then nm.AddChild('gd:nameSuffix').Text:=cntct.Name.FnameSuffix;
    end else if nm<>nil then RootNode.ChildNodes.Remove(nm);
    if cntct.FNotes<>'' then begin
      if content=nil then begin
        content:=RootNode.AddChild('gd:name');
        content.Attributes['type']:='text';
      end;
      content.Text:=cntct.FNotes;
    end else if content<>nil then RootNode.ChildNodes.Remove(content);
    if cntct.FGroupID<>'' then begin
      if grp=nil then
        grp:=RootNode.AddChild('gContact:groupMembershipInfo');
      grp.Attributes['deleted']:='false';
      grp.Attributes['href']:='http://www.google.com/m8/feeds/groups/'+FEMailID+'/base/'+cntct.FGroupID;
    end else if grp<>nil then RootNode.ChildNodes.Remove(grp);
    for I := 0 to Length(mls)-1 do RootNode.ChildNodes.Remove(mls[i]);
    for I := 0 to cntct.EmailCount-1 do begin
      tmp:=RootNode.AddChild('gd:email');
      if cntct.Emails[i].FLabel<>'' then tmp.Attributes['label']:=cntct.Emails[i].FLabel
      else
      //if(cntct.Emails[i].FRel<>gmtOther)or((cntct.Emails[i].FRel=gmtOther)and(cntct.Emails[i].FLabel='')) then
        tmp.Attributes['rel']:=EmailTypeTOgcStr(cntct.Emails[i].FRel);
      tmp.Attributes['address']:=cntct.Emails[i].Address;
      if cntct.Emails[i].FDisplayName<>'' then tmp.Attributes['displayName']:=cntct.Emails[i].FDisplayName;
      if i=0 then tmp.Attributes['primary']:='true';
    end;
    for I := 0 to Length(phns)-1 do RootNode.ChildNodes.Remove(phns[i]);
    for I := 0 to cntct.PhoneCount-1 do begin
      tmp:=RootNode.AddChild('gd:phoneNumber');
      if cntct.Phones[i].FPhoneLabel<>'' then tmp.Attributes['label']:=cntct.Phones[i].FPhoneLabel
      else
      //if(cntct.Phones[i].FPhoneType<>gptOther)or((cntct.Phones[i].FPhoneType=gptOther)and(cntct.Phones[i].FPhoneLabel=''))then
        tmp.Attributes['rel']:=PhoneTypeTOgcStr(cntct.Phones[i].FPhoneType);
      if i=0 then tmp.Attributes['primary']:='true';
      tmp.Text:=cntct.Phones[i].PhoneText;
    end;
    for I := 0 to Length(adrs)-1 do RootNode.ChildNodes.Remove(adrs[i]);
    for I := 0 to cntct.PostalAddressCount-1 do begin
      tmp:=RootNode.AddChild('gd:structuredPostalAddress');
      if cntct.PostalAddress[i].FLabel<>'' then
        tmp.Attributes['label']:=cntct.PostalAddress[i].FLabel
      else
      //if(cntct.PostalAddress[i].FRel<>gpatOther)or((cntct.PostalAddress[i].FRel=gpatOther)and(cntct.PostalAddress[i].FLabel=''))then
        tmp.Attributes['rel']:=PostalAddressTypeTOgcStr(cntct.PostalAddress[i].FRel);

      if cntct.PostalAddress[i].FAgent<>'' then
        tmp.AddChild('gd:agent').Text:=cntct.PostalAddress[i].FAgent;
      if cntct.PostalAddress[i].FHousename<>'' then
        tmp.AddChild('gd:housename').Text:=cntct.PostalAddress[i].FHousename;
      if cntct.PostalAddress[i].FStreet<>'' then
        tmp.AddChild('gd:street').Text:=cntct.PostalAddress[i].FStreet;
      if cntct.PostalAddress[i].FPobox<>'' then
        tmp.AddChild('gd:pobox').Text:=cntct.PostalAddress[i].FPobox;
      if cntct.PostalAddress[i].FNeighborhood<>'' then
        tmp.AddChild('gd:neighborhood').Text:=cntct.PostalAddress[i].FNeighborhood;
      if cntct.PostalAddress[i].FCity<>'' then
        tmp.AddChild('gd:city').Text:=cntct.PostalAddress[i].FCity;
      if cntct.PostalAddress[i].FRegion<>'' then
        tmp.AddChild('gd:region').Text:=cntct.PostalAddress[i].FRegion;
      if cntct.PostalAddress[i].FPostcode<>'' then
        tmp.AddChild('gd:postcode').Text:=cntct.PostalAddress[i].FPostcode;
      if cntct.PostalAddress[i].FCountry.Code<>0 then begin
        nm:=tmp.AddChild('gd:country');
        nm.Text:=cntct.PostalAddress[i].FCountry.ShortName;
        nm.Attributes['code']:=cntct.PostalAddress[i].FCountry.Alpha2codeStr;
      end;

      if i=0 then tmp.Attributes['primary']:='true';
    end;

    for I := 0 to Length(orgs)-1 do RootNode.ChildNodes.Remove(orgs[i]);
    for I := 0 to cntct.OrganizationCount-1 do begin
      tmp:=RootNode.AddChild('gd:organization');
      if cntct.Organizations[i].FLabel<>'' then
        tmp.Attributes['label']:=cntct.Organizations[i].FLabel
      else
      //if(cntct.Organizations[i].FRel<>gotOther)or((cntct.Organizations[i].FRel=gotOther)and(cntct.Organizations[i].FLabel=''))then
        tmp.Attributes['rel']:=OrganizationTypeTOgcStr(cntct.Organizations[i].FRel);

      if cntct.Organizations[i].FOrgName<>'' then
        tmp.AddChild('gd:orgName').Text:=cntct.Organizations[i].FOrgName;
      if cntct.Organizations[i].FOrgTitle<>'' then
        tmp.AddChild('gd:orgTitle').Text:=cntct.Organizations[i].FOrgTitle;

      if i=0 then tmp.Attributes['primary']:='true';
    end;

    xml.SaveToStream(astream);
  finally
    xml.Free;
    CoUninitialize;
  end;
end;

{ TGoogleContactName }

procedure TGoogleContactName.Assign(AName: TGoogleContactName);
begin
  FNameSuffix:=AName.FNameSuffix;
  FFamilyName:=AName.FFamilyName;
  FGivenName:=AName.FGivenName;
  FAdditionalName:=AName.FAdditionalName;
  FNamePrefix:=AName.FNamePrefix;
end;

function TGoogleContactName.fullName: String;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  if fnamePrefix<>'' then sl.Add(fnamePrefix);
  if fgivenName<>'' then sl.Add(fgivenName);
  if fadditionalName<>'' then sl.Add(fadditionalName);
  if ffamilyName<>'' then sl.Add(ffamilyName);
  if fnameSuffix<>'' then sl.Add(fnameSuffix);
  Result:=Funcs.Implode(' ',sl);
  sl.Free;
end;

procedure TGoogleContactName.SetAdditionalName(const Value: String);
begin
  if not SameText(trim(Value),FAdditionalName) then begin
    FAdditionalName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetFamilyName(const Value: String);
begin
  if not SameText(trim(Value),FFamilyName) then begin
    FFamilyName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetGivenName(const Value: String);
begin
  if not SameText(trim(Value),FGivenName) then begin
    FGivenName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetNamePrefix(const Value: String);
begin
  if not SameText(trim(Value),FNamePrefix) then begin
    FNamePrefix := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetNameSuffix(const Value: String);
begin
  if not SameText(trim(Value),FNameSuffix) then begin
    FNameSuffix := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

{ TGoogleContact }

procedure TGoogleContact.Assign(AContact: TGoogleContact);
var
  i: integer;
  email: TGoogleContactEmail;
  Phone: TGoogleContactPhone;
  Organization: TGoogleContactOrganization;
  PostalAddress: TGoogleContactPostalAddress;
begin
  contacts:=AContact.contacts;
  FID:=AContact.FID;
  FID_:=AContact.FID_;
  FETag:=AContact.FETag;
  fxml:=AContact.fxml;
  FName.Assign(AContact.FName);
  FPhoto.Assign(AContact.FPhoto);
  FUpdated:=AContact.FUpdated;
  FModified:=AContact.FModified;
  FNotes:=AContact.Notes;
  self.FGroupID:=AContact.FGroupID;
  self.FDeleted:=AContact.FDeleted;

  for I := 0 to FEmailList.Count-1 do TGoogleContactEMail(FEmailList[i]).Free;
  FEmailList.Clear;
  for I := 0 to AContact.EmailCount-1 do begin
    email:=TGoogleContactEmail.Create;
    email.Assign(AContact.Emails[i]);
    email.contact:=self;
    FEmailList.Add(email);
  end;

  for I := 0 to FPhoneList.Count-1 do TGoogleContactPhone(FPhoneList[i]).Free;
  FPhoneList.Clear;
  for I := 0 to AContact.PhoneCount-1 do begin
    Phone:=TGoogleContactPhone.Create;
    Phone.Assign(AContact.Phones[i]);
    Phone.contact:=self;
    FPhoneList.Add(Phone);
  end;

  for I := 0 to FOrganizationList.Count-1 do TGoogleContactOrganization(FOrganizationList[i]).Free;
  FOrganizationList.Clear;
  for I := 0 to AContact.OrganizationCount-1 do begin
    Organization:=TGoogleContactOrganization.Create;
    Organization.Assign(AContact.Organizations[i]);
    Organization.contact:=self;
    FOrganizationList.Add(Organization);
  end;

  for I := 0 to FPostalAddressList.Count-1 do TGoogleContactPostalAddress(FPostalAddressList[i]).Free;
  FPostalAddressList.Clear;
  for I := 0 to AContact.PostalAddressCount-1 do begin
    PostalAddress:=TGoogleContactPostalAddress.Create;
    PostalAddress.Assign(AContact.PostalAddress[i]);
    PostalAddress.contact:=self;
    FPostalAddressList.Add(PostalAddress);
  end;


end;

constructor TGoogleContact.Create;
begin
  FName:=TGoogleContactName.Create;
  FName.contact:=self;
  FPhoto:=TGoogleContactPhoto.Create;
  FPhoto.contact:=self;
  FEmailList:=TList.Create;
  FPhoneList:=TList.Create;
  FOrganizationList:=TList.Create;
  FPostalAddressList:=TList.Create;
end;

function TGoogleContact.CreateDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Post(FURL,TStream(AData),Response);
  except end;
  Result:=Pointer(response);

  Http.IOHandler.Free;
  Http.Free;
end;

destructor TGoogleContact.Destroy;
var
  i: integer;
begin
  FName.Free;
  FPhoto.Free;
  for I := 0 to FEmailList.Count-1 do TGoogleContactEMail(FEmailList[i]).Free;
  FEmailList.Free;
  for I := 0 to FPhoneList.Count-1 do TGoogleContactPhone(FPhoneList[i]).Free;
  FPhoneList.Free;
  for I := 0 to FOrganizationList.Count-1 do TGoogleContactOrganization(FOrganizationList[i]).Free;
  FOrganizationList.Free;
  for I := 0 to FPostalAddressList.Count-1 do TGoogleContactPostalAddress(FPostalAddressList[i]).Free;
  FPostalAddressList.Free;
  inherited;
end;

function TGoogleContact.EmailAdd(AAddress: String;
  ARel: TGoogleContactEMailType; ADisplayName,
  ALabel: String): TGoogleContactEMail;
begin
  Result:=TGoogleContactEMail.Create;
  Result.contact:=self;
  Result.FAddress:=AAddress;
  Result.FRel:=ARel;
  Result.FDisplayName:=ADisplayName;
  Result.FLabel:=ALabel;
  FEmailList.Add(Result);
  FModified:=true;
end;

procedure TGoogleContact.EmailDelete(AIndex: integer);
begin
  TGoogleContactEmail(FEmailList[AIndex]).Free;
  FEmailList.Delete(AIndex);
  FModified:=true;
end;

function TGoogleContact.GetEmail(AIndex: Integer): TGoogleContactEMail;
begin
  result:=TGoogleContactEMail(FEmailList[AIndex]);
end;

function TGoogleContact.GetEmailCount: integer;
begin
  Result:=FEmailList.Count;
end;

function TGoogleContact.GetOrganization(
  AIndex: Integer): TGoogleContactOrganization;
begin
  result:=TGoogleContactOrganization(FOrganizationList[AIndex]);
end;

function TGoogleContact.GetOrganizationCount: integer;
begin
  Result:=FOrganizationList.Count;
end;

function TGoogleContact.GetPhone(AIndex: Integer): TGoogleContactPhone;
begin
  result:=TGoogleContactPhone(FPhoneList[AIndex]);
end;

function TGoogleContact.GetPhoneCount: integer;
begin
  Result:=FPhoneList.Count;
end;

function TGoogleContact.GetPostalAddress(
  AIndex: Integer): TGoogleContactPostalAddress;
begin
  result:=TGoogleContactPostalAddress(FPostalAddressList[AIndex]);
end;

function TGoogleContact.GetPostalAddressCount: integer;
begin
  Result:=FPostalAddressList.Count;
end;

function TGoogleContact.OrganizationAdd(AOrgName: String;
  ARel: TGoogleContactOrganizationType; AOrgTitle,
  ALabel: String): TGoogleContactOrganization;
begin
  Result:=TGoogleContactOrganization.Create;
  Result.contact:=self;
  Result.FRel:=ARel;
  Result.FOrgName:=AOrgName;
  Result.FOrgTitle:=AOrgTitle;
  Result.FLabel:=ALabel;
  FOrganizationList.Add(Result);
  FModified:=true;
end;

procedure TGoogleContact.OrganizationDelete(AIndex: integer);
begin
  TGoogleContactOrganization(FOrganizationList[AIndex]).Free;
  FOrganizationList.Delete(AIndex);
  FModified:=true;
end;

function TGoogleContact.PhoneAdd(APhone: String; ARel: TGoogleContactPhoneType;
  ALabel: String): TGoogleContactPhone;
begin
  Result:=TGoogleContactPhone.Create;
  Result.contact:=self;
  Result.FPhoneType:=ARel;
  Result.FPhoneText:=APhone;
  Result.FPhoneLabel:=ALabel;
  FPhoneList.Add(Result);
  FModified:=true;
end;

procedure TGoogleContact.PhoneDelete(AIndex: integer);
begin
  TGoogleContactPhone(FPhoneList[AIndex]).Free;
  FPhoneList.Delete(AIndex);
  FModified:=true;
end;

function TGoogleContact.PostalAddressAdd(ACountry: TISO3166; ACity: String;
  ARel: TGoogleContactPostalAddressType; AStreet, APostcode, ALabel: String): TGoogleContactPostalAddress;
begin
  Result:=TGoogleContactPostalAddress.Create;
  Result.contact:=self;
  Result.FRel:=ARel;
  Result.FLabel:=ALabel;
  Result.FCountry:=ACountry;
  Result.FCity:=ACity;
  Result.FStreet:=AStreet;
  Result.FPostcode:=APostcode;
  FPostalAddressList.Add(Result);
  FModified:=true;
end;

procedure TGoogleContact.PostalAddressDelete(AIndex: integer);
begin
  TGoogleContactPostalAddress(FPostalAddressList[AIndex]).Free;
  FPostalAddressList.Delete(AIndex);
  FModified:=true;
end;

function TGoogleContact.Retrieve: boolean;
var
  s: String;
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
begin
  result:=false;
  s:='https://www.google.com/m8/feeds/contacts/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
  Response:=TStringStream(KRRunInThread(Pointer(PChar(s)),RetrieveDo));
  if Response.Size>0 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(contacts);
    try
      Response.Position:=0;
      xml.LoadFromStream(Response);
      xml.Active := true;
      RootNode := xml.DocumentElement;
      if SameText(RootNode.NodeName,'entry') then begin
        if SameText(RootNode.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#contact') then begin
          contacts.parseUserData(RootNode,self);
          result:=true;
          end;
      end;
    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
end;

function TGoogleContact.RetrieveDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Get(String(PChar(AData)),Response);
  except end;
  Result:=Pointer(Response);

  Http.IOHandler.Free;
  Http.Free;
end;

procedure TGoogleContact.SetGroupID(const Value: String);
begin
  if not SameText(trim(Value),FGroupID) then begin
    FGroupID := trim(Value);
    FModified:=true;
  end;
end;

procedure TGoogleContact.SetNotes(const Value: String);
begin
  if not SameText(trim(Value),FNotes) then begin
    FNotes := trim(Value);
    FModified:=true;
  end;
end;

function TGoogleContact.Update: boolean;
var
  xml: TXMLDocument;
  RootNode: IXMLNode;
  stream: TMemoryStream;
  Response: TStringStream;

begin
  Result:=false;
  stream:=TMemoryStream.Create;
  if FID<>'' then begin
    contacts.userDataToXml('<?xml version="1.0" encoding="UTF-8"?>'#$A+fxml,self,stream);
    FURL:='https://www.google.com/m8/feeds/contacts/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
    Response:=TStringStream(KRRunInThread(stream,UpdateDo));
  end else begin
    contacts.userDataToXml('<?xml version="1.0" encoding="UTF-8"?>'#$A+
      '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005" xmlns:gContact="http://schemas.google.com/contact/2008">'#$A+
      ' <category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/contact/2008#contact"/>'#$A+
      '</entry>'#$A
    ,self,stream);
    FURL:='https://www.google.com/m8/feeds/contacts/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
    Response:=TStringStream(KRRunInThread(stream,CreateDo));
  end;
  if Response.Size>10 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(contacts);
    try
      Response.Position:=0;
      xml.LoadFromStream(Response);
      xml.Active:=true;
      RootNode:=xml.DocumentElement;
      if SameText(RootNode.NodeName,'entry') then begin
        if SameText(RootNode.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#contact') then begin
          contacts.parseUserData(RootNode,self);
          result:=true;
          end;
      end;

    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
  stream.Free;
  Response.Free;
end;

function TGoogleContact.UpdateDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;
  Http.Request.CustomHeaders.Values['If-Match']:=Self.FETag;

  Response:=TStringStream.Create;
  try
    Http.Put(FURL,TStream(AData),Response);
  except on e: EIdHTTPProtocolException do
    contacts.checkError(e);
  end;

  Result:=Pointer(response);
  Http.IOHandler.Free;
  Http.Free;
end;

{ TGoogleContactPhoto }

procedure TGoogleContactPhoto.Assign(APhoto: TGoogleContactPhoto);
begin
  FETag:=APhoto.FETag;
  Fhref:=APhoto.Fhref;
end;

function TGoogleContactPhoto.DeleteDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
begin
  http:=contact.contacts.getHTTP;
  Http.Request.CustomHeaders.Values['If-Match']:=String(PChar(AData));

  Result:=Pointer(1);
  try
    Http.Delete(FURL);
  except
    Result:=Pointer(0);
  end;

  Http.IOHandler.Free;
  Http.Free;
end;

destructor TGoogleContactPhoto.destroy;
begin
  if Assigned(bmp) then bmp.Free;
  inherited;
end;

function TGoogleContactPhoto.getBitmap: TBitmap;
begin
  if Assigned(bmp) then bmp.Free;
  bmp:=nil;
  result:=nil;
  if(not Assigned(contact.contacts))then exit;
  if(not Assigned(contact.contacts.FGoogleAuth))then exit;
  if FEtag<>'' then bmp:=contact.contacts.FGoogleAuth.GetUserIcon(Fhref,FEtag,true);
  Result:=bmp;
end;

function TGoogleContactPhoto.getEtagDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  response: TMemoryStream;
begin
  http:=contact.contacts.getHTTP;

  response:=TMemoryStream(AData);
  try
    Http.Get(FURL,response);
  except end;

  _etag:=Http.Response.ETag;

  result:=Pointer(response);

  Http.Free;
end;

function TGoogleContactPhoto.LoadDo(AThread: TThread; AData: Pointer): Pointer;
type
  TBSTData = record
    ETag: Pointer;
    stream: TStream;
  end;

var
  http: TIdHTTP;
  fstream,strm: TmemoryStream;
begin
  fstream:=TmemoryStream(TBSTData(AData^).stream);
  strm:=TmemoryStream.Create;

  http:=contact.contacts.getHTTP;
  Http.Request.ContentType:='image/*';
  if TBSTData(AData^).ETag<>nil then
    Http.Request.CustomHeaders.Values['If-Match']:=String(PChar(TBSTData(AData^).ETag));

  Result:=Pointer(1);
  try
    Http.Put(FURL,fstream,strm);
  except
    Result:=Pointer(0);
  end;

  if integer(Result)=1 then begin
    fstream.Clear;
    fstream.LoadFromStream(strm);
  end;

  strm.Free;
  Http.Free;
end;

procedure TGoogleContactPhoto.setBitmap(const Value: TBitmap);
type
  TBSTData = record
    ETag: Pointer;
    stream: TStream;
  end;

var s: String;
  png: TPngImage;
  fstream: TmemoryStream;
  p: TBSTData;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  i: integer;
begin
  if(not Assigned(contact.contacts))then exit;
  if(not Assigned(contact.contacts.FGoogleAuth))then exit;
  if Value=nil then begin
    if FETag<>'' then begin
      FURL:='https://www.google.com/m8/feeds/photos/media/default/'+
        contact.FID+'?access_token='+contact.contacts.FGoogleAuth.Token;
      s:=FETag;
      if Integer(KRRunInThread(Pointer(PChar(s)),DeleteDo))<>0 then FETag:='';
    end;
  end else begin
    fstream:=TmemoryStream.Create;
    png:=TPngImage.Create;
    try
      png.Assign(Value);
      png.SaveToStream(fstream);
      s:=FETag;
      p.ETag:=Pointer(PChar(s));
      p.stream:=fstream;
      FURL:=Fhref+'?access_token='+contact.contacts.FGoogleAuth.Token;
      if Integer(KRRunInThread(@p,LoadDo))<>0 then begin
        FURL:='https://www.google.com/m8/feeds/contacts/default/full/'+contact.FID+
          '?access_token='+contact.contacts.FGoogleAuth.Token+
          '&fields='+String(HTTPEncode('updated,link(@gd:etag)'));
        fstream.Clear;
        KRRunInThread(Pointer(fstream),getEtagDo);
        if fstream.Size>0 then begin
          CoInitialize(nil);
          xml:=TXMLDocument.Create(contact.contacts);
          try
            fstream.Position:=0;
            xml.LoadFromStream(fstream);
            xml.Active:=true;
            RootNode:=xml.DocumentElement;
            if SameText(RootNode.NodeName,'entry') then begin
              FEtag:=RootNode.ChildNodes['link'].Attributes['gd:etag'];
              contact.FETag:=_etag;
              contact.FUpdated:=gcDateToDateTime(RootNode.ChildNodes['updated'].Text);
              xml.Active:=false;
              xml.LoadFromXML('<?xml version="1.0" encoding="UTF-8"?>'#$A+contact.fxml);
              xml.Active:=true;
              RootNode:=xml.DocumentElement;
              if SameText(RootNode.NodeName,'entry') then begin
                RootNode.Attributes['gd:etag']:=contact.FETag;
                RootNode.ChildNodes['updated'].Text:=DateTimeTOgcDate(contact.FUpdated);
                for i := 0 to RootNode.ChildNodes.Count-1 do begin
                  if(SameText(RootNode.ChildNodes[i].NodeName,'link'))and
                    SameText(RootNode.ChildNodes[i].Attributes['rel'],'http://schemas.google.com/contacts/2008/rel#photo')then begin
                    RootNode.ChildNodes[i].Attributes['gd:etag']:=FEtag;
                    contact.fxml:=RootNode.XML;
                    break;
                  end;
                end;
              end;
            end;
          finally
            xml.Free;
            CoUninitialize;
          end;

        end;

      end;
    finally
      png.Free;
      fstream.Free;
    end;
  end;
end;

{ TGoogleContactEMail }

procedure TGoogleContactEMail.Assign(AEmail: TGoogleContactEMail);
begin
  FDisplayName:=AEmail.FDisplayName;
  FRel:=AEmail.FRel;
  FLabel:=AEmail.FLabel;
  FAddress:=AEmail.FAddress;
end;

procedure TGoogleContactEMail.SetAddress(const Value: String);
begin
  if not SameText(trim(Value),FAddress) then begin
    FAddress := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactEMail.SetDisplayName(const Value: String);
begin
  if not SameText(trim(Value),FDisplayName) then begin
    FDisplayName := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactEMail.SetLabel(const Value: String);
begin
  if not SameText(trim(Value),FLabel) then begin
    FLabel := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactEMail.SetPrimary;
var
  n: integer;
begin
  n:=contact.FEmailList.IndexOf(Self);
  if n=0 then exit;
  contact.FEmailList.Move(n,0);
  contact.FModified:=true;
end;

procedure TGoogleContactEMail.SetRel(const Value: TGoogleContactEMailType);
begin
  if FRel <> Value then begin
    FRel := Value;
    if assigned(contact) then contact.FModified:=true;
  end;
end;

{ TGoogleContactPhone }

procedure TGoogleContactPhone.Assign(APhone: TGoogleContactPhone);
begin
  FPhoneType:=APhone.FPhoneType;
  FPhoneUri:=APhone.FPhoneUri;
  FPhoneText:=APhone.FPhoneText;
  FPhoneLabel:=APhone.FPhoneLabel;
end;

procedure TGoogleContactPhone.SetPhoneLabel(const Value: String);
begin
  if not SameText(trim(Value),FPhoneLabel) then begin
    FPhoneLabel := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPhone.SetPhoneText(const Value: String);
begin
  if not SameText(trim(Value),FPhoneText) then begin
    FPhoneText := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPhone.SetPhoneType(
  const Value: TGoogleContactPhoneType);
begin
  if FPhoneType <> Value then begin
    FPhoneType := Value;
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPhone.SetPrimary;
var
  n: integer;
begin
  n:=contact.FPhoneList.IndexOf(Self);
  if n=0 then exit;
  contact.FPhoneList.Move(n,0);
  contact.FModified:=true;
end;

{ TKRGoogleContactsGroup }

function TKRGoogleContactsGroup.CreateDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Post(FURL,TStream(AData),Response);
  except end;
  Result:=Pointer(response);

  Http.IOHandler.Free;
  Http.Free;
end;

function TKRGoogleContactsGroup.Retrieve: boolean;
var
  s: String;
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
begin
  result:=false;
  s:='https://www.google.com/m8/feeds/groups/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
  Response:=TStringStream(KRRunInThread(Pointer(PChar(s)),RetrieveDo));
  if Response.Size>0 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(contacts);
    try
      Response.Position:=0;
      xml.LoadFromStream(Response);
      xml.Active := true;
      RootNode := xml.DocumentElement;
      if SameText(RootNode.NodeName,'entry') then begin
        if SameText(RootNode.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#group') then begin
          contacts.parseGroupData(RootNode,self);
          result:=true;
          end;
      end;
    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
end;

function TKRGoogleContactsGroup.RetrieveDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;

  Response:=TStringStream.Create;
  try
    Http.Get(String(PChar(AData)),Response);
  except end;
  Result:=Pointer(Response);

  Http.IOHandler.Free;
  Http.Free;
end;

procedure TKRGoogleContactsGroup.SetTitle(const Value: String);
begin
  if not SameText(trim(Value),FTitle) then begin
    FTitle := trim(Value);
    FModified:=true;
  end;
end;

function TKRGoogleContactsGroup.Update: boolean;
var
  xml: TXMLDocument;
  RootNode: IXMLNode;
  stream: TMemoryStream;
  Response: TStringStream;

begin
  Result:=false;
  stream:=TMemoryStream.Create;
  if FID<>'' then begin
    contacts.groupDataToXml('<?xml version="1.0" encoding="UTF-8"?>'#$A+fxml,self,stream);
    FURL:='https://www.google.com/m8/feeds/groups/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
    Response:=TStringStream(KRRunInThread(stream,UpdateDo));
  end else begin
    contacts.groupDataToXml('<?xml version="1.0" encoding="UTF-8"?>'#$A+
      '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005">'#$A+
      ' <category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/g/2008#group"/>'#$A+
      '</entry>'#$A
    ,self,stream);
    FURL:='https://www.google.com/m8/feeds/groups/default/full/'+FID+'?access_token='+contacts.FGoogleAuth.Token;
    Response:=TStringStream(KRRunInThread(stream,CreateDo));
  end;
  if Response.Size>10 then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(contacts);
    try
      Response.Position:=0;
      xml.LoadFromStream(Response);
      xml.Active:=true;
      RootNode:=xml.DocumentElement;
      if SameText(RootNode.NodeName,'entry') then begin
        if SameText(RootNode.ChildNodes['category'].Attributes['term'],
          'http://schemas.google.com/contact/2008#group') then begin
          contacts.parseGroupData(RootNode,self);
          result:=true;
          end;
      end;

    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
  stream.Free;
  Response.Free;
end;

function TKRGoogleContactsGroup.UpdateDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TIdHTTP;
  Response: TStringStream;
begin
  http:=contacts.getHTTP;
  Http.Request.CustomHeaders.Values['If-Match']:=Self.FETag;

  Response:=TStringStream.Create;
  try
    Http.Put(FURL,TStream(AData),Response);
  except on e: EIdHTTPProtocolException do
    contacts.checkError(e);
  end;

  Result:=Pointer(response);
  Http.IOHandler.Free;
  Http.Free;
end;

{ TGoogleContactOrganization }

procedure TGoogleContactOrganization.Assign(AOrganization: TGoogleContactOrganization);
begin
  Self.FOrgName:=AOrganization.FOrgName;
  Self.FRel:=AOrganization.FRel;
  Self.FLabel:=AOrganization.FLabel;
  Self.FOrgTitle:=AOrganization.FOrgTitle;
end;

function TGoogleContactOrganization.getFormatted: String;
begin
  Result:=self.FOrgName;
  if Self.FOrgTitle<>'' then Result:=Result+', '+FOrgTitle;
end;

procedure TGoogleContactOrganization.SetLabel(const Value: String);
begin
  if not SameText(trim(Value),FLabel) then begin
    FLabel := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactOrganization.SetOrgName(const Value: String);
begin
  if not SameText(trim(Value),FOrgName) then begin
    FOrgName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactOrganization.SetOrgTitle(const Value: String);
begin
  if not SameText(trim(Value),FOrgTitle ) then begin
    FOrgTitle  := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactOrganization.SetPrimary;
var
  n: integer;
begin
  n:=contact.FOrganizationList.IndexOf(Self);
  if n=0 then exit;
  contact.FOrganizationList.Move(n,0);
  contact.FModified:=true;
end;

procedure TGoogleContactOrganization.SetRel(
  const Value: TGoogleContactOrganizationType);
begin
  if Value<>FRel then begin
    FRel  := Value;
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

{ TGoogleContactPostalAddress }

procedure TGoogleContactPostalAddress.Assign(
  APostalAddress: TGoogleContactPostalAddress);
begin
  Self.FLabel:=APostalAddress.FLabel;
  Self.FRel:=APostalAddress.FRel;
  Self.FAgent:=APostalAddress.FAgent;
  Self.FStreet:=APostalAddress.FStreet;
  Self.FHousename:=APostalAddress.FHousename;
  Self.FCountry:=APostalAddress.FCountry;
  Self.FNeighborhood:=APostalAddress.FNeighborhood;
  Self.FPostcode:=APostalAddress.FPostcode;
  Self.FCity:=APostalAddress.FCity;
  Self.FPobox:=APostalAddress.FPobox;
  Self.FRegion:=APostalAddress.FRegion;
end;

function TGoogleContactPostalAddress.FormattedAddress: String;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  if Self.FStreet<>'' then sl.Add(Self.FStreet);
  if Self.FPobox<>'' then sl.Add(Self.FPobox);
  if Self.FNeighborhood<>'' then sl.Add(Self.FNeighborhood);
  if Self.FCity<>'' then sl.Add(Self.FCity);
  if Self.FRegion<>'' then sl.Add(Self.FRegion);
  if Self.FCountry.Code>0 then sl.Add(Self.FCountry.ShortName);
  if Self.FPostcode<>'' then sl.Add(Self.FPostcode);
  Result:=Funcs.Implode(' ',sl);
  sl.Free;
end;

procedure TGoogleContactPostalAddress.SetAgent(const Value: String);
begin
  if not SameText(trim(Value),FAgent) then begin
    FAgent := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetCity(const Value: String);
begin
  if not SameText(trim(Value),FCity) then begin
    FCity := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetCountry(const Value: TISO3166);
begin
  if Value.Code<>FCountry.Code then begin
    FCountry := Value;
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetHousename(const Value: String);
begin
  if not SameText(trim(Value),FHousename) then begin
    FHousename := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetLabel(const Value: String);
begin
  if not SameText(trim(Value),FLabel) then begin
    FLabel := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetNeighborhood(const Value: String);
begin
  if not SameText(trim(Value),FNeighborhood) then begin
    FNeighborhood := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetPobox(const Value: String);
begin
  if not SameText(trim(Value),FPobox) then begin
    FPobox := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetPostcode(const Value: String);
begin
  if not SameText(trim(Value),FPostcode) then begin
    FPostcode := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetPrimary;
var
  n: integer;
begin
  n:=contact.FPostalAddressList.IndexOf(Self);
  if n=0 then exit;
  contact.FPostalAddressList.Move(n,0);
  contact.FModified:=true;
end;

procedure TGoogleContactPostalAddress.SetRegion(const Value: String);
begin
  if not SameText(trim(Value),FRegion) then begin
    FRegion := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetRel(
  const Value: TGoogleContactPostalAddressType);
begin
  if Value<>FRel then begin
    FRel  := Value;
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetStreet(const Value: String);
begin
  if not SameText(trim(Value),FStreet) then begin
    FStreet := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

end.

