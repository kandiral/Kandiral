(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGoogleContacts                                                          *)
(*  Ver.: 14.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGoogleContacts;

interface

{$DEFINE GOOGLE_API_LOGS}

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, Vcl.Dialogs,
    Xml.XMLDoc, Xml.XMLIntf, Winapi.ActiveX, System.Variants, Vcl.Graphics,
    Vcl.Imaging.pngimage, System.IniFiles,
  {$ELSE}
    Windows, Classes, SysUtils, StrUtils, Dialogs, XMLDoc, XMLIntf, ActiveX,
    Variants, Graphics, PngImage, IniFiles,
  {$IFEND}

    KRIdHTTP, KRGoogleAPI, KRGoogleCommon, KRWebCommon,

    KRGoogleAuth, KRThread, KRStrUtils, KRWebDownload,
    ISO3166;

type
  TKRGoogleContacts = class;
  TGoogleContact = class;
  TKRGoogleContactsGroup = class;

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
    FETag, Fhref, _etag: String;
    function getBitmap: TBitmap;
    procedure setBitmap(const Value: TBitmap);
    function DeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function LoadDo(AThread: TThread; AData: Pointer): Pointer;
    function SetPhotoDo(AThread: TThread; AData: Pointer): Pointer;
    function getEtagDo(AThread: TThread; AData: Pointer): Pointer;
    function thLoadIcon(AThread: TThread; AData: Pointer): Pointer;
  public
    procedure Assign(APhoto: TGoogleContactPhoto);
    property ETag: String read FETag;
    //property Bitmap: TBitmap read getBitmap write setBitmap;
    function SetPhoto( AFileName: TFileName ): boolean;
  end;

  TGoogleContactWebSite = class
  private
    contact: TGoogleContact;
    FSiteLabel: String;
    FHref: String;
    procedure SetHref(const Value: String);
    procedure SetSiteLabel(const Value: String);
  public
    property Href: String read FHref write SetHref;
    property SiteLabel: String read FSiteLabel write SetSiteLabel;
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
    FChecked: boolean;
    FID, FID_, FETag, fxml: String;
    FName: TGoogleContactName;
    FPhoto: TGoogleContactPhoto;
    FUpdated: TDateTime;
    FModified: boolean;

    FEmailList, FPhoneList, FOrganizationList, FPostalAddressList, FWebSiteList: TList;
    FNotes: String;
    FDeleted: boolean;
    FGroups: TStringList;
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
    function GetGroups(Index: integer): String;
    function GetGroupsCount: integer;
    function GetWebSite(AIndex: Integer): TGoogleContactWebSite;
    function GetWebSiteCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AContact: TGoogleContact);
    property ContactsList: TKRGoogleContacts read contacts;
    property isModified: boolean read FModified;
    property ETag: String read FETag;
    function Update: boolean;
    function Retrieve: boolean;
    property id: String read FID;
    property isDeleted: boolean read FDeleted;
    property Updated: TDateTime read FUpdated;
    function InGroup( AGroup: TKRGoogleContactsGroup ): boolean;
    property Groups[Index: integer ]: String read GetGroups;
    property GroupsCount: integer read GetGroupsCount;
    procedure GroupAdd( AGroup: TKRGoogleContactsGroup );
    procedure GroupDel( AIndex: Integer );
    property Name: TGoogleContactName read FName;
    property Notes: String read FNotes write SetNotes;
    property Photo: TGoogleContactPhoto read FPhoto;
    property EmailCount: integer read GetEmailCount;
    property Emails[AIndex: Integer]: TGoogleContactEMail read GetEmail;
    function EmailAdd(AAddress: String; ARel: TGoogleContactEMailType;
      ADisplayName: String = ''; ALabel: String = ''): TGoogleContactEMail;
    procedure EmailDelete(AIndex: integer);

    property WebSiteCount: integer read GetWebSiteCount;
    property WebSites[AIndex: Integer]: TGoogleContactWebSite read GetWebSite;
    function WebSiteAdd(AHref: String; ALabel: String = ''): TGoogleContactWebSite;
    procedure WebSiteDelete(AIndex: integer);

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
    FID, FID_, FETag, fxml: String;
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

  TKRGoogleContacts = class( TKRGoogleAPI )
  private
    FContacts: TList;
    FGroups, FGroupsTmp: TList;
    FEMailID: String;
    FRetrievingDeleted: boolean;
    FDataFolder: TFileName;
    function GetCount: integer;
    function RetrievingAllDo(AThread: TThread; AData: Pointer): Pointer;
    function RetrievingGroupsDo(AThread: TThread; AData: Pointer): Pointer;
    function DeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function GroupDeleteDo(AThread: TThread; AData: Pointer): Pointer;
    function GetContact(Index: integer): TGoogleContact;
    procedure parseUserData(entry: IXMLNode; cntct: TGoogleContact);
    procedure userDataToXml(AXML: String; cntct: TGoogleContact; AStream: TStream);
    procedure parseGroupData(entry: IXMLNode; grp: TKRGoogleContactsGroup);
    procedure groupDataToXml(AXML: String; grp: TKRGoogleContactsGroup; AStream: TStream);
    function GetGroup(AIndex: Integer): TKRGoogleContactsGroup;
    function GetGroupCount: integer;
    procedure SetGoogleAuth(const Value: TKRGoogleAuth);
    procedure SetDataFolder(const Value: TFileName);
  protected
    function HTTPCreate: TKRIdHTTP; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataFolder: TFileName read FDataFolder write SetDataFolder;

    property RetrievingDeleted: boolean read FRetrievingDeleted write FRetrievingDeleted;
    function RetrievingAll: boolean;
    procedure UpdateModified;
    function Add: TGoogleContact;
    function Delete(AContact: TGoogleContact): boolean;overload;
    function Delete(AIndex: integer): boolean;overload;
    property Count: integer read GetCount;
    property Contacts[Index: integer]: TGoogleContact read GetContact;

    function RetrievingGroups: boolean;
    property GroupCount: integer read GetGroupCount;
    property Groups[AIndex: Integer]: TKRGoogleContactsGroup read GetGroup;
    function GroupAdd(ATitle: String): TKRGoogleContactsGroup;
    function GroupDelete(AGroup: TKRGoogleContactsGroup): boolean;overload;
    function GroupDelete(AIndex: integer): boolean;overload;

  end;

implementation

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
  sl:=TStringList.Create;
  KRSplitStr(adt,'T',sl);
  dt:=sl[0];
  tm:=sl[1];
  KRSplitStr(dt,'-',sl);
  yy:=StrToInt(sl[0]);
  mm:=StrToInt(sl[1]);
  dd:=StrToInt(sl[2]);
  KRSplitStr(tm,':',sl);
  hh:=StrToInt(sl[0]);
  mn:=StrToInt(sl[1]);
  tm:=sl[2];
  if tm[Length(tm)]='Z' then tm:=copy(tm,1,Length(tm)-1);
  KRSplitStr(tm,'.',sl);
  ss:=StrToInt(sl[0]);
  if sl.Count>1 then ms:=StrToInt(sl[1]) else ms:=0;
  sl.Free;
  Result:=EncodeDate(yy,mm,dd)+EncodeTime(hh,mn,ss,ms)-_d;
end;

function gcStrToEmailType(rel: String): TGoogleContactEMailType;
begin
  result:=gmtOther;
  if(rel='http://schemas.google.com/g/2005#home')then result:=gmtHome
  else if(rel='http://schemas.google.com/g/2005#work')then result:=gmtWork;
end;

function gcStrToOrganizationType(rel: String): TGoogleContactOrganizationType;
begin
  result:=gotOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gotWork;
end;

function gcStrToPostalAddressType(rel: String): TGoogleContactPostalAddressType;
begin
  result:=gpatOther;
  if(SameText(rel,'http://schemas.google.com/g/2005#work'))then result:=gpatWork
  else if(SameText(rel,'http://schemas.google.com/g/2005#home'))then result:=gpatHome;
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

function EmailTypeTOgcStr(tp: TGoogleContactEMailType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  case tp of
    gmtHome: result:='http://schemas.google.com/g/2005#home';
    gmtWork: result:='http://schemas.google.com/g/2005#work';
  end;
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

function PostalAddressTypeTOgcStr(rel: TGoogleContactPostalAddressType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  case rel of
    gpatWork: result:='http://schemas.google.com/g/2005#work';
    gpatHome: result:='http://schemas.google.com/g/2005#home';
  end;
end;

function OrganizationTypeTOgcStr(rel: TGoogleContactOrganizationType): String;
begin
  result:='http://schemas.google.com/g/2005#other';
  if(rel=gotWork)then result:='http://schemas.google.com/g/2005#work';
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
  Result:=KRIntToStrL(yy,4)+'-'+KRIntToStrL(mm,2)+'-'+KRIntToStrL(dd,2)+'T'+
    KRIntToStrL(hh,2)+':'+KRIntToStrL(mn,2)+':'+KRIntToStrL(ss,2)+'.'+
    KRIntToStrL(ms,3)+'Z';
end;

{ TKRGoogleContacts }

function TKRGoogleContacts.Add: TGoogleContact;
begin
  Result:=TGoogleContact.Create;
  Result.FModified:=true;
  Result.contacts:=self;
  Result.FGroups.Add( '6' );
  FContacts.Add(result);
end;

constructor TKRGoogleContacts.Create(AOwner: TComponent);
begin
  inherited;

  FContacts:=TList.Create;
  FGroups:=TList.Create;
  FGroupsTmp:=TList.Create;

  FNeedScopes := [gscContacts];
end;

function TKRGoogleContacts.Delete(AIndex: integer): boolean;
var
  data: TKRGoogleRESTRequestData;
begin
  result:=true;
  if TGoogleContact(FContacts[AIndex]).FID<>'' then begin
    FParams.Clear;
    FParams.Add( 'access_token', FGoogleAuth.Token );
    data.url := 'https://www.google.com/m8/feeds/contacts/default/full/'+
      TGoogleContact(FContacts[AIndex]).FID + getParams;
    data.res := TGoogleContact(FContacts[AIndex]).FETag;
    KRRunInThread( @data, DeleteDo );
    Result := data.isOk;
  end;
  TGoogleContact(FContacts[AIndex]).Free;
  FContacts.Delete(AIndex);
end;

function TKRGoogleContacts.DeleteDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  http:=HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=data.res;
  DEL( Http, data );
  HTTPDestroy( Http );
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
  for I := 0 to FGroups.Count-1 do TKRGoogleContactsGroup(FGroups[i]).Free;
  FGroups.Free;
  FGroupsTmp.Free;
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
var
  data: TKRGoogleRESTRequestData;
begin
  result:=true;
  if TKRGoogleContactsGroup(FGroups[AIndex]).FID<>'' then begin
    FParams.Clear;
    FParams.Add( 'access_token', FGoogleAuth.Token);
    data.url := 'https://www.google.com/m8/feeds/groups/default/full/' +
      TKRGoogleContactsGroup(FGroups[AIndex]).FID + getParams;
    data.res := TKRGoogleContactsGroup(FGroups[AIndex]).FETag;
    KRRunInThread( @data, GroupDeleteDo );
    Result := data.isOk;
  end;
  TKRGoogleContactsGroup(FGroups[AIndex]).Free;
  FGroups.Delete(AIndex);
end;

function TKRGoogleContacts.GroupDeleteDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  http:=HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=data.res;
  DEL( Http, data );
  HTTPDestroy( Http );
end;

function TKRGoogleContacts.HTTPCreate: TKRIdHTTP;
begin
  Result := inherited HTTPCreate;
  Result.Request.Accept:='application/atom+xml';
  Result.Request.CustomHeaders.Values['GData-Version']:='3.0';
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
  sl:=TStringList.Create;
  KRSplitStr(grp.FID_,'/',sl);
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
  for I := 0 to cntct.FWebSiteList.Count-1 do TGoogleContactWebSite(cntct.FWebSiteList[i]).Free;
  cntct.FWebSiteList.Clear;
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
  sl:=TSTringList.Create;
  KRSplitStr(cntct.FID_,'/',sl);
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
      sl:=TStringList.Create;
      KRSplitStr(entry.ChildNodes[j].Attributes['href'],'/',sl);
      cntct.FGroups.Add( sl[sl.Count-1] );
      sl.Free;
    end else if SameText(entry.ChildNodes[j].NodeName,'gContact:website') then begin
      s:='';
      if entry.ChildNodes[j].HasAttribute('label') then s:=entry.ChildNodes[j].Attributes['label'];
      cntct.WebSiteAdd( entry.ChildNodes[j].Attributes['href'], s);
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

function TKRGoogleContacts.RetrievingAll: boolean;
var
  i,n1{,n2,k}: integer;
  data: TKRGoogleRESTRequestData;
  xml: TXMLDocument;
  RootNode, entry: IXMLNode;
  cntct: TGoogleContact;
begin
  Result:=false;
  if FGoogleAuth.Token='' then exit;

  if FRetrievingDeleted then FParams.Add( 'showdeleted', 'true');
  FParams.Add( 'access_token', FGoogleAuth.Token);
  data.url := 'https://www.google.com/m8/feeds/contacts/default/full' + getParams;

  data.res := GoogleAuth.UserEmail;
  data.res := copy( data.res, 1, Pos( '@', data.res ) - 1 );
  data.res := FDataFolder + data.res + '.xml';

  KRRunInThread( @data, RetrievingAllDo );
  Result := data.isOk;

  if Result then begin
    //n2:=FContacts.Count-1;
    //for j := 0 to n2 do TGoogleContact(FContacts[j]).FChecked := false;
    FContacts.Clear;
    CoInitialize(nil);
    xml:=TXMLDocument.Create( Self );
    try
      xml.LoadFromFile( data.res );
      xml.Active:=true;
      RootNode:=xml.DocumentElement;
      n1:=RootNode.ChildNodes.Count - 1;
      for i := 0 to n1 do begin
        if RootNode.ChildNodes[i].NodeName <> 'entry' then continue;
        entry:=RootNode.ChildNodes[i];
        if entry.ChildNodes['category'].Attributes['term'] <> 'http://schemas.google.com/contact/2008#contact' then continue;

        {k:=-1;
        n2:=FContacts.Count-1;
        for j := 0 to n2 do begin
          cntct:=TGoogleContact(FContacts[j]);
          if cntct.FID_ <> entry.ChildNodes['id'].Text then continue;
          cntct.FChecked := true;
          break;
        end;

        if k>-1 then begin
          if(not cntct.FModified) and (cntct.FETag = entry.Attributes['gd:etag'])then continue;
        end else begin }
          cntct:=TGoogleContact.Create;
          cntct.contacts:=Self;
          FContacts.Add(cntct);
          {cntct.FChecked := true;
        end;}
        parseUserData(entry,cntct);
        cntct.FModified:=false;
      end;
      {n2:=FContacts.Count-1;
      j:=0;
      while j < FContacts.Count do begin
        cntct:=TGoogleContact(FContacts[j]);
        if cntct.FChecked then begin
          inc( j );
          continue;
        end;
        FContacts.Remove( cntct );
      end;}

    finally
      xml.Free;
      CoUninitialize;
    end;
  end;
end;

function TKRGoogleContacts.RetrievingAllDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
  Response: TStringStream;
  xml, xmlFull: TXMLDocument;
  RootNode, fr: IXMLNode;
  cnt, totalResults, itemsPerPage: integer;
  url: String;
  //updt: TDateTime;

  procedure prs2;
  var
    i, n1{, n2, j, k}: integer;
    entry, fe: IXMLNode;
    cntct: TGoogleContact;
    //dt: TDateTime;
    nn: String;
  begin
    n1 := RootNode.ChildNodes.Count - 1;
    for i := 0 to n1 do begin
      nn:=RootNode.ChildNodes[i].NodeName;
      if nn = 'openSearch:totalResults' then begin
        totalResults:=StrToIntDef( RootNode.ChildNodes[i].Text, 0);
        continue;
      end;
      if nn = 'openSearch:itemsPerPage' then begin
        itemsPerPage:=StrToIntDef( RootNode.ChildNodes[i].Text, 0);
        continue;
      end;
      if nn <> 'entry' then continue;
      entry:=RootNode.ChildNodes[i];
      if entry.ChildNodes['category'].Attributes['term'] <> 'http://schemas.google.com/contact/2008#contact' then continue;
      inc( cnt );
      {n2 := fr.ChildNodes.Count - 1;
      k:=-1;
      for j := 0 to n2 do begin
        if fr.ChildNodes[j].NodeName <> 'entry' then continue;
        fe:=fr.ChildNodes[j];
        if fe.ChildNodes['category'].Attributes['term'] <> 'http://schemas.google.com/contact/2008#contact' then continue;
        if fe.ChildNodes['id'].Text <> entry.ChildNodes['id'].Text then continue;
        k := j;
        break;
      end;}
      //dt := gcDateToDateTime( entry.ChildNodes['updated'].Text );
      {if k > -1 then begin
        if fe.Attributes['gd:etag'] = entry.Attributes['gd:etag'] then continue;
        fr.ChildNodes.Remove( fe );
        fr.ChildNodes.Insert( k, entry );
      end else }fr.ChildNodes.Add( entry );
      //if dt > updt then updt := dt;
    end;
  end;

begin
  Result := nil;
  CoInitialize(nil);
  data := AData;
  http := HTTPCreate;

  xmlFull := TXMLDocument.Create( Self );
  try

    if FileExists( data.res ) then begin
      DeleteFile( data.res );
      //xmlFull.LoadFromFile( data.res );
      xmlFull.Active:=true;
      //FEMailID := KRHTTPEncode( xmlFull.DocumentElement.ChildNodes['id'].Text );
      //data.url := data.url + '&updated-min=' + xmlFull.DocumentElement.ChildNodes['updated'].Text;
    end else xmlFull.Active:=true;

    Response := GET( http, data );

    if data.isOk then begin
      xml:=TXMLDocument.Create( Self );
      try
        Response.Position:=0;
        //updt := 0;
        xml.LoadFromStream(Response);
        xml.Active := true;
        RootNode := xml.DocumentElement;
        //if xmlFull.DocumentElement = nil then begin
          xmlFull.Version := xml.Version;
          xmlFull.Encoding := xml.Encoding;
          xmlFull.DocumentElement := xmlFull.CreateNode('feed', ntElement, RootNode.NamespaceURI );
          FEMailID := RootNode.ChildNodes['id'].Text;
          xmlFull.DocumentElement.AddChild('id').Text := FEMailID;
          FEMailID := KRHTTPEncode( FEMailID );
          //xmlFull.DocumentElement.AddChild('updated').Text := '';
        //end;
        fr := xmlFull.DocumentElement;

        totalResults:=0;
        itemsPerPage:=0;
        cnt := 0;
        prs2;

        url := data.url;
        while cnt<totalResults do begin
          data.url := url + '&start-index=' + IntToStr( cnt + 1 );
          FreeAndNil( Response );
          Response := GET( Http, data );
          if Response.Size=0 then break;
          xml.Active:=false;
          Response.Position:=0;
          xml.LoadFromStream(Response);
          xml.Active := true;
          RootNode := xml.DocumentElement;

          prs2;
        end;

        //if updt > 0 then begin
          //fr.ChildNodes['updated'].Text := DateTimeTOgcDate( updt );
          xmlFull.SaveToFile( data.res );
        //end;
      finally
        xml.Free;
      end;
    end;

  except end;
  xmlFull.Free;
  HTTPDestroy( Http );
  Response.Free;
  CoUninitialize;
end;

function TKRGoogleContacts.RetrievingGroups: boolean;
var
  i: integer;
  data: TKRGoogleRESTRequestData;
begin
  Result:=false;
  for I := 0 to FGroups.Count-1 do TKRGoogleContactsGroup(FGroups[i]).Free;
  FGroups.Clear;

  FParams.Clear;
  FParams.Add( 'access_token', FGoogleAuth.Token);
  data.url := 'https://www.google.com/m8/feeds/groups/default/full' + getParams;
  KRRunInThread( @data, RetrievingGroupsDo );
  if data.isOk then begin
    for I := 0 to FGroupsTmp.Count-1 do FGroups.Add(FGroupsTmp[i]);
    Result := true;
  end;
end;

function TKRGoogleContacts.RetrievingGroupsDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  i, cnt, totalResults, itemsPerPage: integer;
  url: String;

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
  Result := nil;
  data := AData;
  http := HTTPCreate;
  Response := GET( http, data );

  if data.isOk then begin
    CoInitialize(nil);
    FGroupsTmp.Clear;
    xml:=TXMLDocument.Create(Self);
    try
      cnt:=0;
      Response.Position:=0;
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
          if FEMailID='' then FEMailID:=KRHTTPEncode( RootNode.ChildNodes[i].Text );
      end;

      prs;
      cnt:=cnt+itemsPerPage;
      url := data.url;
      while cnt<totalResults do begin
        data.url := url + '&start-index=' + IntToStr( cnt + 1 );
        FreeAndNil( Response );
        Response := GET( http, data );
        if Response.Size=0 then break;
        xml.Active:=false;
        Response.Position:=0;
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

procedure TKRGoogleContacts.SetDataFolder(const Value: TFileName);
begin
  if (csDesigning in ComponentState) then
    FDataFolder:=ExtractFilePath(Value)
  else FDataFolder:=Value;
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
  RootNode, tmp, title, nm, content, appEdited: IXMLNode;
  phns, mls, orgs, adrs, grps, sites: array of IXMLNode;
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
    for i := 0 to RootNode.ChildNodes.Count - 1 do begin
      if SameText(RootNode.ChildNodes[i].NodeName,'title') then
        title:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'content') then
        content:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'app:edited') then
        appEdited:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'gd:name') then
        nm:=RootNode.ChildNodes[i]
      else if SameText(RootNode.ChildNodes[i].NodeName,'gContact:groupMembershipInfo') then begin
        SetLength(grps,Length(grps)+1);
        grps[Length(grps)-1]:=RootNode.ChildNodes[i];
      end else if SameText(RootNode.ChildNodes[i].NodeName,'gContact:website') then begin
        SetLength(sites,Length(sites)+1);
        sites[Length(sites)-1]:=RootNode.ChildNodes[i];
      end else if SameText(RootNode.ChildNodes[i].NodeName,'gd:email') then begin
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

    for i := 0 to Length(grps)-1 do RootNode.ChildNodes.Remove(grps[i]);
    for i := 0 to cntct.FGroups.Count-1 do begin
      tmp:=RootNode.AddChild('gContact:groupMembershipInfo');
      tmp.Attributes['xmlns:gContact']:='http://schemas.google.com/contact/2008';
      tmp.Attributes['deleted']:='false';
      tmp.Attributes['href']:='http://www.google.com/m8/feeds/groups/'+FEMailID+'/base/'+cntct.FGroups[i];
    end;

    for i := 0 to Length(sites)-1 do RootNode.ChildNodes.Remove(sites[i]);
    for i := 0 to cntct.FWebSiteList.Count-1 do begin
      tmp:=RootNode.AddChild('gContact:website');
      tmp.Attributes['xmlns:gContact']:='http://schemas.google.com/contact/2008';
      tmp.Attributes['href']:= TGoogleContactWebSite( cntct.FWebSiteList[i] ).FHref;
      tmp.Attributes['label']:= TGoogleContactWebSite( cntct.FWebSiteList[i] ).FSiteLabel;
    end;

    for I := 0 to Length(mls)-1 do RootNode.ChildNodes.Remove(mls[i]);
    for I := 0 to cntct.EmailCount-1 do begin
      tmp:=RootNode.AddChild('gd:email');
      if cntct.Emails[i].FLabel<>'' then tmp.Attributes['label']:=cntct.Emails[i].FLabel
      else
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
        //nm.Text:=cntct.PostalAddress[i].FCountry.Alpha2codeStr;
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
  sl.Delimiter := ' ';
  Result := sl.DelimitedText;
  sl.Free;
end;

procedure TGoogleContactName.SetAdditionalName(const Value: String);
begin
  if trim(Value)<>FAdditionalName then begin
    FAdditionalName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetFamilyName(const Value: String);
begin
  if trim(Value)<>FFamilyName then begin
    FFamilyName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetGivenName(const Value: String);
begin
  if trim(Value)<>FGivenName then begin
    FGivenName := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetNamePrefix(const Value: String);
begin
  if trim(Value)<>FNamePrefix then begin
    FNamePrefix := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactName.SetNameSuffix(const Value: String);
begin
  if trim(Value)<>FNameSuffix then begin
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

  self.FGroups.Assign( AContact.FGroups );

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
  FGroups:=TStringList.Create;
  FName:=TGoogleContactName.Create;
  FName.contact:=self;
  FPhoto:=TGoogleContactPhoto.Create;
  FPhoto.contact:=self;
  FEmailList:=TList.Create;
  FPhoneList:=TList.Create;
  FOrganizationList:=TList.Create;
  FPostalAddressList:=TList.Create;
  FWebSiteList:=TList.Create;
end;

function TGoogleContact.CreateDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Result:=Pointer( contacts.Post( http, data, 'application/atom+xml' ) );
  contacts.HTTPDestroy( Http );
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
  for I := 0 to FWebSiteList.Count-1 do TGoogleContactWebSite(FWebSiteList[i]).Free;
  FWebSiteList.Free;
  FGroups.Free;
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

function TGoogleContact.GetGroups(Index: integer): String;
begin
  Result := FGroups[ Index ];
end;

function TGoogleContact.GetGroupsCount: integer;
begin
  Result := FGroups.Count;
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

function TGoogleContact.GetWebSite(AIndex: Integer): TGoogleContactWebSite;
begin
  result:=TGoogleContactWebSite(FWebSiteList[AIndex]);end;

function TGoogleContact.GetWebSiteCount: integer;
begin
  Result:=FWebSiteList.Count;
end;

procedure TGoogleContact.GroupAdd(AGroup: TKRGoogleContactsGroup);
var
  I: Integer;
begin
  for I := 0 to FGroups.Count-1 do
    if FGroups[i] = AGroup.id then exit;
  FGroups.Add( AGroup.id );
end;

procedure TGoogleContact.GroupDel(AIndex: Integer);
begin
  FGroups.Delete( AIndex );
  FModified:=true;
end;

function TGoogleContact.InGroup(AGroup: TKRGoogleContactsGroup): boolean;
var
  I: Integer;
begin
  Result:=true;
  for I := 0 to FGroups.Count-1 do
    if FGroups[i]=AGroup.id then exit;
  Result:=false;
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
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  data: TKRGoogleRESTRequestData;
begin
  result:=false;
  contacts.FParams.Clear;
  contacts.FParams.Add( 'access_token', contacts.FGoogleAuth.Token);
  data.url := 'https://www.google.com/m8/feeds/contacts/default/full/' + FID + contacts.getParams;
  Response := TStringStream( KRRunInThread( @data, RetrieveDo ) );
  if data.isOk then begin
    CoInitialize(nil);
    xml:=TXMLDocument.Create(contacts);
    try
      Response.Position:=0;
      xml.LoadFromStream(Response);
      xml.Active := true;
      RootNode := xml.DocumentElement;
      if RootNode.NodeName = 'entry' then begin
        if RootNode.ChildNodes['category'].Attributes['term'] = 'http://schemas.google.com/contact/2008#contact' then begin
          contacts.parseUserData(RootNode,self);
          result:=true;
          end;
      end;
    finally
      xml.Free;
      CoUninitialize;
      Response.Free;
    end;
  end;
end;

function TGoogleContact.RetrieveDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Result:=Pointer( contacts.GET( http, data ) );
  contacts.HTTPDestroy( Http );
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
  Response: TStringStream;
  Data: TKRGoogleRESTRequestData;
begin
  Result := false;
  data.sData := TMemoryStream.Create;
  contacts.FParams.Create;
  contacts.FParams.Add( 'access_token', contacts.FGoogleAuth.Token);
  if FID<>'' then begin
    contacts.userDataToXml( '<?xml version="1.0" encoding="UTF-8"?>'#$A + fxml, self, data.sData );
    data.url := 'https://www.google.com/m8/feeds/contacts/default/full/' + FID + contacts.getParams;
    Response := TStringStream( KRRunInThread( @data, UpdateDo ) );
  end else begin
    contacts.userDataToXml('<?xml version="1.0" encoding="UTF-8"?>'#$A+
      '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005" xmlns:gContact="http://schemas.google.com/contact/2008">'#$A+
      ' <category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/contact/2008#contact"/>'#$A+
      '</entry>'#$A
    ,self,data.sData);
    data.url := 'https://www.google.com/m8/feeds/contacts/default/full/' + FID + contacts.getParams;
    Response := TStringStream( KRRunInThread( @data, CreateDo ) );
  end;
  if data.isOk then begin
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
      Response.Free;
    end;
  end;
  data.sData.Free;
end;

function TGoogleContact.UpdateDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=FETag;
  Result:=Pointer( contacts.PUT( http, data, 'application/atom+xml' ) );
  contacts.HTTPDestroy( Http );
end;

function TGoogleContact.WebSiteAdd(AHref,
  ALabel: String): TGoogleContactWebSite;
begin
  Result:=TGoogleContactWebSite.Create;
  Result.contact:=self;
  Result.FHref:=AHref;
  Result.FSiteLabel:=ALabel;
  FWebSiteList.Add(Result);
  FModified:=true;
end;

procedure TGoogleContact.WebSiteDelete(AIndex: integer);
begin
  TGoogleContactWebSite(FWebSiteList[AIndex]).Free;
  FWebSiteList.Delete(AIndex);
  FModified:=true;
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
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  http := contact.contacts.HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match'] := data.res;
  contact.contacts.DEL( http, data );
  contact.contacts.HTTPDestroy( Http );
end;

function TGoogleContactPhoto.getBitmap: TBitmap;
var
  ini: TIniFile;
  ms, s, s0: String;
  n, i: integer;
  png: TPngImage;

  function mkPath(APath: String): String;
  begin
    if not DirectoryExists(APath) then mkDir(APath);
    Result:=APath+'\';
  end;

begin
  result:=nil;
  if(not Assigned(contact.contacts))then exit;
  if(not Assigned(contact.contacts.FGoogleAuth))then exit;
  if FEtag<>'' then begin
    ini:=TIniFile.Create( contact.contacts.FDataFolder + 'contacts.ini' );
    try
      ms := FHref + '!' + FETag;
      n := ini.ReadInteger('Photos', 'Count', 0) - 1;
      for I := 0 to n do begin
        s := IntToStr( i );
        if ini.ReadString('Photos', 'Photo' + s, '') = ms then begin
          png:=TPngImage.Create;
          Result:=TBitmap.Create;
          try
            png.LoadFromFile( ini.ReadString('Photos', 'PhotoPath' + s, '') );
            Result.Assign(png);
          except
            FreeAndNil( Result );
          end;
          png.Free;
          exit;
        end;
      end;

      s:=FHref;
      Result := TBitmap( KRRunInThread( Pointer(PChar(s)), thLoadIcon) );
      if Result<>nil then begin
        inc(n);
        randomize;
        s:=contact.contacts.FDataFolder;

        i:=100000000 + Random(899999999);
        while fileExists( s + IntToStr( i div 10000000 ) + '\' + IntToStr( i div 10000 ) + '\' + IntToStr( i ) + '.png' ) do  i:=Random(99999999);
        s := mkPath( mkPath( s + IntToStr( i div 10000000 ) ) + IntToStr( i div 10000 ) ) + IntToStr( i ) + '.png';
        png := TPngImage.Create;
        png.Assign( Result );
        png.SaveToFile( s );
        png.Free;
        s0 := IntToStr( n );
        ini.WriteString( 'Photos', 'Photo' + s0, ms );
        ini.WriteString( 'Photos', 'PhotoPath' + s0, s );
        ini.WriteInteger( 'Photos', 'Count', n + 1 );
      end;

    finally
      ini.Free;
    end;

  end;
end;

function TGoogleContactPhoto.getEtagDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contact.contacts.HTTPCreate;
  Result := Pointer( contact.contacts.GET( http, data ) );
  if data.isOk then
    data.res := Http.Response.ETag;
  contact.contacts.HTTPDestroy( Http );
end;

function TGoogleContactPhoto.LoadDo(AThread: TThread; AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  Result := nil;
  data := AData;
  http:=contact.contacts.HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=data.res;
  contact.contacts.PUT( http, data, 'image/png' ).Free;
  contact.contacts.HTTPDestroy( Http );
end;

procedure TGoogleContactPhoto.setBitmap(const Value: TBitmap);
var
  png: TPngImage;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  i: integer;
  data: TKRGoogleRESTRequestData;
  Response: TStringStream;
begin
  if(not Assigned(contact.contacts))then exit;
  if(not Assigned(contact.contacts.FGoogleAuth))then exit;
  if Value=nil then begin
    if FETag<>'' then begin
      contact.contacts.FParams.Clear;
      contact.contacts.FParams.Add( 'access_token', contact.contacts.FGoogleAuth.Token);
      data.url := 'https://www.google.com/m8/feeds/photos/media/default/' + contact.FID + contact.contacts.getParams;
      data.res := FETag;
      KRRunInThread( @data, DeleteDo );
      if data.isOk then FETag:='';
    end;
  end else begin
    data.sData := TMemoryStream.Create;
    png:=TPngImage.Create;
    try
      png.Assign(Value);
      png.SaveToStream( data.sData );
      data.res := FETag;
      data.url := Fhref + contact.contacts.getParams;
      KRRunInThread( @data, LoadDo );
      {if data.isOk then begin
        contact.contacts.FParams.Add( 'fields', 'updated,link(@gd:etag)' );
        data.url := 'https://www.google.com/m8/feeds/contacts/default/full/' + contact.FID + contact.contacts.getParams;
        Response := TStringStream( KRRunInThread( @data, getEtagDo ) );
        if data.isOk then begin
          CoInitialize(nil);
          xml:=TXMLDocument.Create(contact.contacts);
          try
            Response.Position:=0;
            xml.LoadFromStream( Response );
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
            Response.Free;
          end;
        end;
      end;}
    finally
      png.Free;
      data.sData.Free;
    end;
  end;
end;

function TGoogleContactPhoto.SetPhoto(AFileName: TFileName): boolean;
var
  data: TKRGoogleRESTRequestData;
  buf: array[0..3] of byte;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  Response: TStringStream;
  n: integer;
begin
  Result:=false;
  if(not Assigned(contact.contacts))then exit;
  if(not Assigned(contact.contacts.FGoogleAuth))then exit;

  data.sData:=TMemoryStream.Create;
  try
    TMemoryStream( data.sData ).LoadFromFile( AFileName );
    data.sData.Position:=0;
    data.sData.Read(buf,4);
    data.res:='';
    if(buf[0]=$FF)and(buf[1]=$D8)and(buf[2]=$FF)and(buf[3] shr 4 = $E)then data.res:='image/jpeg'
    else if(buf[1]=$50)and(buf[2]=$4E)and(buf[3]=$47)then data.res:='image/png';
    if data.res <> '' then begin
      data.data := FETag;
      data.url := Fhref + contact.contacts.getParams;
      Response := TStringStream( KRRunInThread( @data, SetPhotoDo ) );
      if data.isOk then begin
        CoInitialize(nil);
        xml:=TXMLDocument.Create(contact.contacts);
        try
          Response.Position := 0;
          xml.LoadFromStream( Response );
          xml.Active:=true;

          Result:=true;
        finally
          xml.Free;
          CoUninitialize;
          Response.Free;
        end;
      end;
    end;

  except end;
  if not Result then begin
    n:=0;
  end;

  data.sData.Free;
end;

function TGoogleContactPhoto.SetPhotoDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http:=contact.contacts.HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=data.data;
  data.data:='';
  result:=Pointer( contact.contacts.PUT( http, data, data.res ) );
  contact.contacts.HTTPDestroy( Http );
end;

function TGoogleContactPhoto.thLoadIcon(AThread: TThread;
  AData: Pointer): Pointer;
var
  url: String;
  http: TKRIdHTTP;
  bmp: TBitmap;
begin
  Http:=contact.contacts.HTTPCreate;
  Http.Request.Accept := '';
  url := String( PChar( AData ) );

  bmp:=TBitmap.Create;
  if not KRDownloadImageToBMP( url, Http, bmp) then FreeAndNil( bmp );

  Result:=Pointer( bmp );
  contact.contacts.HTTPDestroy( http );
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
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Result:=Pointer( contacts.Post( http, data, 'application/atom+xml' ) );
  contacts.HTTPDestroy( Http );
end;

function TKRGoogleContactsGroup.Retrieve: boolean;
var
  Response: TStringStream;
  xml: TXMLDocument;
  RootNode: IXMLNode;
  data: TKRGoogleRESTRequestData;
begin
  result:=false;
  contacts.FParams.Clear;
  contacts.FParams.Add( 'access_token', contacts.FGoogleAuth.Token);
  data.url := 'https://www.google.com/m8/feeds/groups/default/full/' + FID + contacts.getParams;
  Response := TStringStream( KRRunInThread( @data, RetrieveDo ) );
  if data.isOk then begin
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
      Response.Free;
    end;
  end;
end;

function TKRGoogleContactsGroup.RetrieveDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Result:=Pointer( contacts.GET( http, data ) );
  contacts.HTTPDestroy( Http );
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
  Response: TStringStream;
  Data: TKRGoogleRESTRequestData;
begin
  Result:=false;
  data.sData := TMemoryStream.Create;
  contacts.FParams.Create;
  contacts.FParams.Add( 'access_token', contacts.FGoogleAuth.Token);
  if FID<>'' then begin
    contacts.groupDataToXml( '<?xml version="1.0" encoding="UTF-8"?>'#$A + fxml, self, data.sData );
    data.url := 'https://www.google.com/m8/feeds/groups/default/full/' + FID + contacts.getParams;
    Response := TStringStream( KRRunInThread( @data, UpdateDo ) );
  end else begin
    contacts.groupDataToXml( '<?xml version="1.0" encoding="UTF-8"?>'#$A+
      '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005">'#$A+
      ' <category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/g/2008#group"/>'#$A+
      '</entry>'#$A
    , self, data.sData );
    data.url := 'https://www.google.com/m8/feeds/groups/default/full/' + FID + contacts.getParams;
    Response := TStringStream( KRRunInThread( @data, CreateDo ) );
  end;
  if data.isOk then begin
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
      Response.Free;
    end;
  end;
  data.sData.Free;
end;

function TKRGoogleContactsGroup.UpdateDo(AThread: TThread;
  AData: Pointer): Pointer;
var
  http: TKRIdHTTP;
  data: PKRGoogleRESTRequestData;
begin
  data := AData;
  http := contacts.HTTPCreate;
  Http.Request.CustomHeaders.Values['If-Match']:=FETag;
  Result:=Pointer( contacts.PUT( http, data, 'application/atom+xml' ) );
  contacts.HTTPDestroy( Http );
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
  sl.Delimiter := ' ';
  Result:=sl.DelimitedText;
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
  if trim(Value)<>FHousename then begin
    FHousename := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetLabel(const Value: String);
begin
  if trim(Value) <> FLabel  then begin
    FLabel := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetNeighborhood(const Value: String);
begin
  if trim(Value) <> FNeighborhood then begin
    FNeighborhood := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetPobox(const Value: String);
begin
  if trim(Value)<>FPobox then begin
    FPobox := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactPostalAddress.SetPostcode(const Value: String);
begin
  if trim(Value)<>FPostcode then begin
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
  if trim(Value)<>FRegion then begin
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
  if trim(Value)<>FStreet then begin
    FStreet := trim(Value);
    if Assigned(contact) then contact.FModified:=true;
  end;
end;

{ TGoogleContactWebSite }

procedure TGoogleContactWebSite.SetHref(const Value: String);
begin
  if trim(Value) <> FHref then begin
    FHref := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

procedure TGoogleContactWebSite.SetSiteLabel(const Value: String);
begin
  if trim(Value) <> FSiteLabel then begin
    FSiteLabel := trim(Value);
    if assigned(contact) then contact.FModified:=true;
  end;
end;

end.

