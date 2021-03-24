(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRIniConfig                                                               *)
(*  Ver.: 16.09.2019                                                          *)
(*  https://kandiral.ru/delphi/kriniconfig.pas.html                           *)
(*                                                                            *)
(******************************************************************************)
unit KRIniConfig;

interface

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils, System.Classes, System.Variants, System.IniFiles, Vcl.Forms,
    System.StrUtils,
  {$ELSE}
    SysUtils, Classes, Variants, IniFiles, Forms, StrUtils,
  {$IFEND}
    KRComponentCollection, KRRuntimeErrors, KRVariants, KRWindows, KRCryptCommon,
    KRBase64, KRBCrypt;

type

  TKRIniCfgParam = class;

  IKRCfgParam = interface
    ['{9DCC67B8-0E18-4C87-A14C-F5E2843707D6}']
    procedure CfgParamChange(AParam: TKRIniCfgParam); stdcall;
  end;

  IKRCfgParamEditor = interface
    ['{8403B718-0DC3-493B-9AC2-F367BB5E9710}']
    procedure OK;
    procedure Cancel;
  end;

  TKRIniCfgValType = (icvtString, icvtInteger, icvtFloat, icvtBool, icvtDWORD);

  TKRIniConfig = class;

  TKRCfgBackupEv = procedure(ACfgFile: TIniFile; var ACompl: boolean) of object;

  TKRIniCfgParam = class(TKRComponentCollectionItem)
  private
    FIniConfig: TKRIniConfig;
    FValueType: TKRIniCfgValType;
    FSection: String;
    FValue: Variant;
    FList: TList;
    FDefStr: String;
    FDefInt: integer;
    FDefFlt: Extended;
    FDefBool: boolean;
    FDefDWORD: Cardinal;
    FEncrypt: boolean;
    FChange: TNotifyEvent;
    procedure SetSection(const Value: String);
    procedure SetValue(const Value: Variant);
    procedure SetValueType(const Value: TKRIniCfgValType);
    function GetValue: Variant;
    procedure SetDefaultValue(const Value: String);
    function GetDefaultValue: String;
    procedure SetEncrypt(const Value: boolean);
  protected
    procedure DoChange;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure AddMon(AMon: IKRCfgParam);
    procedure DelMon(AMon: IKRCfgParam);
    procedure UpValue;
    property IniConfig: TKRIniConfig read FIniConfig;
    property Value: Variant read GetValue write SetValue;
  published
    property Section: String read FSection write SetSection;
    property ValueType: TKRIniCfgValType read FValueType write SetValueType default icvtString;
    property DefaultValue: String read GetDefaultValue write SetDefaultValue;
    property Encrypt: boolean read FEncrypt write SetEncrypt;
    property Name;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TKRIniConfig = class(TKRComponentCollection)
  private
    FIniFile: TIniFile;
    FFileName: TFileName;
    FApplicationPath: boolean;
    FBkSave: TKRCfgBackupEv;
    FBkLoad: TKRCfgBackupEv;
    FAppDataPath: boolean;
    FAppDataPathS: String;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FPassword: String;
    FPassHash: TBytes;
    procedure SetFileName(const Value: TFileName);
    procedure SetApplicationPath(const Value: boolean);
    procedure SetAppDataPath(const Value: boolean);
    function EncryptStr(AString: String): String;
    function DecryptStr(AString: String): String;
    procedure SetPassword(const Value: String);
  protected
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    class procedure SetOK(AParent: TComponent);
    class procedure SetCancel(AParent: TComponent);
    property IniFile: TIniFile read FIniFile;
    procedure SaveConfig(AFileName: String);
    procedure LoadConfig(AFileName: String);
    function AddParam(AName, ASection: String; AType: TKRIniCfgValType;
        ADefaultValue: Variant): TKRIniCfgParam;
    function ConfigPath: String;
    function GetParamByName(AName: String;
      ACaseSensitive: boolean = false): TKRIniCfgParam;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property ApplicationPath: boolean read FApplicationPath write SetApplicationPath default true;
    property AppDataPath: boolean read FAppDataPath write SetAppDataPath default false;
    property Password: String read FPassword write SetPassword;
    property OnBkSave: TKRCfgBackupEv read FBkSave write FBkSave;
    property OnBkLoad: TKRCfgBackupEv read FBkLoad write FBkLoad;
    property OnRuntimeError: TKRRuntimeErrorEv read FRuntimeErrorEv write FRuntimeErrorEv;
  end;

implementation

{ TKRIniConfig }

function TKRIniConfig.AddParam(AName, ASection: String; AType: TKRIniCfgValType;
  ADefaultValue: Variant): TKRIniCfgParam;
begin
  Result:=TKRIniCfgParam.Create(Self);
  Result.Name:=AName;
  Result.Section:=ASection;
  Result.ValueType:=AType;
  Result.DefaultValue:=ADefaultValue;
  inherited AddItem(Result);
end;

procedure TKRIniConfig.AftAddItem(var AItem: TKRComponentCollectionItem);
begin
  TKRIniCfgParam(AItem).FIniConfig:=self;
end;

function TKRIniConfig.ConfigPath: String;
begin
  if FApplicationPath then Result:=ExtractFilePath(Application.ExeName)
  else if FAppDataPath then Result:=FAppDataPathS
  else Result:=ExtractFilePath(FFileName);
end;

constructor TKRIniConfig.Create(AOwner: TComponent);
begin
  inherited;
  inherited SetItemClass(TKRIniCfgParam);
  FFileName:='config.ini';
  AppDataPath:=false;
  FApplicationPath:=true;
end;

function TKRIniConfig.DecryptStr(AString: String): String;
var
  smFrom, smTo: TMemoryStream;
  s: AnsiString;
  n,i: integer;
  buf: TBytes;
begin
  if (Length(FPassHash)=0) or (AString='') then begin
    Result:=AString;
    exit;
  end;

  s:=AnsiString(AString);
  smFrom:=TMemoryStream.Create;
  smTo:=TMemoryStream.Create;
  try
    SetLength(buf,(Length(s) shr 2) * 3);
    n:=KRBase64Decode(@s[1],@buf[0],Length(s),@KRBASE64_DEFAULT_DECODE_TABLE,KRBASE64_DEFAULT_SUFFIX);
    smFrom.Write((@buf[0])^,n);
    KRBCrypt_RC4_Decrypt(smFrom,smTo,FPassHash);
    SetLength(buf,smTo.Size);
    smTo.Position:=0;
    smTo.Read((@buf[0])^,smTo.Size);
    Result:='';
    n:=(smTo.Size shr 1)-1;
    for i := 0 to n do
      Result:=Result+Char((word(buf[i+i])shl 8)or buf[i+i+1]);
  finally
    smFrom.Free;
    smTo.Free;
  end;
end;

destructor TKRIniConfig.Destroy;
begin
  if Assigned(FIniFile) then FIniFile.Free;
  inherited;
end;

function TKRIniConfig.EncryptStr(AString: String): String;
var
  smFrom, smTo: TMemoryStream;
  n, i: integer;
  buf: TBytes;
  s: AnsiString;
begin
  if (Length(FPassHash)=0) or (AString='') then begin
    Result:=AString;
    exit;
  end;

  smFrom:=TMemoryStream.Create;
  smTo:=TMemoryStream.Create;
  try
    n:=Length(AString);
    SetLength(buf,n shl 1);
    for i := 1 to n do begin
      buf[i+i-2]:=Ord(AString[i])shr 8;
      buf[i+i-1]:=Ord(AString[i]);
    end;
    smFrom.Write((@buf[0])^,n shl 1);
    KRBCrypt_RC4_Encrypt(smFrom,smTo,FPassHash);
    SetLength(buf,smTo.Size);
    smTo.Position:=0;
    smTo.Read((@buf[0])^,smTo.Size);
    KRBase64EncodeBytes(@buf[0],smTo.Size,s,@KRBASE64_DEFAULT_CHAR_TABLE,KRBASE64_DEFAULT_SUFFIX);
    Result:=String(s);
  finally
    smFrom.Free;
    smTo.Free;
  end;
end;

function TKRIniConfig.GetParamByName(AName: String; ACaseSensitive: boolean = false): TKRIniCfgParam;
var
  i: integer;
  s0,s1: String;
begin
  Result:=nil;
  if ACaseSensitive then s0:=LowerCase(AName) else s0:=AName;
  for i := 0 to ItemsCount-1 do begin
    if ACaseSensitive then s1:=LowerCase(Items[i].Name) else s1:=Items[i].Name;
    if CompareText(s0,s1)=0 then begin
      Result:=TKRIniCfgParam(Items[i]);
      exit;
    end;
  end;
end;

procedure TKRIniConfig.LoadConfig(AFileName: String);
var
  i: integer;
  _inifl: TIniFile;
  prm: TKRIniCfgParam;
  b: boolean;
begin
  _inifl:=TIniFile.Create(AFileName);
  b:=true;
  if Assigned(FBkLoad) then FBkLoad(_iniFl,b);
  if b then begin
    for i := 0 to ItemsCount-1 do begin
      prm:=TKRIniCfgParam(Items[i]);
      case prm.FValueType of
        icvtString: prm.Value:=_iniFl.ReadString(prm.FSection,prm.Name,prm.Value);
        icvtInteger: prm.Value:=_iniFl.ReadInteger(prm.FSection,prm.Name,prm.Value);
        icvtFloat: prm.Value:=_iniFl.ReadFloat(prm.FSection,prm.Name,prm.Value);
        icvtBool: prm.Value:=_iniFl.ReadBool(prm.FSection,prm.Name,prm.Value);
        icvtDWORD: prm.Value:=StrToIntDef(_iniFl.ReadString(prm.FSection,prm.Name,IntToStr(prm.Value)),prm.Value);
      end;
      TKRIniCfgParam(Items[i]).FValue:=null;
    end;
  end;
  _iniFl.Free;
end;

procedure TKRIniConfig.SaveConfig(AFileName: String);
var
  i: integer;
  _inifl: TIniFile;
  prm: TKRIniCfgParam;
  b: boolean;
begin
  _inifl:=TIniFile.Create(AFileName);
  b:=true;
  if Assigned(FBkSave) then FBkSave(_iniFl,b);
  if b then begin
    for i := 0 to ItemsCount-1 do begin
      prm:=TKRIniCfgParam(Items[i]);
      case prm.FValueType of
        icvtString: begin
          prm.GetValue;
          _iniFl.WriteString(prm.FSection,prm.Name,prm.FValue);
        end;
        icvtInteger: _iniFl.WriteInteger(prm.FSection,prm.Name,prm.Value);
        icvtFloat: _iniFl.WriteFloat(prm.FSection,prm.Name,prm.Value);
        icvtBool: _iniFl.WriteBool(prm.FSection,prm.Name,prm.Value);
        icvtDWORD: _iniFl.WriteString(prm.FSection,prm.Name,IntToStr(prm.Value));
      end;
      TKRIniCfgParam(Items[i]).FValue:=null;
    end;
  end;
  _iniFl.Free;
end;

procedure TKRIniConfig.SetAppDataPath(const Value: boolean);
begin
  FAppDataPath := Value;
  if FAppDataPath then begin
    FApplicationPath := false;
    if Application.Name<>'' then FAppDataPathS:=Application.Name else begin
      FAppDataPathS:=ExtractFileName(Application.ExeName);
      FAppDataPathS:=LeftStr(FAppDataPathS,Length(FAppDataPathS)-
        Length(ExtractFileExt(Application.ExeName)));
    end;

    FAppDataPathS:=KREnvGetValue('APPDATA')+'\'+FAppDataPathS;
    if not DirectoryExists(FAppDataPathS) then MkDir(FAppDataPathS);
    FAppDataPathS:=FAppDataPathS+'\';
  end;
  SetFileName(FFileName);
end;

procedure TKRIniConfig.SetApplicationPath(const Value: boolean);
begin
  FApplicationPath := Value;
  if FApplicationPath then FAppDataPath:=false;
  SetFileName(FFileName);
end;

class procedure TKRIniConfig.SetCancel(AParent: TComponent);
var i: integer;
elem: IKRCfgParamEditor;
begin
  for i := 0 to AParent.ComponentCount-1 do
    if Supports(AParent.Components[i], IKRCfgParamEditor, elem)then
      elem.Cancel;
end;

procedure TKRIniConfig.SetFileName(const Value: TFileName);
var
  s: String;
  i,j: integer;
begin
  FFileName:=Value;
  if FApplicationPath then s:=ExtractFilePath(Application.ExeName)+ExtractFileName(FFileName)
  else if FAppDataPath then s:=FAppDataPathS+ExtractFileName(FFileName)
  else s:=FFileName;
  if(FIniFile=nil)or(FIniFile.FileName<>s)then begin
    if Assigned(FIniFile) then FIniFile.Free;
    FIniFile:=TIniFile.Create(s);
    for i := 0 to ItemsCount-1 do begin
      TKRIniCfgParam(Items[i]).FValue:=null;
      for j := 0 to TKRIniCfgParam(Items[i]).FList.Count-1 do
        if TKRIniCfgParam(Items[i]).Value<>null then begin
//          TKRIniCfgParam(Items[i]).FValue:=null;
          IKRCfgParam(TKRIniCfgParam(Items[i]).FList.Items[j]).CfgParamChange(TKRIniCfgParam(Items[i]));
        end;
    end;
  end;
end;

class procedure TKRIniConfig.SetOK(AParent: TComponent);
var i: integer;
elem: IKRCfgParamEditor;
begin
  for i := 0 to AParent.ComponentCount-1 do
    if Supports(AParent.Components[i], IKRCfgParamEditor, elem)then
      elem.Ok;
end;

procedure TKRIniConfig.SetPassword(const Value: String);
var
  ms: TMemorySTream;
  n: integer;
  buf: TBytes;
  hash: TKRSHA1Hash;
begin
  FPassword := Value;
  ms:=TMemorySTream.Create;
  try
    n:=Length(FPassword)shl 1;
    SetLength(buf,n);
    ms.Write((@buf[0])^,n);
    KRBCrypt_SHA1(ms,hash);
    SetLength(FPassHash,20);
    Move((@hash.bytes[0])^,(@FPassHash[0])^,20);
  finally
    ms.Free;
  end;
end;

{ TKRIniCfgParam }

procedure TKRIniCfgParam.AddMon(AMon: IKRCfgParam);
var
  i: integer;
begin
  if FList.Count>0 then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]=Pointer(AMon) then exit;

  FList.Add(Pointer(AMon));
end;

constructor TKRIniCfgParam.Create(AOwner: TComponent);
begin
  inherited;
  FIniConfig:=nil;
  FValue:=null;
  FValueType:=icvtString;
  FDefStr:='';
  FList:=TList.Create;
end;

procedure TKRIniCfgParam.DelMon(AMon: IKRCfgParam);
begin
  FList.Remove(Pointer(AMon));
end;

destructor TKRIniCfgParam.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TKRIniCfgParam.DoChange;
var
  i: integer;
begin
  if FList.Count>0 then
    for i := 0 to FList.Count-1 do
      IKRCfgParam(FList.Items[i]).CfgParamChange(Self);
  if Assigned(FChange) then FChange(Self);
end;

function TKRIniCfgParam.GetDefaultValue: String;
begin
  Result:='';
  case FValueType of
    icvtString: Result:=FDefStr;
    icvtInteger: Result:=IntToStr(FDefInt);
    icvtFloat: Result:=FloatToStr(FDefFlt);
    icvtBool: Result:=BoolToStr(FDefBool,true);
    icvtDWORD: Result:=IntToStr(FDefDWORD);
  end;
end;

function TKRIniCfgParam.GetValue: Variant;
begin
  if(FValue=null)and(Assigned(FIniConfig))and(Assigned(FIniConfig.FIniFile))then begin
    case FValueType of
      icvtString:
        if FEncrypt then
            FValue:=FIniConfig.DecryptStr(FIniConfig.FIniFile.ReadString(FSection,Name,FIniConfig.EncryptStr(FDefStr)))
        else
            FValue:=FIniConfig.FIniFile.ReadString(FSection,Name,FDefStr);
      icvtInteger: FValue:=FIniConfig.FIniFile.ReadInteger(FSection,Name,FDefInt);
      icvtFloat: FValue:=FIniConfig.FIniFile.ReadFloat(FSection,Name,FDefFlt);
      icvtBool: FValue:=FIniConfig.FIniFile.ReadBool(FSection,Name,FDefBool);
      icvtDWord: FValue:=StrToIntDef(FIniConfig.FIniFile.ReadString(FSection,Name,IntToStr(FDefDWORD)),FDefDWORD);
    end;
  end;
  Result:=FValue;
end;

procedure TKRIniCfgParam.SetDefaultValue(const Value: String);
var
  n,e: integer;
  i64: int64;
  s: String;
  ex: extended;
  _fs: TFormatSettings;
  b: boolean;
begin
  s:=Trim(Value);
  case FValueType of
    icvtString: FDefStr:=Value;
    icvtInteger: begin
      Val(s,n,e);
      if e=0 then FDefInt:=n;
    end;
    icvtFloat: begin
      _fs:=FormatSettings;
      if TextToFloat(PChar(S), ex, fvExtended, _fs) then begin
        FDefFlt:=ex;
      end else begin
        if _fs.DecimalSeparator=#46 then _fs.DecimalSeparator:=#44 else _fs.DecimalSeparator:=#46;
        if TextToFloat(PChar(S), ex, fvExtended, _fs) then FDefFlt:=ex;
      end;
    end;
    icvtBool: begin
      if TryStrToBool(S, b) then FDefBool:=b;
    end;
    icvtDWORD: begin
      Val(s,i64,e);
      if e=0 then FDefDWORD:=i64;
    end;
  end;
end;

procedure TKRIniCfgParam.SetEncrypt(const Value: boolean);
begin
  if FEncrypt<>Value then begin
    FEncrypt:=Value;
    FValue:=null;
  end;
end;

procedure TKRIniCfgParam.SetSection(const Value: String);
begin
  if FSection<>Value then begin
    FSection := Value;
    FValue:=null;
  end;
end;

procedure TKRIniCfgParam.SetValue(const Value: Variant);
begin
  if(Assigned(FIniConfig))and(Assigned(FIniConfig.FIniFile))then begin
    if(FValue=null)then GetValue;
    if(not KRVarIsEqually(FValue,Value)) then begin
      case FValueType of
        icvtString: if VarIsStr(Value) then begin
          FValue := Value;
          if FEncrypt then
            FIniConfig.FIniFile.WriteString(FSection,Name,FIniConfig.EncryptStr(FValue))
          else
            FIniConfig.FIniFile.WriteString(FSection,Name,FValue);
        end;
        icvtInteger: if(FindVarData(Value)^.VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64])then begin
          FValue := Value;
          FIniConfig.FIniFile.WriteInteger(FSection,Name,FValue);
        end;
        icvtFloat: if(FindVarData(Value)^.VType in [varSingle, varDouble, varCurrency,varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64])then  begin
          FValue := Value;
          FIniConfig.FIniFile.WriteFloat(FSection,Name,FValue);
        end;
        icvtBool: if(FindVarData(Value)^.VType in [varBoolean]) then begin
          FValue := Value;
          FIniConfig.FIniFile.WriteBool(FSection,Name,FValue);
        end;
        icvtDWORD: if(FindVarData(Value)^.VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord])then begin
          FValue := Value;
          FIniConfig.FIniFile.WriteString(FSection,Name,IntToStr(FValue));
        end;
      end;
      DoChange;
    end;
  end;
end;

procedure TKRIniCfgParam.SetValueType(const Value: TKRIniCfgValType);
begin
  if FValueType<>Value then begin
    FValue:=null;
    case Value of
      icvtString: FDefStr:='';
      icvtInteger: FDefInt:=0;
      icvtFloat: FDefFlt:=0;
      icvtBool: FDefBool:=false;
      icvtDWORD: FDefDWORD:=0;
    end;
    FValueType := Value;
    if(csDesigning in ComponentState)and(FValueType<>icvtString) then FEncrypt:=false;
  end;
end;

procedure TKRIniCfgParam.UpValue;
begin
  FValue:=null;
end;

end.
