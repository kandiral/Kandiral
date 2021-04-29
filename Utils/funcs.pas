(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  funcs                                                                     *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit funcs;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils,
    Winapi.MMSystem, Vcl.Forms, System.Math, System.Variants,
    Vcl.Dialogs, Vcl.StdCtrls,
  {$ELSE}
    Windows, Classes, SysUtils, StrUtils, MMSystem, Forms, Math, Variants,
    Dialogs, StdCtrls,
  {$IFEND}
    KRTypes, JwaWinType, JwaWinCrypt, IdCoderMIME;

type
  TIntArray = array of integer;
  TLeng = (lngRU, lngUA);

  function CryptString(Const  Input: string; password : AnsiString;  Encrypt: Boolean) : string;
  function ArrayOfCharToStr(AArrayOfChar: PChar; ALength: integer): String;
  procedure StrToArrayOfChar(AArrayOfChar: PChar; ALength: integer; AStr: String);
  function sourceToHex(buf: PAnsiChar; len: integer): String;
  function Implode(const delim: String; strlist: TStrings): String;
  procedure SortArray(_array: TIntArray; _desc: boolean = false);
  procedure SortStrings(AStrings: TStrings; _desc: boolean = false);
  function TimeSToStr(t:real):string;
  function MAKEBYTE(a,b:byte):byte;
  function decdt(d: TDateTime): integer;
  function DWORDToHex(ANum: DWORD;ACnt: Integer):String;
  function DWordToStr(dw: DWORD):String;
  function parseNum(num: integer; s0,s1,s2:String):String;
  function DateTimeToMKTime(d: TDateTime): DWORD;
  function MKTimeToDateTime(d: DWORD): TDateTime;
  procedure GetComPorts(aList: TStrings; aNameStart: string);
  function GetNextSubstring(aBuf: string; var aStartPos: integer): string;
  function StartsStr(const APrefix : String; const ASource : String): Boolean;
  function HexToInt(HexStr : string) : Int64;overload;
  function HexToInt(HexStr : string; out ARes : Int64): boolean;overload;
  function HexToIntDef(HexStr : string; ADefVal : Int64): Int64;
  function IntToHexStr(ANum,ACnt: Integer):String;
  function PHPTimeToDateTime(tm: cardinal): TDateTime;
  function DateTimeToPHPTime(d: TDateTime): cardinal;
  function ISOTransliterate(AText: String):String;
  procedure AppDelay(AMSec: Cardinal);
  function CTL_CODE(ADeviceType, AFunction, AMethod, AAccess: Cardinal ): Cardinal;
  function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
  function StringToWideString(const s: AnsiString; codePage: Word): WideString;
  function isSumTime(UTC: Cardinal):boolean;
  function Pow10(AExp: byte): Cardinal;
  procedure FindDirs(APath: String; ADirs: TStrings);
  procedure FindFiles(APath: String; AFiles: TStrings; AExclude: Integer = 0);overload;
  procedure FindFiles(APath: String; AFiles: TStrings; ASMask: String = ''; AExclude: Integer = 0);overload;
  procedure FindAllFiles(APath: String; AFiles: TStrings; ASMask: String = ''; AExclude: Integer = 0);
  function PrmStr(ACmdLine: PChar; Index: Integer): string;
  function PrmCount(ACmdLine: PChar): Integer;
  function GetPrmStr(P: PChar; var Param: string): PChar;
  function SecondsToTimeString(ASec: DWORD): String;
  function IsVariantsEqual(A,B: Variant): boolean;
  function AppMsgBox(AText: String; Flags: Longint; ACaption: String = ''): integer;
  function AppMsgBoxErr(AText: String; ACaption: String = ''): integer;
  function AppMsgBoxInf(AText: String; ACaption: String = ''): integer;
  function AppMsgBoxQst(AText: String; ACaption: String = ''): integer;
  function AppMsgBoxQstCh(AText, AChText: String; var ACh: boolean): integer;
  function KRDialogTypeToFlag(ADialogType: TKRDialogType): LongInt;
  function ElapsedTime(ALastTime: Cardinal): Cardinal;
  function StringReplaceExt(const S : string; OldPattern, NewPattern:  array of string; Flags: TReplaceFlags):string;
  function _mysql_real_escape_string(const unescaped_string : string ) : string;
  function parseDigits(ANum: integer; ALeng: TLeng = lngRU; isMale: boolean = true): String;
  function FileTimeToDateTime(FileTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): TDateTime;
  function DateTimeToFileTime(FileTime: TDateTime): {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME;
  function ChangeFileDate(Dir: string; ALastAccessTime, ACreationTime, ALastWriteTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): boolean;overload;
  function ChangeFileDate(Dir: string; ALastAccessTime, ACreationTime, ALastWriteTime: TDateTime): boolean;overload;
  function ReadFileDate(Dir: string; var ALastAccessTime, ACreationTime, ALastWriteTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): boolean;overload;

implementation

function CryptString(Const  Input: string; password : AnsiString;  Encrypt: Boolean) : string;
const
  BufferSize=1024*1024;
var
  StreamSource  : TStringStream;
  StreamDest    : TStringStream;
  CRYPTPROV     : HCRYPTPROV;
  CRYPTHASH     : HCRYPTHASH;
  CRYPTKEY      : HCRYPTKEY;
  Buffer        : LPBYTE;
  BytesIn       : DWORD;
  Final         : Boolean;
  Encoder     : TIdEncoderMIME;
  Decoder     : TIdDecoderMIME;
  DestStream  : TStringStream;
begin
  CryptAcquireContext(CRYPTPROV, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
  try
      //create a valid key  based in the password
      if not CryptCreateHash(CRYPTPROV, CALG_SHA1, 0, 0, CRYPTHASH) then RaiseLastOSError;
      try
        if not CryptHashData(CRYPTHASH, @Password[1], Length(Password), 0) then RaiseLastOSError;
        if not CryptDeriveKey(CRYPTPROV,  CALG_RC4, CRYPTHASH, 0, CRYPTKEY)  then RaiseLastOSError;
      finally
        CryptDestroyHash(CRYPTHASH);
      end;

      StreamSource := TStringStream.Create(Input);
      StreamSource.Position:=0;
      StreamDest   := TStringStream.Create;
      try
        GetMem(Buffer, BufferSize);
        try

          if not Encrypt then
          begin
            //decode the string using base64
            Decoder := TIdDecoderMIME.Create(nil);
            try
              DestStream := TStringStream.Create;
              try
                StreamDest.Position:=0;
                Decoder.DecodeBegin(DestStream);
                Decoder.Decode(StreamSource);
                Decoder.DecodeEnd;
                StreamSource.Clear;
                DestStream.Position:=0;
                StreamSource.CopyFrom(DestStream,DestStream.Size);
                StreamSource.Position:=0;
              finally
                DestStream.Free;
              end;
            finally
              Decoder.Free;
            end;

          end;


            repeat
              BytesIn   := StreamSource.Read(Buffer^, BufferSize);
              Final     := (StreamSource.Position >= StreamSource.Size);
              if Encrypt then
              begin
               if not CryptEncrypt(CRYPTKEY, 0, Final, 0, Buffer, BytesIn, BytesIn) then RaiseLastOSError;
              end
              else
              if not CryptDecrypt(CRYPTKEY, 0, Final, 0, Buffer, BytesIn) then RaiseLastOSError;

              StreamDest.Write(Buffer^, BytesIn);
            until Final;


          //encode the string using base64
          if Encrypt then
          begin
            Encoder := TIdEncoderMIME.Create(nil);
            try
              DestStream:=TStringStream.Create;
              try
                StreamDest.Position:=0;
                Encoder.Encode(StreamDest,DestStream);
                Result := DestStream.DataString;
              finally
                DestStream.Free;
              end;
            finally
              Encoder.Free;
            end;
          end
          else
          Result:= StreamDest.DataString;


        finally
         FreeMem(Buffer, BufferSize);
        end;

      finally
        StreamSource.Free;
        StreamDest.Free;
      end;
  finally
    CryptReleaseContext(CRYPTPROV, 0);
  end;
end;

function ReadFileDate(Dir: string; var ALastAccessTime, ACreationTime, ALastWriteTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): boolean;overload;
var
  h: THandle;
begin
  result:=false;
  H := CreateFile(PChar(Dir), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if H <> INVALID_HANDLE_VALUE then begin
    Result := GetFileTime(H, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
    CloseHandle(H);
  end;
end;

function ChangeFileDate(Dir: string; ALastAccessTime, ACreationTime, ALastWriteTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): boolean;
var
  h: THandle;
begin
  result:=false;
  H := CreateFile(PChar(Dir), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if H <> INVALID_HANDLE_VALUE then begin
    Result := SetFileTime(H, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
    CloseHandle(H);
  end;
end;

function ChangeFileDate(Dir: string; ALastAccessTime, ACreationTime, ALastWriteTime: TDateTime): boolean;
var
  h: THandle;
  LastAccessTime, CreationTime, LastWriteTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME;
begin
  result:=false;
  H := CreateFile(PChar(Dir), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if H <> INVALID_HANDLE_VALUE then begin
    LastAccessTime:=DateTimeToFileTime(ALastAccessTime);
    LastWriteTime:=DateTimeToFileTime(ALastWriteTime);
    CreationTime:=DateTimeToFileTime(ACreationTime);
    Result := SetFileTime(H, @CreationTime, @LastAccessTime, @LastWriteTime);
    CloseHandle(H);
  end;
end;

function FileTimeToDateTime(FileTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME): TDateTime;
var
  ModifiedTime: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if (FileTime.dwLowDateTime = 0) and (FileTime.dwHighDateTime = 0) then Exit;
  try
    FileTimeToLocalFileTime(FileTime, ModifiedTime);
    FileTimeToSystemTime(ModifiedTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  except
    Result := Now;
  end;
end;

function DateTimeToFileTime(FileTime: TDateTime): {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME;
var
  LocalFileTime, Ft: {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows._FILETIME;
  SystemTime: TSystemTime;
begin
  Result.dwLowDateTime  := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(FileTime, SystemTime);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Ft);
  Result := Ft;
end;

function ArrayOfCharToStr(AArrayOfChar: PChar; ALength: integer): String;
var i: integer;
begin
  Result:='';
  for i := 0 to ALength-1 do
    if AArrayOfChar[i]=#0 then break else Result:=Result+AArrayOfChar[i];
end;

procedure StrToArrayOfChar(AArrayOfChar: PChar; ALength: integer; AStr: String);
var
  i,n,m: integer;
begin
  n:=0;
  m:=Min(ALength-2,Length(AStr)-1);
  for i := 0 to m do begin
    inc(n);
    AArrayOfChar[i]:=AStr[n];
  end;
  AArrayOfChar[n]:=#0;
end;

function parseDigits(ANum: integer; ALeng: TLeng = lngRU; isMale: boolean = true): String;
const
  _Dgts : array[1..22,lngRU..lngUA] of String = (
    ('один', 'один'),
    ('два', 'два'),
    ('три', 'три'),
    ('четыре', 'чотири'),
    ('пять', 'п''ять'),
    ('шесть', 'шість'),
    ('семь', 'сім'),
    ('восемь', 'вісім'),
    ('девять', 'дев''ять'),
    ('десять', 'десять'),
    ('одинадцать', 'одинадцять'),
    ('двенадцать', 'дванадцять'),
    ('тринадцать', 'тринадцять'),
    ('четырнадцать', 'чотирнадцять'),
    ('пятнадцать', 'п''ятнадцять'),
    ('шестнадцать', 'шістнадцять'),
    ('семьнадцать', 'сімнадцять'),
    ('восемьнадцать', 'вісімнадцять'),
    ('девятнадцать', 'дев''ятнадцять'),
    ('одна', 'одна'),
    ('две', 'дві'),
    ('три', 'три')
    );
  _Dgts10 : array[1..9,lngRU..lngUA] of String = (
    ('десять', 'десять'),
    ('двадцать', 'двадцять'),
    ('тридцать', 'тридцять'),
    ('сорок', 'сорок'),
    ('пятьдесят', 'п''ятдесят'),
    ('шестьдесят', 'шістдесят'),
    ('семьдесят', 'сімдесят'),
    ('восемьдесят', 'вісімдесят'),
    ('девяносто', 'дев''яносто'));
  _Dgts100 : array[1..9,lngRU..lngUA] of String = (
    ('сто', 'сто'),
    ('двести', 'двісті'),
    ('триста', 'триста'),
    ('четыреста', 'чотириста'),
    ('пятьсот', 'п''ятсот'),
    ('шестьсот', 'шістсот'),
    ('семьсот', 'сімсот'),
    ('восемьсот', 'вісімсот'),
    ('девятьсот', 'дев''ятсот'));

  _Razr : array[0..1,0..2,lngRU..lngUA] of string = (
     (('тысяча', 'тисяча'), ('тысячи', 'тисячі'), ('тысяч', 'тисяч')),
     (('миллион', 'мільйон'), ('миллиона', 'мільйони'), ('миллионов', 'мільйонів'))
  );

  _RMale : array[0..1] of boolean = (
    false,
    true
  );

var
  num, n, k, n_razr: integer;


  function SetByLeng(ARU, AUA: String): String;
  begin
    case ALeng of
      lngRU: Result:=ARU;
      lngUA: Result:=AUA;
    end;
  end;

  function _parse(_nm: integer; _male: boolean): String;
  var
    _n: integer;
  begin
    Result:='';
    _n:=_nm div 100;
    _nm:=_nm-_n*100;
    if _n>0 then Result:=_Dgts100[_n,ALeng];
    _n:=_nm div 10;
    if _n>1 then begin
      if Result='' then Result:=_Dgts10[_n,ALeng] else Result:=Result+' '+_Dgts10[_n,ALeng];
      _nm:=_nm-_n*10;
    end;
    _n:=_nm;
    if _n>0 then begin
      if Result<>'' then Result:=Result+' ';
      if _male or (_n>3) then Result:=Result+_Dgts[_n,ALeng]
      else Result:=Result+_Dgts[_n+19,ALeng];
    end;
  end;

begin
  num:=ANum;
  k:=1000000;
  Result:='';
//  n_razr:=1;
  if ANum=0 then Result:=SetByLeng('ноль','нуль') else
    for n_razr:=1 downto -1 do begin
      n:=num div k;
      num:=num-n*k;
      k:=k div 1000;
      if n>0 then begin
        if n_razr>-1 then begin
          if Result<>'' then Result:=Result+' ';
          Result:=Result+_parse(n,_RMale[n_razr]);
          Result:=Result+' '+parseNum(n,
            _Razr[n_razr,0,ALeng],
            _Razr[n_razr,1,ALeng],
            _Razr[n_razr,2,ALeng]);

        end else begin
          if Result<>'' then Result:=Result+' ';
          Result:=Result+_parse(n,isMale);
        end;
      end;
    end;

end;

function StringReplaceExt(const S : string; OldPattern, NewPattern:  array of string; Flags: TReplaceFlags):string;
var
 i : integer;
begin
   Assert(Length(OldPattern)=(Length(NewPattern)));
   Result:=S;
   for  i:= Low(OldPattern) to High(OldPattern) do
    Result:=StringReplace(Result,OldPattern[i], NewPattern[i], Flags);
end;

function _mysql_real_escape_string(const unescaped_string : string ) : string;
begin
  Result:=StringReplaceExt(unescaped_string,
    ['\', #39, #34, #0, #10, #13, #26], ['\\','\'#39,'\'#34,'\0','\n','\r','\Z'] ,
    [rfReplaceAll]
  );
end;

function ElapsedTime(ALastTime: Cardinal): Cardinal;
begin
  result:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime-ALastTime;
end;

function KRDialogTypeToFlag(ADialogType: TKRDialogType): LongInt;
begin
  Result:=0;
  case ADialogType of
  krdtInformation: Result:=MB_ICONINFORMATION;
  krdtWarning: Result:=MB_ICONWARNING;
  krdtError: Result:=MB_ICONERROR;
  krdtQuestion: Result:=MB_ICONQUESTION;
  end;
end;

function AppMsgBoxErr(AText: String; ACaption: String = ''): integer;
begin
  Result:=AppMsgBox(AText,MB_OKCANCEL or MB_ICONERROR or MB_DEFBUTTON2,ACaption);
end;

function AppMsgBoxInf(AText: String; ACaption: String = ''): integer;
begin
  Result:=AppMsgBox(AText,MB_OKCANCEL or MB_ICONINFORMATION or MB_DEFBUTTON2,ACaption);
end;

function AppMsgBoxQst(AText: String; ACaption: String = ''): integer;
begin
  Result:=AppMsgBox(AText,MB_YESNOCANCEL or MB_ICONQUESTION or MB_DEFBUTTON3,ACaption);
end;

function AppMsgBoxQstCh(AText, AChText: String; var ACh: boolean): integer;
var
  dlg: TForm;
  ch: TCheckBox;
begin
  dlg:=CreateMessageDialog(AText,mtConfirmation, [mbYes, mbNo, mbCancel], mbCancel);
  dlg.Position:=poScreenCenter;
  ch:=TCheckBox.Create(dlg);
  ch.Parent:=dlg;
  ch.Caption:=AChText;
  ch.Checked:=ACh;
  ch.Width:=dlg.Canvas.TextWidth(ch.Caption) + GetSystemMetrics(SM_CXMENUCHECK)+
      GetSystemMetrics(SM_CXEDGE) * 2;
  ch.Top:=dlg.ClientHeight;
  ch.Left:=7;
  dlg.Height:=dlg.Height + (dlg.Canvas.TextHeight(ch.Caption) * 2);
  result:=dlg.ShowModal;
  ACh:=ch.Checked;
  dlg.Free;
end;

function AppMsgBox(AText: String; Flags: Longint; ACaption: String = ''): integer;
var
  s: String;
begin
  if ACaption='' then s:=Application.Title else s:=ACaption;
  Application.NormalizeTopMosts;
  Result:=Application.MessageBox(PChar(AText),PChar(s),Flags);
  Application.RestoreTopMosts;
end;

function IsVariantsEqual(A,B: Variant): boolean;
begin
  Result:=false;
  if VarIsNumeric(A) then begin
    if VarIsNumeric(B) and (A=B) then Result:=true;
  end else if VarIsStr(A) then begin
    if VarIsStr(B) and(CompareStr(A,B)=0) then Result:=true;
  end else if VarIsNull(A) then begin
    if VarIsNull(B) then Result:=true;
  end;
end;

function SecondsToTimeString(ASec: DWORD): String;
var
  mn: DWord;
  _sk, _mn: String;
begin
  mn:=ASec div 60;
  _mn:=IntToStr(mn);
  if Length(_mn)<2 then _mn:='0'+_mn;
  _sk:=IntToStr(ASec-mn*60);
  if Length(_sk)<2 then _sk:='0'+_sk;
  Result:=_mn+':'+_sk;
end;

  function GetPrmStr(P: PChar; var Param: string): PChar;
  var
    i, Len: Integer;
    Start, S: PChar;
  begin
    while True do begin
      while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
      if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
    end;
    Len := 0;
    Start := P;
    while P[0] > ' ' do begin
      if P[0] = '"' then begin
        Inc(P);
        while (P[0] <> #0) and (P[0] <> '"') do begin
          Inc(Len);
          Inc(P);
        end;
        if P[0] <> #0 then Inc(P);
      end else begin
        Inc(Len);
        Inc(P);
      end;
    end;
    SetLength(Param, Len);
    P := Start;
    S := Pointer(Param);
    i := 0;
    while P[0] > ' ' do begin
      if P[0] = '"' then begin
        Inc(P);
        while (P[0] <> #0) and (P[0] <> '"') do begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
        if P[0] <> #0 then Inc(P);
      end else begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
    Result := P;
  end;

  function PrmCount(ACmdLine: PChar): Integer;
  var
    P: PChar;
    S: string;
  begin
    Result := 0;
    P := ACmdLine;
    while True do begin
      P := GetPrmStr(P, S);
      if S = '' then Break;
      Inc(Result);
    end;
  end;

  function PrmStr(ACmdLine: PChar; Index: Integer): string;
  var
    P: PChar;
  begin
    Result := '';
    P := ACmdLine;
    while True do begin
      P := GetPrmStr(P, Result);
      if (Index = 0) or (Result = '') then Break;
      Dec(Index);
    end;
  end;


procedure FindDirs(APath: String; ADirs: TStrings);
var
  sr : TSearchRec;
begin
  ADirs.Clear;
  if APath[Length(APath)]<>#92 then APath:=APath+#92;
  if FindFirst(APath+'*.*', faDirectory, sr)=0 then
    repeat
      if(sr.Name<>'.')and(sr.Name<>'..')and(sr.Attr and faDirectory > 0)then ADirs.Add(sr.Name);
    until(FindNext(sr)<>0);
  FindClose(sr);
end;

procedure FindFiles(APath: String; AFiles: TStrings; AExclude: Integer = 0);
var
  sr : TSearchRec;
begin
  AFiles.Clear;
  if APath[Length(APath)]<>#92 then APath:=APath+#92;
  if FindFirst(APath+'*.*', faAnyFile - faDirectory - AExclude, sr)=0 then
    repeat
      AFiles.Add(sr.Name);
    until(FindNext(sr)<>0);
  FindClose(sr);
end;

procedure FindFiles(APath: String; AFiles: TStrings; ASMask: String = ''; AExclude: Integer = 0);
var
  sr : TSearchRec;
  SMask: String;
begin
  if ASMask='' then SMask:='*.*' else SMask:=ASMask;
  AFiles.Clear;
  if APath[Length(APath)]<>#92 then APath:=APath+#92;
  if FindFirst(APath+SMask, faAnyFile - faDirectory - AExclude, sr)=0 then
    repeat
      AFiles.Add(sr.Name);
    until(FindNext(sr)<>0);
  FindClose(sr);
end;

procedure FindAllFiles(APath: String; AFiles: TStrings; ASMask: String = ''; AExclude: Integer = 0);
var
  SMask: String;

  procedure _findFiles(AMainPath, APath: String; AFiles: TStrings; ASMask: String = ''; AExclude: Integer = 0);
  var
    sr : TSearchRec;
  begin
    if FindFirst(AMainPath+APath+SMask, faAnyFile - AExclude, sr)=0 then
      repeat
        if(sr.Name<>'.')and(sr.Name<>'..')then
          if sr.Attr and faDirectory <> 0 then
            _findFiles(AMainPath,APath+sr.Name+'\',AFiles,ASMask,AExclude)
          else
            AFiles.Add(APath+sr.Name);
      until(FindNext(sr)<>0);
    FindClose(sr);
  end;

begin
  if ASMask='' then SMask:='*.*' else SMask:=ASMask;
  if APath[Length(APath)]<>#92 then APath:=APath+#92;
  _findFiles(APath,'',AFiles,ASMask,AExclude)
end;

function Pow10(AExp: byte): Cardinal;
var i: integer;
begin
  Result:=1;
  if AExp=0 then exit;
  for i := 1 to AExp do Result:=Result*10;
end;

function isSumTime(UTC: Cardinal):boolean;
var
	yr : DWORD;
	yr4 : DWORD;
	ltc: DWORD;
	idate : DWORD;
begin
  yr := (UTC + 43200) DIV 31557600 + 1970;
  ltc := UTC;
  idate := DateTimeToMKTime(EncodeDate(yr,3,31)+EncodeTime(1,0,0,0));
  yr4 := ((5 * yr) shr 2) + 1;
  Result := (idate - ((yr4 + 3) MOD 7) * 86400 <= ltc) AND (idate + (214 - (yr4) MOD 7) * 86400 > ltc);
end;

{
Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}


function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], -1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], -1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }

{:Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
 }

function StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: integer;
begin
  if s = '' then
    Result := ''
else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PAnsiChar(@s[1]), -1, nil,
      0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@s[1]),
        -1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString }

function CTL_CODE(ADeviceType, AFunction, AMethod, AAccess: Cardinal ): Cardinal;
begin
  Result:=(ADeviceType shl 16) or (AAccess shl 14) or (AFunction shl 2) or AMethod;
end;

procedure AppDelay(AMSec: Cardinal);
var tm0: cardinal;
begin
  tm0:=timeGetTime;
  while ElapsedTime(tm0)<AMSec do Application.ProcessMessages;
end;

function IntToHexStr(ANum,ACnt: Integer):String;
begin
  Result:=IntToHex(ANum,ACnt);
  while Length(Result)<ACnt do Result:='0'+Result;
end;

function PHPTimeToDateTime(tm: cardinal): TDateTime;
var
  SystemTime: TSystemTime;
  dt,_d: TDateTime;
begin
  GetSystemTime(SystemTime);
  dt:=now;
  with SystemTime do
    _d:=EncodeDate(wYear, wMonth, wDay)+EncodeTime(wHour, wMinute, wSecond, wMilliseconds)-dt;
  Result:=tm/86400+25569-_d;
end;

function DateTimeToPHPTime(d: TDateTime): cardinal;
var
  SystemTime: TSystemTime;
  dt,_d: TDateTime;
begin
  GetSystemTime(SystemTime);
  dt:=now;
  with SystemTime do
    _d:=EncodeDate(wYear, wMonth, wDay)+EncodeTime(wHour, wMinute, wSecond, wMilliseconds)-dt;
  Result:=Trunc((d-25569+_d)*86400.000001);
end;

function HexToInt(HexStr : string) : Int64;
var
  RetVar : Int64;
  i : byte;
begin
  HexStr := UpperCase(HexStr);
  RetVar := 0;

  for i := 1 to length(HexStr) do begin
    if CharInSet(HexStr[i],['0'..'9']) then begin
      RetVar := RetVar shl 4;
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    end else
      if CharInSet(HexStr[i],['A'..'F']) then begin
        RetVar := RetVar shl 4;
        RetVar := RetVar + (byte(HexStr[i]) - 55)
      end else begin
        break;
      end;
  end;

  Result := RetVar;
end;

function HexToInt(HexStr : string; out ARes : Int64): boolean;
var
  RetVar : Int64;
  i : byte;
begin
  HexStr := UpperCase(trim(HexStr));
  Result:=HexStr<>'';
  if not Result then exit;

  RetVar := 0;

  for i := 1 to length(HexStr) do begin
    if CharInSet(HexStr[i],['0'..'9']) then begin
      RetVar := RetVar shl 4;
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    end else
      if CharInSet(HexStr[i],['A'..'F']) then begin
        RetVar := RetVar shl 4;
        RetVar := RetVar + (byte(HexStr[i]) - 55)
      end else begin
        Result:=false;
        exit;
      end;
  end;

  ARes:= RetVar;
end;

function HexToIntDef(HexStr : string; ADefVal : Int64): Int64;
begin
  if not HexToInt(HexStr,Result) then Result:=ADefVal;
end;

function MKTimeToDateTime(d: DWORD): TDateTime;
begin
  Result:=d/86400+25569;
end;

function DateTimeToMKTime(d: TDateTime): DWORD;
begin
  Result:=Trunc((d-25569)*86400.000001);
end;

function parseNum(num: integer; s0,s1,s2:String):String;
var n: integer;
begin
  n:=num-trunc(num/100) * 100;
  if(n>10)and(n<15)then Result:=s2 else begin
    n:=num-trunc(num/10) * 10;
    if n=1 then Result:=s0 else
    if(n>1)and(n<5) then Result:=s1 else Result:=s2;
  end;
end;

function DWORDToHex(ANum: DWORD;ACnt: Integer):String;
var
  i: int64;
begin
  i:=ANum;
  Result:=IntToHex(i,ACnt);
  while Length(Result)<ACnt do Result:='0'+Result;
end;

function DWordToStr(dw: DWORD):String;
var
  dw0,dw1: dword;
  i: integer;
begin
  dw0:=dw;
  Result:=''; //4294967295
  for i := 9 downto 0 do begin
    dw1:=dw0 div Pow10(i);
    dw0:=dw0-dw1*Pow10(i);
    if Result='' then if dw1=0 then continue;
    Result:=Result+Chr(dw1+48);
  end;
  if Result='' then Result:='0';  
end;

function decdt(d: TDateTime): integer;
begin
  Result:=Trunc((d-25569)*86400.000001);
end;

function sourceToHex(buf: PAnsiChar; len: integer): String;
var i: integer;
begin
  result:='';
  for i:=0 to len-1 do result:=result+IntToHex(Ord(buf[i]),2)+' ';
end;

function Implode(const delim: String; strlist: TStrings): String;
var i: integer;
begin
  Result:='';
  for I := 0 to strlist.Count-1 do begin
    Result:=Result+strlist[i];
    if i<strlist.Count-1 then Result:=Result+delim;
  end;
end;

procedure SortArray(_array: TIntArray; _desc: boolean = false);
var
  i, j, v, _size : integer;
begin
  _size:=Length(_array);
  for i:=1 to _size-1 do
    for j:=_size-1 downto i do
      if(_desc and(_array[j-1]<_array[j]))or(not _desc and(_array[j-1]>_array[j]))then begin
        v:=_array[j];
        _array[j]:=_array[j-1];
        _array[j-1]:=v;
      end;
end;

procedure SortStrings(AStrings: TStrings; _desc: boolean = false);
var
  i, j, _size : integer;
  v: String;
begin
  _size:=AStrings.Count;
  for i:=1 to _size-1 do
    for j:=_size-1 downto i do
      if(_desc and({$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.CompareText(AStrings[j-1],AStrings[j])<0))or
      (not _desc and({$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.CompareText(AStrings[j-1],AStrings[j])>0))then begin
        v:=AStrings[j];
        AStrings[j]:=AStrings[j-1];
        AStrings[j-1]:=v;
      end;
end;

function TimeSToStr(t:real):string;
var
 ms:integer;
begin
ms := round(t);
SetLength(Result,4);
Result[4] := char(ms mod 10 + 48);
ms := ms div 10;
Result[3] := char(ms mod 6 + 48);
ms := ms div 6;
Result[2] := ':';
Result[1] := char(ms mod 10 + 48);
ms := ms div 10;
if ms = 0 then exit;
Result := char(ms mod 6 + 48) + Result;
ms := ms div 6;
if ms = 0 then exit;
Result := IntToStr(ms) + ':' + Result
end;

function MAKEBYTE(a,b:byte):byte;
begin
Result := (a and 15) or (b shl 4)
end;

function SubStrCnt(Text,character:string):integer;
begin
  result := 0;
  while Pos(Character,Text) <> 0 do begin
    Result := Result + 1;
    Text := Copy(Text,0,Pos(Character,Text)-1)+Copy(Text,Pos(Character,Text)+1,Length(Text));
  end;
end;

//Заполняет список aList наденными в системе COM портами
procedure GetComPorts(aList: TStrings; aNameStart: string);
var
  vBuf: string;
  vRes: integer;
  vErr: Integer;
  vBufSize: Integer;
  vNameStartPos: Integer;
  vName: string;
begin
  vBufSize := 1024 * 5;
  vRes := 0;
 
  while vRes = 0 do
    begin
      setlength(vBuf, vBufSize) ;
      SetLastError(ERROR_SUCCESS);
      vRes := QueryDosDevice(nil, @vBuf[1], vBufSize) ;
      vErr := GetLastError();
 
      //Вариант для двухтонки
      if (vRes <> 0) and (vErr = ERROR_INSUFFICIENT_BUFFER) then
        begin
          vBufSize := vRes;
          vRes := 0;
        end;
 
      if (vRes = 0) and (vErr = ERROR_INSUFFICIENT_BUFFER) then
        begin
          vBufSize := vBufSize + 1024;
        end;
 
      if (vErr <> ERROR_SUCCESS) and (vErr <> ERROR_INSUFFICIENT_BUFFER) then
        begin
          raise Exception.Create(SysErrorMessage(vErr) );
        end
    end;
  setlength(vBuf, vRes) ;
 
  vNameStartPos := 1;
  vName := GetNextSubstring(vBuf, vNameStartPos);
 
  aList.BeginUpdate();
  try
    aList.Clear();
    while vName <> '' do
      begin
        if StartsStr(aNameStart, vName) then
          aList.Add(vName);
        vName := GetNextSubstring(vBuf, vNameStartPos);
      end;
  finally
    aList.EndUpdate();
  end;
end;

function GetNextSubstring(aBuf: string; var aStartPos: integer): string;
var
  vLastPos: integer;
begin
  if (aStartPos < 1) then
    begin
      raise ERangeError.Create('aStartPos должен быть больше 0');
    end;
 
  if (aStartPos > Length(aBuf) ) then
    begin
      Result := '';
      Exit;
    end;
 
  vLastPos := PosEx(#0, aBuf, aStartPos);
  Result := Copy(aBuf, aStartPos, vLastPos - aStartPos);
  aStartPos := aStartPos + (vLastPos - aStartPos) + 1;
end;

function StartsStr(const APrefix : String; const ASource : String)
   : Boolean;
begin
   StartsStr := (CompareStr(APrefix, Copy(ASource, 0, Length(APrefix))) = 0);
end;

function ISOTransliterate(AText: String):String;
const
  kir : array[0..65] of String[1] = (
    'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж', 'З', 'И',
    'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т',
    'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы', 'Ь',
    'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ё',
    'ж', 'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п',
    'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
    'ъ', 'ы', 'ь', 'э', 'ю', 'я'
  );
  trans : array[0..65] of String[3] = (
    'A', 'B', 'V', 'G', 'D', 'E', 'YO', 'ZH', 'Z', 'I',
    'J', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T',
    'U', 'F', 'X', 'C', 'CH', 'SH', 'SHH', #39, 'Y', '',
    'E', 'YU', 'YA', 'a', 'b', 'v', 'g', 'd', 'e', 'yo',
    'zh', 'z', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
    'r', 's', 't', 'u', 'f', 'x', 'c', 'ch', 'sh', 'shh',
    '', 'y', '', 'e', 'yu', 'ya'
  );
var
  i,j,n: integer;
begin
  Result:='';
  for i:=1 to length(AText) do begin
    n:=-1;
    for j:=0 to 65 do
      if String(AText[i])=String(kir[j]) then begin
        n:=j;
        break;
      end;
    if n=-1 then Result:=Result+String(AText[i]) else Result:=Result+String(trans[n]);
  end;

end;

end.
