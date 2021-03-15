(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRStrUtils                                                                *)
(*  Ver.: 06.02.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRStrUtils;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.SysUtils, System.StrUtils, System.Classes;
  {$ELSE}
    Windows, SysUtils, StrUtils, Classes;
  {$IFEND}


type
  TKRStrAr = array of String;

  function KRDelSpaces(S: String): String;
  function KRIsNum(Ch: Char): boolean;
  function KRStrExist(Str,SubStr: String): boolean;
  function KRStrExistC(Str,SubStr: String): boolean;
  function KRReplStr(AText,ASubStr,AStr: String): String;
  function KRReplStrC(AText,ASubStr,AStr: String): String;
  function KRPosC(ASubStr, AStr: String): integer;
  function KRPosExC(ASubStr, AStr: String; APos: integer): integer;
  function KRBackPos(ASubStr, AStr: String): integer;
  function KRBackPosEx(ASubStr, AStr: String; APos: integer): integer;
  function KRBackPosC(ASubStr, AStr: String): integer;
  function KRBackPosExC(ASubStr, AStr: String; APos: integer): integer;
  function KRRightFrom(AText: String; APos: integer): String;overload;
  function KRRightFrom(AText, AST: String): String;overload;
  function KRRightFromC(AText, AST: String): String;
  function KRLeftTo(AText, AST: String): String;
  function KRLeftToC(AText, AST: String): String;
  function KRCopyFromTo(AText: String;APos0,APos1: Integer): String;overload;
  function KRCopyFromTo(AText, ASTBg: String; APosEd, APos: Integer): String;overload;
  function KRCopyFromTo(AText, ASTBg: String; APosEd: integer): String;overload;
  function KRCopyFromTo(AText: String;APosBg: Integer;ASTEd: String;out OPos: integer): String;overload;
  function KRCopyFromTo(AText: String;APosBg: Integer;ASTEd: String): String;overload;
  function KRCopyFromTo(AText, ASTBg, ASTEd: String; APos: integer;out OPos: integer): string;overload;
  function KRCopyFromTo(AText, ASTBg, ASTEd: String; APos: integer): string;overload;
  function KRCopyFromTo(AText, ASTBg, ASTEd: String): string;overload;
  function KRCopyFromToC(AText, ASTBg: String; APosEd, APos: Integer): String;overload;
  function KRCopyFromToC(AText, ASTBg: String; APosEd: integer): String;overload;
  function KRCopyFromToC(AText: String;APosBg: Integer;ASTEd: String;out OPos: integer): String;overload;
  function KRCopyFromToC(AText: String;APosBg: Integer;ASTEd: String): String;overload;
  function KRCopyFromToC(AText, ASTBg, ASTEd: String; APos: integer;out OPos: integer): string;overload;
  function KRCopyFromToC(AText, ASTBg, ASTEd: String; APos: integer): string;overload;
  function KRCopyFromToC(AText, ASTBg, ASTEd: String): string;overload;
  function KRCutFromTo(AText, ASTBg, ASTEd: String): string;overload;
  function KRCutFromTo(AText, ASTBg, ASTEd: String; out ACutStr: String): string;overload;
  procedure KRSplitStr(AText, ADlm: String; AStrings: TStrings);
  procedure KRSplitStrC(AText, ADlm: String; AStrings: TStrings);
  function KRUTF8ToStr(Value: String): String;
  function KRUTF8ToStrSmart(Value: String): String;
  function KRFastValToHexChar(AVal: UInt8): Char;
  procedure KRFastByteToHex(bt: UInt8; out Res: String);
  procedure KRFastWordToHex(wd: UInt16; out Res: String);
  procedure KRFastDWordToHex(dw: UInt32; out Res: String);
  function KRFastHexCharToVal(ACh: Char): UInt8;
  function KRFastHexToByte(s: String): UInt8;
  function KRFastHexToWord(s: String): UInt16;
  function KRFastHexToDWord(s: String): UInt32;
  function KRIntToStrL(ANum, ACnt: UInt32): String;
  function KRIsIPAddress( Text: String ): Boolean;
  function KRIsNumber( const AText: String; out AValue: Int32): Boolean;


implementation

function KRIsNumber( const AText: String; out AValue: Int32): Boolean;
var
  e: Int32;
begin
  Val( AText, AValue, e );
  if e=0 then Result := True else Result := False;
end;

function KRIsIPAddress( Text: String ): Boolean;
var
  sl: TStringList;
  vl,i: Int32;
begin
  Result:=false;
  sl:=TStringList.Create;
  try
    KRSplitStr(Text,'.',sl);
    if sl.Count=4 then begin
      Result:=true;
      for i:=0 to 3 do
        if not KRIsNumber(sl[i],vl) then begin
          result:=false;
          break;
        end else
          if(vl<0)or(vl>255)then begin
            result:=false;
            break;
          end;
    end;
  finally
    sl.free;
  end;
end;


function KRIntToStrL(ANum, ACnt: UInt32): String;
begin
  Result:=IntToStr(ANum);
  while Length(Result)<ACnt do Result:='0'+Result;
end;

function KRFastValToHexChar(AVal: UInt8): Char;
begin
  if AVal>9 then Result:=Chr(AVal+55) else Result:=Chr(AVal+48)
end;

procedure KRFastByteToHex(bt: UInt8; out Res: String);
begin
  SetLength(Res,2);
  Res[1]:=KRFastValToHexChar(bt shr 4);
  Res[2]:=KRFastValToHexChar(bt and $f);
end;

procedure KRFastWordToHex(wd: UInt16; out Res: String);
begin
  SetLength(Res,4);
  Res[1]:=KRFastValToHexChar(wd shr 12);
  Res[2]:=KRFastValToHexChar((wd shr 8) and $f);
  Res[3]:=KRFastValToHexChar((wd shr 4) and $f);
  Res[4]:=KRFastValToHexChar(wd and $f);
end;

procedure KRFastDWordToHex(dw: UInt32; out Res: String);
begin
  SetLength(Res, 8);
  Res[1]:=KRFastValToHexChar(dw shr 28);
  Res[2]:=KRFastValToHexChar((dw shr 24) and $f);
  Res[3]:=KRFastValToHexChar((dw shr 20) and $f);
  Res[4]:=KRFastValToHexChar((dw shr 16) and $f);
  Res[5]:=KRFastValToHexChar((dw shr 12) and $f);
  Res[6]:=KRFastValToHexChar((dw shr 8) and $f);
  Res[7]:=KRFastValToHexChar((dw shr 4) and $f);
  Res[8]:=KRFastValToHexChar(dw and $f);
end;

function KRFastHexCharToVal(ACh: Char): UInt8;
var bt: byte;
begin
  bt:=Ord(ACh);
  if bt>57 then Result:=bt-55 else Result:=bt-48;
end;

function KRFastHexToByte(s: String): UInt8;
begin
  Result:=(KRFastHexCharToVal(s[1]) shl 4) or KRFastHexCharToVal(s[2]);
end;

function KRFastHexToWord(s: String): UInt16;
begin
  Result:=(Word(KRFastHexToByte(copy(s,1,2))) shl 8) or KRFastHexToByte(copy(s,3,2));
end;

function KRFastHexToDWord(s: String): UInt32;
begin
  result:=(Cardinal(KRFastHexToWord(copy(s,1,4))) shl 16) or KRFastHexToWord(copy(s,5,4));
end;

function KRDelSpaces(S: String): String;
var
  i,j,n: uint32;
begin
  S:=Trim(s);
  n:=Length(s);
  SetLength(Result,n);
  dec(n);
  j:=0;
  for i:=2 to n do begin
   if (S[i]=#9) or (S[i]=#10) or (S[i]=#13) or (S[i]=#32) or (S[i]=#160) then continue;
   Result[j]:=S[i];
   inc(j);
  end;
  SetLength(Result,j);
end;

function KRIsNum(Ch: Char): boolean;
var
  n: uint32;
begin
  n:=Ord(n);
  Result:=(n>47)and(n<58);
end;

function KRStrExist(Str,SubStr: String): boolean;
begin
  Result:=Pos(SubStr,Str)>0;
end;

function KRStrExistC(Str,SubStr: String): boolean;
begin
  Result:=KRPosC(SubStr,Str)>0;
end;

function KRReplStr(AText,ASubStr,AStr: String): String;
var ips: integer;
begin
  ips:=PosEx(PChar(ASubStr),PChar(AText),1);
  if ips>0 then begin
    Result:=LeftStr(AText,ips-1)+AStr+RightStr(AText,Length(AText)-ips-Length(ASubStr)+1);
    Result:=KRReplStr(Result,ASubStr,AStr);
  end else Result:=AText;
end;

function KRReplStrC(AText,ASubStr,AStr: String): String;
var ips: integer;
begin
  ips:=KRPosExC(PChar(ASubStr),PChar(AText),1);
  if ips>0 then begin
    Result:=LeftStr(AText,ips-1)+AStr+RightStr(AText,Length(AText)-ips-Length(ASubStr)+1);
    Result:=KRReplStrC(Result,ASubStr,AStr);
  end else Result:=AText;
end;

function KRPosC(ASubStr, AStr: String): integer;
begin
  Result:=Pos(LowerCase(ASubStr),LowerCase(AStr));
end;

function KRPosExC(ASubStr, AStr: String; APos: integer): integer;
begin
  Result:=PosEx(LowerCase(ASubStr),LowerCase(AStr),APos);
end;

function KRBackPos(ASubStr, AStr: String): integer;
begin
  Result:=KRBackPosEx(ASubStr, AStr, Length(AStr));
end;

function KRBackPosEx(ASubStr, AStr: String; APos: integer): integer;
var _ps,i: integer;
begin
  Result:=0;
  if APos-Length(ASubStr)<0 then exit;
  if Length(ASubStr)>Length(AStr) then exit;
  if APos+Length(ASubStr)-1>Length(AStr)
  then _ps:=Length(AStr)-Length(ASubStr)+1
  else _ps:=APos;
  for i:=_ps downto 1 do
    if Copy(AStr,i,Length(ASubStr))=ASubStr then begin Result:=i;exit;end;
end;

function KRBackPosC(ASubStr, AStr: String): integer;
begin
  Result:=KRBackPosExC(ASubStr, AStr, Length(AStr));
end;

function KRBackPosExC(ASubStr, AStr: String; APos: integer): integer;
var _ps,i: integer;
begin
  Result:=0;
  if APos-Length(ASubStr)<0 then exit;
  if Length(ASubStr)>Length(AStr) then exit;
  if APos+Length(ASubStr)-1>Length(AStr)
  then _ps:=Length(AStr)-Length(ASubStr)+1
  else _ps:=APos;
  for i:=_ps downto 1 do
    if LowerCase(Copy(AStr,i,Length(ASubStr)))=LowerCase(ASubStr) then begin Result:=i;exit;end;
end;

function KRRightFrom(AText: String; APos: integer): String;
begin
  Result:=RightStr(AText,Length(AText)-APos+1);
end;

function KRRightFrom(AText, AST: String): String;
var _ps: integer;
begin
  Result:='';
  _ps:=KRBackPos(AST,AText);
  if _ps=0 then exit;
  Result:=KRRightFrom(AText,_ps+Length(AST));
end;

function KRRightFromC(AText, AST: String): String;
var _ps: integer;
begin
  Result:='';
  _ps:=KRBackPosC(AST,AText);
  if _ps=0 then exit;
  Result:=KRRightFrom(AText,_ps+Length(AST));
end;

function KRLeftTo(AText, AST: String): String;
var _ps: integer;
begin
  Result:='';
  _ps:=Pos(AST,AText);
  if _ps=0 then exit;
  Result:=LeftStr(AText,_ps-1);
end;

function KRLeftToC(AText, AST: String): String;
var _ps: integer;
begin
  Result:='';
  _ps:=KRPosC(AST,AText);
  if _ps=0 then exit;
  Result:=LeftStr(AText,_ps-1);
end;

function KRCopyFromTo(AText: String;APos0,APos1: Integer): String;
begin
  Result:=Copy(AText,APos0,APos1-APos0+1);
end;

function KRCopyFromTo(AText, ASTBg: String; APosEd, APos: Integer): String;
var _ps: integer;
begin
  Result:='';
  _ps:=PosEx(ASTBg,AText,APos);
  if _ps=0 then exit;
  result:=KRCopyFromTo(AText,_ps+Length(ASTBg),APosEd);
end;

function KRCopyFromTo(AText, ASTBg: String; APosEd: integer): String;
begin
  Result:=KRCopyFromTo(AText, ASTBg, APosEd, 1);
end;

function KRCopyFromTo(AText: String;APosBg: Integer;ASTEd: String;out OPos: integer): String;
var _ps: integer;
begin
  Result:='';
  OPos:=APosBg;
  _ps:=PosEx(ASTEd,AText,APosBg)-1;
  if _ps=-1 then _ps:=Length(AText);
  OPos:=_ps;
  Result:=KRCopyFromTo(AText,APosBg,OPos);
  Inc(OPos);
end;

function KRCopyFromTo(AText: String;APosBg: Integer;ASTEd: String): String;
var _ps: integer;
begin
  Result:=KRCopyFromTo(AText,APosBg,ASTEd,_ps);
end;

function KRCopyFromTo(AText, ASTBg, ASTEd: String; APos: integer;out OPos: integer): string;
var _ps0,_ps1: integer;
begin
  Result:='';
  OPos:=APos;
  _ps0:=PosEx(ASTBg,AText,APos);
  if _ps0=0 then exit;
  Inc(_ps0,Length(ASTBg));
  _ps1:=PosEx(ASTEd,AText,_ps0)-1;
  if _ps1=-1 then _ps1:=Length(AText);
  OPos:=_ps1;
  Result:=KRCopyFromTo(AText,_ps0,_ps1);
  Inc(OPos);
end;

function KRCopyFromTo(AText, ASTBg, ASTEd: String; APos: integer): string;
var _ps: integer;
begin
  Result:=KRCopyFromTo(AText,ASTBg,ASTEd,APos,_ps);
end;

function KRCopyFromTo(AText, ASTBg, ASTEd: String): string;
var _ps: integer;
begin
  Result:=KRCopyFromTo(AText, ASTBg, ASTEd, 1,_ps);
end;

function KRCopyFromToC(AText, ASTBg: String; APosEd, APos: Integer): String;
var _ps: integer;
begin
  Result:='';
  _ps:=KRPosExC(ASTBg,AText,APos);
  if _ps=0 then exit;
  result:=KRCopyFromTo(AText,_ps+Length(ASTBg),APosEd);
end;

function KRCopyFromToC(AText, ASTBg: String; APosEd: integer): String;
begin
  result:=KRCopyFromToC(AText, ASTBg, APosEd, 1);
end;

function KRCopyFromToC(AText: String;APosBg: Integer;ASTEd: String;out OPos: integer): String;
var _ps: integer;
begin
  Result:='';
  OPos:=APosBg;
  _ps:=KRPosExC(ASTEd,AText,APosBg)-1;
  if _ps=-1 then _ps:=Length(AText);
  OPos:=_ps;
  Result:=KRCopyFromTo(AText,APosBg,OPos);
  Inc(OPos);
end;

function KRCopyFromToC(AText: String;APosBg: Integer;ASTEd: String): String;
var _ps: integer;
begin
  Result:=KRCopyFromToC(AText,APosBg,ASTEd,_ps);
end;

function KRCopyFromToC(AText, ASTBg, ASTEd: String; APos: integer;out OPos: integer): string;
var _ps0,_ps1: integer;
begin
  Result:='';
  OPos:=APos;
  _ps0:=KRPosExC(ASTBg,AText,APos);
  if _ps0=0 then exit;
  Inc(_ps0,Length(ASTBg));
  _ps1:=KRPosExC(ASTEd,AText,_ps0)-1;
  if _ps1=-1 then _ps1:=Length(AText);
  OPos:=_ps1;
  Result:=KRCopyFromTo(AText,_ps0,_ps1);
  Inc(OPos);
end;

function KRCopyFromToC(AText, ASTBg, ASTEd: String; APos: integer): string;
var _ps: integer;
begin
  Result:=KRCopyFromToC(AText,ASTBg,ASTEd,APos,_ps);
end;

function KRCopyFromToC(AText, ASTBg, ASTEd: String): string;
var _ps: integer;
begin
  Result:=KRCopyFromToC(AText, ASTBg, ASTEd, 1,_ps);
end;

function KRCutFromTo(AText, ASTBg, ASTEd: String): string;
var
  n,n1: integer;
begin
  n:=Pos(ASTBg,AText);
  if n>0 then begin
    n1:=Pos(ASTEd,AText);
    if n1=0 then begin
      result:=LeftStr(AText,n-1);
      exit;
    end;
    result:=LeftStr(AText,n-1)+RightStr(AText,Length(AText)-n1-Length(ASTEd)+1);
    exit;
  end;
  n:=Pos(ASTEd,AText);
  if n=0 then begin
    result:=AText;
    exit;
  end;
  result:=RightStr(AText,Length(AText)-n-Length(ASTEd)+1);
end;

function KRCutFromTo(AText, ASTBg, ASTEd: String; out ACutStr: String): string;
var
  n,n1: integer;
begin
  n:=Pos(ASTBg,AText);
  if n>0 then begin
    n1:=Pos(ASTEd,AText);
    if n1=0 then begin
      ACutStr:=RightStr(AText,Length(AText)-n+1);
      result:=LeftStr(AText,n-1);
      exit;
    end;
    ACutStr:=Copy(AText,n,n1+Length(ASTEd)-n);
    result:=LeftStr(AText,n-1)+RightStr(AText,Length(AText)-n1-Length(ASTEd)+1);
    exit;
  end;
  n:=Pos(ASTEd,AText);
  if n=0 then begin
    ACutStr:='';
    result:=AText;
    exit;
  end;
  ACutStr:=LeftStr(AText,n+Length(ASTEd)-1);
  result:=RightStr(AText,Length(AText)-n-Length(ASTEd)+1);
end;

procedure KRSplitStr(AText, ADlm: String; AStrings: TStrings);
var
  _ps0,_ps1, i: integer;
begin
  AStrings.Clear;
  if ADlm='' then
    for i:=1 to Length(AText) do AStrings.Add(AText[i])
  else begin
    _ps0:=1;
    _ps1:=Pos(ADlm,AText);
    while _ps1>0 do begin
      AStrings.Add(KRCopyFromTo(ATExt,_ps0,_ps1-1));
      _ps0:=_ps1+Length(ADlm);
      _ps1:=PosEx(ADlm,AText,_ps0);
    end;
    AStrings.Add(KRCopyFromTo(ATExt,_ps0,Length(AText)));
  end;
end;

procedure KRSplitStrC(AText, ADlm: String; AStrings: TStrings);
var
  _ps0,_ps1, i: integer;
begin
  AStrings.Clear;
  if ADlm='' then
    for i:=1 to Length(AText) do AStrings.Add(AText[i])
  else begin
    _ps0:=1;
    _ps1:=KRPosC(ADlm,AText);
    while _ps1>0 do begin
      AStrings.Add(KRCopyFromTo(ATExt,_ps0,_ps1-1));
      _ps0:=_ps1+Length(ADlm);
      _ps1:=KRPosExC(ADlm,AText,_ps0);
    end;
    AStrings.Add(KRCopyFromTo(ATExt,_ps0,Length(AText)));
  end;
end;

function KRUTF8ToStr(Value: String): String;
var
  buffer: PWideChar;
  BufLen: LongWord;
begin
  BufLen := Length(Value)*2 + 4;
  GetMem(buffer, BufLen);
  FillChar(buffer^, BufLen, 0);
  MultiByteToWideChar(CP_UTF8, 0, @Value[1], BufLen - 4, buffer, BufLen);
  Result := WideCharToString(buffer);
  FreeMem(buffer, BufLen);
end;

function KRUTF8ToStrSmart(Value: String): String;
var
  Digit: String;
  i: LongWord;
  HByte: Byte;
  Len: Byte;
begin
  Result := '';
  Len := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
  begin
    if Len > 0 then
    begin
      Digit := Digit + Value[i];
      Dec(Len);
      if Len = 0 then
        Result := Result + KRUTF8ToStr(Digit);
    end else
    begin
      HByte := Ord(Value[i]);
      if HByte in [$00..$7f] then       //Standart ASCII chars
        Result := Result + Value[i]
      else begin
        //Get length of UTF-8 char
        if HByte and $FC = $FC then
          Len := 6
        else if HByte and $F8 = $F8 then
          Len := 5
        else if HByte and $F0 = $F0 then
          Len := 4
        else if HByte and $E0 = $E0 then
          Len := 3
        else if HByte and $C0 = $C0 then
          Len := 2
        else begin
          Result := Result + Value[i];
          Continue;
        end;
        Dec(Len);
        Digit := Value[i];
      end;
    end;
  end;
end;

end.
