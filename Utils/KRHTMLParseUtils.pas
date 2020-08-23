(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRHTMLParseUtils                                                          *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRHTMLParseUtils;

interface

uses SysUtils, StrUtils, KRStrUtils;

const
  KRCHARSETCOUNT = 2;

type
  TKRCharSet = 1..KRCHARSETCOUNT;

  TKRAttribute = record
    Name: String;
    Value: String;
  end;

  TKRAttributes = array of TKRAttribute;
  PKRAttributes = ^TKRAttributes;

const
  csUTF8 = TKRCharSet(1);
  csWin1251 = TKRCharSet(2);

  HTMLChars: array[0..15,0..1] of String = (
  ('&quot','"'),
  ('&lt','<'),
  ('&laquo','«'),
  ('&ldquo','“'),
  ('&mdash','—'),
  ('&nbsp',' '),
  ('&copy','©'),
  ('&amp','&'),
  ('&gt','>'),
  ('&hellip','…'),
  ('&raquo','»'),
  ('&rdquo','”'),
  ('&ndash','–'),
  ('&reg','®'),
  ('&deg','°'),
  ('&trade','™'));

  function KRCSText(AText: PChar; ACharSet: TKRCharSet): String;
  function KRDeCodeHTML(AText: String): String;
  function KRCutBR(AText: String): String;overload;
  function KRCutBR(AText,AStr: String): String;overload;
  function KRCutTags(ATag, AText: String): String;overload;
  function KRCutTags(AText: String):String;overload;
  function KRReplTags(ATag, AText, ARStr: String):String;
  function KRCutComments(AText: String): string;
  function KRCutScripts(AText: String): string;
  function KRCutStyles(AText: String): string;
  function KRClearHTML(AText: String):String;
  function KRClearText(AText: String):String;
  function KRGetInnerHTML(ATag,AText: String; APos: integer; out AInnerHTML: String; out OPos: integer): boolean;overload;
  function KRGetInnerHTML(ATag,AText: String; APos: integer; out AInnerHTML: String): boolean;overload;
  function KRGetInnerText(ATag,AText: String; APos: integer; out AInnerText: STring; out OPos: integer): boolean;overload;
  function KRGetInnerText(ATag,AText: String; APos: integer; out AInnerText: STring): boolean;overload;
  function KRCutAttributes(AText: String; APos: integer; out OTag: String;out OPos: integer): String;overload;
  function KRCutAttributes(AText, ATag: String; APos: integer; out OPos: integer): String;overload;
  function KRAttribute(AName, AValue: String): TKRAttribute;
  function KRGetAttributes(AText, AAssgn: String; ADlms: array of String): TKRAttributes;overload;
  function KRGetAttributes(AText: String): TKRAttributes;overload;
  function KRGetAttrVal(ATag, AText, AAttr: String; APos: integer): String;
  function KRBackAttrVal(ATag, AText, AAttr: String; APos: integer): String;
  function KRGetElementByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer): boolean;overload;
  function KRGetElementByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer): boolean;overload;
  function KRGetElementByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer): boolean;overload;
  function KRGetElementByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer): boolean;overload;
  function KRGetInnerHTMLByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerHTML: String): boolean;overload;
  function KRGetInnerHTMLByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerHTML: String): boolean;overload;
  function KRGetInnerHTMLByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerHTML: String): boolean;overload;
  function KRGetInnerHTMLByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerHTML: String): boolean;overload;
  function KRGetInnerTextByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerText: String): boolean;overload;
  function KRGetInnerTextByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerText: String): boolean;overload;
  function KRGetInnerTextByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerText: String): boolean;overload;
  function KRGetInnerTextByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerText: String): boolean;overload;

implementation

function KRCSText(AText: PChar; ACharSet: TKRCharSet): String;
begin
  case ACharSet of
    csUTF8:  Result:= KRUTF8ToStrSmart(String(AText))
    else Result:= String(AText);
  end;
end;

function KRDeCodeHTML(AText: String): String;
var
  n,i: integer;
label nextChar;
begin
  n:=0;
  Result:='';
  while n<length(AText) do begin
    inc(n);
    if AText[n]=#38 then begin
      for i:=0 to Length(HTMLChars)-1 do
        if Copy(AText,n,Length(HTMLChars[i,0]))=HTMLChars[i,0] then begin
            Result:=Result+HTMLChars[i,1];
            n:=n+Length(HTMLChars[i,0])-1;
            if AText[n+1]=#59 then Inc(n);
            goto nextChar;
      end;
    end;
    Result:=Result+AText[n];
nextChar:
  end;
end;

function KRCutBR(AText: String): String;
var i: integer;
begin
  Result:='';
  for i:=1 to Length(AText) do
    if not ((AText[i]=#13)or(AText[i]=#10)) then Result:=Result+AText[i];
end;

function KRCutBR(AText,AStr: String): String;
var i: integer;
begin
  Result:='';
  for i:=1 to Length(AText) do
    if (AText[i]=#13) then Result:=Result+AStr else
      if (AText[i]<>#10) then Result:=Result+AText[i];
end;

function KRCutTags(ATag, AText: String): String;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<'+LowerCase(ATag),LowerCase(AText));
  if ips1>0 then begin
    ips2:=PosEx('</'+LowerCase(ATag),LowerCase(AText),ips1);
    if ips2=0
    then ips2:=PosEx('>',AText,ips1)
    else ips2:=PosEx('>',AText,ips2);
    if ips2=0 then ips2:=length(AText);
    Result:=LeftStr(AText,ips1-1)+RightStr(AText,Length(AText)-ips2);
    Result:=KRCutTags(ATag, Result);
  end else Result:=AText;
end;

function KRCutTags(AText: String):String;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<',AText);
  if ips1>0 then begin
    ips2:=PosEx('>',AText,ips1);
    if ips2=0 then ips2:=length(AText);
    Result:=LeftStr(AText,ips1-1)+RightStr(AText,Length(AText)-ips2);
    Result:=KRCutTags(Result);
  end else Result:=AText;
end;

function KRReplTags(ATag, AText, ARStr: String):String;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<'+LowerCase(ATag),LowerCase(AText));
  if ips1>0 then begin
    ips2:=PosEx('>',AText,ips1);
    if ips2=0 then ips2:=length(AText);
    Result:=LeftStr(AText,ips1-1)+ARStr+RightStr(AText,Length(AText)-ips2);
    Result:=KRReplTags(ATag,Result,ARStr);
  end else Result:=AText;
end;

function KRCutComments(AText: String): string;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<!--',LowerCase(AText));
  if ips1>0 then begin
    ips2:=PosEx('-->',LowerCase(AText),ips1+4);
    if ips2=0 then ips2:=length(AText) else ips2:=ips2+2;
    Result:=LeftStr(AText,ips1-1)+RightStr(AText,Length(AText)-ips2);
    Result:=KRCutComments(Result);
  end else Result:=AText;
end;

function KRCutScripts(AText: String): string;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<script',LowerCase(AText));
  if ips1>0 then begin
    ips2:=PosEx('</script',LowerCase(AText),ips1);
    if ips2=0 then ips2:=length(AText) else begin
      ips2:=PosEx('>',AText,ips2);
      if ips2=0 then ips2:=length(AText);
    end;
    Result:=LeftStr(AText,ips1-1)+RightStr(AText,Length(AText)-ips2);
    Result:=KRCutScripts(Result);
  end else Result:=AText;
end;

function KRCutStyles(AText: String): string;
var
  ips1,ips2: integer;
begin
  ips1:=Pos('<style',LowerCase(AText));
  if ips1>0 then begin
    ips2:=PosEx('</style',LowerCase(AText),ips1);
    if ips2=0 then ips2:=length(AText) else begin
      ips2:=PosEx('>',AText,ips2);
      if ips2=0 then ips2:=length(AText);
    end;
    Result:=LeftStr(AText,ips1-1)+RightStr(AText,Length(AText)-ips2);
    Result:=KRCutStyles(Result);
  end else Result:=AText;
end;

function KRClearHTML(AText: String):String;
begin
  Result:=KRCutComments(AText);
  Result:=KRCutScripts(Result);
  Result:=KRCutStyles(Result);
end;

function KRClearText(AText: String):String;
begin
  Result:=KRClearHTML(AText);
  Result:=KRCutBR(Result);
  Result:=KRReplTags('br',Result,#13#10);
  Result:=KRReplTags('p',Result,#13#10);
  Result:=KRReplTags('li',Result,#13#10);
  Result:=KRCutTags(Result);
  Result:=KRDeCodeHTML(Result);
  Result:=Trim(Result);
  Result:=KRReplStr(Result,'  ',' ');
end;

function KRGetInnerHTML(ATag,AText: String; APos: integer; out AInnerHTML: String; out OPos: integer): boolean;
var
  ps,ps1,ps2,ps3,n: integer;
  lcTag, lcText, _Text: String;
label lb01;
begin
  Result:=false;
  AInnerHTML:='';
  _Text:=AText;
  if KRDelSpaces(_Text)='' then exit;
  lcTag:=LowerCase(ATag);
  lcText:=LowerCase(_Text);
  ps2:=APos;
lb01:
  ps:=PosEx('<'+lcTag,lcText,ps2);
  if ps=0 then exit;
  if(_Text[ps+length(ATag)+1]=#32)or(_Text[ps+length(ATag)+1]=#9)or(_Text[ps+length(ATag)+1]=#62)or(_Text[ps+length(ATag)+1]=#47)then begin
    Result:=true;
    ps1:=PosEx('>',_Text,ps);
    if not((ps1=0)or(_Text[ps1-1]=#47))then begin
      n:=1;
      ps3:=ps1;
      repeat
        ps3:=PosEx(lcTag,lcText,ps3);
        if ps3=0 then begin
          ps3:=Length(_Text)+4;
          break;
        end;
        if(_Text[ps3-1]=#47)and(_Text[ps3-2]=#60)then Dec(n) else
          if(_Text[ps3-1]=#60)then begin
            ps:=PosEx('>',_Text,ps3);
            if ps>0 then if _Text[ps-1]<>#47 then inc(n);
          end;
        inc(ps3);
      until n=0;
      OPos:=ps3-3;
      AInnerHTML:=KRCopyFromTo(_Text,ps1+1,OPos-1);
    end;
  end else begin inc(ps2);goto lb01;end;
end;

function KRGetInnerHTML(ATag,AText: String; APos: integer; out AInnerHTML: String): boolean;
var _ps: integer;
begin
  Result:=KRGetInnerHTML(ATag,AText,APos,AInnerHTML,_ps);
end;

function KRGetInnerText(ATag,AText: String; APos: integer; out AInnerText: STring; out OPos: integer): boolean;
begin
  result:=KRGetInnerHTML(ATag,AText,APos,AInnerText,OPos);
  if Not(Result) then exit;
  AInnerText:=KRClearText(AInnerText);
  if KRDelSpaces(AInnerText)='' then exit;
  result:=true;
end;

function KRGetInnerText(ATag,AText: String; APos: integer; out AInnerText: STring): boolean;overload;
var _ps: integer;
begin
  Result:=KRGetInnerText(ATag,AText,APos,AInnerText,_ps);
end;

function KRAttribute(AName, AValue: String): TKRAttribute;
begin
  Result.Name:=AName;
  Result.Value:=AValue;
end;

function KRCutAttributes(AText: String; APos: integer; out OTag: String;out OPos: integer): String;
var
  _ps: integer;
  _str: String;
begin
  Result:='';
  OTag:='';
  OPos:=PosEx('<',AText,APos);
  if OPos=0 then exit;
  _str:=KRCopyFromTo(AText,'<','>',OPos);
  if _str<>'' then begin
    _ps:=Pos(' ',_str);
    if _ps>0 then begin
      OTag:=KRLeftTo(_str,' ');
      if _str[Length(_str)]=#47 then _str:=LeftStr(_str,Length(_str)-1);
      Result:=KRRightFrom(_str,_ps+1);
    end else
      if _str[Length(_str)]=#47 then OTag:=LeftStr(_str,Length(_str)-1) else OTag:=_str;
  end;
end;

function KRCutAttributes(AText, ATag: String; APos: integer; out OPos: integer): String;
var
  _ps: integer;
  _str: String;
begin
  Result:='';
  OPos:=KRPosExC('<'+ATag,AText,APos);
  if OPos=0 then exit;
  _str:=KRCopyFromTo(AText,'<','>',OPos);
  if _str<>'' then begin
    _ps:=Pos(' ',_str);
    if _ps>0 then begin
      if _str[Length(_str)]=#47 then _str:=LeftStr(_str,Length(_str)-1);
      Result:=KRRightFrom(_str,_ps+1);
    end;
  end;
end;

function KRGetAttributes(AText, AAssgn: String; ADlms: array of String): TKRAttributes;
var
  _attr: TKRAttribute;
  _attrs: TKRAttributes;
  _str: String;
  i,st,dlm: Integer;

  function isDlm(n: integer): integer;
  var j: integer;
  begin
    Result:=-2;
    for j:=0 to length(ADlms)-1 do
      if copy(_str,n,Length(ADlms[j]))=ADlms[j] then begin
        result:=j;
        exit;
      end;
  end;

  function isVal(n: integer): boolean;
  begin
    result:=true;
    if dlm=-2 then begin if _str[n]=#32 then begin result:=false;exit;end;end else
    if isDlm(n)=dlm then result:=false;
  end;

  procedure addAtr;
  var n: integer;
  begin
    n:=Length(_attrs);
    SetLength(_attrs,n+1);
    _attrs[n].Name:=_attr.Name;
    _attrs[n].Value:=_attr.Value;
  end;

begin
  SetLength(_attrs,0);
  _str:=KRReplStr(AText,#9,#32);
  _str:=KRReplStr(_str,#10,#32);
  _str:=KRReplStr(_str,#13,#32);
  _str:=KRReplStr(_str,#32#32,#32);
  _str:=Trim(_str);
  st:=2;_attr.Name:='';_attr.Value:='';i:=1;dlm:=-1;
  while i<=length(_str) do begin
    case st of
      0:if copy(_str,i,Length(AAssgn))=AAssgn then begin
          st:=1;
          i:=i+Length(AAssgn)-1;
        end else _attr.Name:=_attr.Name+_str[i];
      1:if dlm=-1 then begin
          dlm:=isDlm(i);
          if dlm<>-2 then i:=i+Length(ADlms[dlm]);
          Dec(i);
        end else
          if isVal(i) then _attr.Value:=_attr.Value+_str[i] else begin
            addAtr;_attr.Name:='';_attr.Value:='';
            if dlm>-2 then begin
              i:=i+Length(ADlms[dlm]);
              if _str[i]<>#32 then Dec(i);
            end;
            st:=2;
            dlm:=-1;
          end;
        2:begin st:=0;dec(i); end;
    end;
    inc(i);
  end;
  if st<2 then addAtr;
  Result:=_attrs
end;

function KRGetAttributes(AText: String): TKRAttributes;
begin
  Result:=KRGetAttributes(AText,'=',[#34,#39]);
end;

function KRGetAttrVal(ATag, AText, AAttr: String; APos: integer): String;
var
  _ps: integer;
  _str,_tag,_Text: String;
  _attrs: TKRAttributes;
begin
  Result:='';
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=KRPosExC('<'+ATag,_Text,APos);
  if _ps>0 then Begin
    _str:=KRCutAttributes(_Text,_ps,_tag,_ps);
    _attrs:=KRGetAttributes(_str);
    for _ps:=0 to Length(_attrs)-1 do
      if LowerCase(AAttr)=LowerCase(_attrs[_ps].Name) then begin
        Result:=_attrs[_ps].Value;
        exit;
      end;
  end;
end;

function KRBackAttrVal(ATag, AText, AAttr: String; APos: integer): String;
var
  _ps: integer;
  _str,_tag,_Text: String;
  _attrs: TKRAttributes;
begin
  Result:='';
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=KRBackPosExC('<'+ATag,_Text,APos);
  if _ps>0 then Begin
    _str:=KRCutAttributes(_Text,_ps,_tag,_ps);
    _attrs:=KRGetAttributes(_str);
    for _ps:=0 to Length(_attrs)-1 do
      if LowerCase(AAttr)=LowerCase(_attrs[_ps].Name) then begin
        Result:=_attrs[_ps].Value;
        exit;
      end;
  end;
end;

function KRGetElementByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer): boolean;
var
  i,_ps: Integer;
  _str,_tag, _Text: String;
  _attrs: TKRAttributes;
begin
  Result:=false;
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=APos;
  while true do begin
    _str:=KRCutAttributes(_Text,_ps,_tag,_ps);
    if(_ps=0)or(_tag='')then exit;
    _attrs:=KRGetAttributes(_str);
    for i:=0 to Length(_attrs)-1 do
      if (LowerCase(_attrs[i].Name)=LowerCase(AAttribute.Name))and
        (_attrs[i].Value=AAttribute.Value)then begin
        OPos:=_ps;
        ATag:=_tag;
        Result:=true;
        exit;
      end;
    Inc(_ps);
  end;
end;

function KRGetElementByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer): boolean;
var
  i,_ps: Integer;
  _str,_tag, _Text: String;
  _attrs: TKRAttributes;
begin
  Result:=false;
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=APos;
  while true do begin
    _str:=KRCutAttributes(_Text,_ps,_tag,_ps);
    if(_ps=0)or(_tag='')then exit;
    _attrs:=KRGetAttributes(_str);
    for i:=0 to Length(_attrs)-1 do
      if (LowerCase(_attrs[i].Name)=LowerCase(AAttribute.Name))and
        (LowerCase(_attrs[i].Value)=LowerCase(AAttribute.Value))then begin
        OPos:=_ps;
        ATag:=_tag;
        Result:=true;
        exit;
      end;
    Inc(_ps);
  end;
end;

function KRGetElementByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer): boolean;
var
  i,_ps: Integer;
  _str, _Text: String;
  _attrs: TKRAttributes;
begin
  Result:=false;
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=APos;
  while true do begin
    _str:=KRCutAttributes(_Text,ATag,_ps,_ps);
    if(_ps=0)then exit;
    _attrs:=KRGetAttributes(_str);
    for i:=0 to Length(_attrs)-1 do
      if (LowerCase(_attrs[i].Name)=LowerCase(AAttribute.Name))and
        (_attrs[i].Value=AAttribute.Value)then begin
        OPos:=_ps;
        Result:=true;
        exit;
      end;
    Inc(_ps);
  end;
end;

function KRGetElementByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer): boolean;
var
  i,_ps: Integer;
  _str, _Text: String;
  _attrs: TKRAttributes;
begin
  Result:=false;
  SetLength(_attrs,0);
  _Text:=AText;
  _ps:=APos;
  while true do begin
    _str:=KRCutAttributes(_Text,ATag,_ps,_ps);
    if(_ps=0)then exit;
    _attrs:=KRGetAttributes(_str);
    for i:=0 to Length(_attrs)-1 do
      if(LowerCase(_attrs[i].Name)=LowerCase(AAttribute.Name))and
        (LowerCase(_attrs[i].Value)=LowerCase(AAttribute.Value))then begin
        OPos:=_ps;
        Result:=true;
        exit;
      end;
    Inc(_ps);
  end;
end;

function KRGetInnerHTMLByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerHTML: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttr(AText,AAttribute,APos,ATag,_ps);
  if Result then KRGetInnerHTML(ATag,AText,_ps,InnerHTML,OPos);
end;

function KRGetInnerHTMLByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerHTML: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttrC(AText,AAttribute,APos,ATag,_ps);
  if Result then KRGetInnerHTML(ATag,AText,_ps,InnerHTML,OPos);
end;

function KRGetInnerHTMLByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerHTML: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttr(AText,ATag,AAttribute,APos,_ps);
  if Result then KRGetInnerHTML(ATag,AText,_ps,InnerHTML,OPos);
end;

function KRGetInnerHTMLByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerHTML: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttrC(AText,ATag,AAttribute,APos,_ps);
  if Result then KRGetInnerHTML(ATag,AText,_ps,InnerHTML,OPos);
end;

function KRGetInnerTextByAttr(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerText: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttr(AText,AAttribute,APos,ATag,_ps);
  if Result then KRGetInnerText(ATag,AText,_ps,InnerText,OPos);
end;

function KRGetInnerTextByAttrC(AText: String; AAttribute: TKRAttribute; APos: integer;
    out ATag: String; out OPos: integer; out InnerText: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttrC(AText,AAttribute,APos,ATag,_ps);
  if Result then KRGetInnerText(ATag,AText,_ps,InnerText,OPos);
end;

function KRGetInnerTextByAttr(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerText: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttr(AText,ATag,AAttribute,APos,_ps);
  if Result then KRGetInnerText(ATag,AText,_ps,InnerText,OPos);
end;

function KRGetInnerTextByAttrC(AText, ATag: String; AAttribute: TKRAttribute; APos: integer;
    out OPos: integer; out InnerText: String): boolean;
var _ps: integer;
begin
  Result:=KRGetElementByAttrC(AText,ATag,AAttribute,APos,_ps);
  if Result then KRGetInnerText(ATag,AText,_ps,InnerText,OPos);
end;

end.
