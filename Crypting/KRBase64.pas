(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBase64                                                                  *)
(*  Ver.: 16.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBase64;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes;
  {$ELSE}
    Classes;
  {$IFEND}

type
  TKRBase64CharTable = array[0..63] of AnsiChar;
  PKRBase64CharTable = ^TKRBase64CharTable;
  TKRBase64Suffix = AnsiChar;
  TKRBase64DecodeTable = array[0..127] of byte;
  PKRBase64DecodeTable = ^TKRBase64DecodeTable;

const
  KRBASE64_DEFAULT_CHAR_TABLE: TKRBase64CharTable = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
  );

  KRBASE64_DEFAULT_DECODE_TABLE: TKRBase64DecodeTable = (
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61,  0,  0,  0,  0,  0,  0,
     0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,  0,  0,  0,  0,  0,
     0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,  0,  0,  0,  0,  0
  );

  KRBASE64_DEFAULT_SUFFIX: TKRBase64Suffix = '=';

  KRBASE64_CACHE_SAZE = 1023; //must be a multiple of three

  function KRBase64Decode(ASource, ADest: PByte; ALength: integer;
    ADecodeTable: PKRBase64DecodeTable; ASuffix: TKRBase64Suffix): integer;

  function KRBase64Encode(ASource, ADest: PByte; ALength: integer;
    ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix): integer;
  procedure KRBase64EncodeBytes(ABuffer: PByte; ALength: integer; out ACode: AnsiString;
    ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);
  procedure KRBase64EncodeAnsi(var ASource: AnsiString; out ACode: AnsiString;
    ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);
  procedure KRBase64EncodeStream(ASource, ADest: TStream; ALength: integer;
    ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);

implementation

function KRBase64Decode(ASource, ADest: PByte; ALength: integer;
  ADecodeTable: PKRBase64DecodeTable; ASuffix: TKRBase64Suffix): integer;
var
  n: integer;
  bt1, bt2: byte;
begin
  n:=(ALength shr 2);
  Result:=n*3;
  while n>1 do begin
    bt1:=ADecodeTable^[ASource^] shl 2;
    inc(ASource);

    bt2:=ADecodeTable^[ASource^];
    inc(ASource);

    ADest^:=bt1 or (bt2 shr 4);
    inc(ADest);

    bt1:=ADecodeTable^[ASource^];
    inc(ASource);

    ADest^:=(bt2 shl 4)or(bt1 shr 2);
    inc(ADest);

    ADest^:=(bt1 shl 6) or ADecodeTable^[ASource^];
    inc(ADest);
    inc(ASource);

    dec(n);
  end;

  bt1:=ADecodeTable^[ASource^] shl 2;
  inc(ASource);

  bt2:=ADecodeTable^[ASource^];
  inc(ASource);

  ADest^:=bt1 or (bt2 shr 4);

  if ASource^=Ord(ASuffix) then begin
    Dec(Result,2);
    exit;
  end;

  inc(ADest);

  bt1:=ADecodeTable^[ASource^];
  inc(ASource);

  ADest^:=(bt2 shl 4)or(bt1 shr 2);

  if ASource^=Ord(ASuffix) then begin
    Dec(Result);
    exit;
  end;

  inc(ADest);

  ADest^:=(bt1 shl 6) or ADecodeTable^[ASource^];
end;

function KRBase64Encode(ASource, ADest: PByte; ALength: integer;
  ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix): integer;
var
  i,n,i1,i2: integer;
begin
  n:=ALength div 3;
  Result:=n shl 2;
  i1:=-1;i2:=0;dec(n);
  for i := 0 to n do begin
    inc(i1);
    ADest[i2]:=Ord(ACharTable^[ASource[i1] shr 2]);
    inc(i2);
    ADest[i2]:=Ord(ACharTable^[((ASource[i1] and 3) shl 4) or (ASource[i1+1] shr 4)]);
    inc(i1);inc(i2);
    ADest[i2]:=Ord(ACharTable^[((ASource[i1] and $f) shl 2) or (ASource[i1+1] shr 6)]);
    inc(i1);inc(i2);
    ADest[i2]:=Ord(ACharTable^[ASource[i1] and $3f]);
    inc(i2);
  end;
  n:=ALength mod 3;
  case n of
    1: begin
      inc(i1);
      ADest[i2]:=Ord(ACharTable^[ASource[i1] shr 2]);
      ADest[i2+1]:=Ord(ACharTable^[(ASource[i1] and 3) shl 4]);
      ADest[i2+2]:=Ord(ASuffix);
      ADest[i2+3]:=Ord(ASuffix);
      inc(Result,4);
    end;
    2: begin
      inc(i1);
      ADest[i2]:=Ord(ACharTable^[ASource[i1] shr 2]);
      ADest[i2+1]:=Ord(ACharTable^[((ASource[i1] and 3) shl 4) or (ASource[i1+1] shr 4)]);
      inc(i1);
      ADest[i2+2]:=Ord(ACharTable^[(ASource[i1] and $f) shl 2]);
      ADest[i2+3]:=Ord(ASuffix);
      inc(Result,4);
    end;
  end;
end;

procedure KRBase64EncodeBytes(ABuffer: PByte; ALength: integer; out ACode: AnsiString;
  ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);
var
  i,n,i1,i2,m: integer;
begin
  n:=ALength div 3;
  m:=n shl 2;
  SetLength(ACode,m);
  i1:=-1;i2:=1;dec(n);
  for i := 0 to n do begin
    inc(i1);
    ACode[i2]:=ACharTable^[ABuffer[i1] shr 2];
    inc(i2);
    ACode[i2]:=ACharTable^[((ABuffer[i1] and 3) shl 4) or (ABuffer[i1+1] shr 4)];
    inc(i1);inc(i2);
    ACode[i2]:=ACharTable^[((ABuffer[i1] and $f) shl 2) or (ABuffer[i1+1] shr 6)];
    inc(i1);inc(i2);
    ACode[i2]:=ACharTable^[ABuffer[i1] and $3f];
    inc(i2);
  end;
  n:=ALength mod 3;
  case n of
    1: begin
      inc(i1);
      SetLength(ACode,m+4);
      ACode[i2]:=ACharTable^[ABuffer[i1] shr 2];
      ACode[i2+1]:=ACharTable^[(ABuffer[i1] and 3) shl 4];
      ACode[i2+2]:=ASuffix;
      ACode[i2+3]:=ASuffix;
    end;
    2: begin
      inc(i1);
      SetLength(ACode,m+4);
      ACode[i2]:=ACharTable^[ABuffer[i1] shr 2];
      ACode[i2+1]:=ACharTable^[((ABuffer[i1] and 3) shl 4) or (ABuffer[i1+1] shr 4)];
      inc(i1);
      ACode[i2+2]:=ACharTable^[(ABuffer[i1] and $f) shl 2];
      ACode[i2+3]:=ASuffix;
    end;
  end;
end;

procedure KRBase64EncodeAnsi(var ASource: AnsiString; out ACode: AnsiString;
  ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);
var
  i,n,i1,i2,m,_ln: integer;
begin
  _ln:=Length(ASource);
  n:=_ln div 3;
  m:=n shl 2;
  SetLength(ACode,m);
  i1:=0;i2:=1;dec(n);
  for i := 0 to n do begin
    inc(i1);
    ACode[i2]:=ACharTable^[Ord(ASource[i1]) shr 2];
    inc(i2);
    ACode[i2]:=ACharTable^[((Ord(ASource[i1]) and 3) shl 4) or (Ord(ASource[i1+1]) shr 4)];
    inc(i1);inc(i2);
    ACode[i2]:=ACharTable^[((Ord(ASource[i1]) and $f) shl 2) or (Ord(ASource[i1+1]) shr 6)];
    inc(i1);inc(i2);
    ACode[i2]:=ACharTable^[Ord(ASource[i1]) and $3f];
    inc(i2);
  end;
  n:=_ln mod 3;
  case n of
    1: begin
      inc(i1);
      SetLength(ACode,m+4);
      ACode[i2]:=ACharTable^[Ord(ASource[i1]) shr 2];
      ACode[i2+1]:=ACharTable^[(Ord(ASource[i1]) and 3) shl 4];
      ACode[i2+2]:=ASuffix;
      ACode[i2+3]:=ASuffix;
    end;
    2: begin
      inc(i1);
      SetLength(ACode,m+4);
      ACode[i2]:=ACharTable^[Ord(ASource[i1]) shr 2];
      ACode[i2+1]:=ACharTable^[((Ord(ASource[i1]) and 3) shl 4) or (Ord(ASource[i1+1]) shr 4)];
      inc(i1);
      ACode[i2+2]:=ACharTable^[(Ord(ASource[i1]) and $f) shl 2];
      ACode[i2+3]:=ASuffix;
    end;
  end;
end;

procedure KRBase64EncodeStream(ASource, ADest: TStream; ALength: integer;
  ACharTable: PKRBase64CharTable; ASuffix: TKRBase64Suffix);
var
  buff: array[0..KRBASE64_CACHE_SAZE-1] of byte;
  dest: array[0..(KRBASE64_CACHE_SAZE div 3)*4+4] of byte;
  l,n,k,m: integer;
begin
  l:=ALength;
  ASource.Position:=0;
  repeat
    if ASource.Position=ASource.Size then break;
    if l>KRBASE64_CACHE_SAZE then k:=KRBASE64_CACHE_SAZE else k:=l;
    n:=ASource.Read(buff,k);
    m:=KRBase64Encode(@buff,@dest,n,ACharTable,ASuffix);
    ADest.Write(dest,m);
    dec(l,n);
  until l>0;
end;

end.
