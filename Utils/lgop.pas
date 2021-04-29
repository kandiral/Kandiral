(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  lgop                                                                      *)
(*  Ver.: 08.10.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit lgop;

interface

uses Windows, Variants, SysUtils, KRTypes;

  function _is(cond: boolean; _then, _else: Variant): Variant;
  function _sel(index: integer; const _list: array of Variant): Variant;
  function IntToBin(IValue : Int64; NumBits : word = 64) : string;
  function GetBit(const aValue: Cardinal; const Bit: Byte): Boolean;
  function SetBit(const aValue: Cardinal; const Bit: Byte): Cardinal;
  function SetBitTo(const aValue: Cardinal; const Bit: Byte; Val: boolean): Cardinal;
  function ClearBit(const aValue: Cardinal; const Bit: Byte): Cardinal;
  function EnableBit(const aValue: Cardinal; const Bit: Byte; const Flag: Boolean): Cardinal;
  function BitsToByte(b0,b1,b2,b3,b4,b5,b6,b7: boolean):byte;
  procedure BitsFromByte(bt: byte; out b0,b1,b2,b3,b4,b5,b6,b7: boolean);
  function BytesToWord(bt0,bt1: byte): Word;
  procedure BytesFromWord(wd: Word; out bt0,bt1: byte);
  function WordsToDWord(wd0,wd1: Word): DWord;
  procedure WordsFromDWord(dw: DWord; out wd0,wd1: Word);
  function SetCount(Val: Cardinal; Bits: byte):integer;
  function SetBits(Val: Cardinal; Bits: byte; var B: TKRBuffer): integer;
  function mshr(x:Integer; mypow: Byte):Integer;

implementation

uses Funcs;

function mshr(x:Integer; mypow: Byte):Integer;
asm
  PUSH EAX;
  PUSH CX;
  MOV EAX, x
  MOV CL, mypow
  SAR EAX, CL  // Используем i80x86 SAR команду (Деление со знаком двойного слова)
  MOV Result, EAX
  POP CX;
  POP EAX;
end;

function SetBits(Val: Cardinal; Bits: byte; var B: TKRBuffer):integer;
var
  i: integer;
begin
  Result:=0;
  for i := 0 to Bits-1 do
    if GetBit(Val,i) then begin
      B[Result]:=i;
      Inc(Result);
    end;
end;

function SetCount(Val: Cardinal; Bits: byte):integer;
var
  i: integer;
begin
  Result:=0;
  for i := 0 to Bits-1 do
    if GetBit(Val,i) then Inc(Result);
end;

function _is(cond: boolean; _then, _else: Variant): Variant;
begin
  if cond then Result:=_then else Result:=_else;
end;

function _sel(index: integer; const _list: array of Variant): Variant;
begin
  Result:=null;
  if(index<0)or(index>=Length(_list))then exit;
  Result:=_list[index];
end;

function IntToBin(IValue : Int64; NumBits : word = 64) : string;
var
  RetVar : string;
begin
  RetVar := '';

  case NumBits of
    32 : IValue := dword(IValue);
    16 : IValue := word(IValue);
    8  : IValue := byte(IValue);
  end;

  while IValue <> 0 do begin
    Retvar := char(48 + (IValue and 1)) + RetVar;
    IValue := IValue shr 1;
  end;

  if RetVar = '' then Retvar := '0';
  Result := RetVar;
end;

function GetBit(const aValue: Cardinal; const Bit: Byte): Boolean;
begin
  Result := (aValue and (1 shl Bit)) <> 0;
end;

function SetBit(const aValue: Cardinal; const Bit: Byte): Cardinal;
begin
  Result := aValue or (1 shl Bit);
end;

function SetBitTo(const aValue: Cardinal; const Bit: Byte; Val: boolean): Cardinal;
begin
  if Val then Result:=SetBit(aValue,Bit) else Result:=ClearBit(aValue,Bit);
end;

function ClearBit(const aValue: Cardinal; const Bit: Byte): Cardinal;
begin
  Result := aValue and not (1 shl Bit);
end;

function EnableBit(const aValue: Cardinal; const Bit: Byte; const Flag: Boolean): Cardinal;
begin
  Result := (aValue or (1 shl Bit)) xor (Byte(not Flag) shl Bit);
end;

function BitsToByte(b0,b1,b2,b3,b4,b5,b6,b7: Boolean):byte;
begin
  Result:=
    Integer(b7) shl 7 or
    Integer(b6) shl 6 or
    Integer(b5) shl 5 or
    Integer(b4) shl 4 or
    Integer(b3) shl 3 or
    Integer(b2) shl 2 or
    Integer(b1) shl 1 or
    Integer(b0);
end;

procedure BitsFromByte(bt: byte; out b0,b1,b2,b3,b4,b5,b6,b7: boolean);
begin
  b0:=GetBit(bt,0);
  b1:=GetBit(bt,1);
  b2:=GetBit(bt,2);
  b3:=GetBit(bt,3);
  b4:=GetBit(bt,4);
  b5:=GetBit(bt,5);
  b6:=GetBit(bt,6);
  b7:=GetBit(bt,7);
end;

function BytesToWord(bt0,bt1: byte): Word;
begin
  Result:= bt0 or bt1 shl 8;
end;

procedure BytesFromWord(wd: Word; out bt0,bt1: byte);
begin
  bt0:=wd and $ff;
  bt1:=wd shr 8;
end;

function WordsToDWord(wd0,wd1: Word): DWord;
begin
  Result:= wd0 or wd1 shl 16;
end;

procedure WordsFromDWord(dw: DWord; out wd0,wd1: Word);
begin
  wd0:=dw and $ffff;
  wd1:=dw shr 16;
end;

end.
