(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRModbus                                                                  *)
(*  Ver.: 23.08.2020                                                          *)
(*  https://kandiral.ru/delphi/krmodbus.pas.html                              *)
(*                                                                            *)
(******************************************************************************)
unit KRModbus;

interface

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils,
  {$ELSE}
    SysUtils,
  {$IFEND}
  KRModbusLng, KRTypes;

type
  TMBFunc = byte;
  TMBType = (mbtRTU, mbtTCP, mbtASCII);

const
  mbfReadCoils = TMBFunc($01);
  mbfReadDiscretInputs = TMBFunc($02);
  mbfReadHoldingRegisters = TMBFunc($03);
  mbfReadInputRegisters = TMBFunc($04);
  mbfWriteCoil = TMBFunc($05);
  mbfWriteHoldingRegister = TMBFunc($06);
  mbfReadStatus = TMBFunc($07);
  mbfWriteCoils = TMBFunc($0F);
  mbfWriteHoldingRegisters = TMBFunc($10);

  MBHEX_CHARS : array[0..15] of byte =
    (48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70);

  function MBHexCharToValue(AChar: byte): byte;
  function MBHexToValue(AChar1, AChar2: byte): byte;
  function MBLRC(ABuffer: PKRBuffer; ALength: byte): byte;
  function MBRegsToBYTE(AData: TKRRegisters; AHighByteFirst: boolean = true): Byte;
  function MBRegsToWORD(AData: TKRRegisters; AHighByteFirst: boolean = true): WORD;
  function MBRegsToDWORD(AData: TKRRegisters; AHighWordFirst: boolean = false;
    AHighByteFirst: boolean = true): Cardinal;
  function MBRegsToSMALLINT(AData: TKRRegisters; AHighByteFirst: boolean = true): Smallint;
  function MBRegsToINT(AData: TKRRegisters; AHighWordFirst: boolean = false;
    AHighByteFirst: boolean = true): Integer;
  function MBRegsToINT64(AData: TKRRegisters; AHighDWordFirst: boolean = false;
    AHighWordFirst: boolean = true; AHighByteFirst: boolean = true): Int64;
  function MBRegsToUINT64(AData: TKRRegisters; AHighDWordFirst: boolean = false;
    AHighWordFirst: boolean = true; AHighByteFirst: boolean = true): UInt64;
  function MBRegsToSINGLE(AData: TKRRegisters; AHighWordFirst: boolean = false;
    AHighByteFirst: boolean = true): Single;
  function MBRegsToDOUBLE(AData: TKRRegisters; AHighDWordFirst: boolean = false;
    AHighWordFirst: boolean = true; AHighByteFirst: boolean = true): Double;
  function MBRegsToSTRING(AData: TKRRegisters; AHighByteFirst: boolean = true): String;

implementation

function MBHexCharToValue(AChar: byte): byte;
begin
  if AChar>57 then Result:=AChar-55 else Result:=AChar-48;
end;

function MBHexToValue(AChar1, AChar2: byte): byte;
begin
  Result:=(MBHexCharToValue(AChar1) shl 4) or MBHexCharToValue(AChar2);
end;

function MBLRC(ABuffer: PKRBuffer; ALength: byte): byte;
var
  i,n: Integer;
begin
  Result:= 0;
  i := 1;
  n:=ALength-4;
  while i<n do begin
    Inc(Result,MBHexToValue(ABuffer^[i],ABuffer^[i+1]));
    Inc(i,2);
  end;
  Result:=(Result xor $FF)+1;
end;

function _RegsToWord(reg: word; AHighByteFirst: boolean): word;
begin
  if AHighByteFirst then Result:=reg
  else Result:=(reg shr 8) or(reg shl 8);
end;

function MBRegsToBYTE(AData: TKRRegisters; AHighByteFirst: boolean): Byte;
begin
  Result:=0;
  if Length(AData)>0 then Result:=_RegsToWord(AData[0],AHighByteFirst) and $FF;
end;

function MBRegsToWORD(AData: TKRRegisters; AHighByteFirst: boolean): WORD;
begin
  Result:=0;
  if Length(AData)>0 then Result:=_RegsToWord(AData[0],AHighByteFirst)
end;

function MBRegsToSMALLINT(AData: TKRRegisters; AHighByteFirst: boolean): Smallint;
var
  w: Word;
  p: pointer;
begin
  Result:=0;
  if Length(AData)>0 then begin
    w:=_RegsToWord(AData[0],AHighByteFirst);
    p:=@w;
    Result:=SmallInt(p^);
  end;
end;

function _regsToDword(reg0,reg1: word; AHighWordFirst, AHighByteFirst: boolean): Cardinal;
begin
  if AHighWordFirst then
    Result:=(Cardinal(_RegsToWord(reg0,AHighByteFirst))shl 16)or _RegsToWord(reg1,AHighByteFirst)
  else
    Result:=(Cardinal(_RegsToWord(reg1,AHighByteFirst))shl 16)or _RegsToWord(reg0,AHighByteFirst)
end;

function MBRegsToDWORD(AData: TKRRegisters; AHighWordFirst, AHighByteFirst: boolean): Cardinal;
begin
  Result:=0;
  if Length(AData)=1 then Result:=_RegsToWord(AData[0],AHighByteFirst)else
  if Length(AData)>1 then Result:=_regsToDword(AData[0],AData[1],AHighWordFirst,AHighByteFirst);
end;

function MBRegsToINT(AData: TKRRegisters; AHighWordFirst, AHighByteFirst: boolean): Integer;
var
  dw: Cardinal;
  p: pointer;
begin
  Result:=0;
  if Length(AData)>0 then begin
    dw:=MBRegsToDWORD(AData, AHighWordFirst, AHighByteFirst);
    p:=@dw;
    Result:=Integer(p^);
  end;
end;

function MBRegsToSINGLE(AData: TKRRegisters; AHighWordFirst, AHighByteFirst: boolean): Single;
var
  dw: Cardinal;
  p: pointer;
begin
  Result:=0;
  if Length(AData)>0 then begin
    dw:=MBRegsToDWORD(AData, AHighWordFirst, AHighByteFirst);
    p:=@dw;
    Result:=Single(p^);
  end;
end;

function _regsToDDword(reg0,reg1,reg2,reg3: word; AHighDWordFirst, AHighWordFirst,
  AHighByteFirst: boolean): uint64;
begin
  if AHighDWordFirst then
    Result:=(uint64(_RegsToDWord(reg0,reg1,AHighWordFirst,AHighByteFirst))shl 32)or _RegsToDWord(reg2,reg3,AHighWordFirst,AHighByteFirst)
  else
    Result:=(uint64(_RegsToDWord(reg2,reg3,AHighWordFirst,AHighByteFirst))shl 32)or _RegsToDWord(reg0,reg1,AHighWordFirst,AHighByteFirst)
end;

function MBRegsToUINT64(AData: TKRRegisters; AHighDWordFirst, AHighWordFirst,
  AHighByteFirst: boolean): UInt64;
begin
  case Length(AData) of
  0: Result:=0;
  1: Result:=_RegsToWord(AData[0],AHighByteFirst);
  2: Result:=_regsToDword(AData[0],AData[1],AHighWordFirst,AHighByteFirst);
  3: Result:=_regsToDDword(AData[0],AData[1],AData[2],0,AHighDWordFirst,AHighWordFirst,AHighByteFirst)
  else Result:=_regsToDDword(AData[0],AData[1],AData[2],AData[3],AHighDWordFirst,AHighWordFirst,AHighByteFirst);
  end;
end;

function MBRegsToINT64(AData: TKRRegisters; AHighDWordFirst, AHighWordFirst,
  AHighByteFirst: boolean): int64;
var
  ui64: uint64;
  p: pointer;
begin
  Result:=0;
  if Length(AData)>0 then begin
    ui64:=MBRegsToUINT64(AData, AHighDWordFirst, AHighWordFirst, AHighByteFirst);
    p:=@ui64;
    Result:=int64(p^);
  end;
end;

function MBRegsToDOUBLE(AData: TKRRegisters; AHighDWordFirst, AHighWordFirst,
  AHighByteFirst: boolean): Double;
var
  ui64: uint64;
  p: pointer;
begin
  Result:=0;
  if Length(AData)>0 then begin
    ui64:=MBRegsToUINT64(AData, AHighDWordFirst, AHighWordFirst, AHighByteFirst);
    p:=@ui64;
    Result:=double(p^);
  end;
end;

function MBRegsToSTRING(AData: TKRRegisters; AHighByteFirst: boolean): String;
var
  i,j: Integer;
  s: AnsiString;
begin
  SetLength(s,Length(AData)*2);
  if AHighByteFirst then
    for i := 0 to Length(AData)-1 do begin
      s[i*2+1]:=AnsiChar(AData[i] and $ff);
      s[i*2+2]:=AnsiChar(AData[i] shr 8);
    end
  else begin
    j:=1;
    for i := Length(AData)-1 downto 0 do begin
      s[j*2]:=AnsiChar(AData[i] and $ff);
      s[j*2+1]:=AnsiChar(AData[i] shr 8);
      inc(j);
    end
  end;
  Result:=String(s);
end;

end.
