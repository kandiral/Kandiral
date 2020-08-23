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

{$I '..\Includes\language.inc'}

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils,
  {$ELSE}
    SysUtils,
  {$IFEND}
  KRTypes, lgop, Funcs;

type
  TModbusError = integer;
  TMBFunc = byte;
  TMBType = (mbtRTU, mbtTCP, mbtASCII);

const
  MB_QUEUE_MAX_ITEMS = 255;

  MB_ERRORS_COUNT = 9;

  MBERR_OK = TModbusError(0);
  MBERR_ILLEGAL_FUNCTION = TModbusError(1);
  MBERR_ILLEGAL_DATA_ADDRESS = TModbusError(2);
  MBERR_ILLEGAL_DATA_VALUE = TModbusError(3);
  MBERR_FAILURE_IN_ASSOCIATED_DEVICE = TModbusError(4);
  MBERR_ACKNOWLEDGE = TModbusError(5);
  MBERR_BUSY_REJECTED_MESSAGE = TModbusError(6);
  MBERR_NAK_NEGATIVE_ACKNOWLEDGMENT = TModbusError(7);
  MBERR_MEMORY_PARITY_ERROR = TModbusError(8);

  MODBUS_ERRORS_MSG : array[0..MB_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
     'Нет ошибок',
     'Недопустимый номер функции',
     'Некорректный адрес регистра',
     'Некорректные данные',
     'Отказ оборудования прибора',
     'Данные не готовы',
     'Система занята',
     'Отрицательное подтверждение',
     'Ошибка четности памяти'
{$ELSE}
     'No errors',
     'Invalid function number',
     'Invalid register address',
     'Invalid data',
     'Instrument hardware failure',
     'Data not ready',
     'System busy',
     'Negative acknowledgment',
     'Memory parity error'
{$ENDIF}
  );

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
  Result:=AChar-48;
  if Result>9 then Result:=Result-7;
end;

function MBHexToValue(AChar1, AChar2: byte): byte;
begin
  Result:=MBHexCharToValue(AChar1)*16 + MBHexCharToValue(AChar2);
end;

function MBLRC(ABuffer: PKRBuffer; ALength: byte): byte;
var
  i: Integer;
  wd: Word;
begin
  wd := 0;
  i := 1;
  while i<ALength-4 do begin
    wd:=WordRec(wd).Lo+MBHexToValue(ABuffer^[i],ABuffer^[i+1]);
    Inc(i,2);
  end;
  Result:=$FF-WordRec(wd).Lo;
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
  bt0, bt1: Byte;
  s: AnsiString;
begin
  SetLength(s,Length(AData)*2);
  if AHighByteFirst then
    for i := 0 to Length(AData)-1 do begin
      BytesFromWord(AData[i],bt0,bt1);
      s[i*2+1]:=AnsiChar(bt0);
      s[i*2+2]:=AnsiChar(bt1);
    end
  else begin
    j:=1;
    for i := Length(AData)-1 downto 0 do begin
      BytesFromWord(AData[i],bt0,bt1);
      s[j*2]:=AnsiChar(bt0);
      s[j*2+1]:=AnsiChar(bt1);
      inc(j);
    end
  end;
  Result:=StringToWideString(s,1251);
end;

end.
