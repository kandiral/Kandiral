(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRUtils                                                                   *)
(*  Ver.: 16.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRUtils;

interface

  function KRHexByteToVal(AStr: AnsiString): byte; overload;
  function KRHexByteToVal(ACh0, ACh1: AnsiChar): byte; overload;
  function KRHexCharToVal(ACh: AnsiChar): byte;
  function KRByteToHexChar(AVal: Byte): AnsiString;
  function KRValToHexChar(AVal: Byte): AnsiChar;


implementation

function KRHexByteToVal(AStr: AnsiString): byte; overload;
begin
  Result:=(KRHexCharToVal(AStr[1]) shl 4)or KRHexCharToVal(AStr[2]);
end;

function KRHexByteToVal(ACh0, ACh1: AnsiChar): byte;
begin
  Result:=(KRHexCharToVal(ACh0) shl 4)or KRHexCharToVal(ACh1);
end;

function KRHexCharToVal(ACh: AnsiChar): byte;
var bt: byte;
begin
  bt:=Ord(ACh);
  if bt>57 then Result:=bt-55 else Result:=bt-48;
end;

function KRByteToHexChar(AVal: Byte): AnsiString;
begin
  Result:=KRValToHexChar(AVal shr 4)+KRValToHexChar(AVal and $f);
end;

function KRValToHexChar(AVal: Byte): AnsiChar;
begin
  if AVal>9 then Result:=AnsiChar(AVal+55) else Result:=AnsiChar(AVal+48)
end;

end.
