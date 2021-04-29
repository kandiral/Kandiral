(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRLogical                                                                 *)
(*  Ver.: 11.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRLogical;

interface

  function KRGetBit64(const Value: int64; const ABit: integer): boolean;{$IFDEF MSWINDOWS}assembler;{$ENDIF}

  function KRGetBit32(const Value: integer; const ABit: integer): boolean;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRSetBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRClearBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRToggleBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRSetBitTo32(const ASetTo: boolean; const Value: integer; const ABit: integer): Integer; assembler;


  function KRSwap32(const Value: Integer): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}


  function KRLeftRot8(const Value: Byte; const Count: Byte): Byte;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRLeftRot16(const Value: Word; const Count: Byte): Word;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRLeftRot32(const Value: Integer; const Count: Byte): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRLeftRot64(const Value: Int64; const Count: Byte): Int64;{$IF Defined(MSWINDOWS) and Defined(CPUX64)}assembler;{$IFEND}
  function KRRightRot8(const Value: Byte; const Count: Byte): Byte;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRRightRot16(const Value: Word; const Count: Byte): Word;{$IFDEF MSWINDOWS}assembler;{$ENDIF}
  function KRRightRot32(const Value: Integer; const Count: Byte): Integer;{$IFDEF MSWINDOWS}assembler;{$ENDIF}



implementation

function KRGetBit64(const Value: int64; const ABit: integer): boolean;{$IFDEF MSWINDOWS}assembler;
asm
{$IFDEF CPUX64}
  MOV      EDX, ABit
  BT       RAX, RDX
  SBB      RAX, RAX
  AND      RAX, 1
{$ELSE}
  CMP      ABit, 32
  JAE      @HighDWord
  MOV      EAX, [DWORD PTR EBP + 8]
  BT       EAX, ABit
  JMP      @Exit
@HighDWord:
  MOV      EDX, ABit
  SUB      EDX, 32
  MOV      EAX,[DWORD PTR EBP + 12]
  BT       EAX, EDX
@Exit:
  SBB      EAX, EAX
  AND      EAX, 1
{$ENDIF}
end;
{$ELSE}
begin
  Result:=(Value and (int64(1) shl ABit))<>0;
end;
{$ENDIF}

function KRGetBit32(const Value: integer; const ABit: integer): boolean;{$IFDEF MSWINDOWS}assembler;
asm
  BT       EAX, ABit
  SBB      EAX, EAX
  AND      EAX, 1
end;
{$ELSE}
begin
  Result:=(Value and (integer(1) shl ABit))<>0;
end;
{$ENDIF}

function KRSetBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  BTS      EAX, ABit
end;
{$ELSE}
begin
  Result:=Value or (integer(1) shl ABit);
end;
{$ENDIF}

function KRClearBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  BTR      EAX, ABit
end;
{$ELSE}
begin
  Result:=Value and not(integer(1) shl ABit);
end;
{$ENDIF}

function KRToggleBit32(const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  BTC      EAX, ABit
end;
{$ELSE}
begin
  if (Value and (integer(1) shl ABit))=0 then
    Result:=Value or (integer(1) shl ABit)
  else
    Result:=Value and not(integer(1) shl ABit);
end;
{$ENDIF}

function KRSetBitTo32(const ASetTo: boolean; const Value: integer; const ABit: integer): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  CMP      AL, 0
  MOV      EAX, Value
  JZ       @ClearBit
  BTS      EAX, ABit
  JMP      @Exit
@ClearBit:
  BTR      EAX, ABit
@Exit:
end;
{$ELSE}
begin
  if ASetTo then
    Result:=Value or (integer(1) shl ABit)
  else
    Result:=Value and not(integer(1) shl ABit);
end;
{$ENDIF}

function KRSwap32(const Value: Integer): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  MOV      EDX, Value
  BSWAP    EDX
  MOV      Result, EDX
end;
{$ELSE}
begin
  Result := Swap(Value shr 16) or (Swap(Value) shl 16);
end;
{$ENDIF}


function KRLeftRot8(const Value: Byte; const Count: Byte): Byte;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROL AL, CL
end;
{$ELSE}
begin
  Result:=(Value shl Count) or (Value shr (8-Count));
end;
{$ENDIF}


function KRLeftRot16(const Value: Word; const Count: Byte): Word;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROL AX, CL
end;
{$ELSE}
begin
  Result:=(Value shl Count) or (Value shr (16-Count));
end;
{$ENDIF}

function KRLeftRot32(const Value: Integer; const Count: Byte): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROL EAX, CL
end;
{$ELSE}
begin
  Result:=(Value shl Count) or (Value shr (32-Count));
end;
{$ENDIF}

function KRLeftRot64(const Value: Int64; const Count: Byte): Int64;{$IF Defined(MSWINDOWS) and Defined(CPUX64)} assembler;
asm
  MOV CL, Count
  ROL RAX, CL
end;
{$ELSE}
begin
  Result:=(Value shl Count) or (Value shr (64-Count));
end;
{$IFEND}

function KRRightRot8(const Value: Byte; const Count: Byte): Byte;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROR AL, CL
end;
{$ELSE}
begin
  Result:=(Value shr Count) or (Value shl (8-Count));
end;
{$ENDIF}

function KRRightRot16(const Value: Word; const Count: Byte): Word;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROR AX, CL
end;
{$ELSE}
begin
  Result:=(Value shr Count) or (Value shl (16-Count));
end;
{$ENDIF}

function KRRightRot32(const Value: Integer; const Count: Byte): Integer;{$IFDEF MSWINDOWS}assembler;
asm
  MOV CL, Count
  ROR EAX, CL
end;
{$ELSE}
begin
  Result:=(Value shr Count) or (Value shl (32-Count));
end;
{$ENDIF}

end.
