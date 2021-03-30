(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRModbusSlave                                                             *)
(*  Ver.: 18.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRModbusSlave;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$IFEND}
    KRTypes, KRModbus, KRCRC, lgop;

  //------ Modbus Slave Responses ---------------------------------------------
  function MBSReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;

  //------ Modbus RTU Slave Responses ---------------------------------------------
  function MBSRTUReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSRTUReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSRTUWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSRTUWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSRTUError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;

  //------ Modbus TCP Slave Responses ---------------------------------------------
  function MBSTCPReadHoldingRegisters(TransactionID: word; AAddr: byte;
    AData: TKRRegisters; AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSTCPReadInputRegisters(TransactionID: word; AAddr: byte;
    AData: TKRRegisters; AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSTCPWriteHoldingRegister(TransactionID: word; AAddr: byte;
    AStartReg: Word; AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSTCPWriteHoldingRegisters(TransactionID: word; AAddr: byte;
    AStartReg, ACount: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSTCPError(TransactionID: word; AAddr, AFunc, AError: byte;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  //------ Modbus ASCII Slave Responses ---------------------------------------------
  function MBSASCIIReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSASCIIReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSASCIIWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
  function MBSASCIIWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;

  function MBSASCIIError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;


implementation

function MBSReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
var i,n: integer;
begin
  ABuffer^[AFrom]:=AAddr;
  ABuffer^[AFrom+1]:=mbfReadHoldingRegisters;
  n:=Length(AData);
  ABuffer^[AFrom+2]:=n+n;
  result:=n+3;
  dec(n);
  for I := 0 to n do begin
    ABuffer^[AFrom+3+i*2]:=AData[i] shr 8;
    ABuffer^[AFrom+4+i*2]:=AData[i];
  end;
end;

function MBSReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
var i,n: integer;
begin
  ABuffer^[AFrom]:=AAddr;
  ABuffer^[AFrom+1]:=mbfReadInputRegisters;
  n:=Length(AData);
  ABuffer^[AFrom+2]:=n+n;
  result:=n+3;
  dec(n);
  for I := 0 to n do begin
    ABuffer^[AFrom+3+i*2]:=AData[i] shr 8;
    ABuffer^[AFrom+4+i*2]:=AData[i];
  end;
end;

function MBSWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[AFrom]:=AAddr;
  ABuffer^[AFrom+1]:=mbfWriteHoldingRegister;
  BytesFromWord(AStartReg,ABuffer^[AFrom+3],ABuffer^[AFrom+2]);
  BytesFromWord(AData,ABuffer^[AFrom+5],ABuffer^[AFrom+4]);
  Result:=6;
end;

function MBSWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[AFrom]:=AAddr;
  ABuffer^[AFrom+1]:=mbfWriteHoldingRegisters;
  BytesFromWord(AStartReg,ABuffer^[AFrom+3],ABuffer^[AFrom+2]);
  BytesFromWord(ACount,ABuffer^[AFrom+5],ABuffer^[AFrom+4]);
  Result:=6;
end;

function MBSError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[AFrom]:=AAddr;
  ABuffer^[AFrom+1]:=SetBit(AFunc,7);
  ABuffer^[AFrom]:=AError;
end;

function MBSRTUReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  Result:=MBSReadHoldingRegisters(AAddr,AData,0,ABuffer);
  KRCRC16( PByte( ABuffer ), Result, ABuffer^[ Result + 1 ], ABuffer^[ Result ] );
  Result:=Result+2;
end;

function MBSRTUReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  Result:=MBSReadInputRegisters(AAddr,AData,0,ABuffer);
  KRCRC16( PByte( ABuffer ),Result,ABuffer^[Result+1],ABuffer^[Result]);
  Result:=Result+2;
end;

function MBSRTUWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  Result:=MBSWriteHoldingRegister(AAddr,AStartReg,AData,0,ABuffer);
  KRCRC16( PByte( ABuffer ),Result,ABuffer^[Result+1],ABuffer^[Result]);
  Result:=Result+2;
end;

function MBSRTUWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  Result:=MBSWriteHoldingRegisters(AAddr,AStartReg,ACount,0,ABuffer);
  KRCRC16( PByte( ABuffer ),Result,ABuffer^[Result+1],ABuffer^[Result]);
  Result:=Result+2;
end;

function MBSRTUError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;
begin
  Result:=MBSError(AAddr,AFunc,AError,0,ABuffer);
  KRCRC16( PByte( ABuffer ),Result,ABuffer^[Result+1],ABuffer^[Result]);
  Result:=Result+2;
end;

function MBSTCPReadHoldingRegisters(TransactionID: word; AAddr: byte;
    AData: TKRRegisters; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[5]:=MBSReadHoldingRegisters(AAddr,AData,6,ABuffer);
  ABuffer^[0]:=TransactionID shr 8;
  ABuffer^[1]:=TransactionID;
  ABuffer^[2]:=0;
  ABuffer^[3]:=0;
  ABuffer^[4]:=0;
  Result:=ABuffer^[5]+6;
end;

function MBSTCPReadInputRegisters(TransactionID: word; AAddr: byte;
    AData: TKRRegisters; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[5]:=MBSReadInputRegisters(AAddr,AData,6,ABuffer);
  ABuffer^[0]:=TransactionID shr 8;
  ABuffer^[1]:=TransactionID;
  ABuffer^[2]:=0;
  ABuffer^[3]:=0;
  ABuffer^[4]:=0;
  Result:=ABuffer^[5]+6;
end;

function MBSTCPWriteHoldingRegister(TransactionID: word; AAddr: byte;
    AStartReg: Word; AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[5]:=MBSWriteHoldingRegister(AAddr,AStartReg,AData,6,ABuffer);
  ABuffer^[0]:=TransactionID shr 8;
  ABuffer^[1]:=TransactionID;
  ABuffer^[2]:=0;
  ABuffer^[3]:=0;
  ABuffer^[4]:=0;
  Result:=ABuffer^[5]+6;
end;

function MBSTCPWriteHoldingRegisters(TransactionID: word; AAddr: byte;
    AStartReg, ACount: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[5]:=MBSWriteHoldingRegisters(AAddr,AStartReg,ACount,6,ABuffer);
  ABuffer^[0]:=TransactionID shr 8;
  ABuffer^[1]:=TransactionID;
  ABuffer^[2]:=0;
  ABuffer^[3]:=0;
  ABuffer^[4]:=0;
  Result:=ABuffer^[5]+6;
end;

function MBSTCPError(TransactionID: word; AAddr, AFunc, AError: byte;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
begin
  ABuffer^[5]:=MBSError(AAddr,AFunc,AError,6,ABuffer);
  ABuffer^[0]:=TransactionID shr 8;
  ABuffer^[1]:=TransactionID;
  ABuffer^[2]:=0;
  ABuffer^[3]:=0;
  ABuffer^[4]:=0;
  Result:=ABuffer^[5]+6;
end;

function MBSASCIIReadHoldingRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
var
  p: PKRBuffer;
  i: integer;
  wd: Word;
  bt: byte;
begin
  ABuffer^[0]:=58;
  New(p);
  Result:=MBSReadHoldingRegisters(AAddr,AData,0,p);
  wd:=0;
  for i := 0 to Result-1 do begin
    wd:=WordRec(wd).Lo + p^[i];
    ABuffer^[i*2+1]:=MBHEX_CHARS[p^[i] shr 4];
    ABuffer^[i*2+2]:=MBHEX_CHARS[p^[i] and $F]
  end;
  Dispose(p);
  Result:=Result*2+1;
  bt:=-WordRec(wd).Lo;
  ABuffer^[Result]:=MBHEX_CHARS[bt shr 4];
  Inc(Result);
  ABuffer^[Result]:=MBHEX_CHARS[bt and $F];
  Inc(Result);
  ABuffer^[Result]:=13;
  Inc(Result);
  ABuffer^[Result]:=10;
  Inc(Result);
end;

function MBSASCIIReadInputRegisters(AAddr: byte; AData: TKRRegisters;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
var
  p: PKRBuffer;
  i: integer;
  wd: Word;
  bt: byte;
begin
  ABuffer^[0]:=58;
  New(p);
  Result:=MBSReadInputRegisters(AAddr,AData,0,p);
  wd:=0;
  for i := 0 to Result-1 do begin
    wd:=WordRec(wd).Lo + p^[i];
    ABuffer^[i*2+1]:=MBHEX_CHARS[p^[i] shr 4];
    ABuffer^[i*2+2]:=MBHEX_CHARS[p^[i] and $F]
  end;
  Dispose(p);
  Result:=Result*2+1;
  bt:=-WordRec(wd).Lo;
  ABuffer^[Result]:=MBHEX_CHARS[bt shr 4];
  Inc(Result);
  ABuffer^[Result]:=MBHEX_CHARS[bt and $F];
  Inc(Result);
  ABuffer^[Result]:=13;
  Inc(Result);
  ABuffer^[Result]:=10;
  Inc(Result);
end;

function MBSASCIIWriteHoldingRegister(AAddr: byte; AStartReg: Word;
    AData: Word; AFrom: byte; ABuffer: PKRBuffer): Byte;
var
  p: PKRBuffer;
  i: integer;
  wd: Word;
  bt: byte;
begin
  ABuffer^[0]:=58;
  New(p);
  Result:=MBSWriteHoldingRegister(AAddr,AStartReg,AData,0,p);
  wd:=0;
  for i := 0 to Result-1 do begin
    wd:=WordRec(wd).Lo + p^[i];
    ABuffer^[i*2+1]:=MBHEX_CHARS[p^[i] shr 4];
    ABuffer^[i*2+2]:=MBHEX_CHARS[p^[i] and $F]
  end;
  Dispose(p);
  Result:=Result*2+1;
  bt:=-WordRec(wd).Lo;
  ABuffer^[Result]:=MBHEX_CHARS[bt shr 4];
  Inc(Result);
  ABuffer^[Result]:=MBHEX_CHARS[bt and $F];
  Inc(Result);
  ABuffer^[Result]:=13;
  Inc(Result);
  ABuffer^[Result]:=10;
  Inc(Result);
end;

function MBSASCIIWriteHoldingRegisters(AAddr: byte; AStartReg, ACount: Word;
    AFrom: byte; ABuffer: PKRBuffer): Byte;
var
  p: PKRBuffer;
  i: integer;
  wd: Word;
  bt: byte;
begin
  ABuffer^[0]:=58;
  New(p);
  Result:=MBSWriteHoldingRegisters(AAddr,AStartReg,ACount,0,p);
  wd:=0;
  for i := 0 to Result-1 do begin
    wd:=WordRec(wd).Lo + p^[i];
    ABuffer^[i*2+1]:=MBHEX_CHARS[p^[i] shr 4];
    ABuffer^[i*2+2]:=MBHEX_CHARS[p^[i] and $F]
  end;
  Dispose(p);
  Result:=Result*2+1;
  bt:=-WordRec(wd).Lo;
  ABuffer^[Result]:=MBHEX_CHARS[bt shr 4];
  Inc(Result);
  ABuffer^[Result]:=MBHEX_CHARS[bt and $F];
  Inc(Result);
  ABuffer^[Result]:=13;
  Inc(Result);
  ABuffer^[Result]:=10;
  Inc(Result);
end;

function MBSASCIIError(AAddr, AFunc, AError: byte; AFrom: byte;
    ABuffer: PKRBuffer): Byte;
var
  p: PKRBuffer;
  i: integer;
  wd: Word;
  bt: byte;
begin
  ABuffer^[0]:=58;
  New(p);
  Result:=MBSError(AAddr,AFunc,AError,0,p);
  wd:=0;
  for i := 0 to Result-1 do begin
    wd:=WordRec(wd).Lo + p^[i];
    ABuffer^[i*2+1]:=MBHEX_CHARS[p^[i] shr 4];
    ABuffer^[i*2+2]:=MBHEX_CHARS[p^[i] and $F]
  end;
  Dispose(p);
  Result:=Result*2+1;
  bt:=-WordRec(wd).Lo;
  ABuffer^[Result]:=MBHEX_CHARS[bt shr 4];
  Inc(Result);
  ABuffer^[Result]:=MBHEX_CHARS[bt and $F];
  Inc(Result);
  ABuffer^[Result]:=13;
  Inc(Result);
  ABuffer^[Result]:=10;
  Inc(Result);
end;

end.
