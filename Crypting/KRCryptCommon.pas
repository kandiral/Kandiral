(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCryptCommon                                                             *)
(*  Ver.: 28.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCryptCommon;

interface

type
  TKRSHA1Hash = record
    case Integer of
    0: (bytes: array[0..19] of UInt8);
    1: (A, B, C, D, E: UInt32);
  end;
  PKRSHA1Hash = ^TKRSHA1Hash;

  TKRMD5Hash = record
    case Integer of
    0: (bytes: array[0..15] of UInt8);
    1: (A, B, C, D: UInt32);
  end;
  PKRMD5Hash = ^TKRMD5Hash;

  function KRSHA1HashEqual(h1, h2: PKRSHA1Hash): boolean;
  function KRMD5HashEqual(h1, h2: PKRMD5Hash): boolean;

implementation

function KRSHA1HashEqual(h1, h2: PKRSHA1Hash): boolean;
begin
  Result := ( h1.A = h2.A ) and ( h1.B = h2.B ) and ( h1.C = h2.C ) and ( h1.D = h2.D ) and ( h1.E = h2.E );
end;

function KRMD5HashEqual(h1, h2: PKRMD5Hash): boolean;
begin
  Result := ( h1.A = h2.A ) and ( h1.B = h2.B ) and ( h1.C = h2.C ) and ( h1.D = h2.D );
end;

end.
