unit KRCryptCommon;

interface

type
  TKRSHA1Hash = record
    case Integer of
    0: (bytes: array[0..19] of byte);
    1: (A, B, C, D, E: Cardinal);
  end;
  PKRSHA1Hash=^TKRSHA1Hash;

implementation

end.
