(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRStreamCoder                                                             *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRStreamCoder;

interface

uses Classes, md5;

  procedure KREncodeStream(AInStream, AOutStream: TStream; APassword: String);
  procedure KRDecodeStream(AInStream, AOutStream: TStream; APassword: String);

implementation

uses KRTypes, Funcs, lgop, StrUtils;

function CreateHash(APassword: String): TKRBuffer;
var
  s: AnsiString;
  i,j: integer;
  dg: TMD5Digest;
begin
  s:=WideStringToString(APassword,1251);
  for i := 0 to 7 do begin
    dg:=MD5String(s);
    s:=MD5DigestToStr(dg);
    for j := 0 to 15 do begin
      Result[i*32+j*2]:=dg.v[j] and $F;
      if i*32+j*2+1=255 then exit;
      Result[i*32+j*2+1]:=dg.v[j] shr 4;
    end;
  end;
end;

procedure KREncodeStream(AInStream, AOutStream: TStream; APassword: String);
var
  _hash: TKRBuffer;
  _bf: TKRBuffer;
  bt: byte;
  i: integer;
begin
  _hash:=CreateHash(APassword);
  AOutStream.Size:=0;
  repeat
    bt:=AInStream.Read(_bf,254);
    for i := 0 to bt-1 do _bf[i]:=LRot(_bf[i],_hash[i]);
    if bt>0 then AOutStream.Write(_bf,bt);    
  until bt<254;
end;

procedure KRDecodeStream(AInStream, AOutStream: TStream; APassword: String);
var
  _hash: TKRBuffer;
  _bf: TKRBuffer;
  bt: byte;
  i: integer;
begin
  _hash:=CreateHash(APassword);
  AOutStream.Size:=0;
  repeat
    bt:=AInStream.Read(_bf,254);
    for i := 0 to bt-1 do _bf[i]:=RRot(_bf[i],_hash[i]);
    if bt>0 then AOutStream.Write(_bf,bt);
  until bt<254;
end;

end.
