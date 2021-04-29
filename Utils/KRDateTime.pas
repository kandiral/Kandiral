(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRDateTime                                                                *)
(*  Ver.: 11.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRDateTime;

interface

  function KRUnixToDateTime(ATimeStamp: int64): TDateTime;
  function KRUnixMsToDateTime(ATimeStamp: int64): TDateTime;
  function KRDateTimeToUnix(ADateTime: TDateTime): int64;
  function KRDateTimeToUnixMs(ADateTime: TDateTime): int64;
  function KRYearOfDate( ATimeStamp: int64 ): Word;
  function KRMonthOfDate( ATimeStamp: int64 ): Word;
  function KRDayOfMonth( ATimeStamp: int64 ): Word;

implementation

function KRUnixToDateTime(ATimeStamp: int64): TDateTime;
begin
  Result := ATimeStamp / 86400 + 25569;
end;

function KRUnixMsToDateTime(ATimeStamp: int64): TDateTime;
begin
  Result := ATimeStamp / 86400000 + 25569;
end;

function KRDateTimeToUnix(ADateTime: TDateTime): int64;
begin
    Result := Trunc( ( ADateTime - 25569 ) * 86400.000001);
end;

function KRDateTimeToUnixMs(ADateTime: TDateTime): int64;
begin
    Result := Trunc( ( ADateTime - 25569 ) * 86400000.001 );
end;

function  KRDayOfYear( ATimeStamp: int64 ): Word;
var
  res: uint32;
begin
  res := ( ( ATimeStamp div 86400 ) mod 1461 );
  if( res > 729 )then begin
    if( res > 1095 ) then res := res - 1095 else res := res - 729;
  end else if( res > 364 )then res := res - 364 else res := res + 1;
  Result := res;
end;

function KRLeapOfDate( ATimeStamp: int64 ): boolean;
begin
  Result := ( ( ( ATimeStamp + 43200 ) div 31557600 ) shl 30) = $80000000 ;
end;

function KRMonthOfDate( ATimeStamp: int64 ): Word;
begin
  Result := KRDayOfYear( ATimeStamp );
  if( Result < 32)then Result := 1
  else if( KRLeapOfDate( ATimeStamp ) )then Result := ( Result * 53 + 1668 ) div 1623
  else Result := ( Result * 53 + 1700 ) div 1620;
end;

function KRDayOfMonth( ATimeStamp: int64 ): Word;
const
  MTH_OFS : array[ 0..12 ] of word = ( 0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 );

var
  leap: word;
begin
  Result := KRDayOfYear( ATimeStamp );
  if KRLeapOfDate( ATimeStamp )then leap := 1 else leap := 0;
  Result := Result - leap;
  if( Result > MTH_OFS[9] )then begin
    if( Result > MTH_OFS[11] )then begin
      if( Result > MTH_OFS[12] )then Result := Result - MTH_OFS[12] else Result := Result - MTH_OFS[11];
    end else begin
      if( Result > MTH_OFS[10] )then Result := Result - MTH_OFS[10] else Result := Result - MTH_OFS[9];
    end;
  end else if( Result > MTH_OFS[5] )then begin
    if( Result > MTH_OFS[7] )then begin
      if( Result > MTH_OFS[8] )then Result := Result - MTH_OFS[8] else Result := Result - MTH_OFS[7];
    end else begin
      if( Result > MTH_OFS[6] )then Result := Result - MTH_OFS[6] else Result := Result - MTH_OFS[5];
    end;
  end else if( Result > MTH_OFS[3] )then begin
    if( Result > MTH_OFS[4] )then Result := Result - MTH_OFS[4] else Result := Result - MTH_OFS[3];
  end else begin
    Result := Result + leap;
    if( Result > MTH_OFS[2] )then Result := Result - MTH_OFS[2];
  end;
end;

function KRYearOfDate( ATimeStamp: int64 ): Word;
begin
  Result := ( ( ATimeStamp + 43200 ) div 31557600 + 1970 );
end;

end.
