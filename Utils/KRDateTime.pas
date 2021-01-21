unit KRDateTime;

interface

  function KRUnixToDateTime(ATimeStamp: int64): TDateTime;
  function KRDateTimeToUnix(ADateTime: TDateTime): int64;

implementation

function KRUnixToDateTime(ATimeStamp: int64): TDateTime;
begin
  Result:=ATimeStamp/86400+25569;
end;

function KRDateTimeToUnix(ADateTime: TDateTime): int64;
begin
    Result:=Trunc((ADateTime-25569)*86400.000001);
end;

end.
