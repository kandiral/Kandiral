unit KRVariants;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Variants, System.SysUtils;
  {$ELSE}
    Variants, SysUtils;
  {$IFEND}

  function KRVarIsEqually(const AVar1, AVar2: Variant): boolean;

implementation

function KRVarIsEqually(const AVar1, AVar2: Variant): boolean;
var
  vt1, vt2: TVarType;
begin
  result:=false;
  vt1:=FindVarData(AVar1)^.VType;
  vt2:=FindVarData(AVar2)^.VType;
  if((vt1>=2)and(vt1<=6))or((vt1>=$10)and(vt1<=$15)) then begin
    if ((vt2>=2)and(vt2<=6))or((vt2>=$10)and(vt2<=$15)) then Result:=AVar1=AVar2;
  end else if(vt1=varOleStr)or(vt1=varString)or(vt1=varUString)then begin
    if(vt2=varOleStr)or(vt2=varString)or(vt2=varUString)then Result:=CompareText(AVar1,AVar2)=0;
  end else if vt1=varBoolean then begin
    if vt2=varBoolean then Result:=AVar1=AVar2;
  end else if vt1=varEmpty then
    Result:=vt2=varEmpty
  else if vt1=varNull then
    Result:=vt2=varNull;
end;

end.
