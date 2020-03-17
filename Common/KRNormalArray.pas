(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRNormalArray                                                             *)
(*  Ver.: 28.01.2014                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRNormalArray;

interface

uses Math;

type
  TKRNormalArray255 = class
  private
    FArray: array[0..254] of Extended;
    FSum: Extended;
    FIndex: byte;
    FLen: byte;
    FCount: byte;
    FAverage: Extended;
    FPrecision: byte;
    procedure SetLen(const Value: byte);
    procedure SetPrecision(const Value: byte);
  public
    constructor Create;
    property Count: byte read FCount;
    property Len: byte read FLen write SetLen;
    property Average: Extended read FAverage;
    procedure Clear;
    procedure Add(AValue: Extended);
    property Precision: byte read FPrecision write SetPrecision;
  end;

implementation

{ TKRNormalArray255 }

procedure TKRNormalArray255.Add(AValue: Extended);
begin
  AValue:=RoundTo(AValue,-FPrecision);
  if FCount=FLen then begin
    if FIndex=FLen then FIndex:=0;
    FSum:=FSum-FArray[FIndex]+AValue;
    FArray[FIndex]:=AValue;
    Inc(FIndex);
  end else begin
    FSum:=FSum+AValue;
    FArray[FIndex]:=AValue;
    Inc(FIndex);
    Inc(FCount);
  end;
  FAverage:=FSum/FCount;
end;

procedure TKRNormalArray255.Clear;
begin
  FCount:=0;
  FSum:=0;
  FIndex:=0;
  FAverage:=0;
end;

constructor TKRNormalArray255.Create;
begin
  FPrecision:=4;
  FLen:=255;
  Clear;
end;

procedure TKRNormalArray255.SetLen(const Value: byte);
begin
  Clear;
  if Value<1 then FLen:=1 else FLen := Value;
end;

procedure TKRNormalArray255.SetPrecision(const Value: byte);
begin
  Clear;
  FPrecision := Value;
end;

end.
