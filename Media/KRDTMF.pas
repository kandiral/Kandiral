(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRDTMF                                                                    *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRDTMF;

interface

uses lgop;//, Windows, KRRuntimeErrors, SysUtils;

const
  COEFF_NUMBER = 18;

type
  TArrayOfSmallInt = array of smallint;

  TKRDTMF = class
  private
    T: array[0..COEFF_NUMBER-1] of integer;
    //internalArray: TArrayOfSmallInt;
    //pArraySamples: TArrayOfSmallInt;
    //frame_count: integer;
    function Detection(smpl: PSmallInt; ALength: integer): byte;
  public
    Row, Column: integer;
    fr: boolean;
    constructor Create;
    procedure Generating(AButton: byte; var Buf; ALength: integer);
    function Detecting(frameSize: integer; var Buf; ALength: integer):byte;
  end;

function MPY48SR(o16: smallint; o32: integer): integer;
procedure frequencyOscillator(Coeff0, Coeff1: smallint;
                                 var Buf; COUNT: cardinal;
                                 var y1_0: integer; var y1_1: integer;
                                 var y2_0: integer; var y2_1: integer);

var frcd: int64;
maxTMD: integer;

implementation

const
  tempCoeff : array[0..11] of smallint = (
    //Low frequencies (row)
    27980, // 697Hz     cos( 2пи f / fs ) * 2^15 ; f=697hz ; fs=8000hz
    26956, // 770Hz
    25701, // 852Hz
    24218, // 941Hz
    //High frequencies (column)
    19073, // 1209Hz
    16325, // 1335Hz
    13085, // 1477Hz
    9315,  // 1633Hz

    30467, // 480Hz
    28959, // 620Hz
    31538, // 350Hz
    30831  // 440Hz
  );

  CONSTANTS : array[0..COEFF_NUMBER-1] of smallint = (
//    27860, 26745, 25529, 24216, 19747, 16384, 12773, 8967,
//    27980, 26956, 25701, 24218, 19073, 16325, 13085, 9315,
//    30467, 30831, 28959, 31538, 21319, 29769, -1009, -12772, -22811, -30555);
      31538, 30467, 27980, 26956, 25701, 24218,
      30890, 28900, 19073, 16325, 13085, 9315,
      32550, 22000, -8000, -15000, -22000, -30500);

  //SAMPLES = 160;
  powerThreshold = 1100;//1500;

  dialTonesToOhersDialTones = 5;//6;
  dialTonesToOhersTones = 14;//16;

function MPY48SR(o16: smallint; o32: integer): integer;
var
  Temp0, Temp1: integer;
begin
  Temp0 := mshr((word(o32) * o16) + $4000,15);
  Temp1 := smallint(mshr(o32,16)) * o16;
  result := (Temp1 shl 1) + Temp0;
end;

procedure frequencyOscillator(Coeff0, Coeff1: smallint;
                                 var Buf; COUNT: cardinal;
                                 var y1_0: integer; var y1_1: integer;
                                 var y2_0: integer; var y2_1: integer);
var
  i,Subject,Temp0,Temp1: integer;
  B: PByte;
begin
    B:=PByte(@Buf);
    Subject := Coeff0 * Coeff1;
    for i := 0 to COUNT-1 do begin
        Temp0 := MPY48SR(Coeff0, y1_0 shl 1) - y2_0;
        Temp1 := MPY48SR(Coeff1, y1_1 shl 1) - y2_1;
        y2_0 := y1_0;
        y2_1 := y1_1;
        y1_0 := Temp0;
        y1_1 := Temp1;
        Temp0 := Temp0+Temp1;
        if(Subject<>0)then Temp0:=mshr(Temp0,1);
        B[i*2+1] := mshr(Temp0,8) and $FF;
        B[i*2] := Temp0 and $FF;
    end;
end;

procedure goertzelFilter(Koeff0, Koeff1: SmallInt; arraySamples: TArrayOfSmallInt; var Magnitude0: integer;
  var Magnitude1: integer; COUNT: Cardinal);
var
  i, Temp0, Temp1: integer;
  Vk1_0, Vk2_0, Vk1_1, Vk2_1: integer;
begin
  Vk1_0:=0; Vk2_0:=0; Vk1_1:=0; Vk2_1:=0;
  for i:=0 to COUNT-1 do begin
    Temp0 := mpy48sr(Koeff0, Vk1_0 shl 1) - Vk2_0 + arraySamples[i];
    Temp1 := mpy48sr(Koeff1, Vk1_1 shl 1) - Vk2_1 + arraySamples[i];

    Vk2_0 := Vk1_0;
    Vk2_1 := Vk1_1;
    Vk1_0 := Temp0;
    Vk1_1 := Temp1;
  end;

  Vk1_0 := mshr(Vk1_0,10);
  Vk1_1 := mshr(Vk1_1,10);
  Vk2_0 := mshr(Vk2_0,10);
  Vk2_1 := mshr(Vk2_1,10);
  Temp0 := mpy48sr(Koeff0, Vk1_0 shl 1);
  Temp1 := mpy48sr(Koeff1, Vk1_1 shl 1);
  Temp0 := smallint(Temp0) * smallint(Vk2_0);
  Temp1 := smallint(Temp1) * smallint(Vk2_1);
  Temp0 := smallint(Vk1_0) * smallint(Vk1_0) + smallint(Vk2_0) * smallint(Vk2_0) - Temp0;
  Temp1 := smallint(Vk1_1) * smallint(Vk1_1) + smallint(Vk2_1) * smallint(Vk2_1) - Temp1;
  Magnitude0 := Temp0;
  Magnitude1 := Temp1;
end;

function norm_l(L_var1: integer):smallint;
var
  var_out: smallint;
begin
  if (L_var1 = 0)then var_out := 0
  else if (L_var1 = Integer($ffffffff))then var_out := 31
  else begin
    if (L_var1 < 0) then L_var1 := not L_var1;
    var_out := 0;
    while L_var1 < integer($40000000) do begin
      L_var1 := L_var1 shl 1;
      Inc(var_out);
    end;
  end;
  result:=var_out;
end;

{ TKRDTMF }

constructor TKRDTMF.Create;
begin
  //frame_count:=0;
//  SetLength(internalArray,SAMPLES);
end;

function TKRDTMF.Detecting(frameSize: integer; var Buf; ALength: integer): byte;
//var
  {i, temp_index, n: integer;
  wd: word;
  B: PByte;
  p: pointer;
  tempArray: TArrayOfSmallint;}

//  tm0,tm: int64;

begin
//QueryPerformanceCounter(tm0);

        result := Detection(@Buf, ALength div 2);


  {Result:=255;
  B:=PByte(@Buf);
  p:=@wd;
  SetLength(pArraySamples,frame_count+frameSize);
  for i:=0 to frameSize-1 do begin
    wd:=BytesToWord(B[i*2],B[i*2+1]);
    pArraySamples[i + frame_count] := Smallint(p^);
  end;
  frame_count := frame_count+frameSize;
  temp_index := 0;
  if(frame_count >= SAMPLES)then begin
    while(frame_count >= SAMPLES)do begin
      if(temp_index = 0) then begin
        result := Detection(pArraySamples);
      end else begin
        n:=Length(pArraySamples) - temp_index;
        SetLength(tempArray,n);
        for I := 0 to n-1 do
          tempArray[i] := pArraySamples[temp_index + i];
        result:= Detection(tempArray);
      end;
      temp_index := temp_index+SAMPLES;
      frame_count := frame_count - SAMPLES;
    end;
    for i:=0 to frame_count-1 do
      pArraySamples[i] := pArraySamples[i + temp_index];
  end;}


{QueryPerformanceCounter(tm);
tm:=((tm-tm0)*1000000) div frcd;
if tm>maxTMD then maxTMD:=tm;}
//REAddLog(IntToStr(tm));

end;

function TKRDTMF.Detection(smpl: PSmallInt; ALength: integer): byte;
var
  Dial, Sum, i, temp: integer;
  sm: smallint;
  internalArray: TArrayOfSmallInt;
//  s: String;
  b: boolean;
begin
  fr:=false;
  Dial:=32;
  Result:=255;
  Sum := 0;
  b:=true;
  for i := 0 to ALength-1 do begin
    sm:=PSmallInt(integer(smpl)+i*2)^;
    b:=b and (sm=0);
    if(sm <> 0)then begin
      if(Dial > norm_l(sm))then Dial := norm_l(sm);
      if(sm > 0)then Sum := Sum + sm
      else Sum := Sum - sm;
    end;
  end;
  if b then Result:=254;

//s:='Main='+IntToStr(Sum div ALength)+'; ';
  if(Sum div ALength < powerThreshold)then exit;
  Dial := Dial - 16;
  SetLength(internalArray,ALength);
  for i := 0 to ALength-1 do begin
    sm:=PSmallInt(integer(smpl)+i*2)^;
    internalArray[i] := SmallInt(sm shl Dial);
  end;

  goertzelFilter(CONSTANTS[0], CONSTANTS[1], internalArray, T[0], T[1], ALength);
  goertzelFilter(CONSTANTS[2], CONSTANTS[3], internalArray, T[2], T[3], ALength);
  goertzelFilter(CONSTANTS[4], CONSTANTS[5], internalArray, T[4], T[5], ALength);
  goertzelFilter(CONSTANTS[6], CONSTANTS[7], internalArray, T[6], T[7], ALength);
  goertzelFilter(CONSTANTS[8], CONSTANTS[9], internalArray, T[8], T[9], ALength);
  goertzelFilter(CONSTANTS[10], CONSTANTS[11], internalArray, T[10], T[11], ALength);
  goertzelFilter(CONSTANTS[12], CONSTANTS[13], internalArray, T[12], T[13], ALength);
  goertzelFilter(CONSTANTS[14], CONSTANTS[15], internalArray, T[14], T[15], ALength);
  goertzelFilter(CONSTANTS[16], CONSTANTS[17], internalArray, T[16], T[17], ALength);

  {s:=s+'T: '+
    IntToStr(T[0])+' '+IntToStr(T[1])+' '+
    IntToStr(T[2])+' '+IntToStr(T[3])+' '+
    IntToStr(T[4])+' '+IntToStr(T[5])+' '+
    IntToStr(T[6])+' '+IntToStr(T[7])+' '+
    IntToStr(T[8])+' '+IntToStr(T[9])+' '+
    IntToStr(T[10])+' '+IntToStr(T[11])+' '+
    IntToStr(T[12])+' '+IntToStr(T[13])+' '+
    IntToStr(T[14])+' '+IntToStr(T[15])+' '+
    IntToStr(T[16])+' '+IntToStr(T[17]);}

  Row := 0;
  Temp := 0;
  for i := 0 to 5 do begin
    if(Temp < T[i])then begin
      Row := i;
      Temp := T[i];
    end;
  end;
  Column := 6;
  Temp := 0;
  for i := 6 to 11 do begin
    if(Temp < T[i])then begin
      Column := i;
      Temp := T[i];
    end;
  end;
  Sum:=0;
  for i := 0 to 12 do Sum := Sum + T[i];
  Sum := Sum - T[Row];
  Sum := Sum - T[Column];
  Sum := mshr(Sum,3);
//REAddLog('row='+intToStr(row)+'; col='+intToStr(column));
  fr:=true;
//REAddLog('row/sum='+intToStr(T[Row] div Sum)+'; col/sum='+intToStr(T[Row] div Sum)+'; '+s);
  if(Sum = 0)then Sum := 1;
  if(T[Row] div Sum < dialTonesToOhersDialTones)then exit;
  if(T[Column] div Sum < dialTonesToOhersDialTones)then exit;
  if(row>1)and(T[Row] < mshr(T[Column], 2))then exit;
  if(T[Column] < (mshr(T[Row], 1) - mshr(T[Row], 3)))then exit;
  for i := 0 to COEFF_NUMBER-1 do
    if(T[i] = 0)then T[i] := 1;
  for i := 13 to COEFF_NUMBER-1 do begin
    if(T[Row] div T[i] < dialTonesToOhersTones)then exit;
    if(T[Column] div T[i] < dialTonesToOhersTones)then exit;
  end;
  for i := 0 to 12 do begin
    if(T[i] <> T[Column])then begin
      if(T[i] <> T[Row])then begin
        if Row>1 then
          if(T[Row] div T[i] < dialTonesToOhersDialTones)then exit
        else
          if(T[Row] div T[i] < dialTonesToOhersDialTones div 3)then exit;
        if(Column > 8)then begin
          if(T[Column] div T[i] < dialTonesToOhersDialTones)then exit;
        end else
          if(T[Column] div T[i] < (dialTonesToOhersDialTones div 3))then exit;
      end;
    end;
  end;
  case(Row)of
    0: if Column=6 then result:=2;
    1: if Column=7 then result:=1
       else if Column=6 then result:=3;
    2: case(Column)of
        8: result:=49;
        9: result:=50;
        10: result:=51;
        11: result:=65;
       end;
    3: case(Column)of
        8: result:=52;
        9: result:=53;
        10: result:=54;
        11: result:=66;
       end;
    4: case(Column)of
        8: result:=55;
        9: result:=56;
        10: result:=57;
        11: result:=67;
       end;
    5: case(Column)of
        8: result:=42;
        9: result:=48;
        10: result:=35;
        11: result:=68;
       end;
  end;
//REAddLog('DTMF='+intToStr(result));
end;

procedure TKRDTMF.Generating(AButton: byte; var Buf; ALength: integer);
var
  y1_1, y1_2, y2_1, y2_2: integer;
  tempCoeff1, tempCoeff2: smallint;
begin
  case AButton of
    49:begin // '1'
      tempCoeff1 := tempCoeff[0];
      tempCoeff2 := tempCoeff[4];
      y1_1 := tempCoeff[0];
      y2_1 := 31000;
      y1_2 := tempCoeff[4];
      y2_2 := 31000;
    end;
    50:begin // '2'
      tempCoeff1 := tempCoeff[0];
      tempCoeff2 := tempCoeff[5];
      y1_1 := tempCoeff[0];
      y2_1 := 31000;
      y1_2 := tempCoeff[5];
      y2_2 := 31000;
    end;
    51:begin // '3'
       tempCoeff1:=tempCoeff[0];
       tempCoeff2:=tempCoeff[6];
       y1_1:=tempCoeff[0];
       y2_1:=31000;
       y1_2:=tempCoeff[6];
       y2_2:=31000;
    end;
    65:begin // 'A'
       tempCoeff1:=tempCoeff[0];
       tempCoeff2:=tempCoeff[7];
       y1_1:=tempCoeff[0];
       y2_1:=31000;
       y1_2:=tempCoeff[7];
       y2_2:=31000;
    end;
    52:begin // '4'
       tempCoeff1:=tempCoeff[1];
       tempCoeff2:=tempCoeff[4];
       y1_1:=tempCoeff[1];
       y2_1:=31000;
       y1_2:=tempCoeff[4];
       y2_2:=31000;
    end;
    53:begin // '5'
       tempCoeff1:=tempCoeff[1];
       tempCoeff2:=tempCoeff[5];
       y1_1:=tempCoeff[1];
       y2_1:=31000;
       y1_2:=tempCoeff[5];
       y2_2:=31000;
    end;
    54:begin // '6'
       tempCoeff1:=tempCoeff[1];
       tempCoeff2:=tempCoeff[6];
       y1_1:=tempCoeff[1];
       y2_1:=31000;
       y1_2:=tempCoeff[6];
       y2_2:=31000;
    end;
    66:begin // 'B'
       tempCoeff1:=tempCoeff[1];
       tempCoeff2:=tempCoeff[7];
       y1_1:=tempCoeff[1];
       y2_1:=31000;
       y1_2:=tempCoeff[7];
       y2_2:=31000;
    end;
    55:begin // '7'
       tempCoeff1:=tempCoeff[2];
       tempCoeff2:=tempCoeff[4];
       y1_1:=tempCoeff[2];
       y2_1:=31000;
       y1_2:=tempCoeff[4];
       y2_2:=31000;
    end;
    56:begin // '8'
       tempCoeff1:=tempCoeff[2];
       tempCoeff2:=tempCoeff[5];
       y1_1:=tempCoeff[2];
       y2_1:=31000;
       y1_2:=tempCoeff[5];
       y2_2:=31000;
    end;
    57:begin // '9'
       tempCoeff1:=tempCoeff[2];
       tempCoeff2:=tempCoeff[6];
       y1_1:=tempCoeff[2];
       y2_1:=31000;
       y1_2:=tempCoeff[6];
       y2_2:=31000;
    end;
    67:begin // 'C'
       tempCoeff1:=tempCoeff[2];
       tempCoeff2:=tempCoeff[7];
       y1_1:=tempCoeff[2];
       y2_1:=31000;
       y1_2:=tempCoeff[7];
       y2_2:=31000;
    end;
    42:begin // '*'
       tempCoeff1:=tempCoeff[3];
       tempCoeff2:=tempCoeff[4];
       y1_1:=tempCoeff[3];
       y2_1:=31000;
       y1_2:=tempCoeff[4];
       y2_2:=31000;
    end;
    48:begin // '0'
       tempCoeff1:=tempCoeff[3];
       tempCoeff2:=tempCoeff[5];
       y1_1:=tempCoeff[3];
       y2_1:=31000;
       y1_2:=tempCoeff[5];
       y2_2:=31000;
    end;
    35:begin // '#'
       tempCoeff1:=tempCoeff[3];
       tempCoeff2:=tempCoeff[6];
       y1_1:=tempCoeff[3];
       y2_1:=31000;
       y1_2:=tempCoeff[6];
       y2_2:=31000;
    end;
    68:begin // 'D':
       tempCoeff1:=tempCoeff[3];
       tempCoeff2:=tempCoeff[7];
       y1_1:=tempCoeff[3];
       y2_1:=31000;
       y1_2:=tempCoeff[7];
       y2_2:=31000;
    end ;
    1: begin
       tempCoeff1:=tempCoeff[8];
       tempCoeff2:=tempCoeff[9];
       y1_1:=tempCoeff[8];
       y2_1:=31000;
       y1_2:=tempCoeff[9];
       y2_2:=31000;
    end;
    2: begin
       tempCoeff1:=tempCoeff[10];
       tempCoeff2:=tempCoeff[11];
       y1_1:=tempCoeff[10];
       y2_1:=31000;
       y1_2:=tempCoeff[11];
       y2_2:=31000;
    end
    else begin
       tempCoeff1:=0;
       tempCoeff2:=0;
       y1_1:=0;
       y2_1:=0;
       y1_2:=0;
       y2_2:=0;
    end;
  end;
  frequencyOscillator(tempCoeff1, tempCoeff2,
                                 Buf, ALength,
                                 y1_1, y1_2,
                                 y2_1, y2_2
                                );
end;

end.
