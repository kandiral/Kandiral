(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRNMEA0183                                                                *)
(*  Ver.: 26.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRNMEA0183;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.StrUtils, System.SysUtils, Winapi.MMSystem,
    Vcl.ExtCtrls, Vcl.Forms, System.Variants,
  {$ELSE}
    Windows, Classes, StrUtils, SysUtils, MMSystem, ExtCtrls, Forms, Variants,
  {$IFEND}
  KRRuntimeErrors, KRTypes, KRThread, KRComPort, KRCOMPortSets, Funcs, lgop;

type
  TKRNMEA0183Thread = class;

  TKRNMEA0183 = class(TComponent,IKRCOMPortSets)
  private
    FThread:  TKRNMEA0183Thread;
    FComPort: TKRComPort;
    FGMT: integer;
    FGMTFromPC: boolean;
    FTime: TDateTime;
    FTime_lu: Cardinal;
    FDate: TDateTime;
    FDate_lu: Cardinal;
    FLatitude: Double;
    FLatitude_lu: Cardinal;
    FLatitudeP: boolean;
    FLatitudeP_lu: Cardinal;
    FLongitude: Double;
    FLongitude_lu: Cardinal;
    FLongitudeJ: boolean;
    FLongitudeJ_lu: Cardinal;
    function GetRuntimeErrorEv: TKRRuntimeErrorEv;
    procedure SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetGMT(const Value: integer);
    procedure SetGMTFromPC(const Value: boolean);
    procedure SetTime(ATime: TDateTime);
    procedure SetDate(ADate: TDateTime);
    function GetGMDate: Variant;
    function GetLocalDate: Variant;
    procedure SetLatitude(ALatitude: Double);
    procedure SetLatitudeP(ALatitudeP: boolean);
    procedure SetLongitude(ALongitude: Double);
    procedure SetLongitudeJ(ALongitudeJ: boolean);
    function GetLatitude: Variant;
    function GetLongitude: Variant;
    function GetBaudRate: integer;
    function GetDataBits: integer;
    function GetFlowControl: TKRFlowControl;
    function GetParity: integer;
    function GetPort: String;
    function GetStopBits: integer;
    procedure SetBaudRate(const Value: integer);
    procedure SetDataBits(const Value: integer);
    procedure SetFlowControl(const Value: TKRFlowControl);
    procedure SetParity(const Value: integer);
    procedure SetPort(const Value: String);
    procedure SetStopBits(const Value: integer);
    function getIsData: boolean;
    function getIsVData: boolean;
    function getIsConnected: boolean;
    function GetTalkerIdentifier: String;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property isData: boolean read getIsData;
    property isValidData: boolean read getIsVData;
    property isConnected: boolean read getIsConnected;
    property TalkerIdentifier: String read GetTalkerIdentifier;
    property GMDate: Variant read GetGMDate;
    property LocalDate: Variant read GetLocalDate;
    property Latitude: Variant read GetLatitude;
    property Longitude: Variant read GetLongitude;
  published
    property Active: boolean read GetActive write SetActive;
    property OnRuntimeError: TKRRuntimeErrorEv read GetRuntimeErrorEv write SetRuntimeErrorEv;
    property GMT: integer read FGMT write SetGMT;
    property GMTFromPC: boolean read FGMTFromPC write SetGMTFromPC;
    property Port: String read GetPort write SetPort;
    property Parity: integer read GetParity write SetParity;
    property StopBits: integer read GetStopBits write SetStopBits;
    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property DataBits: integer read GetDataBits write SetDataBits;
    property FlowControl: TKRFlowControl read GetFlowControl write SetFlowControl;
  end;

  TKRNMEA0183Thread = class(TKRThread)
  private
    FNMEA0183: TKRNMEA0183;
    FDATA_T, FVDATA_T: Cardinal;
    FTID: Word;
    FTalkerIdentifier: String;
    FDATA, FVDATA: boolean;
    FConnected: boolean;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FBuf, FBuf_prm: TKRBuffer;
    FBuf_i, FBuf_prm_i, FBuf_prm_len: integer;
    FSentenceIdentifier: Cardinal;
    _crc: byte;
    _crc_stop: boolean;
    _start: boolean;
    procedure RMC;
    procedure GGA;
    function GetPrm: boolean;
    procedure ParsePrm_Time;
    procedure ParsePrm_Date;
    procedure ParsePrm_Latitude;
    procedure ParsePrm_LatitudeP;
    procedure ParsePrm_Longitude;
    procedure ParsePrm_LongitudeJ;
    procedure clearData;
  protected
    procedure KRExecute; override;
    procedure KRExecutePausedFirst; override;
  public
    constructor CreateTh(ANMEA0183: TKRNMEA0183);
  end;

implementation

{ TKRNMEA0183 }

constructor TKRNMEA0183.Create(AOwner: TComponent);
begin
  inherited;
  FComPort:=TKRComPort.Create;
  FComPort.InputBuffer:=32;
  FComPort.OutputBuffer:=32;
  FThread:=TKRNMEA0183Thread.CreateTh(Self);
  FGMTFromPC:=true;
  SetGMT(0);

  FTime_lu:=0;
  FDate_lu:=0;
  FLatitude_lu:=0;
  FLongitude_lu:=0;

end;

destructor TKRNMEA0183.Destroy;
begin
  SetActive(false);
  FreeAndNil(FThread);
  inherited;
end;

function TKRNMEA0183.GetActive: boolean;
begin
  Result:=FThread.Active;
end;

function TKRNMEA0183.GetBaudRate: integer;
begin
  Result:=FComPort.BaudRate;
end;

function TKRNMEA0183.GetDataBits: integer;
begin
  Result:=FComPort.DataBits;
end;

function TKRNMEA0183.GetFlowControl: TKRFlowControl;
begin
  Result:=FComPort.FlowControl;
end;

function TKRNMEA0183.GetGMDate: Variant;
begin
  if(ElapsedTime(FTime_lu)<2000)and(ElapsedTime(FDate_lu)<5000)then
    Result:=FTime+FDate
  else Result:=Null;
end;

function TKRNMEA0183.getIsConnected: boolean;
begin
  result:=FThread.FConnected;
end;

function TKRNMEA0183.getIsData: boolean;
begin
  result:=FThread.FDATA
end;

function TKRNMEA0183.getIsVData: boolean;
begin
  result:=FThread.FVDATA
end;

function TKRNMEA0183.GetLatitude: Variant;
begin
  if(ElapsedTime(FLatitude_lu)<2000)and(ElapsedTime(FLatitudeP_lu)<2000)then begin
    Result:=FLatitude;
    if not FLatitudeP then Result:=-Result;
  end else Result:=Null;
end;

function TKRNMEA0183.GetLocalDate: Variant;
var
  tzInf: TTimeZoneInformation;
begin
  Result:=GetGMDate;
  if not VarIsNull(Result) then begin
    if FGMTFromPC then begin
      GetTimeZoneInformation(tzInf);
      FGMT:=tzInf.Bias;
    end;
    Result:=Result-FGMT/1440;
  end;
end;

function TKRNMEA0183.GetLongitude: Variant;
begin
  if(ElapsedTime(FLongitude_lu)<2000)and(ElapsedTime(FLongitudeJ_lu)<2000)then begin
    Result:=FLongitude;
    if not FLongitudeJ then Result:=-Result;
  end else Result:=Null;
end;

function TKRNMEA0183.GetParity: integer;
begin
  Result:=FComPort.Parity;
end;

function TKRNMEA0183.GetPort: String;
begin
  Result:=FComPort.Port;
end;

function TKRNMEA0183.GetRuntimeErrorEv: TKRRuntimeErrorEv;
begin
  RECS_Enter;
  Result:=FThread.FRuntimeErrorEv;
  RECS_Leave;
end;

function TKRNMEA0183.GetStopBits: integer;
begin
  Result:=FComPort.StopBits;
end;

function TKRNMEA0183.GetTalkerIdentifier: String;
begin
  Result:=FThread.FTalkerIdentifier;
end;

procedure TKRNMEA0183.SetActive(const Value: boolean);
begin
  FThread.Active:=Value;
end;

procedure TKRNMEA0183.SetBaudRate(const Value: integer);
begin
  FComPort.BaudRate:=Value;
end;

procedure TKRNMEA0183.SetDataBits(const Value: integer);
begin
  FComPort.DataBits:=Value;
end;

procedure TKRNMEA0183.SetDate(ADate: TDateTime);
begin
  FDate:=ADate;
  FDate_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

procedure TKRNMEA0183.SetFlowControl(const Value: TKRFlowControl);
begin
  FComPort.FlowControl:=Value;
end;

procedure TKRNMEA0183.SetGMT(const Value: integer);
var
  tzInf: TTimeZoneInformation;
begin
  if not FGMTFromPC then FGMT := Value else begin
    GetTimeZoneInformation(tzInf);
    FGMT:=tzInf.Bias;
  end;
end;

procedure TKRNMEA0183.SetGMTFromPC(const Value: boolean);
begin
  FGMTFromPC := Value;
  SetGMT(FGMT);
end;

procedure TKRNMEA0183.SetLatitude(ALatitude: Double);
begin
  FLatitude:=ALatitude;
  FLatitude_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

procedure TKRNMEA0183.SetLatitudeP(ALatitudeP: boolean);
begin
  FLatitudeP:=ALatitudeP;
  FLatitudeP_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

procedure TKRNMEA0183.SetLongitude(ALongitude: Double);
begin
  FLongitude:=ALongitude;
  FLongitude_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

procedure TKRNMEA0183.SetLongitudeJ(ALongitudeJ: boolean);
begin
  FLongitudeJ:=ALongitudeJ;
  FLongitudeJ_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

procedure TKRNMEA0183.SetParity(const Value: integer);
begin
  FComPort.Parity:=Value;
end;

procedure TKRNMEA0183.SetPort(const Value: String);
begin
  FComPort.Port:=Value;
end;

procedure TKRNMEA0183.SetRuntimeErrorEv(const Value: TKRRuntimeErrorEv);
begin
  RECS_Enter;
  FThread.FRuntimeErrorEv:=Value;
  RECS_Leave;
end;

procedure TKRNMEA0183.SetStopBits(const Value: integer);
begin
  FComPort.StopBits:=Value;
end;

procedure TKRNMEA0183.SetTime(ATime: TDateTime);
begin
  FTime:=ATime;
  FTime_lu:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
end;

{ TKRNMEA0183Thread }

procedure TKRNMEA0183Thread.clearData;
begin
  FDATA:=false;
  FVDATA:=false;
  FTID:=0;
  FTalkerIdentifier:='';
  FBuf_i:=0;
  _crc:=0;
  _crc_stop:=false;
end;

constructor TKRNMEA0183Thread.CreateTh(ANMEA0183: TKRNMEA0183);
begin
  FNMEA0183:=ANMEA0183;
  _start:=true;
  inherited Create;
end;

function TKRNMEA0183Thread.GetPrm: boolean;
begin
  FBuf_prm_len:=0;
  while(FBuf_prm_i-1<FBuf_i)and(FBuf[FBuf_prm_i]<>$2C)and
    (FBuf[FBuf_prm_i]<>$2A)and(FBuf[FBuf_prm_i]<>$D)do begin
    FBuf_prm[FBuf_prm_len]:=FBuf[FBuf_prm_i];
    Inc(FBuf_prm_len);
    Inc(FBuf_prm_i);
  end;
  Result:=FBuf[FBuf_prm_i]=$2C;
  Inc(FBuf_prm_i);
end;

procedure TKRNMEA0183Thread.GGA;
var
  n: integer;
  b: boolean;
begin
  FBuf_prm_i:=7;
  n:=0;
  repeat
    b:=GetPrm;
    inc(n);
    case n of
    1: ParsePrm_Time;
    2: ParsePrm_Latitude;
    3: ParsePrm_LatitudeP;
    4: ParsePrm_Longitude;
    5: ParsePrm_LongitudeJ;
    end;
  until not b;
end;

procedure TKRNMEA0183Thread.KRExecute;
var
  _Buf: TKRBuffer;
  i,btRec: integer;
  wd: Word;
begin
  if _start then begin
    clearData;
    _start:=false;
  end;

  try
    if not FNMEA0183.FComPort.Connected then begin
      FConnected:=false;
      FNMEA0183.FComPort.Open;
      if FNMEA0183.FComPort.Connected then FNMEA0183.FComPort.Read(_Buf,32);
    end;

    if FNMEA0183.FComPort.Connected then begin

      FConnected:=true;

      btRec:=FNMEA0183.FComPort.Read(_Buf,32);
      if (btRec>0) then begin
        FDATA:=true;
        FDATA_T:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
      end;

      for i := 0 to btRec-1 do begin

        if _Buf[i]=0 then break;
        if FBuf_i=1 then begin
          _crc:=0;
          _crc_stop:=false;
        end;
        FBuf[FBuf_i]:=_Buf[i];
        if FBuf[FBuf_i]=$2A then _crc_stop:=true;
        if not _crc_stop then _crc:=_crc xor FBuf[FBuf_i];
        if(FBuf_i>0)and(FBuf[FBuf_i-1]=$D)and(FBuf[FBuf_i]=$A)then begin
          if(_crc_stop and (HexToInt(Chr(FBuf[FBuf_i-3])+Chr(FBuf[FBuf_i-2]))=_crc))then begin
            if(FBuf_i>5)and(FBuf[0]=$24)and(FBuf[6]=$2C)then begin
              FVDATA:=true;
              FVDATA_T:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
              wd:=BytesToWord(FBuf[1],FBuf[2]);
              if wd<>FTID then begin
                FTID:=wd;
                FTalkerIdentifier:=Char(FBuf[1])+Char(FBuf[2]);
              end;
              FSentenceIdentifier:=WordsToDWORD(BytesToWord(FBuf[5],FBuf[4]),BytesToWord(FBuf[3],0));
              case FSentenceIdentifier of
              $524D43: RMC;
              $474741: GGA;
              end;
            end;
          end;
          FBuf_i:=0;
        end else inc(FBuf_i);
      end;
      if(FDATA)and(ElapsedTime(FDATA_T)>2000)then FDATA:=false;
      if(FVDATA)and(ElapsedTime(FVDATA_T)>2000)then FVDATA:=false;


    end;
  except on E: Exception do
    REEvent(FRuntimeErrorEv,FNMEA0183,'TKRNMEA0183Thread[Name="'+
      FNMEA0183.Name+'"] procedure KRExecute;',E);
  end;
end;

procedure TKRNMEA0183Thread.KRExecutePausedFirst;
begin
  try
    if FNMEA0183.FComPort.Connected then FNMEA0183.FComPort.Close;
  except end;
  _start:=true;
  FConnected:=false;
end;

procedure TKRNMEA0183Thread.ParsePrm_Date;
var
  mm,dd,yy: word;
begin
  mm:=0;
  dd:=0;
  yy:=0;
  if FBuf_prm_len=6 then begin
    dd:=(FBuf_prm[0]-48)*10+FBuf_prm[1]-48;
    mm:=(FBuf_prm[2]-48)*10+FBuf_prm[3]-48;
    yy:=2000+(FBuf_prm[4]-48)*10+FBuf_prm[5]-48;
  end else if FBuf_prm_len=8 then begin
    dd:=(FBuf_prm[0]-48)*10+FBuf_prm[1]-48;
    mm:=(FBuf_prm[2]-48)*10+FBuf_prm[3]-48;
    yy:=(((FBuf_prm[4]-48)*10+FBuf_prm[5]-48)*10+FBuf_prm[6]-48)*10+FBuf_prm[7]-48;
  end;
  if(mm>0)and(mm<13)and(dd>0)and(dd<32)and(yy>2000)then FNMEA0183.SetDate(EncodeDate(yy,mm,dd));

end;

procedure TKRNMEA0183Thread.ParsePrm_Latitude;
var
  FLatitude: Double;
  l,j: integer;
begin
  if FBuf_prm_len>3 then begin
    FLatitude:=(FBuf_prm[2]-48)*10+FBuf_prm[3]-48;
    if(FBuf_prm_len>5)and(FBuf_prm[4]=$2E)then begin
      l:=1;
      for j := 5 to FBuf_prm_len-1 do begin
        l:=l*10;
        FLatitude:=FLatitude+(FBuf_prm[j]-48)/l;
      end;
    end;
    FLatitude:=FLatitude/60+(FBuf_prm[0]-48)*10+FBuf_prm[1]-48;
    FNMEA0183.SetLatitude(FLatitude);
  end;
end;

procedure TKRNMEA0183Thread.ParsePrm_LatitudeP;
begin
  if FBuf_prm_len=1 then begin
    FNMEA0183.SetLatitudeP(FBuf_prm[0]<>83)
  end;
end;

procedure TKRNMEA0183Thread.ParsePrm_Longitude;
var
  FLongitude: Double;
  l,j: integer;
begin
  if FBuf_prm_len>4 then begin
    FLongitude:=(FBuf_prm[3]-48)*10+FBuf_prm[4]-48;
    if(FBuf_prm_len>6)and(FBuf_prm[5]=$2E)then begin
      l:=1;
      for j := 6 to FBuf_prm_len-1 do begin
        l:=l*10;
        FLongitude:=FLongitude+(FBuf_prm[j]-48)/l;
      end;
    end;
    FLongitude:=FLongitude/60+((FBuf_prm[0]-48)*10+FBuf_prm[1]-48)*10+FBuf_prm[2]-48;
    FNMEA0183.SetLongitude(FLongitude);
  end;
end;

procedure TKRNMEA0183Thread.ParsePrm_LongitudeJ;
begin
  if FBuf_prm_len=1 then begin
    FNMEA0183.SetLongitudeJ(FBuf_prm[0]<>87)
  end;
end;

procedure TKRNMEA0183Thread.ParsePrm_Time;
var
  l,j: integer;
  ms: word;
  dl:double;
begin
  if FBuf_prm_len>5 then begin
    ms:=0;
    if(FBuf_prm_len>7)and(FBuf_prm[6]=$2E)then begin
      l:=1;
      dl:=0;
      for j := 7 to FBuf_prm_len-1 do begin
        l:=l*10;
        dl:=dl+(FBuf_prm[j]-48)/l;
      end;
      ms:=Round(dl*1000);
    end;
    FNMEA0183.SetTime(EncodeTime(
      (FBuf_prm[0]-48)*10+FBuf_prm[1]-48,
      (FBuf_prm[2]-48)*10+FBuf_prm[3]-48,
      (FBuf_prm[4]-48)*10+FBuf_prm[5]-48,ms));
  end;
end;

procedure TKRNMEA0183Thread.RMC;
var
  n: integer;
  b: boolean;
begin
  FBuf_prm_i:=7;
  n:=0;
  repeat
    b:=GetPrm;
    inc(n);
    case n of
    1: ParsePrm_Time;
    3: ParsePrm_Latitude;
    4: ParsePrm_LatitudeP;
    5: ParsePrm_Longitude;
    6: ParsePrm_LongitudeJ;
    9: ParsePrm_Date;
    end;
  until not b;
end;

end.
