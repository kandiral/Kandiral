(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWebCommon                                                               *)
(*  Ver.: 14.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWebCommon;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$IFEND}
  {$IF CompilerVersion >= 28}
    System.NetEncoding;
  {$ELSE}
    HTTPApp;
  {$IFEND}

type
  TKRWebParam = class
  private
    FName, FValue: String;
  public
    constructor Create(AName, AValue: String);
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
  end;

  TKRWebParams = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AName, AValue: String);
    function GetParams: String;
  end;

  function KRHTTPEncode( AStr: String ): String;

implementation

function KRHTTPEncode( AStr: String ): String;
begin
  {$IF CompilerVersion >= 28}
    Result := TNetEncoding.URL.Encode( AStr );
  {$ELSE}
    Result := HTTPEncode( AStr );
  {$IFEND}
end;

{ TKRWebParams }

procedure TKRWebParams.Add(AName, AValue: String);
var
  i, n: integer;
  prm: TKRWebParam;
begin
  n := FList.Count - 1;
  for I := 0 to n do begin
    prm := TKRWebParam(FList[i]);
    if prm.Name = AName then begin
      prm.Value := prm.Value + '+' + KRHTTPEncode( AValue );
      exit;
    end;
  end;
  FList.Add( TKRWebParam.Create( AName, AValue ) );
end;

procedure TKRWebParams.Clear;
var
  i, n: integer;
begin
  n := FList.Count - 1;
  for I := 0 to n do TKRWebParam(FList[i]).Free;
  FList.Clear;
end;

constructor TKRWebParams.Create;
begin
  FList:=TList.Create;
end;

destructor TKRWebParams.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TKRWebParams.GetParams: String;
var
  i, n: integer;
begin
  Result:='';
  n:=FList.Count;
  if n=0 then exit;
  Result := '?' + TKRWebParam( FList[0] ).FName + '=' +  TKRWebParam( FList[0] ).FValue ;
  dec( n );
  for I := 1 to n do
    Result := Result + '&' + TKRWebParam( FList[i] ).FName + '=' + TKRWebParam( FList[i] ).FValue ;
end;

{ TKRWebParam }

constructor TKRWebParam.Create(AName, AValue: String);
begin
  FName := AName;
  FValue := KRHTTPEncode( AValue );
end;

end.
