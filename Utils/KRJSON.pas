(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRJSON                                                                    *)
(*  Ver.: 30.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRJSON;

interface

uses
  {$IF CompilerVersion >= 23}
    System.SysUtils, System.Math;
  {$ELSE}
    SysUtils, Math;
  {$IFEND}

const
  KRJSON_STRING_MEMPAGE = 32;
  KRJSON_LIST_MEMPAGE = 32;

type
  TKRJSONType = (jstNone, jstNull, jstBool, jstInt, jstFloat,
    jstString, jstArray, jstObject);

  TKRJSONArray = class;

  TKRJSON = class
  protected
    FKey: String;
    FParent: TKRJSONArray;
    function getValueType: TKRJSONType; virtual;
  public
    destructor Destroy; override;
    class function parse( AData: PByte; ALength: integer ): TKRJSON;
    property ValueType: TKRJSONType read getValueType;
    property Key: String read FKey write FKey;
    function toString: AnsiString; virtual;
  end;

  TKRJSONNull = class( TKRJSON )
  protected
    function getValueType: TKRJSONType; override;
  public
    function toString: AnsiString; override;
  end;

  TKRJSONBool = class( TKRJSON )
  private
    FValue: boolean;
  protected
    function getValueType: TKRJSONType; override;
  public
    constructor Create;overload;
    constructor Create(AValue: boolean);overload;
    constructor Create(AKey: String; AValue: boolean);overload;
    property Value: boolean read FValue write FValue;
    function toString: AnsiString; override;
  end;

  TKRJSONString = class( TKRJSON )
  private
    FValue: String;
  protected
    function getValueType: TKRJSONType; override;
  public
    constructor Create;overload;
    constructor Create(AValue: String);overload;
    constructor Create(AKey, AValue: String);overload;
    property Value: String read FValue write FValue;
    function toString: AnsiString; override;
  end;

  TKRJSONInt = class( TKRJSON )
  private
    FValue: int64;
  protected
    function getValueType: TKRJSONType; override;
  public
    constructor Create;overload;
    constructor Create(AValue: int64);overload;
    constructor Create(AKey: String; AValue: int64);overload;
    property Value: int64 read FValue write FValue;
    function toString: AnsiString; override;
  end;

  TKRJSONFloat = class( TKRJSON )
  private
    fs: TFormatSettings;
    FValue: double;
  protected
    function getValueType: TKRJSONType; override;
  public
    constructor Create;overload;
    constructor Create(AValue: double);overload;
    constructor Create(AKey: String; AValue: double);overload;
    property Value: double read FValue write FValue;
    function toString: AnsiString; override;
  end;

  TKRJSONArray = class( TKRJSON )
  private
    function GetChild(Index: integer): TKRJSON;
    function GetFloat(Index: integer): TKRJSONFloat;
    function GetInteger(Index: integer): TKRJSONInt;
    function GetString(Index: integer): TKRJSONString;
    function GetBool(Index: integer): TKRJSONBool;
  protected
    FListLen, FCount: integer;
    FChildren: array of TKRJSON;
    function getValueType: TKRJSONType; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild( js: TKRJSON );
    procedure RemoveChild( AChild: TKRJSON ); overload;
    procedure RemoveChild( AIndex: integer ); overload;
    function toString: AnsiString; override;
    property Count: Integer read FCount;
    property Child[ Index: integer ]: TKRJSON read GetChild; default;
    property Strings[ Index: integer ]: TKRJSONString read GetString;
    property Integers[ Index: integer ]: TKRJSONInt read GetInteger;
    property Floats[ Index: integer ]: TKRJSONFloat read GetFloat;
    property Bools[ Index: integer ]: TKRJSONBool read GetBool;
  end;

  TKRJSONObject = class( TKRJSONArray )
  protected
    function getValueType: TKRJSONType; override;
  public
    function toString: AnsiString; override;
    function IndexOf( AKey: String): integer;
    function ArrayOf( AKey: String; out Index: integer): boolean;
    function StringOf( AKey: String; out Index: integer): boolean;
    function IntegerOf( AKey: String; out Index: integer): boolean;
    function FloatOf( AKey: String; out Index: integer): boolean;
    function BooleanOf( AKey: String; out Index: integer): boolean;
    function GetStringDef( AKey: String; ADef: String ): String;
    function GetIntegerDef( AKey: String; ADef: Int64 ): Int64;
    function GetFloatDef( AKey: String; ADef: Double ): Double;
    function GetBooleanDef( AKey: String; ADef: boolean ): boolean;
  end;

implementation

{ TKRJSON }

destructor TKRJSON.Destroy;
begin
  if FParent<>nil then FParent.RemoveChild( Self );
  inherited;
end;

function TKRJSON.getValueType: TKRJSONType;
begin
  Result := jstNone;
end;

class function TKRJSON.parse(AData: PByte; ALength: integer): TKRJSON;
var
  n: integer;
  pb: PByte;
  str: String;
  jsObject: TKRJSONObject;
  jsArray: TKRJSONArray;

  procedure _inc;
  begin
    inc(pb);
    inc(n);
  end;

  procedure skip_spc;
  begin
    while ( n<ALength ) and ( pb^ < 33) do _inc
  end;

  function parseString: boolean;
  var
    i,l,wd: integer;
    ch: Char;

    procedure addCh;
    begin
      inc( i );
      str[ i ] := ch;
      if i = l then begin
        inc( l, KRJSON_STRING_MEMPAGE );
        SetLength(str, l);
      end;
    end;

    function HexToDec(bt: Byte): integer; inline;
    begin
      if bt>57 then Result:=bt-55 else Result:=bt-48;
    end;

  begin
    Result := false;
    _inc;
    if n=ALength then exit;
    l:=KRJSON_STRING_MEMPAGE;i:=0;
    SetLength(str, l);
    while ( n<ALength ) and ( pb^ <> Ord('"') )  do begin
      if pb^ = Ord( '\' ) then begin
        _inc;
        if n=ALength then exit;
        case pb^ of
          Ord('"'): ch := '"';
          Ord('\'): Ch := '\';
          Ord('/'): Ch := '/';
          Ord('b'): Ch := #$8;
          Ord('f'): Ch := #$c;
          Ord('n'): Ch := #$a;
          Ord('r'): Ch := #$d;
          Ord('t'): Ch := #$9;
          Ord('u'): begin
            _inc;
            if (n + 4) > ALength then exit;
            wd := HexToDec( pb^ );
            _inc;
            wd := ( wd shl 4 ) or HexToDec( pb^ );
            _inc;
            wd := ( wd shl 4 ) or HexToDec( pb^ );
            _inc;
            wd := ( wd shl 4 ) or HexToDec( pb^ );
            ch := WideChar( wd );
          end else exit;
        end;
        addCh;
        _inc;
        continue;
      end;
      if ( pb^ and $80 ) = 0 then begin
        ch := WideChar( pb^ and $7F );
        addCh;
        _inc;
        continue;
      end;
      if ( pb^ and $E0 ) = $C0 then begin
        wd := pb^ and $1F;
        _inc;
        if n=ALength then exit;
        wd := ( wd shl 6 ) or ( pb^ and $3F );
        ch := WideChar( wd );
        addCh;
        _inc;
        continue;
      end;
      if ( pb^ and $F0 ) = $E0 then begin
        if (n + 3) > ALength then exit;
        wd := pb^ and $F;
        _inc;
        wd := ( wd shl 6 ) or ( pb^ and $3F );
        _inc;
        wd := ( wd shl 6 ) or ( pb^ and $3F );
        ch := WideChar( wd );
        addCh;
        _inc;
        continue;
      end;
      if (n + 4) > ALength then exit;
      wd := pb^ and $7;
      _inc;
      wd := ( wd shl 6 ) or ( pb^ and $3F );
      _inc;
      wd := ( wd shl 6 ) or ( pb^ and $3F );
      _inc;
      wd := ( wd shl 6 ) or ( pb^ and $3F );
      ch := WideChar( wd );
      addCh;
      _inc;
    end;
    if pb^ <> Ord('"') then exit;
    SetLength(str, i);
    _inc;
    Result:=true;
  end;

  function parseFloat( vl: int64; var js: TKRJSON ): boolean;
  var
    i,m: integer;
    f,f1: Double;
    z: boolean;
  begin
    Result := false;
    m := 10;
    i := pb^ - 48;
    _inc;
    while ( n<ALength ) and ( pb^ > 47 ) and ( pb^ < 58 ) do begin
      i := i * 10 - 48 + pb^;
      m := m * 10;
      _inc;
    end;
    if n=ALength then exit;
    f := i / m + vl;
    if ( pb^ = Ord('E') ) or ( pb^ = Ord('e') ) then begin
      _inc;
      if n=ALength then exit;
      z:=false;
      if pb^ = Ord('+') then begin
        _inc;
        if n=ALength then exit;
      end else if pb^ = Ord('-') then begin
        z:=true;
        _inc;
        if n=ALength then exit;
      end;
      i:=0;
      while ( n<ALength ) and ( pb^ > 47 ) and ( pb^ < 58 ) do begin
        i := i * 10 - 48 + pb^;
        _inc;
      end;
      if i=0 then exit;
      if z then i:=-i;
      try
        f1:=i;
        f1:=Power( 10, f1 );
        f:=f * f1;
      except
        exit;
      end;
    end;
    js := TKRJSONFloat.Create( f );
    Result := true;
  end;

  function parseNum( var js: TKRJSON ): boolean;
  var
    i: int64;
  begin
    Result := false;
    i := pb^ - 48;
    _inc;
    while ( n<ALength ) and ( pb^ > 47 ) and ( pb^ < 58 ) do begin
      i := i * 10 - 48 + pb^;
      _inc;
    end;
    if n=ALength then exit;
    if pb^ = Ord('.') then begin
      _inc;
      if n=ALength then exit;
      if ( pb^ < 48 ) or ( pb^ > 57 ) then exit;
      Result := parseFloat( i, js );
      exit;
    end;
    js := TKRJSONInt.Create( i );
    Result := true;
  end;

  function parseArray( var Arr: TKRJSONArray): boolean; forward;
  function parseObject( var Obj: TKRJSONObject): boolean; forward;

  function parseValue( var js: TKRJSON ): boolean;
  var
    jsObj: TKRJSONObject;
    jsArr: TKRJSONArray;
  begin
    Result := false;
    case pb^ of
      Ord('"'): begin
        if not parseString then exit;
        js := TKRJSONString.Create( str );
      end;
      Ord('-'):begin
        _inc;
        if n=ALength then exit;
        if( pb^ > 47 ) and ( pb^ < 58 ) then begin
          if not parseNum( js ) then exit;
        end else exit;
        case js.ValueType of
          jstInt: TKRJSONInt( js ).FValue := -TKRJSONInt( js ).FValue;
          jstFloat: TKRJSONFloat( js ).FValue := -TKRJSONFloat( js ).FValue;
        end;
      end;
      Ord('{'): begin
        jsObj:=TKRJSONObject.Create;
        if not parseObject( jsObj ) then begin
          jsObj.Free;
          exit;
        end;
        js := jsObj;
      end;
      Ord('['): begin
        jsArr:=TKRJSONArray.Create;
        if not parseArray( jsArr ) then begin
          jsArr.Free;
          exit;
        end;
        js := jsArr;
      end;
      Ord('t'): begin
        if (n + 4) > ALength then exit;
        _inc;
        if pb^ <> Ord('r') then exit;
        _inc;
        if pb^ <> Ord('u') then exit;
        _inc;
        if pb^ <> Ord('e') then exit;
        _inc;
        js := TKRJSONBool.Create( true );
      end;
      Ord('f'): begin
        if (n + 5) > ALength then exit;
        _inc;
        if pb^ <> Ord('a') then exit;
        _inc;
        if pb^ <> Ord('l') then exit;
        _inc;
        if pb^ <> Ord('s') then exit;
        _inc;
        if pb^ <> Ord('e') then exit;
        _inc;
        js := TKRJSONBool.Create( false );
      end;
      Ord('n'): begin
        if (n + 4) > ALength then exit;
        _inc;
        if pb^ <> Ord('u') then exit;
        _inc;
        if pb^ <> Ord('l') then exit;
        _inc;
        if pb^ <> Ord('l') then exit;
        _inc;
        js := TKRJSONNull.Create( );
      end
      else begin
        if( pb^ > 47 ) and ( pb^ < 58 ) then begin
          if not parseNum( js ) then exit;
        end else exit;
      end;
    end;
    Result:=true;
  end;

  function parseArray( var Arr: TKRJSONArray): boolean;
  var
    js: TKRJSON;
  begin
    Result:=false;
    _inc;
    skip_spc;
    if pb^ = Ord(']') then begin
      _inc;
      Result:=true;
      exit;
    end;
    while ( n<ALength ) do begin
      if not parseValue( js ) then exit;
      Arr.AddChild( js );
      skip_spc;
      if pb^ = Ord(']') then begin
        _inc;
        Result:=true;
        exit;
      end;
      if pb^ <> Ord(',') then exit;
      _inc;
      skip_spc;
    end;
  end;

  function parseObject( var Obj: TKRJSONObject): boolean;
  var
    js: TKRJSON;
    key: String;
  begin
    Result:=false;
    _inc;
    skip_spc;
    if pb^ = Ord('}') then begin
      _inc;
      Result:=true;
      exit;
    end;
    while ( n<ALength )  do begin
      if pb^ = Ord('"') then begin
        if not parseString then exit;
        skip_spc;
        if n=ALength then exit;
        if pb^ = Ord(':') then begin
          _inc;
          skip_spc;
          if n=ALength then exit;
          key := str;
          if not parseValue( js ) then exit;
          js.FKey := key;
          Obj.AddChild( js );
        end else Obj.AddChild( TKRJSONString.Create( str ) );
      end else begin
        if not parseValue( js ) then exit;
        Obj.AddChild( js );
      end;
      skip_spc;
      if pb^ = Ord('}') then begin
        _inc;
        Result:=true;
        exit;
      end;
      if pb^ <> Ord(',') then exit;
      _inc;
      skip_spc;
    end;
  end;

begin
  Result := nil;
  n := 0;
  pb := AData;
  skip_spc;
  if n=ALength then exit;
  if pb^ = Ord('{') then begin
    jsObject := TKRJSONObject.Create;
    if parseObject( jsObject ) then Result := jsObject else jsObject.Free;
  end else if pb^ = Ord('[') then begin
    jsArray := TKRJSONArray.Create;
    if parseArray( jsArray ) then Result := jsArray else jsArray.Free;
  end;
end;


function TKRJSON.toString: AnsiString;
begin

end;

{ TKRJSONString }

constructor TKRJSONString.Create(AKey, AValue: String);
begin
  FKey := AKey;
  FValue := AValue;
end;

constructor TKRJSONString.Create;
begin
  Create( '' );
end;

constructor TKRJSONString.Create(AValue: String);
begin
  Create( '', AValue );
end;

function TKRJSONString.getValueType: TKRJSONType;
begin
  Result := jstString;
end;

function TKRJSONString.toString: AnsiString;
var
  i,l,n,m,wd: integer;

  procedure addCh( ch: AnsiChar);
  begin
    inc( n );
    Result[ n ] := ch;
    if n = l then begin
      inc( l, KRJSON_STRING_MEMPAGE );
      SetLength(Result, l);
    end;
  end;

begin
  l := KRJSON_STRING_MEMPAGE;
  SetLength( Result, l );
  m := Length( FValue );
  Result[ 1 ] := '"';
  n:=1;
  for I := 1 to m do begin
    wd := Ord( FValue[ i ] );
    if wd <= $7f then begin
      case wd of
        Ord('"'): begin addCh('\');addCh('"'); end;
        Ord('\'): begin addCh('\');addCh('\'); end;
        Ord('/'): begin addCh('\');addCh('/'); end;
        $8: begin addCh('\');addCh('b'); end;
        $c: begin addCh('\');addCh('f'); end;
        $a: begin addCh('\');addCh('n'); end;
        $d: begin addCh('\');addCh('r'); end;
        $9: begin addCh('\');addCh('t'); end;
        else addCh( AnsiChar( wd ) );
      end;
      continue;
    end;
    if wd <= $7ff then begin
      addCh( AnsiChar( ( wd shr 6 ) or $C0 ) );
      addCh( AnsiChar( ( wd and $3f ) or  $80 ) );
      continue;
    end;
    if wd <= $ffff then begin
      addCh( AnsiChar( ( wd shr 12 ) or $E0 ) );
      addCh( AnsiChar( ( ( wd shr 6 ) and $3f ) or  $80 ) );
      addCh( AnsiChar( ( wd and $3f ) or  $80 ) );
      continue;
    end;
    addCh( AnsiChar( ( wd shr 18 ) or $F0 ) );
    addCh( AnsiChar( ( ( wd shr 12 ) and $3f ) or  $80 ) );
    addCh( AnsiChar( ( ( wd shr 6 ) and $3f ) or  $80 ) );
    addCh( AnsiChar( ( wd and $3f ) or  $80 ) );
  end;
  addCh( '"' );
  SetLength( Result, n );
end;

{ TKRJSONArray }

procedure TKRJSONArray.AddChild(js: TKRJSON);
begin
  FChildren[ FCount ] := js;
  if js.FParent <> nil then js.FParent.RemoveChild( js );
  js.FParent := Self;
  inc( FCount );
  if FCount = FListLen then begin
    inc( FListLen, KRJSON_LIST_MEMPAGE);
    SetLength( FChildren, FListLen );
  end;
end;

constructor TKRJSONArray.Create;
begin
  FCount := 0;
  FListLen := KRJSON_LIST_MEMPAGE;
  SetLength( FChildren, FListLen );
end;

destructor TKRJSONArray.Destroy;
var
  i, n: integer;
begin
  n := FCount - 1;
  for I := 0 to n do begin
    FChildren[ i ].FParent := nil;
    FChildren[ i ].Free;
  end;
  inherited;
end;

function TKRJSONArray.GetBool(Index: integer): TKRJSONBool;
begin
  Result:=TKRJSONBool(FChildren[Index]);
end;

function TKRJSONArray.GetChild(Index: integer): TKRJSON;
begin
  Result:=FChildren[Index];
end;

function TKRJSONArray.GetFloat(Index: integer): TKRJSONFloat;
begin
  Result:=TKRJSONFloat(FChildren[Index]);
end;

function TKRJSONArray.GetInteger(Index: integer): TKRJSONInt;
begin
  Result:=TKRJSONInt(FChildren[Index]);
end;

function TKRJSONArray.GetString(Index: integer): TKRJSONString;
begin
  Result:=TKRJSONString(FChildren[Index]);
end;

function TKRJSONArray.getValueType: TKRJSONType;
begin
  Result := jstArray;
end;

procedure TKRJSONArray.RemoveChild(AChild: TKRJSON);
var
  i, n: integer;
begin
  n := FCount - 1;
  for I := 0 to n do
    if FChildren[ i ] = AChild then begin
      RemoveChild( i );
      exit;
    end;
end;

procedure TKRJSONArray.RemoveChild(AIndex: integer);
var
  i, n: integer;
begin
  n := FCount - 2;
  for I := i to n do FChildren[ i ] := FChildren[ i + 1 ];
  Dec( FCount );
end;

function TKRJSONArray.toString: AnsiString;
var
  i, n: integer;
begin
  Result := '[';
  if FCount>0 then begin
    Result := Result + FChildren[ 0 ].toString;
    n := FCount - 1;
    for I := 1 to n do Result := Result + ',' + FChildren[ i ].toString;
  end;
  Result := Result + ']';
end;

{ TKRJSONObject }

function TKRJSONObject.ArrayOf(AKey: String; out Index: integer): boolean;
var
  i, n: integer;
begin
  Result:=false;
  n := FCount - 1;
  for I := 0 to n do
    if(FChildren[ i ].FKey = AKey) and (FChildren[ i ].ValueType = jstArray) then begin
      Result:=true;
      Index:=i;
      exit;
    end;
end;

function TKRJSONObject.BooleanOf(AKey: String; out Index: integer): boolean;
var
  i, n: integer;
begin
  Result:=false;
  n := FCount - 1;
  for I := 0 to n do
    if(FChildren[ i ].FKey = AKey) and (FChildren[ i ].ValueType = jstBool) then begin
      Result:=true;
      Index:=i;
      exit;
    end;
end;

function TKRJSONObject.FloatOf(AKey: String; out Index: integer): boolean;
var
  i, n: integer;
begin
  Result:=false;
  n := FCount - 1;
  for I := 0 to n do
    if(FChildren[ i ].FKey = AKey) and (FChildren[ i ].ValueType = jstFloat) then begin
      Result:=true;
      Index:=i;
      exit;
    end;
end;

function TKRJSONObject.GetBooleanDef(AKey: String; ADef: boolean): boolean;
var i: integer;
begin
  if BooleanOf(AKey,i) then Result:=Bools[i].Value else Result:=ADef;
end;

function TKRJSONObject.GetFloatDef(AKey: String; ADef: Double): Double;
var i: integer;
begin
  if FloatOf(AKey,i) then Result:=Floats[i].Value else Result:=ADef;
end;

function TKRJSONObject.GetIntegerDef(AKey: String; ADef: Int64): Int64;
var i: integer;
begin
  if IntegerOf(AKey,i) then Result:=Integers[i].Value else Result:=ADef;
end;

function TKRJSONObject.GetStringDef(AKey, ADef: String): String;
var i: integer;
begin
  if StringOf(AKey,i) then Result:=Strings[i].Value else Result:=ADef;
end;

function TKRJSONObject.getValueType: TKRJSONType;
begin
  Result := jstObject;
end;

function TKRJSONObject.IndexOf(AKey: String): integer;
var
  i, n: integer;
begin
  Result:=-1;
  n := FCount - 1;
  for I := 0 to n do
    if FChildren[ i ].FKey = AKey then begin
      Result:=i;
      exit;
    end;
end;

function TKRJSONObject.IntegerOf(AKey: String; out Index: integer): boolean;
var
  i, n: integer;
begin
  Result:=false;
  n := FCount - 1;
  for I := 0 to n do
    if(FChildren[ i ].FKey = AKey) and (FChildren[ i ].ValueType = jstString) then begin
      Result:=true;
      Index:=i;
      exit;
    end;
end;

function TKRJSONObject.StringOf(AKey: String; out Index: integer): boolean;
var
  i, n: integer;
begin
  Result:=false;
  n := FCount - 1;
  for I := 0 to n do
    if(FChildren[ i ].FKey = AKey) and (FChildren[ i ].ValueType = jstString) then begin
      Result:=true;
      Index:=i;
      exit;
    end;
end;

function TKRJSONObject.toString: AnsiString;
var
  i, n: integer;
begin
  Result := '{';
  if FCount>0 then begin
    if FChildren[ 0 ].FKey <> '' then
      Result := Result + '"' + AnsiString( FChildren[ 0 ].FKey ) + '":' + FChildren[ 0 ].toString
    else
      Result := Result + FChildren[ 0 ].toString;
    n := FCount - 1;
    for I := 1 to n do
      if FChildren[ i ].FKey <> '' then
        Result := Result + ',"' + AnsiString( FChildren[ i ].FKey ) + '":' + FChildren[ i ].toString
      else
        Result := Result + ',' + FChildren[ i ].toString;
  end;
  Result := Result + '}';
end;

{ TKRJSONBool }

constructor TKRJSONBool.Create;
begin
  Create( false );
end;

constructor TKRJSONBool.Create(AValue: boolean);
begin
  Create( '', AValue );
end;

constructor TKRJSONBool.Create(AKey: String; AValue: boolean);
begin
  FKey := AKey;
  FValue := AValue;
end;

function TKRJSONBool.getValueType: TKRJSONType;
begin
  Result := jstBool;
end;

function TKRJSONBool.toString: AnsiString;
begin
  if FValue then Result := 'true' else Result := 'false';
end;

{ TKRJSONNull }

function TKRJSONNull.getValueType: TKRJSONType;
begin
  Result := jstNull;
end;

function TKRJSONNull.toString: AnsiString;
begin
  Result := 'null';
end;

{ TKRJSONInt }

constructor TKRJSONInt.Create;
begin
  Create( 0 );
end;

constructor TKRJSONInt.Create(AValue: int64);
begin
  Create( '', AValue );
end;

constructor TKRJSONInt.Create(AKey: String; AValue: int64);
begin
  FKey := AKey;
  FValue := AValue;
end;

function TKRJSONInt.getValueType: TKRJSONType;
begin
  Result := jstInt;
end;

function TKRJSONInt.toString: AnsiString;
begin
  Result := IntToStr( FValue );
end;

{ TKRJSONFloat }

constructor TKRJSONFloat.Create;
begin
  Create( 0 );
end;

constructor TKRJSONFloat.Create(AValue: double);
begin
  Create( '', AValue );
end;

constructor TKRJSONFloat.Create(AKey: String; AValue: double);
begin
  FKey := AKey;
  FValue := AValue;
  fs.DecimalSeparator := '.';
end;

function TKRJSONFloat.getValueType: TKRJSONType;
begin
  Result := jstFloat;
end;

function TKRJSONFloat.toString: AnsiString;
begin
  Result := FloatToStr( FValue, fs );
end;

end.
