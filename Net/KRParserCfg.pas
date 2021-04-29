(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRParserCfg                                                               *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRParserCfg;

interface

uses Messages;

type
  TKRParserLogType = byte;

const
  KRPARSER_LOG_EVENT = TKRParserLogType(0);
  KRPARSER_LOG_ERROR = TKRParserLogType(1);

  WM_KRPARSER_BEGIN = WM_USER + 1;
  WM_KRPARSER_END = WM_USER + 2;
  WM_KRPARSER_PROGRESS = WM_USER + 3;
  WM_KRPARSER_STATUS = WM_USER + 4;
  WM_KRPARSER_LOG = WM_USER + 5;
  WM_KRPARSER_ERROR = WM_USER + 6;
  WM_KRPARSER_DATA = WM_USER + 7;

  KRPARSER_ERRORS_COUNT = 1;
  KRPARSER_ERRORS : array[0..KRPARSER_ERRORS_COUNT-1] of string = (
    'Ошибка времени выполнения!'
  );

  KRPARSER_ERRORS_RUNTIMEERROR = 0;

  KRPARSER_STRMSG_MAX_LEN = 1024;

type
  TKRStrMsg = array[0..KRPARSER_STRMSG_MAX_LEN] of Char;
  PKRStrMsg = ^TKRStrMsg;

  procedure StrToKRStrMsg(AStr: String; pStr: PKRStrMsg);

implementation

procedure StrToKRStrMsg(AStr: String; pStr: PKRStrMsg);
var
  i,n: integer;
begin
  n:=Length(AStr);
  if n>KRPARSER_STRMSG_MAX_LEN-1 then n:=KRPARSER_STRMSG_MAX_LEN-1;
  for i := 1 to n do pStr^[i-1]:=AStr[i];
  pStr^[n]:=#0;
end;

end.
