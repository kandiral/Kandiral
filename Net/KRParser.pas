(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRParser                                                                  *)
(*  Ver.: 26.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRParser;

interface

uses Windows, KRThread, KRParserCfg;

type
  TKRParser = class(TKRThread)
  private
    FPrsWndH: THandle;
  protected
    procedure KRExecute; override;
    procedure KRExecutePaused; override;
    procedure KRExecutePausedFirst; override;
  public
    procedure SendBegin;
    procedure SendEnd;
    procedure SendProgress(AProgress: Word);
    procedure SendStatus(AStatus: String);
    procedure SendLog(ALog: String; ALogType: TKRParserLogType);overload;
    procedure SendLog(ALog: String);overload;
    procedure SendLogErr(ALog: String);
    procedure SendError(AError: Cardinal);
    procedure SendData(p: Pointer; inf: integer);
    property PrsWndH: THandle read FPrsWndH write FPrsWndH;
  end;

implementation

{ TKRParser }

procedure TKRParser.KRExecute;
begin

end;

procedure TKRParser.KRExecutePaused;
begin

end;

procedure TKRParser.KRExecutePausedFirst;
begin

end;

procedure TKRParser.SendBegin;
begin
  SendMessage(PrsWndH,WM_KRPARSER_BEGIN,0,0);
end;

procedure TKRParser.SendData(p: Pointer; inf: integer);
begin
  SendMessage(PrsWndH,WM_KRPARSER_DATA,Integer(p),inf);
end;

procedure TKRParser.SendEnd;
begin
  SendMessage(PrsWndH,WM_KRPARSER_END,0,0);
end;

procedure TKRParser.SendError(AError: Cardinal);
begin
  SendMessage(PrsWndH,WM_KRPARSER_ERROR,AError,0);
end;

procedure TKRParser.SendLog(ALog: String);
begin
  SendLog(ALog,KRPARSER_LOG_EVENT);
end;

procedure TKRParser.SendLog(ALog: String; ALogType: TKRParserLogType);
var
  _msg: PKRStrMsg;
begin
  New(_msg);
  StrToKRStrMsg(ALog,_msg);
  SendMessage(PrsWndH,WM_KRPARSER_LOG,Integer(Pointer(_msg)),ALogType);
  Dispose(_msg);
end;

procedure TKRParser.SendProgress(AProgress: Word);
begin
  SendMessage(PrsWndH,WM_KRPARSER_PROGRESS,AProgress,0);
end;

procedure TKRParser.SendStatus(AStatus: String);
var
  _msg: PKRStrMsg;
begin
  New(_msg);
  StrToKRStrMsg(AStatus,_msg);
  SendMessage(PrsWndH,WM_KRPARSER_STATUS,Integer(Pointer(_msg)),0);
  Dispose(_msg);
end;

procedure TKRParser.SendLogErr(ALog: String);
begin
  SendLog(ALog,KRPARSER_LOG_ERROR);
end;

end.
