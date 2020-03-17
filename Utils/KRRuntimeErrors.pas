(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRuntimeErrors                                                           *)
(*  Ver.: 07.09.2014                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRuntimeErrors;

interface

uses SysUtils, SyncObjs;

type
  TKRRuntimeErrorEv=procedure(Sender: TObject; ADescription: String;
    AException: Exception) of object;

  procedure RECS_Enter;
  procedure RECS_Leave;

  procedure REEvent(AREEvent: TKRRuntimeErrorEv; Sender: TObject; ADescription: String;
    AException: Exception);

  procedure REAddLog(AText: String);

var
  KRRuntimeErrorCS: TCriticalSection;
  KRRELogFile: String = '';
  KRRELog: String = '';

implementation

procedure RECS_Enter;
begin
  KRRuntimeErrorCS.Enter;
end;

procedure RECS_Leave;
begin
  KRRuntimeErrorCS.Leave;
end;

procedure REEvent(AREEvent: TKRRuntimeErrorEv; Sender: TObject; ADescription: String;
    AException: Exception);
var
  s: String;
begin
  if Assigned(AREEvent) then begin
    RECS_Enter;
    AREEvent(Sender, ADescription, AException);
    RECS_Leave;
  end;
  if ADescription<>'' then s:=ADescription else s:='';
  s:=s+' {'+AException.Message+'} ';
  if KRRELog<>'' then s:=s+' ['+KRRELog+'] ';
  REAddLog(s);
end;

procedure REAddLog(AText: String);
var
  logFile: TextFile;
begin
  if KRRELogFile<>'' then begin
    RECS_Enter;
    try
      try
        AssignFile(logFile, KRRELogFile);
        if FileExists(KRRELogFile) then System.Append(logFile) else Rewrite(logFile);
        Writeln(logFile,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz    ', NOW)+AText);
      finally
        CloseFile(logFile);
      end;
    except end;
    RECS_Leave;
  end;
end;

initialization
  KRRuntimeErrorCS := TCriticalSection.Create;
finalization
  FreeAndNil(KRRuntimeErrorCS);
end.
