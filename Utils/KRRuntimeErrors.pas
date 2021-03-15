(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRuntimeErrors                                                           *)
(*  Ver.: 09.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRuntimeErrors;

interface

uses Classes, SysUtils, SyncObjs;

type
  TKRRuntimeErrorEv=procedure(Sender: TObject; ADescription: String;
    AException: Exception) of object;

  procedure RECS_Enter;
  procedure RECS_Leave;

  procedure REEvent(AREEvent: TKRRuntimeErrorEv; Sender: TObject; ADescription: String;
    AException: Exception);

  procedure REAddLog(AText: String);
  procedure REAddLogA(AText: AnsiString);
  procedure REAddLogW(AText: String);

var
  KRRuntimeErrorCS: TCriticalSection;
  KRRELogFile: String = '';
  KRRELogDateFormat: String = 'yyyy-mm-dd hh:nn:ss.zzz';
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
        Writeln(logFile,FormatDateTime(KRRELogDateFormat, NOW)+'    '+AText);
      finally
        CloseFile(logFile);
      end;
    except end;
    RECS_Leave;
  end;
end;

procedure REAddLogA(AText: AnsiString);
var
  fs: TFileStream;
  s: AnsiString;
begin
  if KRRELogFile<>'' then begin
    RECS_Enter;
    try
      if FileExists(KRRELogFile) then begin
        fs:=TFileStream.Create(KRRELogFile,fmOpenWrite);
        fs.Seek(0, soFromEnd);
      end else
        fs:=TFileStream.Create(KRRELogFile,fmCreate);
      try
        s := AnsiString( FormatDateTime( KRRELogDateFormat, NOW) ) + '    ' + AText + #$D#$A;
        fs.WriteBuffer(s[1],Length(s));
      finally
        fs.Free;
      end;
    except end;
    RECS_Leave;
  end;
end;

procedure REAddLogW(AText: String);
var
  fs: TFileStream;
  s: String;
begin
  if KRRELogFile<>'' then begin
    RECS_Enter;
    try
      if FileExists(KRRELogFile) then begin
        fs:=TFileStream.Create(KRRELogFile,fmOpenWrite);
        fs.Seek(0, soFromEnd);
      end else
        fs:=TFileStream.Create(KRRELogFile,fmCreate);
      try
        s := FormatDateTime(KRRELogDateFormat, NOW)+'    '+AText+#$D#$A;
        fs.WriteBuffer( s[1], Length(s) shl 1);
      finally
        fs.Free;
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
