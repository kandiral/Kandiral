unit KRMsgBox;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Vcl.Forms,
  {$ELSE}
    Windows, Forms,
  {$IFEND}
  KRTypes;


  function KRDialogTypeToFlag(ADialogType: TKRDialogType): LongInt;

  function KRAppMsgBox(AText: String; Flags: Longint; ACaption: String = ''): integer;
  function KRAppMsgBoxErr(AText: String; ACaption: String = ''): integer;
  function KRAppMsgBoxInf(AText: String; ACaption: String = ''): integer;
  function KRAppMsgBoxQst(AText: String; ACaption: String = ''): integer;

implementation

function KRDialogTypeToFlag(ADialogType: TKRDialogType): LongInt;
begin
  Result:=0;
  case ADialogType of
  krdtInformation: Result:=MB_ICONINFORMATION;
  krdtWarning: Result:=MB_ICONWARNING;
  krdtError: Result:=MB_ICONERROR;
  krdtQuestion: Result:=MB_ICONQUESTION;
  end;
end;

function KRAppMsgBox(AText: String; Flags: Longint; ACaption: String = ''): integer;
var
  s: String;
begin
  if ACaption='' then s:=Application.Title else s:=ACaption;
  Application.NormalizeTopMosts;
  Result:=Application.MessageBox(PChar(AText),PChar(s),Flags);
  Application.RestoreTopMosts;
end;

function KRAppMsgBoxErr(AText: String; ACaption: String = ''): integer;
begin
  Result:=KRAppMsgBox(AText,MB_OKCANCEL or MB_ICONERROR or MB_DEFBUTTON2,ACaption);
end;

function KRAppMsgBoxInf(AText: String; ACaption: String = ''): integer;
begin
  Result:=KRAppMsgBox(AText,MB_OKCANCEL or MB_ICONINFORMATION or MB_DEFBUTTON2,ACaption);
end;

function KRAppMsgBoxQst(AText: String; ACaption: String = ''): integer;
begin
  Result:=KRAppMsgBox(AText,MB_YESNOCANCEL or MB_ICONQUESTION or MB_DEFBUTTON3,ACaption);
end;

end.
