(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWindows                                                                 *)
(*  Ver.: 03.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWindows;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, Winapi.ShlObj,
  {$ELSE}
    Windows, Classes, SysUtils, Forms, ShlObj,
  {$IFEND}
    KRTypes;

const
  KRCmdTOErrMsg = 'Превышен интервал ожидания.';

  procedure KRGetDriveInfo(ADrive: Char; ADriveInfo: PKRDrive);
  function KRGetSpecialFolderPath(AFolder: Integer; ACanCreate: Boolean): string;
  function KRDeleteDir(APath: String): boolean;
//------------- Start With Windows
  function KRSWWEnabled(AAppName: String): boolean;
  function KRSWWSetEnabled(AAppName, AExeName: String; AEnabled: boolean): boolean;
//------------- CreateProcess
  function KRCmd(ACmd: String): String;overload;
  function KRCmd(ACmd: String; ATimeout: Byte): String;overload;
  function KRCmd(ACmd: String; ATimeout: Byte; out ErrorLevel: Cardinal): String;overload;
  function KRCmd(ACmd: String; ATimeout: Byte; out ErrorLevel: Cardinal;
    AStdIn: String; out AStdOut: String; out AStdErr: String; isOem: boolean = true): String;overload;
//------------- Environment
  function KREnvGetValue(AName: String): String;
//  function KREnvVarExists(AName: String): boolean;
  procedure KREnvSetValue(AName, AValue: String);

  function GetVolumeSerialNumber(ARootPath: String): Cardinal;
  function GetSystemVolumeSerialNumber: Cardinal;
  function KRGetComputerName: string;

  function KRGetTempFile(APrefix: String = '~'): string;
  function KRGetFileSize(const AFileName: string): int64;

implementation

uses Funcs;

function KRGetFileSize(const AFileName: string): int64;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else Result := -1;
  FindClose(SearchRec);
end;

function KRGetTempFile(APrefix: String = '~'): string;
var
  buf: array[0..MAX_PATH] of Char;
  tmp: String;
begin
  tmp:=KREnvGetValue('TEMP');
  repeat
    FillChar(buf,SizeOf(buf),0);
    GetTempFileName(PChar(tmp), PChar(APrefix), 0, buf);
    Result:=buf;
  until FileExists(Result);
end;

procedure KRGetDriveInfo(ADrive: Char; ADriveInfo: PKRDrive);
var
  _VolumeName,
  _FileSystemName : array [0..MAX_PATH-1] of Char;
  FVolumeSerialNo : DWord;
  FMaxComponentLength,FFileSystemFlags: Cardinal;
  s: String;
  RootPath: array[0..4] of Char;
  TotalSpace, FreeSpaceAvailable: Int64;
begin
  ADriveInfo^.Drive:=ADrive;
  _VolumeName:=#$00;
  _FileSystemName:=#$00;
  if GetVolumeInformation(PChar(ADrive+':\'),_VolumeName,MAX_PATH,@FVolumeSerialNo,
    FMaxComponentLength,FFileSystemFlags,_FileSystemName,MAX_PATH) then begin
    SetString(s, _VolumeName, StrLen(_VolumeName));
    ADriveInfo^.DriveLabel:=ShortString(s);
    SetString(s, _FileSystemName, StrLen(_FileSystemName));
    ADriveInfo^.FileSystem:=ShortString(s);
  end else begin
    ADriveInfo^.DriveLabel:='';
    ADriveInfo^.FileSystem:='';
  end;
  RootPath[0] := ADrive;
  RootPath[1] := ':';
  RootPath[2] := '\';
  RootPath[3] := #0;
  if GetDiskFreeSpaceEx(@RootPath[0], FreeSpaceAvailable, TotalSpace, nil)then begin
    ADriveInfo^.Size:=TotalSpace;
    ADriveInfo^.FreeSize:=FreeSpaceAvailable;
  end;
  case {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.GetDriveType(PChar(ADrive+':\')) of
    DRIVE_REMOVABLE: ADriveInfo^.DriveType:=drvtpRemovable;
    DRIVE_FIXED: ADriveInfo^.DriveType:=drvtpFixed;
    DRIVE_REMOTE: ADriveInfo^.DriveType:=drvtpRemote;
    DRIVE_CDROM: ADriveInfo^.DriveType:=drvtpCDRom;
    DRIVE_RAMDISK: ADriveInfo^.DriveType:=drvtpRamDisk
    else ADriveInfo^.DriveType:=drvtpUnknown;
  end;
end;

function KRGetSpecialFolderPath(AFolder: Integer; ACanCreate: Boolean): string;
var
  FilePath: array [0..MAX_PATH-1] of char;
begin
 SHGetSpecialFolderPath(0, @FilePath[0], AFolder, ACanCreate);
 Result := FilePath;
end;

function KRDeleteDir(APath: String): boolean;
var
  sr: TSearchRec;
begin
  Result:=true;
  if FindFirst(APath + '\*.*', faAnyFile, sr) = 0 then
    repeat
      if sr.Attr and faDirectory = 0 then Result:=Result and {$IF CompilerVersion >= 23}Winapi.{$IFEND}Windows.DeleteFile(PChar(APath + '\' + sr.name))
      else if pos('.', sr.name) <= 0 then Result:=Result and KRDeleteDir(APath + '\' + sr.name);
    until FindNext(sr) <> 0;
  FindClose(sr);
  Result:=RemoveDirectory(PChar(APath)) and Result;
end;

//------------- Start With Windows

function KRSWWEnabled(AAppName: String): boolean;
var
  StdOut, StdErr: String;
  ErrLv: CARDINAL;
  s: String;
begin
  s:=KRCmd('SCHTASKS /Query /TN "'+AAppName+' Start With Windows"',10,ErrLv,'',StdOut,StdErr);
  Result:=((ErrLv=0)and(Trim(s)='')and(Trim(StdErr)='')and(Pos(AAppName+' Start With Windows',StdOut)>0));
end;

function KRSWWSetEnabled(AAppName, AExeName: String; AEnabled: boolean): boolean;
var
  StdOut, StdErr: String;
  ErrLv: CARDINAL;
  buf: array[0..127] of Char;
  sz: Cardinal;
  s: String;
begin
  FillChar(buf, 128, 0);
  sz:=128;
  GetUserName(buf,sz);
  s:=buf;
  if AEnabled then
    krcmd('schtasks.exe  /Create /TN "'+AAppName+' Start With Windows"'
      +' /SC ONLOGON'
      +' /RL HIGHEST'
      +' /RU "'+s+'"'
      +' /TR "\"'+AExeName+'\" "/start'
      +' /F',10,ErrLv,'',StdOut,StdErr)
  else
    krcmd('schtasks.exe  /Delete /TN "'+AAppName+' Start With Windows" /F',10,ErrLv,'',StdOut,StdErr);
  Result:=KRSWWEnabled(AAppName);
end;

//------------- CreateProcess

function KRCmd(ACmd: String): String;overload;
begin
  Result:=KRCmd(ACmd,255);
end;

function KRCmd(ACmd: String; ATimeout: Byte): String;overload;
var
  el: Cardinal;
begin
  Result:=KRCmd(ACmd,ATimeout,el);
end;

function KRCmd(ACmd: String; ATimeout: Byte; out ErrorLevel: Cardinal): String;overload;
var
  cmdLine: String;
  sa: TSecurityAttributes;
  si: TSTARTUPINFO;
  pi: TPROCESSINFORMATION;

  TmStart: TDateTime;
  i: Cardinal;
  _TimeoutError: boolean;
  _SysError: Cardinal;

  procedure CloseHandles;
  var i: Integer;
  begin
    if(_TimeoutError)then Result:=KRCmdTOErrMsg else
    if(_SysError=1)then Result:=SysErrorMessage(GetLastError);

    if pi.hProcess<>INVALID_HANDLE_VALUE then begin
      GetExitCodeProcess(pi.hProcess, ErrorLevel);
      CloseHandle(pi.hThread);
      i := WaitForSingleObject(pi.hProcess, 1000);
      CloseHandle(pi.hProcess);
      if i<>WAIT_OBJECT_0 then begin
        pi.hProcess := OpenProcess(PROCESS_TERMINATE, FALSE, pi.dwProcessId);
        if pi.hProcess <> 0 then begin
          TerminateProcess(pi.hProcess, 0);
          CloseHandle(pi.hProcess);
        end;
      end;
    end;
  end;

begin
  _TimeoutError:=false;
  _SysError:=1;

  cmdLine:=ACmd+#0;

  sa.nLength := SizeOf(sa);
  sa.bInheritHandle := true;
  sa.lpSecurityDescriptor := nil;

  ZeroMemory(@si, SizeOf(si));
  ZeroMemory(@pi, SizeOf(pi));
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;

  if not CreateProcess(nil, PChar(cmdLine), @sa, @sa, true,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then begin CloseHandleS;exit;end;

  TmStart := Now;

  repeat
    i := WaitForSingleObject(pi.hProcess,100);
    if i = WAIT_OBJECT_0 then break;
    if (Now-TmStart)*SecsPerDay>ATimeout then break;
    if Assigned(Application) then Application.ProcessMessages else Sleep(20);
  until false;

  if i<>WAIT_OBJECT_0 then begin _TimeoutError:=true;CloseHandleS;exit;end;

  _SysError:=0;
  CloseHandles;
end;

function KRCmd(ACmd: String; ATimeout: Byte; out ErrorLevel: Cardinal;
    AStdIn: String; out AStdOut: String; out AStdErr: String; isOem: boolean = true): String;overload;
var
  cmdLine: String;
  sa: TSecurityAttributes;
  hInStdR, hInStdW, hOutStdR, hOutStdW, hErrStdR, hErrStdW: THANDLE;
  si: TSTARTUPINFO;
  pi: TPROCESSINFORMATION;

  BytesRes: Cardinal;
  TmStart: TDateTime;
  i: Cardinal;
  StdErrors, StdOutput: AnsiString;

  _buf: TKRBuffer;
  _TimeoutError: boolean;
  _SysError: Cardinal;

  procedure CloseHandles;
  var i: Integer;
  begin
    if(_TimeoutError)then Result:=KRCmdTOErrMsg else
    if(_SysError=1)then Result:=SysErrorMessage(GetLastError);

    if pi.hProcess<>INVALID_HANDLE_VALUE then begin
      GetExitCodeProcess(pi.hProcess, ErrorLevel);
      CloseHandle(pi.hThread);
      i := WaitForSingleObject(pi.hProcess, 1000);
      CloseHandle(pi.hProcess);
      if i<>WAIT_OBJECT_0 then begin
        pi.hProcess := OpenProcess(PROCESS_TERMINATE, FALSE, pi.dwProcessId);
        if pi.hProcess <> 0 then begin
          TerminateProcess(pi.hProcess, 0);
          CloseHandle(pi.hProcess);
        end;
      end;
    end;
    if hInStdR<>INVALID_HANDLE_VALUE then CloseHandle(hInStdR);
    if hInStdW<>INVALID_HANDLE_VALUE then CloseHandle(hInStdW);
    if hOutStdR<>INVALID_HANDLE_VALUE then CloseHandle(hOutStdR);
    if hOutStdW<>INVALID_HANDLE_VALUE then CloseHandle(hOutStdW);
    if hErrStdR<>INVALID_HANDLE_VALUE then CloseHandle(hErrStdR);
    if hErrStdW<>INVALID_HANDLE_VALUE then CloseHandle(hErrStdW);
  end;

begin
  _TimeoutError:=false;
  _SysError:=1;

  hInStdR:=INVALID_HANDLE_VALUE;
  hInStdW:=INVALID_HANDLE_VALUE;
  hOutStdR:=INVALID_HANDLE_VALUE;
  hOutStdW:=INVALID_HANDLE_VALUE;
  hErrStdR:=INVALID_HANDLE_VALUE;
  hErrStdW:=INVALID_HANDLE_VALUE;
  pi.hProcess:=INVALID_HANDLE_VALUE;

  cmdLine:=ACmd+#0;

  sa.nLength := SizeOf(sa);
  sa.bInheritHandle := true;
  sa.lpSecurityDescriptor := nil;

  if not CreatePipe(hInStdR, hInStdW, @sa, 0) then exit;
  if not CreatePipe(hOutStdR, hOutStdW, @sa, 0) then begin CloseHandleS;exit;end;
  if not CreatePipe(hErrStdR, hErrStdW, @sa, 0) then begin CloseHandleS;exit;end;

  ZeroMemory(@si, SizeOf(si));
  ZeroMemory(@pi, SizeOf(pi));
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  si.wShowWindow := SW_HIDE;
  si.hStdInput := hInStdR;
  si.hStdOutput := hOutStdW;
  si.hStdError := hErrStdW;

  WriteFile(hInStdW,PChar(AStdIn+^Z)^,Length(AStdIn)+1,BytesRes,nil);
  CloseHandle(hInStdR);
  hInStdR:=INVALID_HANDLE_VALUE;

  if not CreateProcess(nil, PChar(cmdLine), @sa, @sa, true,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then begin CloseHandleS;exit;end;

  CloseHandle(hOutStdW);
  hOutStdW:=INVALID_HANDLE_VALUE;
  CloseHandle(hErrStdW);
  hErrStdW:=INVALID_HANDLE_VALUE;

  TmStart := Now;

  AStdOut:='';
  AStdErr:='';
  repeat
    i := WaitForSingleObject(pi.hProcess,100);
    StdOutput := '';
    repeat
      BytesRes:= 0;
      ReadFile(hOutStdR,_buf,1,BytesRes,nil);
      if BytesRes=0 then break;
      i := Length(StdOutput);
      SetLength(StdOutput,i+BytesRes);
      Move(_buf[0],StdOutput[i+1],BytesRes);
      if Assigned(Application) then Application.ProcessMessages else Sleep(20);
    until false;
    if isOem then OemToAnsi(PAnsiChar(StdOutput),PAnsiChar(StdOutput));
    AStdOut:=AStdOut+StringToWideString(StdOutput,1251);
    StdErrors:='';
    repeat
      BytesRes:= 0;
      ReadFile(hErrStdR,_buf,1,BytesRes,nil);
      if BytesRes=0 then break;
      i := Length(StdErrors);
      SetLength(StdErrors,i+BytesRes);
      Move(_buf[0],StdErrors[i+1],BytesRes);
      if Assigned(Application) then Application.ProcessMessages else Sleep(20);
    until false;
    if isOem then OemToAnsi(PAnsiChar(StdErrors),PAnsiChar(StdErrors));
    AStdErr:=AStdErr+StringToWideString(StdErrors,1251);
    if i = WAIT_OBJECT_0 then break;
    if (Now-TmStart)*SecsPerDay>ATimeout then break;
  until false;

  i := WaitForSingleObject(pi.hProcess,100);
  if i<>WAIT_OBJECT_0 then begin _TimeoutError:=true;CloseHandleS;exit;end;

  _SysError:=0;
  CloseHandles;
end;

//------------- Environment

function KREnvGetValue(AName: String): String;
var
  i: integer;
begin
  Result := '';
  try
    i := GetEnvironmentVariable(PChar(AName), nil, 0);
    if i > 0 then begin
      SetLength(Result, i);
      GetEnvironmentVariable(Pchar(AName), PChar(Result), i);
      if Result[i]=#0 then SetLength(Result, i-1);
    end;
  except
  end;
end;

{function KREnvVarExists(AName: String): boolean;
var
  evStr: PChar;
  sl: TStringList;
  n,i: integer;
  s: String;
begin
  Result:=false;
  evStr:=GetEnvironmentStrings;
  sl:=Explode(#0,evStr^);
  try
    for i := 0 to sl.Count-1 do begin
      n:=Pos('=',sl[i]);
      if n>0 then s:=LeftStr(sl[i],n-1) else s:=sl[i];
      if LowerCase(AName)=LowerCase(s) then begin
        Result:=true;
        break;
      end;
    end;
  finally
    sl.Free;
    FreeEnvironmentStrings(evStr);
  end;
end;}

procedure KREnvSetValue(AName, AValue: String);
begin
  try
    SetEnvironmentVariable(PChar(AName),PChar(AValue));
  except

  end;
end;

function GetVolumeSerialNumber(ARootPath: String): Cardinal;
var
  Buffer : array [0..255] of char;
  VolumeSerialNumber, MaximumComponentLength, FileSystemFlags : Cardinal;
begin
  Result:=0;
  if GetVolumeInformation(
    PChar(ARootPath),
    Buffer,
    SizeOf(Buffer),
    @VolumeSerialNumber,
    MaximumComponentLength,
    FileSystemFlags,
    nil,
    0) then Result:=VolumeSerialNumber
end;

function GetSystemVolumeSerialNumber: Cardinal;
begin
  Result:=GetVolumeSerialNumber(KREnvGetValue('SYSTEMDRIVE')+'\');
end;

function KRGetComputerName: string;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;
end.
