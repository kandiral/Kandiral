(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KROpenFolderDlg                                                           *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KROpenFolderDlg;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShlObj, Winapi.ActiveX,
    Vcl.Forms;
  {$ELSE}
    Windows, Classes, SysUtils, ShlObj, ActiveX, Forms;
  {$IFEND}

type
  TKROpenFolderDlg = class(TComponent)
  private
    FTitle: String;
    FFolder : String;
    FNewDialogStyle: boolean;
    FStatusText: boolean;
    FNewFolderBtn: boolean;
    FEditBox: boolean;
  public
    constructor Create(AOwner: TComponent);override;
    function Execute(ParentWnd: HWND): boolean;
    property Folder: String read FFolder write FFolder;
  published
    property Title: String read FTitle write FTitle;
    property NewDialogStyle: boolean read FNewDialogStyle write FNewDialogStyle default true;
    property StatusText: boolean read FStatusText write FStatusText default true;
    property NewFolderBtn: boolean read FNewFolderBtn write FNewFolderBtn default false;
    property EditBox: boolean read FEditBox write FEditBox default true;
  end;

implementation

type
  TBFFData = record
    selFld: LPARAM;
    title: LPARAM;
    flags: UINT;
  end;
  PBFFData=^TBFFData;

function BrowseCallbackProc(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
  Path: array[0..MAX_PATH] of Char;
  data: PBFFData;
begin
  data:=PBFFData(lpData);
  case uMsg of
    BFFM_INITIALIZED: begin
      SetWindowText(wnd, PChar(data.title));
      if Pointer(data.selFld)<>nil then begin
        SendMessage(wnd, BFFM_SETSELECTION, 1, data.selFld);
        SendMessage(wnd, BFFM_SETSTATUSTEXTW, 0, data.selFld);
      end;
    end;
    BFFM_SELCHANGED: if SHGetPathFromIDList(Pointer(lParam), Path) then
        SendMessage(wnd, BFFM_SETSTATUSTEXTW, 0, Integer(@Path));
  end;
  Result:=0;
end;

{ TKROpenFolderDlg }

constructor TKROpenFolderDlg.Create(AOwner: TComponent);
begin
  inherited;
  FTitle:='Выберите папку';
  FNewDialogStyle:=true;
  FStatusText:=true;
  FNewFolderBtn:=false;
  FEditBox:=true;
  FillChar(FFolder, sizeof(FFolder), #0);
end;

function TKROpenFolderDlg.Execute(ParentWnd: HWND): boolean;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;

  fld: String;
  Buffer: PChar;
  ShellMalloc: IMalloc;
  Windows: Pointer;
  data: TBFFData;
begin
  Result:=false;
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      BrowseInfo.hwndOwner:=ParentWnd;
      BrowseInfo.pidlRoot:=nil;
      BrowseInfo.pszDisplayName:=Buffer;
      BrowseInfo.lpszTitle := nil;//PChar(FTitle);
      BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
      if not FNewFolderBtn then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or BIF_NONEWFOLDERBUTTON;
      if FEditBox then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or BIF_EDITBOX or BIF_VALIDATE;
      if FNewDialogStyle then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or BIF_NEWDIALOGSTYLE;
      if FStatusText then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or BIF_STATUSTEXT;

      data.flags:=BrowseInfo.ulFlags;
      data.title:=NativeInt(PChar(FTitle));

      fld:=Trim(FFolder);
      if(Length(fld)>0)then begin
        if(fld[Length(fld)]='\')then Delete(fld, Length(fld), 1);
        data.selFld:=NativeInt(PChar(fld));
      end else data.selFld:=NativeInt(nil);
      BrowseInfo.lParam:=NativeInt(@data);
      BrowseInfo.lpfn:=BrowseCallbackProc;

      if(ParentWnd<>0)and(ParentWnd<>INVALID_HANDLE_VALUE)then Windows := DisableTaskWindows(ParentWnd);
      try
        lpItemID := ShBrowseForFolder(BrowseInfo);
      finally
        if(ParentWnd<>0)and(ParentWnd<>INVALID_HANDLE_VALUE)then EnableTaskWindows(Windows);
      end;

      if lpItemID<>nil then begin
        ShGetPathFromIDList(lpItemID, Buffer);
        ShellMalloc.Free(lpItemID);
        FFolder:=Buffer;
        Result:=true;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

end.
