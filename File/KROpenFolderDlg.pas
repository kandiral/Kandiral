(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KROpenFolderDlg                                                           *)
(*  Ver.: 17.01.2014                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KROpenFolderDlg;

interface

uses Windows, Classes;

type
  TKROpenFolderDlg = class(TComponent)
  private
    FTitle: String;
    FFolder : array[0..MAX_PATH] of char;
    function GetFolder: String;
  public
    constructor Create(AOwner: TComponent);override;
    function Execute(ParentWnd: HWND): boolean;
    property Folder: String read GetFolder;
  published
    property Title: String read FTitle write FTitle;
  end;

implementation

uses ShlObj;

{ TKROpenFolderDlg }

constructor TKROpenFolderDlg.Create(AOwner: TComponent);
begin
  inherited;
  FTitle:='Выберите папку';
  FillChar(FFolder, sizeof(FFolder), #0);
end;

function TKROpenFolderDlg.Execute(ParentWnd: HWND): boolean;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  FDisplayName : array[0..MAX_PATH] of char;
begin
  Result:=false;
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  BrowseInfo.hwndOwner := ParentWnd;
  BrowseInfo.pszDisplayName := @FDisplayName;
  BrowseInfo.lpszTitle := PChar(FTitle);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, FFolder);
    GlobalFreePtr(lpItemID);
    Result:=true;
  end;
end;

function TKROpenFolderDlg.GetFolder: String;
begin
  Result:=FFolder;
end;

end.
