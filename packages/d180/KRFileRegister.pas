(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  http://kandiral.ru                                                        *)
(*                                                                            *)
(******************************************************************************)
unit KRFileRegister;

interface

uses Classes, SysUtils;

procedure Register;

implementation

uses KRDriveComboBox, KRFileList, KROpenFolderDlg;

procedure Register;
begin
  RegisterClasses([TKRDriveComboBox, TKRFileList, TKROpenFolderDlg]);
  RegisterComponents('KRFile', [TKRDriveComboBox, TKRFileList, TKROpenFolderDlg]);
end;

end.
