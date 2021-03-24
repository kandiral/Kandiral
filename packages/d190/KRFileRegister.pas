(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  http://kandiral.ru                                                        *)
(*                                                                            *)
(*  KRFileRegister                                                            *)
(*  Ver.: 31.08.2017                                                          *)
(*                                                                            *)
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
