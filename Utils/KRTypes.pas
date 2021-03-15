(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTypes                                                                   *)
(*  Ver.: 11.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRTypes;

interface

const
  CKR_CR = #13;
  CKR_LF = #10;
  CKR_CRLF = #13#10;
  CKR_TAB = #9;

type
  TKRBuffer = array[0..1023] of byte;
  PKRBuffer = ^TKRBuffer;
  TKRLBuffer = record
    buf: TKRBuffer;
    len: integer;
  end;
  PKRLBuffer = ^TKRLBuffer;

  TKRRegister = word;
  TKRRegisters = array of TKRRegister;
  PKRRegisters = ^TKRRegisters;
  TKRBytes = array of byte;

  TKRDialogType = (krdtInformation, krdtWarning, krdtError, krdtQuestion);

  TKRDriveType = (drvtpUnknown, drvtpRemovable, drvtpFixed, drvtpRemote,
    drvtpCDRom, drvtpRamDisk);

  TKRDrive = record
    Drive: Char;
    DriveType: TKRDriveType;
    Size, FreeSize: Int64;
    DriveLabel, FileSystem: ShortString;
  end;
  PKRDrive = ^TKRDrive;

  TKREmptyEvent = procedure of object;

implementation

end.
