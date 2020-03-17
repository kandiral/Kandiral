(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVersionInfo                                                             *)
(*  Ver.: 03.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRVersionInfo;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.TypInfo;
  {$ELSE}
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    TypInfo;
  {$IFEND}



type
{$M+}
  (* Видели директиву $M+??? Это заставляет Delphi включать в код RTTI-информацию для

  перечислимых типов. В основном допускает работу с перечислимыми типами как
  со строками с помощью GetEnumName *)
  TVersionType = (vtCompanyName, vtFileDescription, vtFileVersion,
    vtInternalName,
    vtLegalCopyright, vtLegalTradeMark, vtOriginalFileName,
    vtProductName, vtProductVersion, vtComments);
{$M-}

  TKRVersionInfo = class(TComponent)
  private
    FVersionInfo: array[0..ord(high(TVersionType))] of string;
  protected
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileVersion: string;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTradeMark: string;
    function GetOriginalFileName: string;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetComments: string;
    function GetVersionInfo(VersionType: TVersionType): string; virtual;
    procedure SetVersionInfo; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVersion: string read GetFileVersion;
    property InternalName: string read GetInternalName;
    property LegalCopyright: string read GetLegalCopyright;
    property LegalTradeMark: string read GetLegalTradeMark;
    property OriginalFileName: string read GetOriginalFileName;
    property ProductName: string read GetProductName;
    property ProductVersion: string read GetProductVersion;
    property Comments: string read GetComments;
  end;

implementation

constructor TKRVersionInfo.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  SetVersionInfo;
end;

function TKRVersionInfo.GetCompanyName: string;
begin

  result := GeTVersionInfo(vtCompanyName);
end;

function TKRVersionInfo.GetFileDescription: string;
begin

  result := GeTVersionInfo(vtFileDescription);
end;

function TKRVersionInfo.GetFileVersion: string;
begin

  result := GeTVersionInfo(vtFileVersion);
end;

function TKRVersionInfo.GetInternalName: string;
begin

  result := GeTVersionInfo(vtInternalName);
end;

function TKRVersionInfo.GetLegalCopyright: string;
begin

  result := GeTVersionInfo(vtLegalCopyright);
end;

function TKRVersionInfo.GetLegalTradeMark: string;
begin

  result := GeTVersionInfo(vtLegalTradeMark);
end;

function TKRVersionInfo.GetOriginalFileName: string;
begin
  result := GeTVersionInfo(vtOriginalFileName);
end;

function TKRVersionInfo.GetProductName: string;
begin
  result := GeTVersionInfo(vtProductName);
end;

function TKRVersionInfo.GetProductVersion: string;
begin
  result := GeTVersionInfo(vtProductVersion);
end;

function TKRVersionInfo.GetComments: string;
begin
  result := GeTVersionInfo(vtComments);
end;

function TKRVersionInfo.GeTVersionInfo(VersionType: TVersionType): string;
begin
  result := FVersionInfo[ord(VersionType)];
end;

procedure TKRVersionInfo.SeTVersionInfo;
var
  sAppName, sVersionType: string;
  i: integer;
  iLenOfValue, iAppSize, buffsize: Cardinal;
  pcBuf, pcValue: PChar;

  trans:    pointer;
  temp: integer;
  LangCharSet: string;
begin
  sAppName := Application.ExeName;
  iAppSize := GetFileVersionInfoSize(PChar(sAppName), buffsize);
  if iAppSize > 0 then begin
    pcBuf := AllocMem(iAppSize);
    GetFileVersionInfo(PChar(sAppName), 0, iAppSize, pcBuf);

    VerQueryValue(pcBuf,'\VarFileInfo\Translation',Trans,buffsize);
    if buffsize >= 4 then begin
      temp:=0;
      StrLCopy(@temp, pchar(Trans), 1);
      LangCharSet:=IntToHex(temp, 4);
      StrLCopy(@temp, pchar(Trans)+1, 1);
      LangCharSet := LangCharSet+IntToHex(temp, 4);
    end else raise EReadError.Create('Invalid language info in file '+sAppName);


    for i := 0 to Ord(High(TVersionType)) do
    begin
      sVersionType := GetEnumName(TypeInfo(TVersionType), i);
      sVersionType := Copy(sVersionType, 3, length(sVersionType));
      if VerQueryValue(pcBuf, PChar('StringFileInfo\'+LangCharSet+'\' +
        sVersionType), Pointer(pcValue), iLenOfValue) then
        FVersionInfo[i] := pcValue;
    end;
    FreeMem(pcBuf, iAppSize);
  end;
end;

end.
