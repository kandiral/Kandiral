(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWaveToFile                                                              *)
(*  Ver.: 29.12.2016                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWaveToFile;

interface

uses Windows, Classes, Controls, KRWavePlayer, SysUtils;

type
  TKRWaveToFile = class(TKRWavePlayer)
  private
    FFile: TFileStream;
    FOpen: boolean;
    FFileName: String;
  public
    property IsOpen: boolean read FOpen;
    procedure Open;
    procedure Close;
    procedure Play(var ABuffer; ALength: integer; AEnd: boolean = false);override;
    property FileName: String read FFileName write FFileName;
  end;

implementation

{ TKRWaveToFile }

procedure TKRWaveToFile.Close;
begin
  FFile.Free;
end;

procedure TKRWaveToFile.Open;
begin
  FFile:=TFileStream.Create(FFileName,fmCreate);
end;

procedure TKRWaveToFile.Play(var ABuffer; ALength: integer; AEnd: boolean);
begin
  FFile.Write(ABuffer,ALength);
end;

end.
