(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWebDownload                                                             *)
(*  Ver.: 04.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWebDownload;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.Graphics, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
    Vcl.Imaging.GIFImg,
  {$ELSE}
    Classes, SysUtils, Graphics, jpeg, pngimage, GIFImg,
  {$IFEND}

  KRIdHTTP;

  function KRDownloadFile( AURL: String; AHTTP: TKRIdHTTP; AStream: TStream ): boolean;
  function KRDownloadImageToBMP( AURL: String; AHTTP: TKRIdHTTP; ABitmap: TBitmap ): boolean;

implementation

function KRDownloadFile( AURL: String; AHTTP: TKRIdHTTP; AStream: TStream ): boolean;
begin
  Result := false;

  try
    AHTTP.Get( AURL, AStream);
    Result:=true;
  except end;

end;

function KRDownloadImageToBMP( AURL: String; AHTTP: TKRIdHTTP; ABitmap: TBitmap ): boolean;
var
  buf: array[0..3] of byte;
  ms: TMemoryStream;
  jpeg: TJPEGImage;
  png: TPngImage;
  gif: TGIFImage;
begin
  Result := false;

  ms:=TMemoryStream.Create;
  try

    if not KRDownloadFile( AURL, AHTTP, ms ) then exit;

    if ms.Size>10 then begin
      ms.Position:=0;
      ms.Read(buf,4);
      ms.Position:=0;
      if(buf[0]=$FF)and(buf[1]=$D8)and(buf[2]=$FF)and(buf[3] shr 4 = $E)then begin
        jpeg:=TJPEGImage.Create;
        try
          jpeg.LoadFromStream( ms );
          ABitmap.Assign( jpeg );
          Result:=true;
        except end;
        jpeg.Free;
      end else if(buf[1]=$50)and(buf[2]=$4E)and(buf[3]=$47)then begin
        png:=TPngImage.Create;
        try
          png.LoadFromStream( ms );
          ABitmap.Assign( png );
          Result:=true;
        except end;
        png.Free;
      end else if(buf[0]=$47)and(buf[1]=$49)and(buf[2]=$46)then begin
        gif:=TGIFImage.Create;
        try
          gif.LoadFromStream( ms );
          ABitmap.Assign( gif );
          Result:=true;
        except end;
        gif.Free;
      end;
    end;
  finally
    ms.Free;
  end;

end;

end.
