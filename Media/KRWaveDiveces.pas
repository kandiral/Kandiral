(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWaveDiveces                                                             *)
(*  Ver.: 09.01.2017                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWaveDiveces;

interface

uses Classes, Windows, MMSystem, KRMMDDK;

  function KRGetDefaultWaveOutDevice: Cardinal;
  procedure KRGetWaveOutDeviceList(List: TStrings);
  procedure KRGetWaveInDeviceList(List: TStrings);

implementation

function KRGetDefaultWaveOutDevice: Cardinal;
var
  LDW2: Cardinal;
begin
  Result := $FFFFFFFF;
  LDW2 := 0;
  waveOutMessage( Integer(WAVE_MAPPER), DRVM_MAPPER_PREFERRED_GET, DWORD(@Result), DWORD(@LDW2) );
end;

procedure KRGetWaveOutDeviceList(List: TStrings);
var
 Index: Integer;
 LCaps: WAVEOUTCAPS;
begin
  List.Clear;
  for Index := 0 to waveOutGetNumDevs -1 do begin
    waveOutGetDevCaps( Index, @LCaps, SizeOf( LCaps ) );
    List.add( LCaps.szPname );
  end;
end;

procedure KRGetWaveInDeviceList(List: TStrings);
var
 Index: Integer;
 LCaps: WAVEINCAPS;
begin
  List.Clear;
  for Index := 0 to waveInGetNumDevs -1 do begin
    waveInGetDevCaps( Index, @LCaps, SizeOf( LCaps ) );
    List.add( LCaps.szPname );
  end;
end;

end.
