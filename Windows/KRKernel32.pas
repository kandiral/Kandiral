(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRKernel32                                                                *)
(*  Ver.: 14.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRKernel32;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows;
  {$ELSE}
    Windows;
  {$IFEND}

  function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
    var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
  {$EXTERNALSYM TzSpecificLocalTimeToSystemTime}
  function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation;
    var lpUniversalTime,lpLocalTime: TSystemTime): BOOL; stdcall;
  {$EXTERNALSYM SystemTimeToTzSpecificLocalTime}

implementation

function TzSpecificLocalTimeToSystemTime; external kernel32 name 'TzSpecificLocalTimeToSystemTime';
function SystemTimeToTzSpecificLocalTime; external kernel32 name 'SystemTimeToTzSpecificLocalTime';

end.
