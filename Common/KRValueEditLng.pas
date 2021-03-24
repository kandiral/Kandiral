(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRValueEditLng                                                            *)
(*  Ver.: 23.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRValueEditLng;

interface

{$I '..\Includes\language.inc'}

const

{$IFDEF RUSSIAN_LANGUAGE}

  KRVALUEEDIT_DEFAULT_MAXIMUMLIMIT_MSG = 'Значение не должно быть больше [#Value]';
  KRVALUEEDIT_DEFAULT_MINIMUMLIMIT_MSG = 'Значение не должно быть меньше [#Value]';
  KRVALUEEDIT_DEFAULT_ASKBEFOREINPUT_MSG = 'Установить значение «[#Value]»?';
  KRVALUEEDIT_DEFAULT_INCORRECTVALUE_MSG = 'Неверно введено значение!';

{$ELSE}

  KRVALUEEDIT_DEFAULT_MAXIMUMLIMIT_MSG = 'The value should not be more than [#Value]';
  KRVALUEEDIT_DEFAULT_MINIMUMLIMIT_MSG = 'The value must not be less than [#Value]';
  KRVALUEEDIT_DEFAULT_ASKBEFOREINPUT_MSG = 'Set to "[#Value]"?';
  KRVALUEEDIT_DEFAULT_INCORRECTVALUE_MSG = 'Invalid value entered!';

{$ENDIF}

implementation

end.
