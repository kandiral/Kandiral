unit KRFieldLng;

interface

{$I '..\Includes\language.inc'}

const

{$IFDEF RUSSIAN_LANGUAGE}

  KRFIELD_DEFAULT_MAXIMUMLIMIT_MSG = 'Значение не должно быть больше [#Value]';
  KRFIELD_DEFAULT_MINIMUMLIMIT_MSG = 'Значение не должно быть меньше [#Value]';
  KRFIELD_DEFAULT_ASKBEFOREINPUT_MSG = 'Установить значение «[#Value]»?';
  KRFIELD_DEFAULT_INCORRECTVALUE_MSG = 'Неверно введено значение!';

{$ELSE}

  KRFIELD_DEFAULT_MAXIMUMLIMIT_MSG = 'The value should not be more than [#Value]';
  KRFIELD_DEFAULT_MINIMUMLIMIT_MSG = 'The value must not be less than [#Value]';
  KRFIELD_DEFAULT_ASKBEFOREINPUT_MSG = 'Set to "[#Value]"?';
  KRFIELD_DEFAULT_INCORRECTVALUE_MSG = 'Invalid value entered!';

{$ENDIF}

implementation

end.
