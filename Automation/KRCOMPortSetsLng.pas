(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCOMPortSetsLng                                                          *)
(*  Ver.: 23.03.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCOMPortSetsLng;

interface

{$I '..\Includes\language.inc'}

const
{$IFDEF RUSSIAN_LANGUAGE}

  COMPORTSETS_FORMCAPTION = 'Настройки порта';
  COMPORTSETS_PORT = 'Порт';
  COMPORTSETS_BAUDRATE = 'Скорость';
  COMPORTSETS_DATABITS = 'Биты данных';
  COMPORTSETS_STOPBITS = 'Стоп биты';
  COMPORTSETS_PARITY = 'Четность';
  COMPORTSETS_PARITY_NO = 'Нет';
  COMPORTSETS_PARITY_ODD = 'Чет';
  COMPORTSETS_PARITY_EVEN = 'Нечет';
  COMPORTSETS_PARITY_MARK = 'Маркер';
  COMPORTSETS_PARITY_SPACE = 'Пробел';
  COMPORTSETS_FLOWCONTROL = 'Управление потоком';
  COMPORTSETS_FLOWCONTROL_NO = 'Нет';
  COMPORTSETS_FLOWCONTROL_HARDWARE = 'Аппаратное';
  COMPORTSETS_FLOWCONTROL_SOFTWARE = 'Прикладное';
  COMPORTSETS_FLOWCONTROL_CUSTOM = 'Стандартное';
  COMPORTSETS_BUTTONCANCEL = 'Отмена';

{$ELSE}

  COMPORTSETS_FORMCAPTION = 'COM Port Settings';
  COMPORTSETS_PORT = 'Port';
  COMPORTSETS_BAUDRATE = 'Baud Rate';
  COMPORTSETS_DATABITS = 'Data Bits';
  COMPORTSETS_STOPBITS = 'Stop Bits';
  COMPORTSETS_PARITY = 'Parity';
  COMPORTSETS_PARITY_NO = 'No';
  COMPORTSETS_PARITY_ODD = 'Odd';
  COMPORTSETS_PARITY_EVEN = 'Even';
  COMPORTSETS_PARITY_MARK = 'Mark';
  COMPORTSETS_PARITY_SPACE = 'Space';
  COMPORTSETS_FLOWCONTROL = 'Flow Control';
  COMPORTSETS_FLOWCONTROL_NO = 'No';
  COMPORTSETS_FLOWCONTROL_HARDWARE = 'Hardware';
  COMPORTSETS_FLOWCONTROL_SOFTWARE = 'Software';
  COMPORTSETS_FLOWCONTROL_CUSTOM = 'Custom';
  COMPORTSETS_BUTTONCANCEL = 'Cancel';

{$ENDIF}

implementation

end.
