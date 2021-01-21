unit KRModbusLng;

interface

{$I '..\Includes\language.inc'}

type
  TModbusError = integer;

const

  MB_ERRORS_COUNT = 9;

  MBERR_OK = TModbusError(0);
  MBERR_ILLEGAL_FUNCTION = TModbusError(1);
  MBERR_ILLEGAL_DATA_ADDRESS = TModbusError(2);
  MBERR_ILLEGAL_DATA_VALUE = TModbusError(3);
  MBERR_FAILURE_IN_ASSOCIATED_DEVICE = TModbusError(4);
  MBERR_ACKNOWLEDGE = TModbusError(5);
  MBERR_BUSY_REJECTED_MESSAGE = TModbusError(6);
  MBERR_NAK_NEGATIVE_ACKNOWLEDGMENT = TModbusError(7);
  MBERR_MEMORY_PARITY_ERROR = TModbusError(8);

  MODBUS_ERRORS_MSG : array[0..MB_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
     'Нет ошибок',
     'Недопустимый номер функции',
     'Некорректный адрес регистра',
     'Некорректные данные',
     'Отказ оборудования прибора',
     'Данные не готовы',
     'Система занята',
     'Отрицательное подтверждение',
     'Ошибка четности памяти'
{$ELSE}
     'No errors',
     'Invalid function number',
     'Invalid register address',
     'Invalid data',
     'Instrument hardware failure',
     'Data not ready',
     'System busy',
     'Negative acknowledgment',
     'Memory parity error'
{$ENDIF}
  );


implementation

end.
