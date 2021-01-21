unit KRModbusMasterLng;

interface

{$I '..\Includes\language.inc'}

type
  TMBMError = integer;

const
  MBM_ERRORS_COUNT = 11;

  mbmeOk = TMBMError(0);
  mbmeNotConnector = TMBMError(1);
  mbmeIncorrectID = TMBMError(2);
  mbmeIncorrectPackageLength = TMBMError(3);
  mbmeCRC = TMBMError(4);
  mbmeIncorrectFunction = TMBMError(5);
  mbmeIncorrectMBPackageLength = TMBMError(6);
  mbmeIncorrectDataLength = TMBMError(7);
  mbmeQueueOverflowed = TMBMError(8);
  mbmeTimeOut = TMBMError(9);
  mbmeRealTimeError = TMBMError(10);
  mbmeModbusNotActive = TMBMError(11);

  MB_PARSER_ERRORS_MSG : array[0..MBM_ERRORS_COUNT] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
    'Нет ошибок',
    'Не подключен модуль передачи данных',
    'Неверный идентификатор пакета',
    'Неверная длина пакета',
    'Ошибка контрольной суммы',
    'Неверный номер функции',
    'Неверная длина Modbus пакета',
    'Неверная длина данных',
    'Очередь переполнена',
    'Таймаут',
    'Ошибка времени выполнения',
    'Обработчик протокола Modbus выключен'
{$ELSE}
    'No errors',
    'Communication module not connected',
    'Invalid package ID',
    'Invalid packet length',
    'Checksum error',
    'Invalid function number',
    'Invalid Modbus packet length',
    'Invalid data length',
    'Queue is overflow',
    'Timeout',
    'Runtime error'
{$ENDIF}
  );

{$IFDEF RUSSIAN_LANGUAGE}
  MBMEL_NO_ERRORS = 'Нет ошибок';
  MBMEL_UNKNOWN_ERROR = 'Неизвестная ошибка';
  MBMEL_DEVICE_ERROR = 'Ошибка прибора';
  MBMEL_DATA_PROCESSING_ERROR = 'Ошибка обработки данных';
  MBMEL_DATA_TRANSMISSION_ERROR = 'Ошибка передачи данных';
{$ELSE}
  MBMEL_NO_ERRORS = 'No errors';
  MBMEL_UNKNOWN_ERROR = 'Unknown error';
  MBMEL_DEVICE_ERROR = 'Device error';
  MBMEL_DATA_PROCESSING_ERROR = 'Data processing error';
  MBMEL_DATA_TRANSMISSION_ERROR = 'Data transmission error';
{$ENDIF}

implementation

end.
