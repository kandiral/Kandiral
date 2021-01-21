unit KRModbusClientLng;

interface

{$I '..\Includes\language.inc'}

type
  TMBCError = integer;

const
  MBC_ERRORS_COUNT = 9;

  mbceAskLimit = TMBCError(65536);
  mbceFileNotFount = TMBCError(65537);
  mbceFileDeleteError = TMBCError(65538);
  mbceFileOpenForReadError = TMBCError(65539);
  mbceFileReadError = TMBCError(65540);
  mbceFileOpenForWriteError = TMBCError(65541);
  mbceFileWriteError = TMBCError(65542);
  mbceNoValue = TMBCError(65543);
  mbceRealTimeError = TMBCError(65544);

  MBC_ERRORS_MSG : array[0..MBC_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
    'Превышен лимит ожидания ответа от потока',
    'Файл не найден',
    'Не удалось удалить файл',
    'Не удается открыть файл для чтения',
    'Ошибка чтения файла',
    'Не удается открыть файл для записи',
    'Ошибка записи в файла',
    'Значение не получено',
    'Ошибка времени выполнения'
{$ELSE}
    'Timeout receive response from the thread',
    'File not found',
    'Failed to delete file',
    'Can''t open file for reading',
    'File read error',
    'Can''t open file for writing',
    'Error writing to file',
    'No value',
    'Runtime error'
{$ENDIF}
  );

implementation

end.
