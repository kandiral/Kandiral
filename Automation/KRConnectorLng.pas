unit KRConnectorLng;

interface

{$I '..\Includes\language.inc'}

type
  TKRConnectorError = integer;

const
  CE_ERRORS_COUNT = 8;

  ceOK = TKRConnectorError(0);
  ceQueueOverflowed = TKRConnectorError(1);
  ceNotConnection = TKRConnectorError(2);
  ceDataNotSended = TKRConnectorError(3);
  ceNoResponse = TKRConnectorError(4);
  ceRequestTimeout = TKRConnectorError(5);
  ceResponseTimeout = TKRConnectorError(6);
  ceNotActive = TKRConnectorError(7);

  CONNECTOR_ERRORS_MSG : array[0..CE_ERRORS_COUNT-1] of String = (
{$IFDEF RUSSIAN_LANGUAGE}
    'Нет ошибок',
    'Очередь коннектора переполнена',
    'Соединение не установлено',
    'Не удалось отправить данные',
    'Ответ не получен',
    'Истекло время ожидания отправки данных',
    'Истекло время ожидания получения ответа',
    'Соединение не активно'
{$ELSE}
    'No errors',
    'Connector queue overflow',
    'Connection not set',
    'Failed to send data',
    'No response received',
    'Sending data timeout',
    'Response receiving timeout',
    'Connection not active'
{$ENDIF}
  );

implementation

end.
