(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  http://kandiral.ru                                                        *)
(*                                                                            *)
(*  KRAutomationRegister                                                      *)
(*  Ver.: 31.08.2017                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRAutomationRegister;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, DesignEditors, DesignIntf;
  {$ELSE}
    Classes, SysUtils, DesignEditors, DesignIntf;
  {$IFEND}

type
  TKRCOMBaudRateProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TKRCOMPortProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TKRCOMDataBitsProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TKRCOMStopBitsProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TKRCOMParityProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TKRCOMFlowControlProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses KRConnector, KRModbusMaster, KRModbusClient, KRField, KRVButton, KRVCheckBox,
  KRNMEA0183, KRIndicator, KRMBRegIndex, KRMBRegInterval, KRMBRegReadFunc, KRMBRegWriteFunc,
  KRBtnVarUpdate, KRVarLabel, KRSpeedInfo, KRVComboBox, KRPlcIO, KRTCPServer, KRBTServer,
  KRTPServer, KRTPClient, KRCOMPortConnector, KRTCPConnector, KRConnectorQueueBar,
  KRBTConnector, KRMBQueueBar, KRCOMPortSets, KRMBMon, KRVCheckGroupBox, KRAsyncComPort,
  KRMBRegs;

procedure Register;
begin
  RegisterClasses([TKRConnector, TKRCOMPortConnector, TKRTCPConnector, TKRModbusMaster,
    TKRModbusClient, TKRMBRegister, TKRField, TKRConnectorQueueBar, TKRMBQueueBar,
    TKRVButton, TKRIndVButton, TKRVCheckBox, TKRBLField, TKRNMEA0183, TKRIndicator,
    TKRBLIndicator, TKRMBRegIndex, TKRMBRegInterval, TKRMBRegReadFunc, TKRMBRegWriteFunc,
    TKRBtnVarUpdate, TKRVarLabel, TKRSpeedInfo, TKRVComboBox, TKRBLVComboBox, TKRPlcIO,
    TKRBTConnector, TKRTCPServer, TKRBTServer, TKRTPServer, TKRTPClient, TKRCOMPortSets,
    TKRMBMon, TKRVCheckGroupBox, TKRAsyncComPort, TKRMBRegs]);
  RegisterComponents('KRAutomation', [TKRCOMPortConnector, TKRTCPConnector,
    TKRModbusMaster, TKRModbusClient, TKRField, TKRConnectorQueueBar, TKRMBQueueBar,
    TKRVButton, TKRIndVButton, TKRVCheckBox, TKRBLField, TKRNMEA0183, TKRIndicator,
    TKRBLIndicator, TKRMBRegIndex, TKRMBRegInterval, TKRMBRegReadFunc, TKRMBRegWriteFunc,
    TKRBtnVarUpdate, TKRVarLabel, TKRSpeedInfo, TKRVComboBox, TKRBLVComboBox, TKRPlcIO,
    TKRBTConnector, TKRTCPServer, TKRBTServer, TKRTPServer, TKRTPClient, TKRCOMPortSets,
    TKRMBMon, TKRVCheckGroupBox, TKRAsyncComPort, TKRMBRegs]);
  RegisterNoIcon([TKRMBRegister]);
  RegisterPropertyEditor(TypeInfo(Integer),TKRCOMPortConnector,'BaudRate',TKRCOMBaudRateProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRNMEA0183,'BaudRate',TKRCOMBaudRateProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRAsyncComPort,'BaudRate',TKRCOMBaudRateProperty);
  RegisterPropertyEditor(TypeInfo(String),TKRCOMPortConnector,'Port',TKRCOMPortProperty);
  RegisterPropertyEditor(TypeInfo(String),TKRNMEA0183,'Port',TKRCOMPortProperty);
  RegisterPropertyEditor(TypeInfo(String),TKRAsyncComPort,'Port',TKRCOMPortProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRCOMPortConnector,'DataBits',TKRCOMDataBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRNMEA0183,'DataBits',TKRCOMDataBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRAsyncComPort,'DataBits',TKRCOMDataBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRCOMPortConnector,'StopBits',TKRCOMStopBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRNMEA0183,'StopBits',TKRCOMStopBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRAsyncComPort,'StopBits',TKRCOMStopBitsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRCOMPortConnector,'Parity',TKRCOMParityProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRNMEA0183,'Parity',TKRCOMParityProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRACPParity,'Bits',TKRCOMParityProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRCOMPortConnector,'FlowControl',TKRCOMFlowControlProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRNMEA0183,'FlowControl',TKRCOMFlowControlProperty);
  RegisterPropertyEditor(TypeInfo(Integer),TKRACPFlowControl,'FlowControl',TKRCOMFlowControlProperty);

end;

{ TKRCOMBaudRateProperty }

function TKRCOMBaudRateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TKRCOMBaudRateProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('110');
  Proc('300');
  Proc('600');
  Proc('1200');
  Proc('2400');
  Proc('4800');
  Proc('9600');
  Proc('14400');
  Proc('19200');
  Proc('38400');
  Proc('56000');
  Proc('57600');
  Proc('115200');
  Proc('128000');
  Proc('256000');
end;

{ TKRCOMPortProperty }

function TKRCOMPortProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TKRCOMPortProperty.GetValues(Proc: TGetStrProc);
var
  sl: TStringList;
  i: integer;
begin
  sl:=TStringList.Create;
  KREnumPorts(GUID_DEVICEINTERFACE_COMPORT,sl);
  for i := 0 to sl.Count-1 do Proc(sl[i]);
  sl.Free;
end;

{ TKRCOMDataBitsProperty }

function TKRCOMDataBitsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TKRCOMDataBitsProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('5');
  Proc('6');
  Proc('7');
  Proc('8');
end;

{ TKRCOMStopBitsProperty }

function TKRCOMStopBitsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TKRCOMStopBitsProperty.GetValue: string;
begin
  result:=inherited GetValue;
  case(StrToInt(result))of
  0: Result:='ONESTOPBIT';
  1: Result:='ONE5STOPBITS';
  2: Result:='TWOSTOPBITS';
  end;
end;

procedure TKRCOMStopBitsProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('ONESTOPBIT');
  Proc('ONE5STOPBITS');
  Proc('TWOSTOPBITS');
end;

procedure TKRCOMStopBitsProperty.SetValue(const Value: string);
var
  E,v: Integer;
begin
  Val(Value, v, E);
  if e<>0 then
    if value='ONESTOPBIT' then v:=0 else
    if value='ONE5STOPBITS' then v:=1 else
    if value='TWOSTOPBITS' then v:=2;
  inherited SetValue(intToStr(v));
end;

{ TKRCOMParityProperty }

function TKRCOMParityProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TKRCOMParityProperty.GetValue: string;
begin
  result:=inherited GetValue;
  case(StrToInt(result))of
  0: Result:='NOPARITY';
  1: Result:='ODDPARITY';
  2: Result:='EVENPARITY';
  3: Result:='MARKPARITY';
  4: Result:='SPACEPARITY';
  end;
end;

procedure TKRCOMParityProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('NOPARITY');
  Proc('ODDPARITY');
  Proc('EVENPARITY');
  Proc('MARKPARITY');
  Proc('SPACEPARITY');
end;

procedure TKRCOMParityProperty.SetValue(const Value: string);
var
  E,v: Integer;
begin
  Val(Value, v, E);
  if e<>0 then
    if value='NOPARITY' then v:=0 else
    if value='ODDPARITY' then v:=1 else
    if value='EVENPARITY' then v:=2 else
    if value='MARKPARITY' then v:=3 else
    if value='SPACEPARITY' then v:=4;
  inherited SetValue(intToStr(v));
end;

{ TKRCOMFlowControlProperty }

function TKRCOMFlowControlProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TKRCOMFlowControlProperty.GetValue: string;
begin
  result:=inherited GetValue;
  case(StrToInt(result))of
  0: Result:='None';
  1: Result:='Hardware';
  2: Result:='Software';
  3: Result:='Custom';
  end;
end;

procedure TKRCOMFlowControlProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('None');
  Proc('Hardware');
  Proc('Software');
  Proc('Custom');
end;

procedure TKRCOMFlowControlProperty.SetValue(const Value: string);
var
  E,v: Integer;
begin
  Val(Value, v, E);
  if e<>0 then
    if value='None' then v:=0 else
    if value='Hardware' then v:=1 else
    if value='Software' then v:=2 else
    if value='Custom' then v:=3;
  inherited SetValue(intToStr(v));
end;

end.

