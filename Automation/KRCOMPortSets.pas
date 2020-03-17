(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRCOMPortSets                                                             *)
(*  Ver.: 31.08.2017                                                          *)
(*  https://kandiral.ru/delphi/krcomportsets.pas.html                         *)
(*                                                                            *)
(******************************************************************************)
unit KRCOMPortSets;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls,
  {$IFEND}
  KRComboBox, KRCOMPort, KRIniConfig, SetupAPI;

const
  GUID_DEVICEINTERFACE_COMPORT:TGUID=(
    D1 : $86E0D1E0;
    D2 : $8089;
    D3 : $11D0;
    D4 : ($9C,$E4,$08,$00,$3E,$30,$1F,$73);
   );
  GUID_DEVICEINTERFACE_MODEM:TGUID=(
    D1 : $2C7089AA;
    D2 : $2E0E;
    D3 : $11D1;
    D4 : ($B1,$14,$00,$C0,$4F,$C2,$AA,$E4);
   );

type
  TKRCOMPortName = string;

  IKRCOMPortSets = interface
  ['{F6369565-D323-4E85-96B0-C5BBB397790D}']
    function GetBaudRate: integer;
    function GetDataBits: integer;
    function GetFlowControl: TKRFlowControl;
    function GetParity: integer;
    function GetPort: TKRCOMPortName;
    function GetStopBits: Integer;
    procedure SetBaudRate(const Value: integer);
    procedure SetDataBits(const Value: integer);
    procedure SetFlowControl(const Value: TKRFlowControl);
    procedure SetParity(const Value: integer);
    procedure SetPort(const Value: TKRCOMPortName);
    procedure SetStopBits(const Value: Integer);
  end;

  TKRCOMPortSetsForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    cbPort: TKRBLComboBox;
    cbBaudRate: TKRBLComboBox;
    cbDataBits: TKRBLComboBox;
    cbStopBits: TKRBLComboBox;
    cbParity: TKRBLComboBox;
    cbFlowControl: TKRBLComboBox;
    constructor Create(AOwner: TComponent);override;
    function Execute(ACOMPort: IKRCOMPortSets; AAutoDetectPorts: boolean): boolean;
  end;

  TKRCOMPortSets = class(TComponent, IKRCfgParam)
  private
    FCOMPort: IKRCOMPortSets;
    FBaudRate: TKRIniCfgParam;
    FParity: TKRIniCfgParam;
    FStopBits: TKRIniCfgParam;
    FPort: TKRIniCfgParam;
    FFlowControl: TKRIniCfgParam;
    FDataBits: TKRIniCfgParam;
    FForm: TKRCOMPortSetsForm;
    FAutoDetectPorts: boolean;
    procedure SetCOMPort(const Value: IKRCOMPortSets);
    procedure SetBaudRate(const Value: TKRIniCfgParam);
    procedure SetDataBits(const Value: TKRIniCfgParam);
    procedure SetFlowControl(const Value: TKRIniCfgParam);
    procedure SetParity(const Value: TKRIniCfgParam);
    procedure SetPort(const Value: TKRIniCfgParam);
    procedure SetStopBits(const Value: TKRIniCfgParam);
    procedure CfgParamChange(AParam: TKRIniCfgParam); stdcall;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent);override;
    function Execute: boolean;
    procedure Init;
    property Form: TKRCOMPortSetsForm read FForm;
    class function GetActivePorts(AList: TStrings): integer;
  published
    property COMPort: IKRCOMPortSets read FCOMPort write SetCOMPort;
    property AutoDetectPorts: boolean read FAutoDetectPorts write FAutoDetectPorts default true;
    property Port: TKRIniCfgParam read FPort write SetPort;
    property Parity: TKRIniCfgParam read FParity write SetParity;
    property StopBits: TKRIniCfgParam read FStopBits write SetStopBits;
    property BaudRate: TKRIniCfgParam read FBaudRate write SetBaudRate;
    property DataBits: TKRIniCfgParam read FDataBits write SetDataBits;
    property FlowControl: TKRIniCfgParam read FFlowControl write SetFlowControl;
  end;

  procedure KREnumPorts(const guid:TGUID; AList: TStrings);

implementation

var SetupApiLoaded: boolean = false;

{$R '..\dfm\KRCOMPortSets.dfm'}

procedure KREnumPorts(const guid:TGUID; AList: TStrings);
var
  h             : HDEVINFO;
  devinfo       : TSPDevInfoData;
  bMoreItems    : boolean;
  nIndex        : longint;
  hDeviceKey    : HKEY;
  szData        : array[0..255]of Char;
  dwType,dwSize : DWORD;
  s          : String;
begin
  h:=SetupDiGetClassDevsW(@guid,nil,0,DIGCF_PRESENT or DIGCF_INTERFACEDEVICE);
  if(Cardinal(h)=INVALID_HANDLE_VALUE)then exit;
  try
    nIndex:=0;
    bMoreItems:=true;
    while(bMoreItems)do begin
      s:='';
      devinfo.cbSize:=sizeof(SP_DEVICE_INTERFACE_DATA);
      bMoreItems:=SetupDiEnumDeviceInfo(h,nIndex,devinfo);
      if(bMoreItems)then begin
        hDeviceKey:=SetupDiOpenDevRegKey(h,devinfo,DICS_FLAG_GLOBAL,0,DIREG_DEV,KEY_QUERY_VALUE);
        if(hDeviceKey>0)then begin
          szData[0]:=#0;
          dwSize:=sizeof(szData);
          dwType:=0;
          if(RegQueryValueEx(hDeviceKey,'PortName',nil,@dwType,@szData,@dwSize)=0)and(dwSize>=2)then s:=szData;
          RegCloseKey(hDeviceKey);
        end;
        if(s<>'')then begin
          {szData[0]:=#0;
          dwSize:=sizeof(szData);
          dwType:=0;
          if(SetupDiGetDeviceRegistryPropertyW(
              h,
              devinfo,
              SPDRP_DEVICEDESC,
              dwType,
              @szData,
              dwSize,
              dwSize))and(dwType = REG_SZ)then
                item.DevDesc:=szData; }
          AList.Add(s);
        end;
      end;
      inc(nIndex);
    end;
  finally
    SetupDiDestroyDeviceInfoList(h);
  end;
end;

{ TKRCOMPortSets }

procedure TKRCOMPortSets.CfgParamChange(AParam: TKRIniCfgParam);
begin
  if not Assigned(FCOMPort) then exit;

  if AParam=FBaudRate then begin
    FCOMPort.SetBaudRate(FBaudRate.Value);
  end else if AParam=FDataBits then begin
    FCOMPort.SetDataBits(FDataBits.Value);
  end else if AParam=FFlowControl then begin
    FCOMPort.SetFlowControl(FFlowControl.Value);
  end else if AParam=FParity then begin
    FCOMPort.SetParity(FParity.Value);
  end else if AParam=FPort then begin
    FCOMPort.SetPort(FPort.Value);
  end else if AParam=FStopBits then begin
    FCOMPort.SetStopBits(FStopBits.Value);
  end;
end;

constructor TKRCOMPortSets.Create(AOwner: TComponent);
begin
  inherited;
  FCOMPort:=nil;
  FBaudRate:=nil;
  FParity:=nil;
  FStopBits:=nil;
  FPort:=nil;
  FFlowControl:=nil;
  FDataBits:=nil;
  FAutoDetectPorts:=true;
  FForm:=TKRCOMPortSetsForm.Create(Application);
end;

function TKRCOMPortSets.Execute: boolean;
var
  I: integer;
begin
  Result:=false;
  if Assigned(FCOMPort) then begin
    Result:=Form.Execute(FCOMPort,FAutoDetectPorts);
    if Result then begin
      if Assigned(FPort) then FPort.Value:=Form.cbPort.Text
      else FCOMPort.SetPort(Form.cbPort.Text);

      i:=BaudRateFromIndex(Form.cbBaudRate.ItemIndex);
      if Assigned(FBaudRate) then FBaudRate.Value:=i
      else FCOMPort.SetBaudRate(i);

      i:=DataBitsFromIndex(Form.cbDataBits.ItemIndex);
      if Assigned(FDataBits) then FDataBits.Value:=i
      else FCOMPort.SetDataBits(i);

      i:=FlowControlFromIndex(Form.cbFlowControl.ItemIndex);
      if Assigned(FFlowControl) then FFlowControl.Value:=i
      else FCOMPort.SetFlowControl(i);

      i:=ParityFromIndex(Form.cbParity.ItemIndex);
      if Assigned(FParity) then FParity.Value:=i
      else FCOMPort.SetParity(i);

      i:=StopBitsFromIndex(Form.cbStopBits.ItemIndex);
      if Assigned(FStopBits) then FStopBits.Value:=i
      else FCOMPort.SetStopBits(i);
    end;
  end;
end;

class function TKRCOMPortSets.GetActivePorts(AList: TStrings): integer;
begin
  AList.Clear;
  KREnumPorts(GUID_DEVICEINTERFACE_COMPORT,AList);
  result:=AList.Count
end;

procedure TKRCOMPortSets.Init;
begin
  if Assigned(FPort) then FCOMPort.SetPort(FPort.Value);
  if Assigned(FBaudRate) then FCOMPort.SetBaudRate(FBaudRate.Value);
  if Assigned(FDataBits) then FCOMPort.SetDataBits(FDataBits.Value);
  if Assigned(FFlowControl) then FCOMPort.SetFlowControl(FFlowControl.Value);
  if Assigned(FParity) then FCOMPort.SetParity(FParity.Value);
  if Assigned(FStopBits) then FCOMPort.SetStopBits(FStopBits.Value);
end;

procedure TKRCOMPortSets.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = TComponent(FCOMPort))then SetCOMPort(nil) else
    if (AComponent = FBaudRate)then SetBaudRate(nil) else
    if (AComponent = FParity)then SetParity(nil) else
    if (AComponent = FStopBits)then SetStopBits(nil) else
    if (AComponent = FPort)then SetPort(nil) else
    if (AComponent = FFlowControl)then SetFlowControl(nil) else
    if (AComponent = FDataBits)then SetDataBits(nil);
end;

procedure TKRCOMPortSets.SetBaudRate(const Value: TKRIniCfgParam);
begin
  if FBaudRate<>Value then begin
    if Assigned(FBaudRate) then begin
      FBaudRate.RemoveFreeNotification(Self);
      FBaudRate.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FBaudRate:=Value;
      FBaudRate.FreeNotification(Self);
      FBaudRate.AddMon(Self);
    end else FBaudRate:=nil;
  end;
end;

procedure TKRCOMPortSets.SetCOMPort(const Value: IKRCOMPortSets);
begin
  if FCOMPort<>Value then begin
    if Assigned(Value) then begin
      if Assigned(TComponent(FCOMPort)) then TComponent(FCOMPort).RemoveFreeNotification(Self);
      FCOMPort:=Value;
      TComponent(FCOMPort).FreeNotification(Self);
      if Assigned(FBaudRate) then
        FCOMPort.SetBaudRate(FBaudRate.Value);
      if Assigned(FDataBits) then
        FCOMPort.SetDataBits(FDataBits.Value);
      if Assigned(FFlowControl) then
        FCOMPort.SetFlowControl(FFlowControl.Value);
      if Assigned(FParity) then
        FCOMPort.SetParity(FParity.Value);
      if Assigned(FPort) then
        FCOMPort.SetPort(FPort.Value);
      if Assigned(FStopBits) then
        FCOMPort.SetStopBits(FStopBits.Value);
    end else FCOMPort:=nil;
  end;
end;

procedure TKRCOMPortSets.SetDataBits(const Value: TKRIniCfgParam);
begin
  if FDataBits<>Value then begin
    if Assigned(FDataBits) then begin
      FDataBits.RemoveFreeNotification(Self);
      FDataBits.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FDataBits:=Value;
      FDataBits.FreeNotification(Self);
      FDataBits.AddMon(Self);
    end else FDataBits:=nil;
  end;
end;

procedure TKRCOMPortSets.SetFlowControl(const Value: TKRIniCfgParam);
begin
  if FFlowControl<>Value then begin
    if Assigned(FFlowControl) then begin
      FFlowControl.RemoveFreeNotification(Self);
      FFlowControl.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FFlowControl:=Value;
      FFlowControl.FreeNotification(Self);
      FFlowControl.AddMon(Self);
    end else FFlowControl:=nil;
  end;
end;

procedure TKRCOMPortSets.SetParity(const Value: TKRIniCfgParam);
begin
  if FParity<>Value then begin
    if Assigned(FParity) then begin
      FParity.RemoveFreeNotification(Self);
      FParity.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FParity:=Value;
      FParity.FreeNotification(Self);
      FParity.AddMon(Self);
    end else FParity:=nil;
  end;
end;

procedure TKRCOMPortSets.SetPort(const Value: TKRIniCfgParam);
begin
  if FPort<>Value then begin
    if Assigned(FPort) then begin
      FPort.RemoveFreeNotification(Self);
      FPort.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FPort:=Value;
      FPort.FreeNotification(Self);
      FPort.AddMon(Self);
    end else FPort:=nil;
  end;
end;

procedure TKRCOMPortSets.SetStopBits(const Value: TKRIniCfgParam);
begin
  if FStopBits<>Value then begin
    if Assigned(FStopBits) then begin
      FStopBits.RemoveFreeNotification(Self);
      FStopBits.DelMon(Self);
    end;
    if Assigned(Value) then begin
      FStopBits:=Value;
      FStopBits.FreeNotification(Self);
      FStopBits.AddMon(Self);
    end else FStopBits:=nil;
  end;
end;

{ TKRCOMPortSetsForm }

constructor TKRCOMPortSetsForm.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  Position:=poScreenCenter;

  cbPort:=TKRBLComboBox.Create(Self);
  cbPort.Parent:=Self;
  cbPort.Left := 137;
  cbPort.Top := 8;
  cbPort.Width := 105;
  cbPort.Height := 21;
  cbPort.Style := csDropDown;
  cbPort.BLabel.Width:=25;
  cbPort.BLabel.Height:=13;
  cbPort.BLabel.AutoSize:=True;
  cbPort.BLabel.Spacing:=6;
  cbPort.BLabel.Caption := #1055#1086#1088#1090;
  cbPort.Items.BeginUpdate;
  for i := 1 to 30 do
    cbPort.Items.Add('COM'+IntToStr(i));
  cbPort.Items.EndUpdate;

  cbBaudRate:=TKRBLComboBox.Create(Self);
  cbBaudRate.Parent:=self;
  cbBaudRate.Left:=137;
  cbBaudRate.Top:=35;
  cbBaudRate.Width:=105;
  cbBaudRate.Height:=21;
  cbBaudRate.Style:=csDropDownList;
  cbBaudRate.BLabel.Width:=48;
  cbBaudRate.BLabel.Height:=13;
  cbBaudRate.BLabel.AutoSize:=True;
  cbBaudRate.BLabel.Spacing:=6;
  cbBaudRate.BLabel.Caption:=#1057#1082#1086#1088#1086#1089#1090#1100;
  cbBaudRate.Items.Add('110');
  cbBaudRate.Items.Add('300');
  cbBaudRate.Items.Add('600');
  cbBaudRate.Items.Add('1200');
  cbBaudRate.Items.Add('2400');
  cbBaudRate.Items.Add('4800');
  cbBaudRate.Items.Add('9600');
  cbBaudRate.Items.Add('14400');
  cbBaudRate.Items.Add('19200');
  cbBaudRate.Items.Add('38400');
  cbBaudRate.Items.Add('56000');
  cbBaudRate.Items.Add('57600');
  cbBaudRate.Items.Add('115200');
  cbBaudRate.Items.Add('128000');
  cbBaudRate.Items.Add('256000');

  cbDataBits:=TKRBLComboBox.Create(Self);
  cbDataBits.Parent:=self;
  cbDataBits.Left:=136;
  cbDataBits.Top:=62;
  cbDataBits.Width:=105;
  cbDataBits.Height:=21;
  cbDataBits.Style:=csDropDownList;
  cbDataBits.BLabel.Width:=68;
  cbDataBits.BLabel.Height:=13;
  cbDataBits.BLabel.AutoSize:=True;
  cbDataBits.BLabel.Spacing:=6;
  cbDataBits.BLabel.Caption:=#1041#1080#1090#1099' '#1076#1072#1085#1085#1099#1093;
  cbDataBits.Items.Add('5');
  cbDataBits.Items.Add('6');
  cbDataBits.Items.Add('7');
  cbDataBits.Items.Add('8');

  cbStopBits:=TKRBLComboBox.Create(Self);
  cbStopBits.Parent:=self;
  cbStopBits.Left:=137;
  cbStopBits.Top:=116;
  cbStopBits.Width:=105;
  cbStopBits.Height:=21;
  cbStopBits.Style:=csDropDownList;
  cbStopBits.BLabel.Width:=54;
  cbStopBits.BLabel.Height:=13;
  cbStopBits.BLabel.AutoSize:=True;
  cbStopBits.BLabel.Spacing:=6;
  cbStopBits.BLabel.Caption:=#1057#1090#1086#1087' '#1073#1080#1090#1099;
  cbStopBits.Items.Add('1');
  cbStopBits.Items.Add('1.5');
  cbStopBits.Items.Add('2');

  cbParity:=TKRBLComboBox.Create(Self);
  cbParity.Parent:=self;
  cbParity.Left:=137;
  cbParity.Top:=89;
  cbParity.Width:=105;
  cbParity.Height:=21;
  cbParity.Style:=csDropDownList;
  cbParity.BLabel.Width:=48;
  cbParity.BLabel.Height:=13;
  cbParity.BLabel.AutoSize:=True;
  cbParity.BLabel.Spacing:=6;
  cbParity.BLabel.Caption:=#1063#1077#1090#1085#1086#1089#1090#1100;
  cbParity.Items.Add(#1053#1077#1090);
  cbParity.Items.Add(#1063#1077#1090);
  cbParity.Items.Add(#1053#1077#1095#1077#1090);
  cbParity.Items.Add(#1052#1072#1088#1082#1077#1088);
  cbParity.Items.Add(#1055#1088#1086#1073#1077#1083);

  cbFlowControl:=TKRBLComboBox.Create(Self);
  cbFlowControl.Parent:=self;
  cbFlowControl.Left:=137;
  cbFlowControl.Top:=143;
  cbFlowControl.Width:=105;
  cbFlowControl.Height:=21;
  cbFlowControl.Style:=csDropDownList;
  cbFlowControl.BLabel.Width:=106;
  cbFlowControl.BLabel.Height:=13;
  cbFlowControl.BLabel.AutoSize:=True;
  cbFlowControl.BLabel.Spacing:=6;
  cbFlowControl.BLabel.Caption:=#1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1087#1086#1090#1086#1082#1086#1084;
  cbFlowControl.Items.Add(#1053#1077#1090);
  cbFlowControl.Items.Add(#1040#1087#1087#1072#1088#1072#1090#1085#1086#1077);
  cbFlowControl.Items.Add(#1055#1088#1080#1082#1083#1072#1076#1085#1086#1077);
  cbFlowControl.Items.Add(#1057#1090#1072#1085#1076#1072#1088#1090#1085#1086#1077);

end;

function TKRCOMPortSetsForm.Execute(ACOMPort: IKRCOMPortSets; AAutoDetectPorts: boolean): boolean;
begin
  if AAutoDetectPorts and SetupApiLoaded then begin
    cbPort.Items.BeginUpdate;
    cbPort.Items.Clear;
    KREnumPorts(GUID_DEVICEINTERFACE_COMPORT,cbPort.Items);
    cbPort.Items.EndUpdate;
  end;
  cbPort.Text:=ACOMPort.GetPort;
  cbBaudRate.ItemIndex:=IndexFromBaudRate(ACOMPort.GetBaudRate);
  cbDataBits.ItemIndex:=IndexFromDataBits(ACOMPort.GetDataBits);
  cbParity.ItemIndex:=IndexFromParity(ACOMPort.GetParity);
  cbStopBits.ItemIndex:=IndexFromStopBits(ACOMPort.GetStopBits);
  cbFlowControl.ItemIndex:=IndexFromFlowControl(ACOMPort.GetFlowControl);
  Result:=ShowModal=mrOk;
end;

initialization
  SetupApiLoaded:=LoadSetupApi;
finalization
  UnloadSetupApi;
end.
