(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBluetooth                                                               *)
(*  Ver.: 28.01.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBluetooth;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
    Windows, Classes, SysUtils,
  {$IFEND}
    JwaBluetoothAPIs;

type
  TKRBTSearch = set of (krbtsAuthenticated, krbtsRemembered, krbtsUnknown, krbtsConnected);
  TKRBluetoothRemote = class
  private
    FAuthenticated: Boolean;
    FConnected: Boolean;
    FName: String;
    FClassofDevice: ULONG;
    FRemembered: Boolean;
    FAddress: BLUETOOTH_ADDRESS;
    FLastSeen: TDateTime;
    FLastUsed: TDateTime;
    FAddressStr: String;
  public
    property Address: BLUETOOTH_ADDRESS read FAddress;
    property AddressStr: String read FAddressStr;
    property ClassofDevice: ULONG read FClassofDevice;
    property Connected: Boolean read FConnected;
    property Remembered: Boolean read FRemembered;
    property Authenticated: Boolean read FAuthenticated;
    property LastSeen: TDateTime read FLastSeen;
    property LastUsed: TDateTime read FLastUsed;
    property Name: String read FName;
  end;

  TKRBluetoothLocal = class
  private
    FHandle: THandle;
    FAddress: BLUETOOTH_ADDRESS;
    FAddressStr: String;
    FName: String;
    FClassofDevice: ULONG;
    FSubversion: Word;
    FManufacturer: Word;
    FDevices: TList;
    procedure FreeDevicesList;
    function GetCount: integer;
    function GetDevice(index: integer): TKRBluetoothRemote;
  public
    constructor Create(AHandle: THandle);
    destructor Destroy;override;
    function SearchDevices(AParams: TKRBTSearch = [krbtsRemembered]): integer;
    property Handle:THandle read FHandle;
    property Address: BLUETOOTH_ADDRESS read FAddress;
    property AddressStr: String read FAddressStr;
    property Name: String read FName;
    property ClassofDevice: ULONG read FClassofDevice;
    property Subversion: Word read FSubversion;
    property Manufacturer: Word read FManufacturer;
    property Device[index:integer]: TKRBluetoothRemote read GetDevice; default;
    property DevicesCount: integer read GetCount;
  end;

  TKRBluetooth = class(TComponent)
  private
    FLocalDevices: TList;
    procedure FreeLocalDevicesList;
    function GetLocalDevicesCount: integer;
    function GetLocalDevice(index: integer): TKRBluetoothLocal;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure UpLocalDevices;
    property LocalDevice[index:integer]: TKRBluetoothLocal read GetLocalDevice; default;
    property LocalDevicesCount: integer read GetLocalDevicesCount;
  end;

implementation

{ TKRBluetooth }

constructor TKRBluetooth.Create(AOwner: TComponent);
begin
  inherited;
  FLocalDevices:=TList.Create;
  UpLocalDevices;
end;

destructor TKRBluetooth.Destroy;
begin
  FreeLocalDevicesList;
  FLocalDevices.Free;
  inherited;
end;

procedure TKRBluetooth.FreeLocalDevicesList;
begin
  while FLocalDevices.Count>0 do begin
    TKRBluetoothLocal(FLocalDevices[FLocalDevices.Count-1]).Free;
    FLocalDevices.Delete(FLocalDevices.Count-1);
  end;
end;

function TKRBluetooth.GetLocalDevice(index: integer): TKRBluetoothLocal;
begin
  Result:=TKRBluetoothLocal(FLocalDevices[index]);
end;

function TKRBluetooth.GetLocalDevicesCount: integer;
begin
  Result:=FLocalDevices.Count;
end;

procedure TKRBluetooth.UpLocalDevices;
var
  hRadio: THandle;
  BFRP: BLUETOOTH_FIND_RADIO_PARAMS;
  hFind: HBLUETOOTH_RADIO_FIND;
  RadioInfo: BLUETOOTH_RADIO_INFO;
  LocalDevice: TKRBluetoothLocal;
begin
  FreeLocalDevicesList;
  BFRP.dwSize := SizeOf(BFRP);
  hFind := BluetoothFindFirstRadio(@BFRP, hRadio);
  if (hFind <> 0) then begin
    repeat
      FillChar(RadioInfo, 0, SizeOf(BLUETOOTH_RADIO_INFO));
      RadioInfo.dwSize := SizeOf(BLUETOOTH_RADIO_INFO);
      if (BluetoothGetRadioInfo(hRadio, RadioInfo) = ERROR_SUCCESS) then begin
        LocalDevice:=TKRBluetoothLocal.Create(hRadio);
        LocalDevice.FAddress:=RadioInfo.address;
        LocalDevice.FAddressStr:=
          IntToHex(RadioInfo.address.rgBytes[5],2)+':'+
          IntToHex(RadioInfo.address.rgBytes[4],2)+':'+
          IntToHex(RadioInfo.address.rgBytes[3],2)+':'+
          IntToHex(RadioInfo.address.rgBytes[2],2)+':'+
          IntToHex(RadioInfo.address.rgBytes[1],2)+':'+
          IntToHex(RadioInfo.address.rgBytes[0],2);
        LocalDevice.FName:=RadioInfo.szName;
        LocalDevice.FClassofDevice:=RadioInfo.ulClassofDevice;
        LocalDevice.FSubversion:=RadioInfo.lmpSubversion;
        LocalDevice.FManufacturer:=RadioInfo.manufacturer;
        FLocalDevices.Add(Pointer(LocalDevice));
      end;
    until (not BluetoothFindNextRadio(hFind, hRadio));
    BluetoothFindRadioClose(hFind);
  end;
end;

{ TKRBluetoothLocal }

constructor TKRBluetoothLocal.Create(AHandle: THandle);
begin
  FHandle:=AHandle;
  FDevices:=TList.Create;
  SearchDevices;
end;

destructor TKRBluetoothLocal.Destroy;
begin
  FreeDevicesList;
  FDevices.Free;
  inherited;
end;

procedure TKRBluetoothLocal.FreeDevicesList;
begin
  while FDevices.Count>0 do begin
    TKRBluetoothRemote(FDevices[FDevices.Count-1]).Free;
    FDevices.Delete(FDevices.Count-1);
  end;
end;

function TKRBluetoothLocal.GetCount: integer;
begin
  Result:=FDevices.Count;
end;

function TKRBluetoothLocal.GetDevice(index: integer): TKRBluetoothRemote;
begin
  Result:=TKRBluetoothRemote(FDevices[index]);
end;

function TKRBluetoothLocal.SearchDevices(AParams: TKRBTSearch = [krbtsRemembered]): integer;
var
  DeviceInfo: _BLUETOOTH_DEVICE_INFO;
  DeviceSearchParams: _BLUETOOTH_DEVICE_SEARCH_PARAMS;
  DeviceFind: HBLUETOOTH_DEVICE_FIND;
  Device: TKRBluetoothRemote;
begin
  Result:=0;
  FreeDevicesList;
  FillChar(DeviceSearchParams, SizeOf(_BLUETOOTH_DEVICE_SEARCH_PARAMS), 0);
  DeviceSearchParams.dwSize:=SizeOf(_BLUETOOTH_DEVICE_SEARCH_PARAMS);
  DeviceSearchParams.fReturnAuthenticated:=krbtsAuthenticated in AParams;
  DeviceSearchParams.fReturnRemembered:=krbtsRemembered in AParams;
  DeviceSearchParams.fReturnUnknown:=krbtsUnknown in AParams;
  DeviceSearchParams.fReturnConnected:=krbtsConnected in AParams;
  DeviceSearchParams.hRadio:=FHandle;

  FillChar(DeviceInfo, SizeOf(_BLUETOOTH_DEVICE_INFO), 0);
  DeviceInfo.dwSize := SizeOf(_BLUETOOTH_DEVICE_INFO);

  DeviceFind := BluetoothFindFirstDevice(DeviceSearchParams, DeviceInfo);
  if(DeviceFind <> 0)then begin
    repeat
      Device:=TKRBluetoothRemote.Create;
      Device.FAddress:=DeviceInfo.Address;
      Device.FAddressStr:=
        IntToHex(DeviceInfo.Address.rgBytes[5],2)+':'+
        IntToHex(DeviceInfo.Address.rgBytes[4],2)+':'+
        IntToHex(DeviceInfo.Address.rgBytes[3],2)+':'+
        IntToHex(DeviceInfo.Address.rgBytes[2],2)+':'+
        IntToHex(DeviceInfo.Address.rgBytes[1],2)+':'+
        IntToHex(DeviceInfo.Address.rgBytes[0],2);
      Device.FClassofDevice:=DeviceInfo.ulClassofDevice;
      Device.FConnected:=DeviceInfo.fConnected;
      Device.FRemembered:=DeviceInfo.fRemembered;
      Device.FAuthenticated:=DeviceInfo.fAuthenticated;
      Device.FLastSeen:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.SystemTimeToDateTime(DeviceInfo.stLastSeen);
      Device.FLastUsed:={$IF CompilerVersion >= 23}System.{$IFEND}SysUtils.SystemTimeToDateTime(DeviceInfo.stLastUsed);
      Device.FName:=DeviceInfo.szName;
      FDevices.Add(Pointer(Device));

      FillChar(DeviceInfo, SizeOf(_BLUETOOTH_DEVICE_INFO), 0);
      DeviceInfo.dwSize := SizeOf(_BLUETOOTH_DEVICE_INFO);
    until (not BluetoothFindNextDevice(DeviceFind, DeviceInfo));
    BluetoothFindDeviceClose(DeviceFind);
  end;
end;

end.
