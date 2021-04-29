(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRDriveComboBox                                                           *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRDriveComboBox;

interface

uses Windows, Classes, Controls, StdCtrls, SysUtils, Graphics, ShellAPI, KRTimer,
  Forms, KRTypes;

type
  TKRCBDriveType = (cbdtAll, cbdtUnknown, cbdtRemovable, cbdtFixed, cbdtRemote,
    cbdtCDRom, cbdtRamDisk);

  TKRDCBGetAdditionDriveSize=procedure(Sender: TObject; var ADriveSize: int64;
    var ADriveFreeSize: int64) of object;

  TKRDriveComboBox = class(TCustomComboBox, IKRTimer)
  private
    FDrivers: AnsiString;
    FDrivers_: String;
    FLastDrive, FDrive: Char;
    FVolumeName: String;
    FFileSystemName: String;
    FShowFileSystemName: boolean;
    FShowVolumeName: boolean;
    FDriveFreeSize: Int64;
    FDriveSize: Int64;
    FInited, FIsWnd: boolean;
    FDriveChanged: TNotifyEvent;
    FTimer: TKRTimer;
    FShowDrives: TKRCBDriveType;
    FADNames: array of string;
    FADIcons: array of TIcon;
    FOnGetAdditionDriveSize: TKRDCBGetAdditionDriveSize;
    procedure SetDrive(const Value: Char);
    procedure DriveChanged;
    procedure GetDriveInfo(ADrive: Char; out AVolumeName: String; out AFileSystemName: String);
    procedure SetShowFileSystemName(const Value: boolean);
    procedure SetShowVolumeName(const Value: boolean);
    procedure SetTimer(const Value: TKRTimer);
    function GetDriveType: TKRDriveType;
    procedure SetShowDrives(const Value: TKRCBDriveType);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Change; override;
  public
    procedure DoTimer; stdcall;
    procedure UpdateDrivers;
    procedure UpdateSize;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property VolumeName: String read FVolumeName;
    property FileSystemName: String read FFileSystemName;
    property DriveSize: Int64 read FDriveSize;
    property DriveFreeSize: Int64 read FDriveFreeSize;
    function AddDrive(AName: String; AIcon: TIcon):integer;
  published
    property Drive: Char read FDrive write SetDrive;
    property ShowVolumeName: boolean read FShowVolumeName write SetShowVolumeName;
    property ShowFileSystemName: boolean read FShowFileSystemName write SetShowFileSystemName;
    property OnDriveChanged: TNotifyEvent read FDriveChanged write FDriveChanged;
    property Timer: TKRTimer read FTimer write SetTimer;
    property DriveType: TKRDriveType read GetDriveType;
    property ShowDrives: TKRCBDriveType read FShowDrives write SetShowDrives;
    property Align;
    property Anchors;
    property AutoComplete;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
    property OnGetAdditionDriveSize: TKRDCBGetAdditionDriveSize read FOnGetAdditionDriveSize write FOnGetAdditionDriveSize;
  end;


implementation

uses Funcs;

{ TKRDriveComboBox }

function TKRDriveComboBox.AddDrive(AName: String; AIcon: TIcon): integer;
begin
  Result:=Length(FADNames);
  SetLength(FADNames,Result+1);
  SetLength(FADIcons,Result+1);
  FADNames[Result]:=AName;
  if AIcon=nil then FADIcons[Result]:=nil else begin
    FADIcons[Result]:=TIcon.Create;
    FADIcons[Result].Assign(AIcon);
  end;
  FDrivers:='';
  FLastDrive:=#0;
  UpdateDrivers;
end;

procedure TKRDriveComboBox.Change;
var
  s: String;
begin
  s:=Items[ItemIndex];
  if ItemIndex<length(FDrivers) div 4 then FDrive:=s[1] else FDrive:=Chr(ItemIndex+1-(length(FDrivers)div 4));
  UpdateDrivers;
  inherited;
end;

constructor TKRDriveComboBox.Create(AOwner: TComponent);
var
  s: String;
begin
  inherited;
  Style := csOwnerDrawFixed;
  GetDir(0, s);
  s:=ExtractFileDrive(s);
  FDrive:=s[1];
  FLastDrive:=#0;
  FDrivers:='';
  FDrivers_:='';
  FShowFileSystemName:=true;
  FShowVolumeName:=true;
  ItemHeight:=17;
  FInited:=false;
  FIsWnd:=false;
  FShowDrives:=cbdtAll;
end;

procedure TKRDriveComboBox.CreateWnd;
begin
  inherited;
  FIsWnd:=true;
  UpdateDrivers;
end;

destructor TKRDriveComboBox.Destroy;
var i: integer;
begin
  if(Assigned(FTimer))then begin
    while FTimer.Working do Application.ProcessMessages;
    FTimer.DelMon(Self);
  end;
  for i := 0 to Length(FADIcons)-1 do FADIcons[i].Free;
  inherited;
end;

procedure TKRDriveComboBox.DoTimer;
begin
  UpdateDrivers;
  UpdateSize;
end;

procedure TKRDriveComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  s: AnsiString;
  ss: String;
  fi: TSHFileInfoA;
  icn: TIcon;
  w,n: integer;
begin
  inherited;
  w:=16;
  Canvas.FillRect(Rect);
  if Index<length(FDrivers) div 4 then begin
    ss:=Items[Index];
    if(Length(ss)>0)and(ss[1]<>#0)then begin
      s:=WideStringToString(ss[1]+':\',1251);
      icn:=TIcon.Create;
      SHGetFileInfoA(PAnsiChar(s), 0, fi, SizeOf(fi), SHGFI_ICON or SHGFI_SMALLICON);
      icn.Handle:=fi.hIcon;
      Canvas.Draw(Rect.Left+3,(Rect.Top+Rect.Bottom-icn.Height) div 2,icn);
      w:=icn.Width;
      icn.Free;
    end;
  end else begin
    n:=(length(FDrivers)div 4)-Index;
    if FADIcons[n]<>nil then begin
      w:=FADIcons[n].Width;
      Canvas.Draw(Rect.Left+3,(Rect.Top+Rect.Bottom-FADIcons[n].Height) div 2,FADIcons[n]);      
    end;    
  end;
  Rect.Left := Rect.Left + w + 6;
    DrawText(Canvas.Handle, Items[Index], -1, Rect,
             DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;

procedure TKRDriveComboBox.DriveChanged;
begin
  if Assigned(FDriveChanged) then FDriveChanged(Self);
end;

procedure TKRDriveComboBox.GetDriveInfo(ADrive: Char; out AVolumeName,
  AFileSystemName: String);
var
  _VolumeName,
  _FileSystemName : array [0..MAX_PATH-1] of Char;
  FVolumeSerialNo : DWord;
  FMaxComponentLength,FFileSystemFlags: Cardinal;
begin
  _VolumeName:=#$00;
  _FileSystemName:=#$00;
  GetVolumeInformation(PChar(ADrive+':\'),_VolumeName,MAX_PATH,@FVolumeSerialNo,
    FMaxComponentLength,FFileSystemFlags,_FileSystemName,MAX_PATH);
  SetString(AVolumeName, _VolumeName, StrLen(_VolumeName));
  SetString(AFileSystemName, _FileSystemName, StrLen(_FileSystemName));
end;

function TKRDriveComboBox.GetDriveType: TKRDriveType;
begin
  case windows.GetDriveType(PChar(FDrive+':\')) of
    DRIVE_REMOVABLE: result:=drvtpRemovable;
    DRIVE_FIXED: result:=drvtpFixed;
    DRIVE_REMOTE: result:=drvtpRemote;
    DRIVE_CDROM: result:=drvtpCDRom;
    DRIVE_RAMDISK: result:=drvtpRamDisk
    else result:=drvtpUnknown;
  end;
end;

procedure TKRDriveComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if (AComponent = FTimer)then begin
      FTimer:=nil;
    end;
end;

procedure TKRDriveComboBox.SetDrive(const Value: Char);
var
  i: Integer;
  b: boolean;
  ch: Char;
  s: String;
begin
  b:=false;
  if ord(Value)<32 then begin
    FDrive:=Value;
    UpdateDrivers;
  end else begin
    s:=Value;
    s:=UpperCase(s);
    if Length(s)>0 then begin
      ch:=s[1];
      for i := 1 to length(FDrivers) do
        if ch=FDrivers_[i] then begin
          b:=true;
          break;
        end;
      if b then begin
        FDrive:=ch;
        UpdateDrivers;
      end;
    end;
  end;
end;

procedure TKRDriveComboBox.SetShowDrives(const Value: TKRCBDriveType);
begin
  FShowDrives := Value;
  UpdateDrivers;
end;

procedure TKRDriveComboBox.SetShowFileSystemName(const Value: boolean);
begin
  FShowFileSystemName := Value;
  FDrivers:='';
  FLastDrive:=#0;
  UpdateDrivers;
end;

procedure TKRDriveComboBox.SetShowVolumeName(const Value: boolean);
begin
  FShowVolumeName := Value;
  FDrivers:='';
  FLastDrive:=#0;
  UpdateDrivers;
end;

procedure TKRDriveComboBox.SetTimer(const Value: TKRTimer);
begin
  if FTimer<>Value then begin
    if(Assigned(FTimer))then begin
      while FTimer.Working do Application.ProcessMessages;
      FTimer.DelMon(Self);
    end;
    FTimer := Value;
    if(Assigned(FTimer))then begin
      FTimer.FreeNotification(Self);
      while FTimer.Working do Application.ProcessMessages;
      FTimer.AddMon(Self);
    end;
  end;
end;

procedure TKRDriveComboBox.UpdateDrivers;
var
  s,s0: AnsiString;
  ss, _vn, _fs: String;
  res: Cardinal;
  i,n: integer;
  b: boolean;

  function _drvTp(_tp: TKRCBDriveType): Cardinal;
  begin
    result:=DRIVE_UNKNOWN;
    case _tp of
      cbdtRemovable: result:=DRIVE_REMOVABLE;
      cbdtFixed: result:=DRIVE_FIXED;
      cbdtRemote: result:=DRIVE_REMOTE;
      cbdtCDRom: result:=DRIVE_CDROM;
      cbdtRamDisk: result:=DRIVE_RAMDISK;
    end;
  end;

begin
  if not FIsWnd then exit;
  SetLength(s,1024);
  res:=GetLogicalDriveStringsA(1024,PAnsiChar(s));
  b:=false;
  if res=Cardinal(length(FDrivers))then begin
    b:=true;
    for i := 1 to res do
      if s[i]<>FDrivers[i] then begin
        b:=false;
        break;
      end;
  end;
  if not b then begin
    SetLength(FDrivers,res);
    for i := 1 to res do FDrivers[i]:=s[i];
    FDrivers_:='';
    Items.BeginUpdate;
    Items.Clear;
    if res>0 then begin
      i:=1;
      s0:='';
      while true do begin
        if(s[i]=#0)then begin
          if(s0='')then break else begin
            ss:=StringToWideString(s0,1251);
            if(FShowDrives=cbdtAll)or(windows.GetDriveType(PChar(ss[1]+':\'))=_drvTp(FShowDrives))then begin
              ss:=ss[1]+':';
              FDrivers_:=FDrivers_+ss[1];
              GetDriveInfo(ss[1],_vn,_fs);
              if FShowFileSystemName and(_fs<>'')then ss:=ss+' '+_fs;
              if FShowVolumeName and(_vn<>'')then ss:=ss+' ['+_vn+']';
              Items.Add(ss);
            end;
            s0:='';
          end;
        end else s0:=s0+s[i];
        inc(i);
      end;
    end;
    for I := 0 to Length(FADNames)-1 do Items.Add(FADNames[i]);
    Items.EndUpdate;
  end;
  if FLastDrive<>FDrive then begin
    n:=-1;
    if ord(FDrive)<32 then begin
      if ord(FDrive)<=Length(FADNames) then
        n:=(length(FDrivers)div 4)+ord(FDrive)-1;
    end else
      for i := 1 to Length(FDrivers_) do
        if FDrivers_[i]=FDrive then begin
          n:=i-1;
          break;
        end;
    if(n=-1)then
      if(Length(FDrivers_)>0)then begin
        n:=0;
        FDrive:=FDrivers_[1];
      end else begin
        FDrive:=#0;
        FVolumeName:='';
        FFileSystemName:='';
      end;
    if Ord(FDrive)>32 then GetDriveInfo(FDrive,FVolumeName,FFileSystemName);
    ItemIndex:=n;
    FLastDrive:=FDrive;
    UpdateSize;
    if FInited then DriveChanged else FInited:=true;
  end;
end;

procedure TKRDriveComboBox.UpdateSize;
var
  RootPath: array[0..4] of Char;
  RootPtr: PChar;
  TotalSpace, FreeSpaceAvailable: Int64;
begin
  FDriveSize:=0;
  FDriveFreeSize:=0;
  if Ord(FDrive)>32 then begin
    RootPath[0] := FDrive;
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
    if GetDiskFreeSpaceEx(RootPtr, FreeSpaceAvailable, TotalSpace, nil)then begin
      FDriveSize:=TotalSpace;
      FDriveFreeSize:=FreeSpaceAvailable;
    end;
  end else if(Ord(FDrive)>0)and(assigned(FOnGetAdditionDriveSize))then
    FOnGetAdditionDriveSize(Self,FDriveSize,FDriveFreeSize);
end;

end.
