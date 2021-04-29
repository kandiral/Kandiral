(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRFileList                                                                *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRFileList;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses Windows, CommCtrl, ComCtrls, Classes, Controls, SysUtils, Graphics,
  ShellAPI, Types, Forms, StrUtils, KRListView;

type
  TKRDriveHistory = record
    Drive: Char;
    Directory: String;
  end;

  TKRFileSort = (krfsNone, krfsDirectoryOnTop, krfsAll);

  TKRFile = class(TPersistent)
  private
    FSize: Int64;
    FTypeName: String;
    FAttr: Integer;
    FName: String;
    FFullName: String;
    FExt: String;
    FSysIconIndex: integer;
    FTag: integer;
    FCreationTime: TFileTime;
    FLastAccessTime: TFileTime;
    FLastWriteTime: TFileTime;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create();overload;
    constructor Create(ATag: integer);overload;
    constructor Create(AFullName, ATypeName: String; ASize: Int64;
      AAttr: integer; ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
      ASysIconIndex: integer);overload;
    property FullName: String read FFullName;
    property Name: String read FName;
    property TypeName: String read FTypeName write FTypeName;
    property Size: Int64 read FSize;
    property Attr: Integer read FAttr;
    property CreationTime: TFileTime read FCreationTime;
    property LastAccessTime: TFileTime read FLastAccessTime;
    property LastWriteTime: TFileTime read FLastWriteTime;
    property Ext: String read FExt;
    property SysIconIndex: integer read FSysIconIndex write FSysIconIndex;
    property Tag: integer read FTag write FTag;
  end;
  PKRFile=^TKRFile;

  TKRFileList = class(TKRListView)
  private
    FUpdating: boolean;
    FDrive: Char;
    FDirectory: String;
    FInited: boolean;
    FFiles: array of TKRFile;
    FDriveHistory: array of TKRDriveHistory;
    FOnKeyPress: TKeyPressEvent;
    FFileSort: TKRFileSort;
    FOnColumnClick: TLVColumnClickEvent;
    FOnCompare: TLVCompareEvent;
    FSortCol: integer;
    FSortBack: boolean;
    FPathChange: TNotifyEvent;
    FShowType: boolean;
    FShowSize: boolean;
    FShowDate: boolean;
    FShowDirPoints: boolean;
    FEndUpdateFiles: TNotifyEvent;
    FBeginUpdateFiles: TNotifyEvent;
    FAllFilesSize: int64;
    FOpenByDblClick: boolean;
    FExts: TStringList;
    FSizeOfSHFileInfoA: integer;
    procedure SetDrive(const Value: Char);
    procedure SetDirectory(const Value: String);
    function GetHistoryIndex(ADrive: Char): integer;
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure SetFileSort(const Value: TKRFileSort);
    procedure DoClmnClick(Sender: TObject; Column: TListColumn);
    procedure DoCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure UpColumnArrow;
    procedure SetShowDate(const Value: boolean);
    procedure SetShowSize(const Value: boolean);
    procedure SetShowType(const Value: boolean);
    procedure UpColumns;
    procedure SetShowDirPoints(const Value: boolean);
    procedure SetSortBack(const Value: boolean);
    procedure SetSortCol(const Value: integer);
  protected
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure Change(Item: TListItem; Change: Integer); override;
    function IsItemsStored: Boolean;override;
    function CanChange(Item: TListItem; Change: Integer): Boolean; override;
  public
    procedure UpdateFiles;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure DirectoryExit;
    property Extentions: TStringList read FExts;
  published
    property Drive: Char read FDrive write SetDrive;
    property Directory: String read FDirectory write SetDirectory;
    property FileSort: TKRFileSort read FFileSort write SetFileSort default krfsDirectoryOnTop;
    property OnPathChange: TNotifyEvent read FPathChange write FPathChange;
    property ShowSize: boolean read FShowSize write SetShowSize;
    property ShowType: boolean read FShowType write SetShowType;
    property ShowDate: boolean read FShowDate write SetShowDate;
    property ShowDirPoints: boolean read FShowDirPoints write SetShowDirPoints;
    property SortCol: integer read FSortCol write SetSortCol;
    property SortBack: boolean read FSortBack write SetSortBack;
    property AllFilesSize: int64 read FAllFilesSize;
    property OnBeginUpdateFiles: TNotifyEvent read FBeginUpdateFiles write FBeginUpdateFiles;
    property OnEndUpdateFiles: TNotifyEvent read FEndUpdateFiles write FEndUpdateFiles;
    property OpenByDblClick: boolean read FOpenByDblClick write FOpenByDblClick default false;
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property Groups;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property GroupView default False;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick
      write FOnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnCreateItemClass;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnItemChecked;
    property OnStartDock;
    property OnStartDrag;
    property OnScrollH;
    property OnScrollV;
    property OnColumnBeginResize;
    property OnColumnResized;
    property OnColumnResizing;
  end;

implementation

uses Funcs, KRWindows;

{ TKRFileList }

function TKRFileList.CanChange(Item: TListItem; Change: Integer): Boolean;
var
  fl: TKRFile;
begin
  result:=inherited CanChange(Item,Change);
  if(Change<>LVIF_TEXT)or(not FInited)or(FUpdating)or(Item.Data=nil)then exit;
  fl:=Item.Data;
  if(Item.Index=0)and(fl.Attr and faDirectory <> 0)and(fl.Name='..') then begin
    Item.Caption:='..';
    result:=false;
  end;
end;

procedure TKRFileList.Change(Item: TListItem; Change: Integer);
var
  fl: TKRFile;
  _newFile: String;
  b: boolean;
begin
  if(not FInited)or(FUpdating)or(Item.Data=nil)then exit;
  fl:=Item.Data;
  if(Change=LVIF_TEXT)and(fl.FName<>Item.Caption)then begin
    b:=true;
    _newFile:=ExtractFilePath(fl.FFullName)+Item.Caption;
    if(fl.Attr and faDirectory = 0) and FileExists(_newFile) then begin
      b:=AppMsgBoxQst('File already exists! Remove it?')=IDYES;
      if b then begin
        b:=false;
        try b:=DeleteFile(_newFile) finally end;
      end;
    end;
    if b then b:=SysUtils.RenameFile(fl.FFullName,_newFile);
    if b then begin
      fl.FFullName:=_newFile;
      fl.FName:=Item.Caption;
      AlphaSort;
    end else Item.Caption:=fl.Name
  end;

  inherited;
end;

constructor TKRFileList.Create(AOwner: TComponent);
var
  s: String;
  fi: TSHFileInfoA;
  pfi: PSHFileInfoA;
begin
  inherited;
  ViewStyle := vsReport;
  FExts:=TStringList.Create;
  FSizeOfSHFileInfoA:=SizeOf(SHFileInfoA);
  ShGetFileInfoA(PAnsiChar(WideStringToString(KREnvGetValue('TEMP'),1251)), 0, fi,
    FSizeOfSHFileInfoA, SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_TYPENAME);
  new(pfi);
  pfi^:=fi;
  FExts.AddObject('\',TObject(Integer(pfi)));
  GetDir(0, FDirectory);
  s:=ExtractFileDrive(FDirectory);
  FDrive:=s[1];
  SetLength(FDriveHistory,1);
  FDriveHistory[0].Drive:=FDrive;
  FDriveHistory[0].Directory:=FDirectory;
  SmallImages:=TImageList.Create(Self);
  SmallImages.Handle:=ShGetFileInfoA(nil, 0, fi,
      FSizeOfSHFileInfoA, SHGFI_SMALLICON or SHGFI_ICON
      or SHGFI_SYSICONINDEX);
  LargeImages:=TImageList.Create(Self);
  LargeImages.Handle:=ShGetFileInfoA(nil, 0, fi,
      FSizeOfSHFileInfoA, SHGFI_LARGEICON or SHGFI_ICON
      or SHGFI_SYSICONINDEX);
  FInited:=false;
  FFileSort:=krfsDirectoryOnTop;
  RowSelect:=true;
  FShowSize:=true;
  FShowType:=true;
  FShowDate:=true;
  FShowDirPoints:=true;
  FOpenByDblClick:=false;
  inherited OnKeyPress:=DoKeyPress;
  inherited OnColumnClick:=DoClmnClick;
  inherited OnCompare:=DoCompare;
end;

procedure TKRFileList.CreateWnd;
begin
  inherited;
  UpColumns;
end;

procedure TKRFileList.DblClick;
begin
  if(ItemFocused<>nil)then
    if(ItemFocused.Caption='..')then begin
      DirectoryExit;
      exit;
    end else if TKRFile(ItemFocused.Data).Attr and faDirectory <> 0 then begin
      SetDirectory(FDirectory+'\'+TKRFile(ItemFocused.Data).FName);
      exit;
    end else if FOpenByDblClick then begin
      ShellExecute(0,'open', PChar(TKRFile(ItemFocused.Data).FFullName), nil, nil, SW_SHOWNORMAL);
      exit;
    end;
  inherited;
end;

destructor TKRFileList.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FFiles)-1 do FFiles[i].Free;
  for I := 0 to FExts.Count-1 do Dispose(PSHFileInfoA(Integer(FExts.Objects[i])));
  FExts.Free;
  inherited;
end;

procedure TKRFileList.DirectoryExit;
var
  s: String;
begin
  s:=ExtractFilePath(FDirectory);
  SetDirectory(LeftStr(s,Length(s)-1));
end;

procedure TKRFileList.DoClmnClick(Sender: TObject; Column: TListColumn);
begin
  if (FSortCol = Column.Tag) then FSortBack := not FSortBack else FSortBack:=False;
  FSortCol := Column.Tag;
  AlphaSort;
  UpColumnArrow;
  if Assigned(FOnColumnClick) then FOnColumnClick(Self,Column);
end;

procedure TKRFileList.DoCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  fl1,fl2: TKRFile;
begin
  if Assigned(FOnCompare) then begin
    FOnCompare(Self,Item1,Item2,Data,Compare);
    exit;
  end;

  if FFileSort=krfsNone then exit;

  if Item1.Data=nil then exit;
  fl1:=TKRFile(Item1.Data);
  if Item2.Data=nil then exit;
  fl2:=TKRFile(Item2.Data);

  if fl1.Name='..' then begin
    Compare:=-1;
    exit;
  end else if fl2.Name='..' then begin
    Compare:=1;
    exit;
  end;


  case FSortCol of
    1: begin
      if FFileSort=krfsAll then begin
        if FSortBack then Compare := CompareText(Item2.Caption,Item1.Caption)
        else Compare := CompareText(Item1.Caption,Item2.Caption);
      end else begin
        if fl1.FAttr and faDirectory <> 0 then
          if fl2.FAttr and faDirectory <> 0 then begin
            if FSortBack then Compare := CompareText(Item2.Caption,Item1.Caption)
            else Compare := CompareText(Item1.Caption,Item2.Caption);
          end else Compare:=-1
        else if fl2.FAttr and faDirectory <> 0 then Compare:=1 else begin
          if FSortBack then Compare := CompareText(Item2.Caption,Item1.Caption)
          else Compare := CompareText(Item1.Caption,Item2.Caption);
        end;
      end;
    end;
    2: begin
      if FFileSort=krfsAll then begin
        if FSortBack then Compare := fl2.Size-fl1.Size
        else Compare := fl1.Size-fl2.Size
      end else begin
        if fl1.FAttr and faDirectory <> 0 then
          if fl2.FAttr and faDirectory <> 0 then begin
            if FSortBack then Compare := fl2.Size-fl1.Size
            else Compare := fl1.Size-fl2.Size
          end else Compare:=-1
        else if fl2.FAttr and faDirectory <> 0 then Compare:=1 else begin
          if FSortBack then Compare := fl2.Size-fl1.Size
          else Compare := fl1.Size-fl2.Size
        end;
      end;
    end;
    3: begin
      if FFileSort=krfsAll then begin
        if FSortBack then Compare := CompareText(fl2.Ext,fl1.Ext)
        else Compare := CompareText(fl1.Ext,fl2.Ext);
      end else begin
        if fl1.FAttr and faDirectory <> 0 then
          if fl2.FAttr and faDirectory <> 0 then begin
            if FSortBack then Compare := CompareText(fl2.Ext,fl1.Ext)
            else Compare := CompareText(fl1.Ext,fl2.Ext);
          end else Compare:=-1
        else if fl2.FAttr and faDirectory <> 0 then Compare:=1 else begin
          if FSortBack then Compare := CompareText(fl2.Ext,fl1.Ext)
          else Compare := CompareText(fl1.Ext,fl2.Ext);
        end;
      end;
    end;
    4: begin
      if FFileSort=krfsAll then begin
        if FSortBack then Compare := Round((FileTimeToDateTime(fl2.FLastWriteTime)-FileTimeToDateTime(fl1.FLastWriteTime))*86400000)
        else Compare := Round((FileTimeToDateTime(fl1.FLastWriteTime)-FileTimeToDateTime(fl2.FLastWriteTime))*86400000)
      end else begin
        if fl1.FAttr and faDirectory <> 0 then
          if fl2.FAttr and faDirectory <> 0 then begin
            if FSortBack then Compare := Round((FileTimeToDateTime(fl2.FLastWriteTime)-FileTimeToDateTime(fl1.FLastWriteTime))*86400000)
            else Compare := Round((FileTimeToDateTime(fl1.FLastWriteTime)-FileTimeToDateTime(fl2.FLastWriteTime))*86400000)
          end else Compare:=-1
        else if fl2.FAttr and faDirectory <> 0 then Compare:=1 else begin
          if FSortBack then Compare := Round((FileTimeToDateTime(fl2.FLastWriteTime)-FileTimeToDateTime(fl1.FLastWriteTime))*86400000)
          else Compare := Round((FileTimeToDateTime(fl1.FLastWriteTime)-FileTimeToDateTime(fl2.FLastWriteTime))*86400000)
        end;
      end;
    end;
  end;
end;

procedure TKRFileList.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if(Key=#13)and(ItemFocused<>nil)then begin
    if (ItemFocused.Caption='..')then DirectoryExit
    else if TKRFile(ItemFocused.Data).Attr and faDirectory <> 0 then
      SetDirectory(FDirectory+'\'+TKRFile(ItemFocused.Data).FName)
    else
      ShellExecute(0,'open', PChar(TKRFile(ItemFocused.Data).FFullName), nil, nil, SW_SHOWNORMAL);
    Key:=#0;
  end;
  if Assigned(FOnKeyPress) then FOnKeyPress(Self,Key);
end;

function TKRFileList.GetHistoryIndex(ADrive: Char): integer;
var
  i: integer;
begin
  Result:=-1;
  for i := 0 to Length(FDriveHistory) do
    if FDriveHistory[i].Drive=ADrive then begin
      Result:=i;
      break;
    end;

end;

function TKRFileList.IsItemsStored: Boolean;
begin
  Result:=false;
end;

procedure TKRFileList.SetDirectory(const Value: String);
var
  s: String;

  procedure UpdateHistory;
  var
    i: integer;
  begin
    i:=GetHistoryIndex(FDrive);
    if i=-1 then begin
      SetLength(FDriveHistory,Length(FDriveHistory)+1);
      FDriveHistory[Length(FDriveHistory)-1].Drive:=FDrive;
      FDriveHistory[Length(FDriveHistory)-1].Directory:=FDirectory;
    end else FDriveHistory[i].Directory:=FDirectory;
  end;

begin
  if FUpdating then exit;
  if Length(Value)=0 then begin
    GetDir(0, FDirectory);
    s:=ExtractFileDrive(FDirectory);
    FDrive:=s[1];
    UpdateHistory;
    UpdateFiles;
    exit;
  end else if Length(Value)=1 then s:=Value+':' else
  if(Length(Value)=2)and(Value[2]<>#58)then exit else
  if(Length(Value)=3)and((Value[2]<>#58)or(Value[3]<>#92))then exit
  else s:=Value;
  if s[Length(s)]=#92 then s:=LeftStr(s,Length(s)-1);
  if DirectoryExists(s) then begin
    FDirectory := UpperCase(''+s[1])+RightStr(s,Length(s)-1);
    FDrive:=FDirectory[1];
    UpdateHistory;
    UpdateFiles;
  end;
  if Assigned(FPathChange) then FPathChange(Self);  
end;

procedure TKRFileList.SetDrive(const Value: Char);
var
  s: String;
  i: integer;
begin
  s:=UpperCase(Value+':');
  if DirectoryExists(s) then begin
    FDrive := s[1];
    i:=GetHistoryIndex(FDrive);
    if i=-1 then SetDirectory(FDrive+':')
    else SetDirectory(FDriveHistory[i].Directory);
  end;
end;

procedure TKRFileList.SetFileSort(const Value: TKRFileSort);
begin
  FFileSort := Value;
  AlphaSort;
end;

procedure TKRFileList.SetShowDate(const Value: boolean);
begin
  FShowDate := Value;
  UpColumns;
end;

procedure TKRFileList.SetShowDirPoints(const Value: boolean);
begin
  FShowDirPoints := Value;
  UpdateFiles;
end;

procedure TKRFileList.SetShowSize(const Value: boolean);
begin
  FShowSize := Value;
  UpColumns;
end;

procedure TKRFileList.SetShowType(const Value: boolean);
begin
  FShowType := Value;
  UpColumns;
end;

procedure TKRFileList.SetSortBack(const Value: boolean);
begin
  FSortBack := Value;
  AlphaSort;
  UpColumnArrow;
end;

procedure TKRFileList.SetSortCol(const Value: integer);
begin
  case Value of
  1: FSortCol := Value;
  2: if FShowSize then FSortCol := Value else exit;
  3: if FShowType then FSortCol := Value else exit;
  4: if FShowDate then FSortCol := Value else exit
  else exit;
  end;
  AlphaSort;
  UpColumnArrow;
end;

procedure TKRFileList.UpColumnArrow;
var
  HeaderHndl: HWND;
  Item: THDItem;
  i: integer;
begin
  HeaderHndl := ListView_GetHeader(Handle);
  FillChar(Item, SizeOf(Item), 0);
  Item.Mask := HDI_FORMAT;
  for i := 0 to Columns.Count-1 do begin
    Header_GetItem(HeaderHndl, 0, Item);
    Item.fmt:= Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
    if(Columns[i].Tag = FSortCol) then begin
      if FSortBack then Item.fmt:= Item.fmt or HDF_SORTDOWN else Item.fmt:= Item.fmt or HDF_SORTUP;
    end;
    Header_SetItem(HeaderHndl, i, Item);
  end;
end;

procedure TKRFileList.UpColumns;
var
  lc: TListColumn;
begin
  inherited Items.Clear;
  Columns.Clear;
  lc := Columns.Add;
  lc.Caption := 'Имя';
  lc.Width:=150;
  lc.Tag:=1;
  if FShowSize then begin
    lc := Columns.Add;
    lc.Caption := 'Размер';
    lc.Width:=80;
    lc.Tag:=2;
  end;
  if FShowType then begin
    lc := Columns.Add;
    lc.Caption := 'Тип';
    lc.Width:=100;
    lc.Tag:=3;
  end;
  if FShowDate then begin
    lc := Columns.Add;
    lc.Caption := 'Дата изминения';
    lc.Width:=120;
    lc.Tag:=4;
  end;
  FSortCol:=1;
  FSortBack:=false;
  UpdateFiles;
end;

procedure TKRFileList.UpdateFiles;
var
  sr: TSearchRec;
  li: TListItem;
  fi: TSHFileInfoA;
  _as: AnsiString;
  i,n,z: integer;
  pfi: PSHFileInfoA;
begin
  if(FUpdating)then exit;
  FUpdating:=true;

  inherited Items.BeginUpdate;
  inherited Items.Clear;

  for i := 0 to Length(FFiles)-1 do FFiles[i].Free;
  SetLength(FFiles,0);

  if Assigned(FBeginUpdateFiles) then FBeginUpdateFiles(self);
  n:=0;
  FAllFilesSize:=0;
  if FindFirst(FDirectory + '\*.*', faAnyFile, sr) = 0 then
    repeat
      if(sr.Name='.')or((sr.Name='..')and(Length(FDirectory)<4))then continue;
      if(sr.Name='..')and(not FShowDirPoints)then continue;
      li := inherited Items.Add;
      li.Data:=nil;
      li.Caption := sr.name;
      SetLength(FFiles,n+1);
      FFiles[n]:=TKRFile.Create;
      FFiles[n].FFullName:=FDirectory+'\'+sr.name;
      FFiles[n].FName:=sr.Name;
      FFiles[n].FLastAccessTime:=sr.FindData.ftLastAccessTime;
      FFiles[n].FLastWriteTime:=sr.FindData.ftLastWriteTime;
      FFiles[n].FCreationTime:=sr.FindData.ftCreationTime;
      FFiles[n].FAttr:=sr.Attr;
      FFiles[n].FSize:=0;
      FFiles[n].FExt:='';
      if(sr.Name='..')then  li.ImageIndex :=-1
      else begin
        _as:=WideStringToString(FFiles[n].FFullName,1251);
        if sr.Attr and faDirectory <> 0 then begin
          fi:=PSHFileInfoA(Integer(FExts.Objects[0]))^;
          if FShowSize then li.SubItems.Add('');
        end else begin
          FFiles[n].FExt:=LowerCase(ExtractFileExt(FFiles[n].FName));
          z:=FExts.IndexOf(FFiles[n].FExt);
          if z=-1 then begin
            ShGetFileInfoA(PAnsiChar(WideStringToString(FFiles[n].FullName,1251)), 0, fi,
              FSizeOfSHFileInfoA, SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_TYPENAME);
            new(pfi);
            pfi^:=fi;
            FExts.AddObject(FFiles[n].FExt,TObject(Integer(pfi)));
          end else fi:=PSHFileInfoA(Integer(FExts.Objects[z]))^;
          FFiles[n].FSize:=sr.Size;
          FAllFilesSize:=FAllFilesSize+sr.Size;
          if FShowSize then begin
            if sr.Size < 1024 then li.SubItems.Add(IntToStr(sr.Size) + ' BT')
            else if sr.Size < 1048576 then li.SubItems.Add(IntToStr(sr.Size div 1024) + ' KB')
            else if sr.Size < 1073741824 then li.SubItems.Add(IntToStr(sr.Size div 1048576) + ' MB')
            else li.SubItems.Add(IntToStr(sr.Size div 1073741824) + ' GB');
          end;
        end;
        li.ImageIndex := fi.iIcon;
        FFiles[n].FTypeName:=StringToWideString(fi.szTypeName,1251);
        if FShowType then li.SubItems.Add(FFiles[n].FTypeName);
        if FShowDate then li.SubItems.Add(DateTimeToStr(FileTimeToDateTime(FFiles[n].FLastWriteTime)));
      end;
      li.Data:=FFiles[n];
      inc(n);
    until FindNext(sr) <> 0;
  FindClose(sr);
  inherited Items.EndUpdate;
  AlphaSort;
  UpColumnArrow;
  FUpdating:=false;
  if Assigned(FEndUpdateFiles) then FEndUpdateFiles(self);
  if inherited Items.Count>0 then ItemIndex:=0;
  FInited:=true;
end;

{ TKRFile }

procedure TKRFile.AssignTo(Dest: TPersistent);
var AFile: TKRFile;
begin
  inherited;
  if Dest is TKRFile then begin
    AFile:=TKRFile(Dest);
    FFullName:=AFile.FullName;
    FName:=AFile.Name;
    FTypeName:=AFile.TypeName;
    FAttr:=AFile.Attr;
    FLastAccessTime:=AFile.FLastAccessTime;
    FLastWriteTime:=AFile.FLastWriteTime;
    FCreationTime:=AFile.FCreationTime;
    FSysIconIndex:=AFile.SysIconIndex;
    FSize:=AFile.Size;
    FExt:=AFile.Ext;
    FTag:=AFile.Tag;
  end;
end;

constructor TKRFile.Create(AFullName, ATypeName: String; ASize: Int64;
  AAttr: integer; ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime; ASysIconIndex: integer);
begin
  FFullName:=AFullName;
  FName:=ExtractFileName(AFullName);
  FTypeName:=ATypeName;
  FAttr:=AAttr;
  FLastAccessTime:=ALastAccessTime;
  FLastWriteTime:=ALastWriteTime;
  FCreationTime:=ACreationTime;
  FSysIconIndex:=ASysIconIndex;
  if AAttr and faDirectory <> 0 then begin
    FSize:=0;
    FExt:='';
  end else begin
    FSize:=ASize;
    FExt:=ExtractFileExt(FName);
  end;
  FTag:=0;
end;

constructor TKRFile.Create;
begin
  FFullName:='';
  FName:='';
  FTypeName:='';
  FAttr:=0;
  FLastAccessTime.dwLowDateTime:=0;
  FLastAccessTime.dwHighDateTime:=0;
  FLastWriteTime.dwLowDateTime:=0;
  FLastWriteTime.dwHighDateTime:=0;
  FCreationTime.dwLowDateTime:=0;
  FCreationTime.dwHighDateTime:=0;
  FSysIconIndex:=-1;
  FSize:=0;
  FExt:='';
  FTag:=0;
end;

constructor TKRFile.Create(ATag: integer);
begin
  FFullName:='';
  FName:='';
  FTypeName:='';
  FAttr:=0;
  FLastAccessTime.dwLowDateTime:=0;
  FLastAccessTime.dwHighDateTime:=0;
  FLastWriteTime.dwLowDateTime:=0;
  FLastWriteTime.dwHighDateTime:=0;
  FCreationTime.dwLowDateTime:=0;
  FCreationTime.dwHighDateTime:=0;
  FSysIconIndex:=-1;
  FSize:=0;
  FExt:='';
  FTag:=ATag;
end;

end.
