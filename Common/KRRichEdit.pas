(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRRichEdit                                                                *)
(*  Ver.: 14.07.2020                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRRichEdit;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtActns,
    Vcl.StdCtrls, Vcl.Graphics, System.StrUtils, System.SysUtils;
  {$ELSE}
    Windows, Classes, Controls, ComCtrls, ExtActns, StdCtrls, Graphics,
    StrUtils, SysUtils;
  {$IFEND}


type
  TKRRichEditFontName = class(TRichEditAction)
  private
    FFontList: TComboBox;
    FEditor: TCustomRichEdit;
    FUpdating, FEntering, FKeyPressed: boolean;
    procedure SetFontList(const Value: TComboBox);
    procedure FontChange(Sender: TObject);
    procedure enter_(Sender: TObject);
    procedure exit_(Sender: TObject);
    procedure keyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property FontList: TComboBox read FFontList write SetFontList;
  end;

  TKRRichEditFontSize = class(TRichEditAction)
  private
    FEditor: TCustomRichEdit;
    FSizeList: TComboBox;
    FUpdating, FEntering, FKeyPressed: boolean;
    procedure SizeChange(Sender: TObject);
    procedure SetSizeList(const Value: TComboBox);
    procedure enter_(Sender: TObject);
    procedure exit_(Sender: TObject);
    procedure keyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property SizeList: TComboBox read FSizeList write SetSizeList;
  end;

  TKRRichEditFontColor = class(TRichEditAction)
  private
    FEditor: TCustomRichEdit;
    FChange: TNotifyEvent;
    FColor: TColor;
    FUpdating: boolean;
    procedure SetColor(AColor: TColor);
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property Color: TColor read FColor write SetColor;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TKRRichEdit = class(TCustomRichEdit)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

var
  FRichEditModule: THandle = 0;

{ TKRRichEdit }

procedure TKRRichEdit.CreateParams(var Params: TCreateParams);
const
{$IFDEF UNICODE}
  OldRichEditClassName = 'RICHEDIT20W';
  NewRichEditClassName = 'RICHEDIT50W';
{$ELSE}
  OldRichEditClassName = 'RICHEDIT20A';
  NewRichEditClassName = 'RICHEDIT50A';
{$ENDIF}
  RichEditModuleName = 'MSFTEDIT.DLL';
begin
  if (FRichEditModule = 0) then begin
    FRichEditModule := LoadLibrary(RichEditModuleName);
    if FRichEditModule <= HINSTANCE_ERROR then FRichEditModule := 0;
  end;
  inherited CreateParams(Params);
  if (FRichEditModule <> 0) then
    CreateSubClass(Params, NewRichEditClassName);
end;

{ TKRRichEditFontName }

procedure TKRRichEditFontName.enter_(Sender: TObject);
begin
  FEntering:=true;
  FKeyPressed:=false;
end;

procedure TKRRichEditFontName.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomRichEdit then
    FEditor:=Target as TCustomRichEdit;
end;

procedure TKRRichEditFontName.exit_(Sender: TObject);
begin
  FEntering:=false;
  if FKeyPressed then FontChange(nil);
end;

procedure TKRRichEditFontName.FontChange(Sender: TObject);
var
  s: String;
  i,n: integer;
begin
  if FUpdating then exit;
  if FEntering then begin
    if FKeyPressed then exit;
    FEntering:=false;
  end;
  FUpdating:=true;
  if not Assigned(FEditor) then exit;
  s:=LowerCase(FFontList.Text);
  n:=-1;
  for I := 0 to FFontList.Items.Count-1 do
    if LowerCase(FFontList.Items[i])=s then begin
      n:=i;
      break;
    end;
  if n=-1 then FFontList.Text:=CurrText(FEditor).Name else begin
    FFontList.Text:=FFontList.Items[n];
    CurrText(FEditor).Name:=FFontList.Text;
  end;
  FEditor.SetFocus;
  FUpdating:=false;
end;

procedure TKRRichEditFontName.keyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=13 then begin
    FEntering:=false;
    FontChange(nil);
    key:=0;
  end else if key=27 then begin
    if Assigned(FEditor) then begin
      FEntering:=false;
      FFontList.Text:=CurrText(FEditor).Name;
      FEditor.SetFocus;
    end;
    key:=0;
  end else FKeyPressed:=true;
end;

procedure TKRRichEditFontName.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(AComponent=FontList)and(Operation=opRemove)then FontList:=nil;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure TKRRichEditFontName.SetFontList(const Value: TComboBox);
var
  DC: HDC;
begin
  if FFontList<>Value then begin
    if Assigned(FFontList) then begin
      FFontList.RemoveFreeNotification(Self);
      FFontList.OnChange:=nil;
      FFontList.OnEnter:=nil;
      FFontList.OnExit:=nil;
      FFontList.OnKeyDown:=nil;
    end;
    FFontList := Value;
    if Assigned(FFontList) then begin
      FFontList.FreeNotification(Self);
      FFontList.Clear;
      DC := GetDC(0);
      EnumFonts(DC, nil, @EnumFontsProc, Pointer(FFontList.Items));
      ReleaseDC(0, DC);
      FFontList.Sorted := True;
      FFontList.OnSelect:=FontChange;
      FFontList.OnEnter:=enter_;
      FFontList.OnExit:=exit_;
      FFontList.OnKeyDown:=keyDown;
    end;
  end;
end;

procedure TKRRichEditFontName.UpdateTarget(Target: TObject);
begin
  inherited UpdateTarget(Target);
  if FUpdating then exit;
  FUpdating:=true;
  if Assigned(FFontList) then begin
    FFontList.Enabled:=Target is TCustomRichEdit;
    if FFontList.Enabled then begin
      FEditor:=Target as TCustomRichEdit;
      FFontList.Text:=CurrText(FEditor).Name;
    end;
  end;
  FUpdating:=false;
end;

{ TKRRichEditFontSize }

procedure TKRRichEditFontSize.enter_(Sender: TObject);
begin
  FEntering:=true;
  FKeyPressed:=false;
end;

procedure TKRRichEditFontSize.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomRichEdit then
    FEditor:=Target as TCustomRichEdit;
end;

procedure TKRRichEditFontSize.exit_(Sender: TObject);
begin
  FEntering:=false;
  if FKeyPressed then SizeChange(nil);
end;

procedure TKRRichEditFontSize.keyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=13 then begin
    FEntering:=false;
    SizeChange(nil);
    key:=0;
  end else if key=27 then begin
    if Assigned(FEditor) then begin
      FEntering:=false;
      FSizeList.Text:=IntToStr(CurrText(FEditor).Size);
      FEditor.SetFocus;
    end;
    key:=0;
  end else FKeyPressed:=true;
end;

procedure TKRRichEditFontSize.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(AComponent=SizeList)and(Operation=opRemove)then SizeList:=nil;
end;

procedure TKRRichEditFontSize.SetSizeList(const Value: TComboBox);
begin
  if FSizeList<>Value then begin
    if Assigned(FSizeList) then begin
      FSizeList.RemoveFreeNotification(Self);
      FSizeList.OnChange:=nil;
      FSizeList.OnEnter:=nil;
      FSizeList.OnExit:=nil;
      FSizeList.OnKeyDown:=nil;
    end;
    FSizeList := Value;
    if Assigned(FSizeList) then begin
      FSizeList.FreeNotification(Self);
      FSizeList.Clear;
      FSizeList.Items.Add('8');
      FSizeList.Items.Add('9');
      FSizeList.Items.Add('10');
      FSizeList.Items.Add('11');
      FSizeList.Items.Add('12');
      FSizeList.Items.Add('14');
      FSizeList.Items.Add('16');
      FSizeList.Items.Add('18');
      FSizeList.Items.Add('20');
      FSizeList.Items.Add('22');
      FSizeList.Items.Add('24');
      FSizeList.Items.Add('26');
      FSizeList.Items.Add('28');
      FSizeList.Items.Add('36');
      FSizeList.Items.Add('48');
      FSizeList.Items.Add('72');
      FSizeList.OnSelect:=SizeChange;
      FSizeList.OnEnter:=enter_;
      FSizeList.OnExit:=exit_;
      FSizeList.OnKeyDown:=keyDown;
    end;
  end;
end;

procedure TKRRichEditFontSize.SizeChange(Sender: TObject);
var
  ta: TTextAttributes;
begin
  if FUpdating then exit;
  if FEntering then begin
    if FKeyPressed then exit;
    FEntering:=false;
  end;
  FUpdating:=true;
  if not Assigned(FEditor) then exit;
  ta:=CurrText(FEditor);
  ta.Size:=StrToIntDef(FSizeList.Text,ta.Size);
  FSizeList.Text:=IntToStr(ta.Size);
  FEditor.SetFocus;
  FUpdating:=false;
end;

procedure TKRRichEditFontSize.UpdateTarget(Target: TObject);
var s: String;
begin
  inherited UpdateTarget(Target);
  if FUpdating then exit;
  FUpdating:=true;
  if Assigned(FSizeList) then begin
    FSizeList.Enabled:=Target is TCustomRichEdit;
    if FSizeList.Enabled then begin
      FEditor:=Target as TCustomRichEdit;
      s:=IntToStr(CurrText(FEditor).Size);
      if(s<>FSizeList.Text)then FSizeList.Text:=s;
    end;
  end;
  FUpdating:=false;
end;

{ TKRRichEditFontColor }

procedure TKRRichEditFontColor.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomRichEdit then begin
    FEditor:=Target as TCustomRichEdit;
    FColor:=CurrText(FEditor).Color;
  end;
end;

procedure TKRRichEditFontColor.SetColor(AColor: TColor);
begin
  if FUpdating then exit;
  FUpdating:=true;
  FColor:=AColor;
  if Assigned(FEditor) then begin
    CurrText(FEditor).Color:=FColor;
    FEditor.SetFocus;
  end;
  FUpdating:=false;
end;

procedure TKRRichEditFontColor.UpdateTarget(Target: TObject);
begin
  inherited UpdateTarget(Target);
  if FUpdating then exit;
  FUpdating:=true;
  Enabled:=Target is TCustomRichEdit;
  if Enabled then begin
    FEditor:=Target as TCustomRichEdit;
    FColor:=CurrText(FEditor).Color;
  end;
  if Assigned(FChange) then FChange(Self);
  FUpdating:=false;
end;

initialization

finalization
  if (FRichEditModule <> 0) then
    FreeLibrary(FRichEditModule);

end.
