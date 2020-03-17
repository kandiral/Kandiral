(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRVButton                                                                 *)
(*  Ver.: 16.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRVButton;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils, Vcl.StdCtrls, Vcl.Controls,
    Winapi.Messages, Winapi.MMSystem,
  {$ELSE}
    Windows, Classes, SysUtils, StdCtrls, Controls, Messages, MMSystem,
  {$IFEND}
  KRIndicator, KRTypes, KRVariables;

type
  TKRVBAType = (vbatSetBit, vbatSetValue, vbatSetValueAftZero, vbatClearBit,
    vbatTggleBit);
  TKRVBStat = (vbsNone, vbsWaitForBit, vbsWaitForValue);

  TKRVButton = class(TCustomButton, IKRVarUp)
  private
    FAction: TKRVBAType;
    FVariable: TKRVariable;
    FClick: TNotifyEvent;
    FBit: byte;
    FValue: Variant;
    FTimeout: Word;
    FStat: TKRVBStat;
    _tm: Cardinal;
    FAskMsg: String;
    FDialogType: TKRDialogType;
    FAfterClick: TNotifyEvent;
    procedure VarUp(AVar: TKRVariable); stdcall;
    procedure VarErr(AVar: TKRVariable); stdcall;
    procedure SetAction(const Value: TKRVBAType);
    procedure SetVariable(const Value: TKRVariable);
    procedure SetValue(const Value: Variant);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure _Click(Sender: TObject);virtual;
  public
    constructor Create(AOvner: TComponent);override;
  published
    property DialogType: TKRDialogType read FDialogType write FDialogType default krdtQuestion;
    property Action: TKRVBAType read FAction write SetAction default vbatSetValue;
    property Variable: TKRVariable read FVariable write SetVariable;
    property Bit: byte read FBit write FBit;
    property Value: Variant read FValue write SetValue;
    property Timeout: Word read FTimeout write FTimeout default 2000;
    property AskMsg: String read FAskMsg write FAskMsg;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property CommandLinkHint;
    property Constraints;
    property Default;
    property DisabledImageIndex;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownMenu;
    property ElevationRequired;
    property Enabled;
    property Font;
    property HotImageIndex;
    property ImageAlignment;
    property ImageIndex;
    property ImageMargins;
    property Images;
    property ModalResult;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PressedImageIndex;
    property SelectedImageIndex;
    property ShowHint;
    property Style;
    property StylusHotImageIndex;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property OnAfterClick: TNotifyEvent read FAfterClick write FAfterClick;
    property OnClick: TNotifyEvent read FClick write FClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDownClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TKRIndVButton = class;

  TKRIndicatorForVBtn = class(TKRIndicator)
  private
    FBtn: TKRIndVButton;
    FStateChanged: TNotifyEvent;
    FClick: TNotifyEvent;
    procedure Click_(Sender: TObject);
    procedure StateChanged_(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
  published
    property OnStateChanged: TNotifyEvent read FStateChanged write FStateChanged;
    property OnClick: TNotifyEvent read FClick write FClick;
  end;

  TKRLabelForVBtn = class(TCustomLabel)
  private
    FBtn: TKRIndVButton;
    procedure Click_(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
  end;

  TKRIndVButton = class(TKRVButton)
  private
    FIndicator: TKRIndicatorForVBtn;
    FLabel: TKRLabelForVBtn;
    FIndFullBtn: boolean;
    FOnOffButton: boolean;
    FBitOff: byte;
    FAskMsgOff: String;
    FBitOn: byte;
    FAskMsgOn: String;
    FValueOn: Variant;
    FValueOff: Variant;
    FCaptionOff: TCaption;
    FCaptionOn: TCaption;
    FClickOff: TNotifyEvent;
    FClickOn: TNotifyEvent;
    FClick_: TNotifyEvent;
    procedure SetIndicator(const Value: TKRIndicatorForVBtn);
    procedure SetIndFullBtn(const Value: boolean);
    function GetCaption_: TCaption;
    procedure SetCaption_(const Value: TCaption);
    procedure SetOnOffButton(const Value: boolean);
    procedure SetValueOff(const AValue: Variant);
    procedure SetValueOn(const AValue: Variant);
    procedure SetAskMsgOff(const Value: String);
    procedure SetAskMsgOn(const Value: String);
    procedure SetBitOff(const Value: byte);
    procedure SetBitOn(const Value: byte);
    procedure ChState;
    procedure SetCaptionOff(const Value: TCaption);
    procedure SetCaptionOn(const Value: TCaption);
    procedure Click_(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
  public
    constructor Create(Owner: TComponent);override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property Indicator: TKRIndicatorForVBtn read FIndicator write SetIndicator;
    property IndFullBtn: boolean read FIndFullBtn write SetIndFullBtn;
    property Caption: TCaption read GetCaption_ write SetCaption_;
    property OnOffButton: boolean read FOnOffButton write SetOnOffButton;
    property AskMsgOn: String read FAskMsgOn write SetAskMsgOn;
    property AskMsgOff: String read FAskMsgOff write SetAskMsgOff;
    property BitOn: byte read FBitOn write SetBitOn;
    property BitOff: byte read FBitOff write SetBitOff;
    property ValueOn: Variant read FValueOn write SetValueOn;
    property ValueOff: Variant read FValueOff write SetValueOff;
    property CaptionOn: TCaption read FCaptionOn write SetCaptionOn;
    property CaptionOff: TCaption read FCaptionOff write SetCaptionOff;
    property OnClick: TNotifyEvent read FClick_ write FClick_;
    property OnClickOn: TNotifyEvent read FClickOn write FClickOn;
    property OnClickOff: TNotifyEvent read FClickOff write FClickOff;
  end;

implementation

uses funcs, lgop;

{ TKRVButton }

constructor TKRVButton.Create(AOvner: TComponent);
begin
  inherited;
  FDialogType:=krdtQuestion;
  FAction:=vbatSetValue;
  FStat:=vbsNone;
  FAskMsg:='';
  FTimeout:=2000;
  _tm:=0;
  inherited OnClick:=_Click;
end;

procedure TKRVButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRVButton.SetAction(const Value: TKRVBAType);
begin
  FAction := Value;
end;

procedure TKRVButton.SetValue(const Value: Variant);
begin
  FValue := Value;
  if Assigned(FVariable) then begin
    case FVariable.VarType of
      VT_BYTE: FValue:=Byte(Value);
      VT_WORD: FValue:=Word(Value);
      VT_DWORD: FValue:=Cardinal(Value);
      VT_SMALLINT: FValue:=SmallInt(Value);
      VT_INT: FValue:=Integer(Value);
      VT_SINGLE: FValue:=Single(Value);
      VT_STRING: FValue:=String(Value);
    end;
  end;
end;

procedure TKRVButton.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    if Assigned(FVariable) then FVariable.DelMon(Self);
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      FVariable.AddMon(Self);
      SetValue(FValue);
    end;
  end;
end;

procedure TKRVButton.VarErr(AVar: TKRVariable);
begin
  FStat:=vbsNone;
end;

procedure TKRVButton.VarUp(AVar: TKRVariable);
begin
  if({$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime-_tm>FTimeout)then FStat:=vbsNone else begin
    case FStat of
      vbsWaitForBit: begin
        FStat:=vbsNone;
        case FAction of
        vbatSetBit: FVariable.Value:=SetBit(FVariable.Value,FBit);
        vbatClearBit: FVariable.Value:=ClearBit(FVariable.Value,FBit);
        vbatTggleBit: if GetBit(FVariable.Value,FBit) then
            FVariable.Value:=ClearBit(FVariable.Value,FBit)
          else
            FVariable.Value:=SetBit(FVariable.Value,FBit);
        end;
      end;
      vbsWaitForValue: begin
        FStat:=vbsNone;
        FVariable.Value:=FValue;
      end;
    end;
  end;
end;

procedure TKRVButton._Click(Sender: TObject);
begin
  if Trim(FAskMsg)<>'' then
    if IDYES<>AppMsgBox(FAskMsg,MB_YESNOCANCEL or MB_DEFBUTTON3 or KRDialogTypeToFlag(FDialogType))then exit;

  if Assigned(FClick) then FClick(Self);

  if not Assigned(FVariable) then exit;
  if(FStat<>vbsNone)and({$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime-_tm<FTimeout)then exit;
  case FAction of
    vbatSetBit, vbatClearBit, vbatTggleBit: begin
      _tm:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
      FStat:=vbsWaitForBit;
      FVariable.UpdateValue;
    end;
    vbatSetValue: FVariable.Value:=FValue;
    vbatSetValueAftZero: begin
      _tm:={$IF CompilerVersion >= 23}Winapi.{$IFEND}MMSystem.timeGetTime;
      FStat:=vbsWaitForValue;
      FVariable.Value:=0;
    end;
  end;

  if Assigned(FAfterClick) then FAfterClick(Self);
end;

{ TKRIndVButton }

procedure TKRIndVButton.ChState;
begin
  if FOnOffButton and Assigned(FIndicator) then
    if FIndicator.IndicatorState=istOn then begin
      AskMsg:=FAskMsgOn;
      Bit:=FBitOn;
      Value:=FValueOn;
      Caption:=FCaptionOn;
    end else begin
      AskMsg:=FAskMsgOff;
      Bit:=FBitOff;
      Value:=FValueOff;
      Caption:=FCaptionOff;
    end;
end;

procedure TKRIndVButton.Click_(Sender: TObject);
begin
  if Assigned(FClick_) then FClick_(Self);
  if FOnOffButton and Assigned(FIndicator) then
    if FIndicator.IndicatorState=istOn then begin
      if Assigned(FClickOff) then FClickOff(Self);
    end else if FIndicator.IndicatorState=istOff then begin
      if Assigned(FClickOn) then FClickOn(Self);
    end;
end;

procedure TKRIndVButton.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FIndicator) then FIndicator.BiDiMode:=BidiMode;
  if Assigned(FLabel) then FLabel.BiDiMode:=BidiMode;
end;

procedure TKRIndVButton.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FIndicator) then FIndicator.Enabled:=Enabled;
  if Assigned(FLabel) then FLabel.Enabled:=Enabled;
end;

procedure TKRIndVButton.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FIndicator) then FIndicator.Visible:=Visible;
  if Assigned(FLabel) then FLabel.Visible:=Visible;
end;

constructor TKRIndVButton.Create(Owner: TComponent);
begin
  inherited;
  inherited OnClick:=Click_;
  FIndFullBtn:=false;
  FOnOffButton:=false;
  SetIndicator(TKRIndicatorForVBtn.Create(Self));
  FLabel:=TKRLabelForVBtn.Create(Self);
  FLabel.Visible:=false;
  FLabel.AutoSize:=false;
  FLabel.Parent:=Self;
  FLabel.Transparent:=true;
  FLabel.Alignment:=taCenter;
  FLabel.Layout:=tlCenter;
  FLabel.FBtn:=self;
end;

function TKRIndVButton.GetCaption_: TCaption;
begin
  Result:=inherited Caption;
end;

procedure TKRIndVButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FIndicator)then FIndicator:=nil;
end;

procedure TKRIndVButton.SetAskMsgOff(const Value: String);
begin
  FAskMsgOff := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOff)then AskMsg:=FAskMsgOff;
end;

procedure TKRIndVButton.SetAskMsgOn(const Value: String);
begin
  FAskMsgOn := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOn)then AskMsg:=FAskMsgOn;
end;

procedure TKRIndVButton.SetBitOff(const Value: byte);
begin
  FBitOff := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOff)then begin
    Bit:=FBitOff;
    ChState;
  end;
end;

procedure TKRIndVButton.SetBitOn(const Value: byte);
begin
  FBitOn := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOn)then begin
    Bit:=FBitOn;
    ChState;
  end;
end;

procedure TKRIndVButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(FLabel) then FLabel.SetBounds(0,0,AWidth,AHeight);
end;

procedure TKRIndVButton.SetCaptionOff(const Value: TCaption);
begin
  FCaptionOff := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOff)then begin
    Caption:=FCaptionOff;
  end;
  ChState;
end;

procedure TKRIndVButton.SetCaptionOn(const Value: TCaption);
begin
  FCaptionOn := Value;
  if FOnOffButton and(FIndicator.IndicatorState=istOn)then begin
    Caption:=FCaptionOn;
  end;
  ChState;
end;

procedure TKRIndVButton.SetCaption_(const Value: TCaption);
begin
  inherited Caption:=Value;
  FLabel.Caption:=Value;
end;

procedure TKRIndVButton.SetIndFullBtn(const Value: boolean);
begin
  FIndFullBtn := Value;
  FLabel.Visible:=FIndFullBtn;
  if FIndFullBtn then FIndicator.SetBounds(0,0,Width,Height);
end;

procedure TKRIndVButton.SetIndicator(const Value: TKRIndicatorForVBtn);
begin
  if FIndicator<>Value then begin
    FIndicator := Value;
    if FIndicator<>nil then begin
      FIndicator.SetSubComponent(true);
      FIndicator.FreeNotification(Self);
      FIndicator.Parent:=Self;
      FIndicator.Width:=12;
      FIndicator.Height:=12;
      FIndicator.Left:=6;
      FIndicator.Top:=8;
      FIndicator.FBtn:=self;
      ChState;
    end;
  end;
end;

procedure TKRIndVButton.SetOnOffButton(const Value: boolean);
begin
  FOnOffButton := Value;
  ChState;
end;

procedure TKRIndVButton.SetValueOff(const AValue: Variant);
begin
  FValueOff := AValue;
  if FOnOffButton and(FIndicator.IndicatorState=istOff)then Value:=FValueOff;
end;

procedure TKRIndVButton.SetValueOn(const AValue: Variant);
begin
  FValueOn := AValue;
  if FOnOffButton and(FIndicator.IndicatorState=istOn)then Value:=FValueOn;
end;

{ TKRIndicatorForVBtn }

procedure TKRIndicatorForVBtn.Click_(Sender: TObject);
begin
  if Assigned(FClick) then FClick(Self);
  if Assigned(FBtn) then FBtn.Click;
end;

constructor TKRIndicatorForVBtn.Create(AOwner: TComponent);
begin
  inherited;
  inherited OnClick:=Click_;
  inherited OnStateChanged:=StateChanged_;
  Name:='Indicator';
end;

procedure TKRIndicatorForVBtn.StateChanged_(Sender: TObject);
begin
  if Assigned(FBtn) then begin
    FBtn.ChState;
  end;
  if Assigned(FStateChanged) then FStateChanged(Self);
end;

{ TKRLabelForVBtn }

procedure TKRLabelForVBtn.Click_(Sender: TObject);
begin
  if Assigned(FBtn) then FBtn.Click;
end;

constructor TKRLabelForVBtn.Create(AOwner: TComponent);
begin
  inherited;
  Self.OnClick:=Click_;
end;

end.
