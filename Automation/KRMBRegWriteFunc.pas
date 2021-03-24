(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBRegWriteFunc                                                          *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBRegWriteFunc;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Controls, Vcl.StdCtrls, Winapi.Messages, System.SysUtils,
  {$ELSE}
    Classes, Controls, StdCtrls, Messages, SysUtils,
  {$IFEND}
    KRModbusClient, KRComboBox;

type
  TKRMBRegWriteFunc = class(TKRComboBox)
  private
    FChange: TNotifyEvent;
    FVariable: TKRMBRegister;
    _inited: boolean;
    procedure _Change(Sender: TObject);
    procedure SetVariable(const Value: TKRMBRegister);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Variable: TKRMBRegister read FVariable write SetVariable;
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style default csDropDownList;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Touch;
    property Visible;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items;
  end;

implementation

{ TKRMBRegWriteFunc }

constructor TKRMBRegWriteFunc.Create(AOwner: TComponent);
begin
  _inited:=false;
  inherited;
  inherited OnChange:=_Change;
  Style:=csDropDownList;
end;

procedure TKRMBRegWriteFunc.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRMBRegWriteFunc.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Parent) and (not _inited) then begin
    Items.Add('(06H)Write Holding Register');
    Items.Add('(10H)Write Holding Registers');
    _inited:=true;
  end;
end;

procedure TKRMBRegWriteFunc.SetStyle(Value: TComboBoxStyle);
begin
  if Value=csDropDownList then inherited SetStyle(Value);
end;

procedure TKRMBRegWriteFunc.SetVariable(const Value: TKRMBRegister);
begin
  if FVariable<>Value then begin
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
      if FVariable.WriteFunction=mbwfWriteHoldingRegister then ItemIndex:=0 else ItemIndex:=1;
    end;
  end;
end;

procedure TKRMBRegWriteFunc._Change(Sender: TObject);
begin
  if Assigned(FVariable) then begin
    if ItemIndex=0 then
      FVariable.WriteFunction:=mbwfWriteHoldingRegister
    else
      FVariable.WriteFunction:=mbwfWriteHoldingRegisters
  end;
  if Assigned(FChange) then FChange(Self);
end;

end.
