(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBtnVarUpdate                                                            *)
(*  Ver.: 15.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBtnVarUpdate;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.StdCtrls,
  {$ELSE}
    Classes, SysUtils, StdCtrls,
  {$IFEND}
  KRVariables;

type
  TKRBtnVarUpdate = class(TCustomButton)
  private
    FVariable: TKRVariable;
    FClick: TNotifyEvent;
    procedure SetVariable(const Value: TKRVariable);
    procedure _Click(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOvner: TComponent);override;
  published
    property Variable: TKRVariable read FVariable write SetVariable;
    property Action;
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

implementation

{ TKRBtnVarUpdate }

constructor TKRBtnVarUpdate.Create(AOvner: TComponent);
begin
  inherited;
  inherited OnClick:=_Click;
end;

procedure TKRBtnVarUpdate.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FVariable)then FVariable:= nil;
end;

procedure TKRBtnVarUpdate.SetVariable(const Value: TKRVariable);
begin
  if FVariable<>Value then begin
    FVariable := Value;
    if Assigned(FVariable) then begin
      FVariable.FreeNotification(Self);
    end;
  end;
end;

procedure TKRBtnVarUpdate._Click(Sender: TObject);
begin
  if Assigned(FVariable) then FVariable.UpdateValue;
  if Assigned(FClick) then FClick(Self);
  
end;

end.
