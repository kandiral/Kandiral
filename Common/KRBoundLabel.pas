(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRBoundLabel                                                              *)
(*  Ver.: 04.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRBoundLabel;

interface

uses
  {$IF CompilerVersion >= 23}
    Vcl.Controls, System.Classes, Vcl.StdCtrls;
  {$ELSE}
    Controls, Classes, StdCtrls;
  {$IFEND}

type
  TKRBoundLabelPosition = (
    blpLeftTop, blpLeftCenter, blpLeftBottom,
    blpRightTop, blpRightCenter, blpRightBottom,
    blpTopLeft, blpTopCenter, blpTopRight,
    blpBottomLeft, blpBottomCenter, blpBottomRight
  );

  TKRBoundLabel = class(TCustomLabel)
  private
    FSpacing: integer;
    FPosition: TKRBoundLabelPosition;
    FControl: TControl;
    function GetTop: Integer;
    function GetLeft: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TKRBoundLabelPosition);
    procedure SetSpacing(const Value: integer);
    procedure SetWidth(const Value: Integer);
    procedure SetAutoSize_(const Value: boolean);
    function GetAutoSize: boolean;
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
  protected
    procedure AdjustBounds; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alignment;
    property AutoSize: boolean read GetAutoSize write SetAutoSize_;
    property Position: TKRBoundLabelPosition read FPosition write SetPosition default blpLeftCenter;
    property Spacing: integer read FSpacing write SetSpacing;
    property BiDiMode;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Top: Integer read GetTop;
    property Touch;
    property Transparent;
    property Layout;
    property WordWrap;
    property Width: Integer read GetWidth write SetWidth;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TKRBoundLabel }

procedure TKRBoundLabel.AdjustBounds;
begin
  inherited AdjustBounds;
  SetPosition(FPosition)
end;

constructor TKRBoundLabel.Create(AOwner: TComponent);
begin
  inherited;
  Name := 'KRBoundLabel';  { do not localize }
  SetSubComponent(True);
  if Assigned(AOwner) then begin
    Caption := AOwner.Name;
    Name := 'KRBoundLabelFor'+AOwner.Name;
    FControl:=TControl(AOwner);
  end;
  FPosition:=blpLeftCenter;
  FSpacing:=6;
end;

function TKRBoundLabel.GetAutoSize: boolean;
begin
  Result:=inherited AutoSize;
end;

function TKRBoundLabel.GetCaption: TCaption;
begin
  Result:=inherited Caption;
end;

function TKRBoundLabel.GetHeight: Integer;
begin
  Result:=inherited Height;
end;

function TKRBoundLabel.GetLeft: Integer;
begin
  Result:=inherited Left;
end;

function TKRBoundLabel.GetTop: Integer;
begin
  Result:=inherited Top
end;

function TKRBoundLabel.GetWidth: Integer;
begin
  Result:=inherited Width;
end;

procedure TKRBoundLabel.SetAutoSize_(const Value: boolean);
begin
  inherited AutoSize:=Value;
  SetPosition(FPosition)
end;

procedure TKRBoundLabel.SetCaption(const Value: TCaption);
begin
  inherited Caption:=Value;
  SetPosition(FPosition)
end;

procedure TKRBoundLabel.SetHeight(const Value: Integer);
begin
  inherited Height:=Value;
  SetPosition(FPosition)
end;

procedure TKRBoundLabel.SetPosition(const Value: TKRBoundLabelPosition);
begin
  FPosition := Value;
  if not Assigned(FControl) then exit;
  case FPosition of
    blpLeftTop: begin
      inherited Top:=FControl.Top;
      inherited Left:=FControl.Left-Width-FSpacing;
    end;
    blpLeftCenter: begin
      inherited Top:=FControl.Top+(FControl.Height - Height) div 2;
      inherited Left:=FControl.Left-Width-FSpacing;
    end;
    blpLeftBottom: begin
      inherited Top:=FControl.Top+FControl.Height-Height;
      inherited Left:=FControl.Left-Width-FSpacing;
    end;
    blpRightTop: begin
      inherited Top:=FControl.Top;
      inherited Left:=FControl.Left+FControl.Width+FSpacing;
    end;
    blpRightCenter: begin
      inherited Top:=FControl.Top+(FControl.Height - Height) div 2;
      inherited Left:=FControl.Left+FControl.Width+FSpacing;
    end;
    blpRightBottom: begin
      inherited Top:=FControl.Top+FControl.Height-Height;
      inherited Left:=FControl.Left+FControl.Width+FSpacing;
    end;
    blpTopLeft: begin
      inherited Top:=FControl.Top - Height - FSpacing;
      inherited Left:=FControl.Left;
    end;
    blpTopCenter: begin
      inherited Top:=FControl.Top - Height - FSpacing;
      inherited Left:=FControl.Left+(FControl.Width-Width) div 2;
    end;
    blpTopRight: begin
      inherited Top:=FControl.Top - Height - FSpacing;
      inherited Left:=FControl.Left+FControl.Width-Width;
    end;
    blpBottomLeft: begin
      inherited Top:=FControl.Top+FControl.Height+FSpacing;
      inherited Left:=FControl.Left;
    end;
    blpBottomCenter: begin
      inherited Top:=FControl.Top+FControl.Height+FSpacing;
      inherited Left:=FControl.Left+(FControl.Width-Width) div 2;
    end;
    blpBottomRight: begin
      inherited Top:=FControl.Top+FControl.Height+FSpacing;
      inherited Left:=FControl.Left+FControl.Width-Width;
    end;
  end;
end;

procedure TKRBoundLabel.SetSpacing(const Value: integer);
begin
  FSpacing := Value;
  SetPosition(FPosition)
end;

procedure TKRBoundLabel.SetWidth(const Value: Integer);
begin
  inherited Width:=Value;
  SetPosition(FPosition)
end;

end.
