(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRListView                                                                *)
(*  Ver.: 04.04.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRListView;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Vcl.Controls, System.Classes, Winapi.CommCtrl, Vcl.ComCtrls,
    Winapi.Messages, Vcl.ListActns;
  {$ELSE}
    Windows, Controls, Classes, CommCtrl, ComCtrls, Messages, ListActns;
  {$IFEND}


type
  TKRLVResizeEvent=procedure(Sender: TObject;const AColIndex: Integer) of object;
  TKRLVResizingEvent=procedure(Sender: TObject;const AColIndex, AWidth: Integer) of object;
  TKRLVScrollMsgEvent=procedure(Sender: TObject;Message: TMessage) of object;

  TKRListView = class(TCustomListView)
  private
    FScrollV: TNotifyEvent;
    FScrollH: TNotifyEvent;
    FColumnBeginResize: TKRLVResizeEvent;
    FColumnResized: TKRLVResizeEvent;
    FColumnResizing: TKRLVResizingEvent;
    FScrollVMsg: TKRLVScrollMsgEvent;
    FPaint: TNotifyEvent;
    FScrollVEnd: TNotifyEvent;
    function GetListItems: TListItems;
    procedure SetListItems(const Value: TListItems);
  protected
    function CanChange(Item: TListItem; Change: Integer): Boolean; override;
    function IsItemsStored: Boolean; virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure DoColumnBeginResize(const AColIndex: Integer);
    procedure DoColumnResizing(const AColIndex, AWidth: Integer);
    procedure DoColumnResized(const AColIndex: Integer);
    procedure DoHScroll;
    procedure DoVScroll;
    procedure DoVScrollMsg(Message: TMessage);
    procedure DoPaint;
    procedure DoVScrollEnd;
  published
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
    property Columns;
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
    property Items: TListItems read GetListItems write SetListItems stored IsItemsStored;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property GroupHeaderImages;
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
    property SmallImages;
    property SortType;
    property StateImages;
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
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
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
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
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
    property OnScrollH: TNotifyEvent read FScrollH write FScrollH;
    property OnScrollV: TNotifyEvent read FScrollV write FScrollV;
    property OnColumnBeginResize: TKRLVResizeEvent read FColumnBeginResize write FColumnBeginResize;
    property OnColumnResized: TKRLVResizeEvent read FColumnResized write FColumnResized;
    property OnColumnResizing: TKRLVResizingEvent read FColumnResizing write FColumnResizing;
    property OnScrollVMsg: TKRLVScrollMsgEvent read FScrollVMsg write FScrollVMsg;
    property OnPaint: TNotifyEvent read FPaint write FPaint;
    property OnScrollVEnd: TNotifyEvent read FScrollVEnd write FScrollVEnd;
  end;

implementation

{ TKRListView }

function TKRListView.CanChange(Item: TListItem; Change: Integer): Boolean;
begin
  result:=inherited CanChange(Item,Change);
end;

procedure TKRListView.DoColumnBeginResize(const AColIndex: Integer);
begin
  if Assigned(FColumnBeginResize) then FColumnBeginResize(Self, AColIndex);
end;

procedure TKRListView.DoColumnResized(const AColIndex: Integer);
begin
  if Assigned(FColumnResized) then FColumnResized(Self, AColIndex);
end;

procedure TKRListView.DoColumnResizing(const AColIndex, AWidth: Integer);
begin
  if Assigned(FColumnResizing) then FColumnResizing(Self, AColIndex, AWidth);
end;

procedure TKRListView.DoHScroll;
begin
  if Assigned(FScrollH) then FScrollH(Self);
end;

procedure TKRListView.DoPaint;
begin
  if Assigned(FPaint) then FPaint(Self);  
end;

procedure TKRListView.DoVScroll;
begin
  if Assigned(FScrollV) then FScrollV(Self);
end;

procedure TKRListView.DoVScrollEnd;
begin
  if Assigned(FScrollVEnd) then FScrollVEnd(Self);
  
end;

procedure TKRListView.DoVScrollMsg(Message: TMessage);
begin
  if Assigned(FScrollVMsg) then FScrollVMsg(Self,Message);
end;

function TKRListView.GetListItems: TListItems;
begin
  result:=inherited Items;
end;

function TKRListView.IsItemsStored: Boolean;
begin
  if Assigned(Action) then
  begin
    if Action is TCustomListAction then
      Result := False
    else
      Result := True;
  end
  else
    Result := not OwnerData;

end;

procedure TKRListView.SetListItems(const Value: TListItems);
begin
  inherited Items:=Value;
end;

procedure TKRListView.WndProc(var Message: TMessage);
var
  vColWidth: Integer;
  vMsgNotify: TWMNotify absolute Message;
  scroll: TWMScroll absolute Message;
begin
  case Message.Msg of
    WM_PAINT: DoPaint;
    WM_HSCROLL: DoHScroll;
    WM_VSCROLL: begin
      DoVScroll;
      DoVScrollMsg(Message);
      if scroll.ScrollCode=SB_ENDSCROLL then DoVScrollEnd;
    end;
    WM_NOTIFY: begin
      case PHDNotify(vMsgNotify.NMHdr)^.Hdr.Code of
        HDN_ENDTRACK: DoColumnResized(PHDNotify(vMsgNotify.NMHdr)^.Item);
        HDN_BEGINTRACK: DoColumnBeginResize(PHDNotify(vMsgNotify.NMHdr)^.Item);
        HDN_TRACK: begin
          vColWidth := -1;
          if (PHDNotify(vMsgNotify.NMHdr)^.PItem<>nil)
             and (PHDNotify(vMsgNotify.NMHdr)^.PItem^.Mask and HDI_WIDTH <> 0)then
            vColWidth := PHDNotify(vMsgNotify.NMHdr)^.PItem^.cxy;
          DoColumnResizing(PHDNotify(vMsgNotify.NMHdr)^.Item, vColWidth);
        end;
      end;
    end;
  end;
  inherited WndProc(Message);
end;

end.
