(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRComponentCollection                                                     *)
(*  Ver.: 16.09.2019                                                          *)
(*  https://kandiral.ru/delphi/krcomponentcollection.pas.html                 *)
(*                                                                            *)
(******************************************************************************)
unit KRComponentCollection;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils, Vcl.Forms
  {$ELSE}
    Classes, SysUtils, Forms
  {$IFEND}
    ;



type
  TKRComponentCollection = class;

  TKRComponentCollectionItem = class(TComponent)
  private
    FCollection: TKRComponentCollection;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  public
    property Collection: TKRComponentCollection read FCollection;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetParentComponent(AParent: TComponent); override;
    destructor Destroy; override;
    property Index: Integer read GetIndex write SetIndex;
  end;

  TKRComponentCollectionItemClass = class of TKRComponentCollectionItem;

  TKRComponentCollection = class(TComponent)
  private
    FItems: TList;
    FItemClass: TKRComponentCollectionItemClass;
    FParentComponent: TComponent;
    function GetItemsCount: Integer;
  protected
    function GetItem(Index: integer): TKRComponentCollectionItem;
    procedure SetItem(Index: integer; const Value: TKRComponentCollectionItem);
    procedure SetItemClass(AItemClass: TKRComponentCollectionItemClass);virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure AftAddItem(var AItem: TKRComponentCollectionItem);virtual;
    procedure BfrAddItem;virtual;
    procedure AftRemItem;virtual;
    procedure BfrRemItem(var AItem: TKRComponentCollectionItem);virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property ItemClass: TKRComponentCollectionItemClass read FItemClass;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent);override;
    procedure SetParentComponent_(AParent: TComponent);
    function HasParent: Boolean; override;
    procedure AddItem(AItem: TKRComponentCollectionItem);
    procedure RemoveItem(AItem: TKRComponentCollectionItem);
    property Items[Index: integer]: TKRComponentCollectionItem read GetItem write SetItem; default;
    property ItemsCount: Integer read GetItemsCount;
  end;

  TKRComponent = class(TKRComponentCollection)
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  end;

implementation

{ TKRComponentCollection }

procedure TKRComponentCollection.AddItem(AItem: TKRComponentCollectionItem);
begin
  BfrAddItem;
  FItems.Add(AItem);
  AItem.FCollection := Self;
  AItem.FreeNotification(Self);
  AftAddItem(AItem);
end;

procedure TKRComponentCollection.AftAddItem(
  var AItem: TKRComponentCollectionItem);
begin

end;

procedure TKRComponentCollection.AftRemItem;
begin

end;

procedure TKRComponentCollection.BfrAddItem;
begin

end;

procedure TKRComponentCollection.BfrRemItem(
  var AItem: TKRComponentCollectionItem);
begin

end;

constructor TKRComponentCollection.Create(AOwner: TComponent);
begin
  inherited;
  FItems:=TList.Create;
  SetItemClass(TKRComponentCollectionItem);
end;

destructor TKRComponentCollection.Destroy;
begin
  while FItems.Count > 0 do TKRComponentCollectionItem(FItems.Last).Free;
  FreeAndNil(FItems);
  inherited;
end;

procedure TKRComponentCollection.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Item: TKRComponentCollectionItem;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Item := TKRComponentCollectionItem(FItems.List[I]);
    if Item.Owner = Root then Proc(Item);
  end;
end;

function TKRComponentCollection.GetItem(
  Index: integer): TKRComponentCollectionItem;
begin
  Result := TKRComponentCollectionItem(FItems[Index]);
end;

function TKRComponentCollection.GetItemsCount: Integer;
begin
  Result:=FItems.Count;
end;

function TKRComponentCollection.GetParentComponent: TComponent;
begin
  if Assigned(FParentComponent) then
    Result := FParentComponent else
    Result := inherited GetParentComponent;
end;

function TKRComponentCollection.HasParent: Boolean;
begin
  if Assigned(FParentComponent) then
    Result := True else
    Result := inherited HasParent;
end;

procedure TKRComponentCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (FItems <> nil) and (AComponent is TKRComponentCollectionItem) then
      RemoveItem(TKRComponentCollectionItem(AComponent));
end;

procedure TKRComponentCollection.RemoveItem(AItem: TKRComponentCollectionItem);
begin
  if FItems.Remove(AItem) >= 0 then begin
    BfrRemItem(AItem);
    AItem.RemoveFreeNotification(Self);
    AItem.FCollection := nil;
    AftRemItem;
  end;
end;

procedure TKRComponentCollection.SetChildOrder(Child: TComponent;
  Order: Integer);
begin
  if FItems.IndexOf(Child) >= 0 then
    (Child as TKRComponentCollectionItem).Index := Order;
end;

procedure TKRComponentCollection.SetItem(Index: integer;
  const Value: TKRComponentCollectionItem);
begin
  TKRComponentCollectionItem(FItems[Index]).Assign(Value);
end;

procedure TKRComponentCollection.SetItemClass(
  AItemClass: TKRComponentCollectionItemClass);
begin
  FItemClass:=AItemClass;
end;

procedure TKRComponentCollection.SetParentComponent(AParent: TComponent);
begin
//  TKRComponent(AParent).SetCollection(Self);
end;

procedure TKRComponentCollection.SetParentComponent_(AParent: TComponent);
begin
  FParentComponent:=AParent;
end;

{ TKRComponentCollectionItem }

destructor TKRComponentCollectionItem.Destroy;
begin
  if(Assigned(FCollection))then FCollection.RemoveItem(Self);
  inherited;
end;

function TKRComponentCollectionItem.GetIndex: Integer;
begin
  Result:=-1;
  if not Assigned(FCollection) then exit;

  Result:=TKRComponentCollection(Owner).FItems.IndexOf(Self);
end;

function TKRComponentCollectionItem.GetParentComponent: TComponent;
begin
  if Assigned(FCollection) then
    Result := FCollection else
    Result := inherited GetParentComponent;
end;

function TKRComponentCollectionItem.HasParent: Boolean;
begin
  if Assigned(FCollection) then
    Result := True else
    Result := inherited HasParent;
end;

procedure TKRComponentCollectionItem.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  if not Assigned(FCollection) then exit;
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := TKRComponentCollection(Owner).FItems.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      TKRComponentCollection(Owner).FItems.Delete(CurIndex);
      TKRComponentCollection(Owner).FItems.Insert(Value, Self);
    end;
  end;
end;

procedure TKRComponentCollectionItem.SetParentComponent(AParent: TComponent);
begin
  if (AParent is TKRComponentCollection) then
    TKRComponentCollection(AParent).AddItem(Self);
end;

{ TKRComponent }

constructor TKRComponent.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TKRComponent.Destroy;
begin

  inherited;
end;

end.


