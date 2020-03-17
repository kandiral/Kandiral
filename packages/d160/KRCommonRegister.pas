(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  http://kandiral.ru                                                        *)
(*                                                                            *)
(*  KRCommonRegister                                                          *)
(*  Ver.: 15.01.2017                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRCommonRegister;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, System.Classes, System.SysUtils,
    {$IF CompilerVersion >= 24}System.Actions,{$IFEND}
    DesignEditors,
    DesignIntf, Vcl.Controls, System.StrUtils, Vcl.ExtActns, Vcl.ActnList;
  {$ELSE}
    Windows, Classes, SysUtils, DesignEditors, DesignIntf, Controls, StrUtils,
    ExtActns, ActnList;
  {$IFEND}

type
  TKRComponentCollectionItemEditor=class(TComponentEditor)
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer);override;
    procedure Edit;override;
  end;

  TKRIniCfgParamSectionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TKRTimerEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
  end;

procedure Register;

implementation

uses KRComponentCollection, KRBoundLabel, KRIniConfig, KRProgressBar, KRValueEdit,
  KRComboBox, KRCheckBox, KRTimer, KRCheckGroupBox, KRRadioButton, KRListView,
  KRImageList, KRVersionInfo, KRRichEdit, ToolsAPI, KRBLEdit;

{$R 'KRImages.res'}

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  if Assigned(SplashScreenServices) then begin
    ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'KRLOGO32X32');
    SplashScreenServices.AddPluginBitmap('Components by Kandiral Ruslan',
      ProductImage,
      False, 'GPLv3');
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterSplashScreen;
  RegisterClasses([TKRComponent, TKRComponentCollection, TKRComponentCollectionItem,
    TKRBoundLabel, TKRIniConfig, TKRIniCfgParam, TKRProgressBar, TKRValueEdit,
    TKRBLValueEdit, TKRComboBox, TKRBLComboBox, TKRCheckBox, TKRBLCheckBox,
    TKRTimer, TKRCheckGroupBox, TKRRadioButton, TKRListView, TKRImageList,
    TKRVersionInfo, TKRRichEdit, TKRBLEdit]);
  RegisterComponents('KRCommon', [TKRProgressBar, TKRIniConfig, TKRValueEdit,
    TKRBLValueEdit, TKRComboBox, TKRBLComboBox, TKRCheckBox, TKRBLCheckBox,
    TKRTimer, TKRCheckGroupBox, TKRRadioButton, TKRListView, TKRImageList,
    TKRVersionInfo, TKRRichEdit, TKRBLEdit]);
  RegisterNoIcon([TKRComponentCollectionItem, TKRBoundLabel, TKRIniCfgParam]);
  RegisterComponentEditor(TKRComponentCollection,TKRComponentCollectionItemEditor);
  RegisterComponentEditor(TKRComponentCollectionItem,TKRComponentCollectionItemEditor);
  RegisterComponentEditor(TKRTimer,TKRTimerEditor);
  RegisterPropertyEditor(TypeInfo(string),TKRIniCfgParam,'Section',TKRIniCfgParamSectionProperty);
  RegisterActions('Format', [TKRRichEditFontName, TKRRichEditFontSize,
    TKRRichEditFontColor], TRichEditAction);

end;

{ TKRComponentCollectionItemEditor }

procedure TKRComponentCollectionItemEditor.Edit;
begin


end;

procedure TKRComponentCollectionItemEditor.ExecuteVerb(Index: integer);
var
  item: TKRComponentCollectionItem;
  Comp: TComponent;
  ItemClass: TKRComponentCollectionItemClass;
  s: String;
  n: integer;
begin
  Case Index of
    0: begin
      Comp:=Component;
      while Assigned(Comp.Owner) do Comp:=Comp.Owner;
      if Component is TKRComponentCollectionItem
        then ItemClass:=TKRComponentCollectionItem(Component).Collection.ItemClass
        else ItemClass:=TKRComponentCollection(Component).ItemClass;
      item:=ItemClass.Create(Comp);
      s:=Item.ClassName;
      if s[1]=#84 then s:=RightStr(s,Length(s)-1);
      n:=1;
      while Comp.FindComponent(s+IntToStr(n))<>nil do inc(n);
      Item.Name:=s+IntToStr(n);
      if Component is TKRComponentCollectionItem
        then TKRComponentCollectionItem(Component).Collection.AddItem(Item)
        else TKRComponentCollection(Component).AddItem(Item);
      Designer.Modified;
    end;
    1: if Component is TKRComponentCollectionItem then begin
        TKRComponentCollectionItem(Component).Free;
        Designer.Modified;
    end;
  end;
end;

function TKRComponentCollectionItemEditor.GetVerb(Index: integer): string;
begin
  Case Index of
    0: Result:='Add';
    1: Result:='Remove';
  end;
end;

function TKRComponentCollectionItemEditor.GetVerbCount: integer;
begin
  if Component is TKRComponentCollectionItem then Result:=2 else Result:=1;
end;

{ TKRIniCfgParamSectionProperty }

function TKRIniCfgParamSectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TKRIniCfgParamSectionProperty.GetValues(Proc: TGetStrProc);
var
  param: TKRIniCfgParam;
  cfg: TKRIniConfig;
  i,j: integer;
  sl: TStringList;
  b: boolean;
begin
  param:=(GetComponent(0) as TKRIniCfgParam);
  if not assigned(param.IniConfig) then exit;
  cfg:=param.IniConfig;
  sl:=TStringList.Create;
  try
    for i := 0 to cfg.ItemsCount-1 do begin
      b:=true;
      for j := 0 to sl.Count-1 do if sl[j]=TKRIniCfgParam(cfg.Items[i]).Section then begin
        b:=false;
        break;
      end;
      if b then sl.Add(TKRIniCfgParam(cfg.Items[i]).Section);
    end;
    for i := 0 to sl.Count-1 do Proc(sl[i]);
  finally
    sl.Free;
  end;

end;

{ TKRTimerEditor }

procedure TKRTimerEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
begin
  if CompareText(PropertyEditor.GetName, 'OnTimer') = 0 then inherited;
end;

end.
