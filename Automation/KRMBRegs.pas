(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBRegs                                                                  *)
(*  Ver.: 20.11.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBRegs;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls,
  {$IFEND}
  KRMBRegWriteFunc, KRComboBox, KRMBRegReadFunc, KRMBRegInterval, KRValueEdit,
  KRMBRegIndex, KRField, KRModbusClient, KRTimer, KRIniConfig;

type
  TKRMBRegParams = record
    reg: TKRMBRegister;
    lbCaption, lbError: Tlabel;
    ch: TCheckBox;
    field: TKRField;
    upBtn: TButton;
    RegIndex: TKRMBRegIndex;
    RegInterval: TKRMBRegInterval;
    RegReadFunc: TKRMBRegReadFunc;
    RegWriteFunc: TKRMBRegWriteFunc;
  end;

  TKRMBRegs = class;

  TKRMBRegsForm = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    KRTimer1: TKRTimer;
    cbTimerInterval: TKRBLComboBox;
    cbRegs: TKRComboBox;
    btnAdd: TButton;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure cbTimerIntervalChange(Sender: TObject);
    procedure KRTimer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    firstShow: boolean;
    FMBRegs: TKRMBRegs;
    procedure CheckBoxClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure save;
  protected
    procedure CreateParams(var Params: TCreateParams);override;
  public
    { Public declarations }
    Regs: array of TKRMBRegParams;
    nIndex: integer;
    procedure AddReg(ARegName: String);
  end;

  TKRMBRegs = class(TComponent)
  private
    FConfig: TKRIniConfig;
    FMBClient: TKRModbusClient;
    FForm: TKRMBRegsForm;
    procedure SetConfig(const Value: TKRIniConfig);
    procedure SetFMBClient(const Value: TKRModbusClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure Execute;
    property Form: TKRMBRegsForm read FForm;
  published
    property ModbusClient: TKRModbusClient read FMBClient write SetFMBClient;
    property Config: TKRIniConfig read FConfig write SetConfig;
  end;

implementation

{$R ..\dfm\KRMBRegs.dfm}

{ TKRMBRegsForm }

procedure TKRMBRegsForm.AddReg(ARegName: String);
var
  s: String;
  i, n, t, l: integer;
  reg: TKRMBRegister;
  lb: TLabel;
  fld: TKRField;
  ch: TCheckBox;

  btnUp: TButton;
  RegIndex: TKRMBRegIndex;
  RegInterval: TKRMBRegInterval;
  RegReadFunc: TKRMBRegReadFunc;
  RegWriteFunc: TKRMBRegWriteFunc;

  function regTypeToStr: string;
  begin
    result:='';
    case reg.MCVarType of
      MCT_BYTE: result:='MCT_BYTE';
      MCT_WORD: result:='MCT_WORD';
      MCT_DWORD: result:='MCT_DWORD';
      MCT_SMALLINT: result:='MCT_SMALLINT';
      MCT_INT: result:='MCT_INT';
      MCT_SINGLE: result:='MCT_SINGLE';
      MCT_STRING: result:='MCT_STRING';
      MCT_DWSTR: result:='MCT_DWSTR';
      MCT_WD_STREAM: result:='MCT_WD_STREAM';
      MCT_DW_STREAM: result:='MCT_DW_STREAM';
      MCT_BT_STREAM: result:='MCT_BT_STREAM';
      MCT_SINT_STREAM: result:='MCT_SINT_STREAM';
      MCT_INT64: result:='MCT_INT64';
      MCT_FILE: result:='MCT_FILE';
      MCT_ARRAY: result:='MCT_ARRAY';
    end;
  end;

begin
  s:=LowerCase(trim(ARegName));
  if s='' then exit;
  reg:=nil;
  for i := 0 to FMBRegs.FMBClient.ItemsCount-1 do
    if s=lowercase(FMBRegs.FMBClient.Items[i].Name) then begin
      reg:=TKRMBRegister(FMBRegs.FMBClient.Items[i]);
      break;
    end;
  if reg=nil then exit;
  for i := 0 to Length(Regs)-1 do
    if reg=Regs[i].reg then exit;

  if reg.MCVarType=MCT_FILE then exit;
  if reg.MCVarType=MCT_ARRAY then exit;

  n:=Length(regs);
  SetLength(regs,n+1);
  regs[n].reg:=reg;

  if n=0 then begin
    t:=6;
    l:=0;
  end else begin
    t:=regs[n-1].lbCaption.Top+65;
    l:=regs[n-1].lbCaption.Left-6;
  end;

  lb:=TLabel.Create(ScrollBox1);
  lb.Tag:=n;
  regs[n].lbCaption:=lb;
  lb.Parent:=ScrollBox1;
  lb.Top:=t;
  lb.Left:=6+l;
  lb.Font.Style:=[fsBold];
  lb.Caption:=regTypeToStr;
  if(reg.MCVarType=MCT_WD_STREAM)or(reg.MCVarType=MCT_DW_STREAM)or
    (reg.MCVarType=MCT_BT_STREAM)or(reg.MCVarType=MCT_SINT_STREAM)then
    lb.Caption:=lb.Caption+'['+IntToStr(reg.StreamIndex)+']';
  if(reg.MCVarType=MCT_STRING)then
    lb.Caption:=lb.Caption+'['+IntToStr(reg.StrLen)+']';
  lb.Caption:=reg.Name+': '+lb.Caption;

  ch:=TCheckBox.Create(ScrollBox1);
  ch.Tag:=n;
  regs[n].ch:=ch;
  ch.Parent:=ScrollBox1;
  ch.Top:=t+21;
  ch.Left:=6+l;
  ch.Checked:=true;
  ch.OnClick:=CheckBoxClick;
  ch.Width:=GetSystemMetrics(SM_CXMENUCHECK)+
      GetSystemMetrics(SM_CXEDGE) * 2;

  fld:=TKRField.Create(ScrollBox1);
  fld.Tag:=n;
  regs[n].field:=fld;
  fld.Parent:=ScrollBox1;
  fld.Top:=t+19;
  fld.Left:=ch.Width+6+l;
  fld.Width:=120;
  fld.Variable:=reg;
  fld.ValType:=reg.VarType;

  btnUp:=TButton.Create(ScrollBox1);
  btnUp.Tag:=n;
  regs[n].upBtn:=btnUp;
  btnUp.Parent:=ScrollBox1;
  btnUp.Top:=t+17;
  btnUp.Left:=fld.Width+6+fld.Left;
  btnUp.Width:=75;
  btnUp.Caption:='Update';
  btnUp.OnClick:=UpBtnClick;

  RegIndex:=TKRMBRegIndex.Create(ScrollBox1);
  RegIndex.Tag:=n;
  regs[n].RegIndex:=RegIndex;
  RegIndex.Parent:=ScrollBox1;
  RegIndex.Top:=t+19;
  RegIndex.Left:=btnUp.Width+6+btnUp.Left;
  RegIndex.Width:=60;
  RegIndex.Variable:=reg;
  RegInterval:=TKRMBRegInterval.Create(ScrollBox1);
  RegInterval.Tag:=n;
  regs[n].RegInterval:=RegInterval;
  RegInterval.Parent:=ScrollBox1;
  RegInterval.Top:=t+19;
  RegInterval.Left:=RegIndex.Width+6+RegIndex.Left;
  RegInterval.Width:=60;
  RegInterval.Variable:=reg;

  RegReadFunc:=TKRMBRegReadFunc.Create(ScrollBox1);
  RegReadFunc.Tag:=n;
  regs[n].RegReadFunc:=RegReadFunc;
  RegReadFunc.Parent:=ScrollBox1;
  RegReadFunc.Top:=t+19;
  RegReadFunc.Left:=RegIndex.Width+6+RegIndex.Left;
  RegReadFunc.Width:=145;
  RegReadFunc.Variable:=reg;

  RegWriteFunc:=TKRMBRegWriteFunc.Create(ScrollBox1);
  RegWriteFunc.Tag:=n;
  regs[n].RegWriteFunc:=RegWriteFunc;
  RegWriteFunc.Parent:=ScrollBox1;
  RegWriteFunc.Top:=t+19;
  RegWriteFunc.Left:=RegReadFunc.Width+6+RegReadFunc.Left;
  RegWriteFunc.Width:=145;
  RegWriteFunc.Variable:=reg;

  lb:=TLabel.Create(ScrollBox1);
  lb.Tag:=0;
  regs[n].lbError:=lb;
  lb.Parent:=ScrollBox1;
  lb.Top:=t+46;
  lb.Left:=RegWriteFunc.Width+6+RegWriteFunc.Left;
  lb.Font.Color:=clRed;

  save;
end;

procedure TKRMBRegsForm.btnAddClick(Sender: TObject);
var
  i: integer;
begin
  if cbRegs.ItemIndex=-1 then exit;
  for i := 0 to Length(regs)-1 do
    if regs[i].reg.Name=cbRegs.Text then exit;

  addReg(cbRegs.Text);
end;

procedure TKRMBRegsForm.cbTimerIntervalChange(Sender: TObject);
begin
  KRTimer1.Interval:=StrToInt(cbTimerInterval.Text);
end;

procedure TKRMBRegsForm.CheckBoxClick(Sender: TObject);
begin
  nIndex:=TCheckBox(Sender).Tag;
  Timer1.Enabled:=true;
end;

procedure TKRMBRegsForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle:=Params.ExStyle or WS_Ex_AppWindow;
  Params.WndParent:=0;
end;

procedure TKRMBRegsForm.FormCreate(Sender: TObject);
begin
  firstShow:=true;
end;

procedure TKRMBRegsForm.FormHide(Sender: TObject);
begin
  KRTimer1.Enabled:=false;
end;

procedure TKRMBRegsForm.FormShow(Sender: TObject);
var
  i,n: integer;
begin
  cbRegs.Items.BeginUpdate;
  cbRegs.Items.Clear;
  if Assigned(FMBRegs.FMBClient) then
    for i := 0 to FMBRegs.FMBClient.ItemsCount-1 do
      cbRegs.Items.Add(FMBRegs.FMBClient.Items[i].Name);
  cbRegs.Items.EndUpdate;
  if cbRegs.Items.Count>0 then cbRegs.ItemIndex:=0;

  if firstShow then begin
    firstShow:=false;
    if assigned(FMBRegs.FConfig) then begin
      n:=FMBRegs.FConfig.IniFile.ReadInteger(FMBRegs.Name,'RegsCount',0);
      for i := 0 to n-1 do
        addReg(FMBRegs.FConfig.IniFile.ReadString(FMBRegs.Name,'Reg'+intToStr(i),''));
    end;
  end;

  cbTimerIntervalChange(nil);
  KRTimer1.Enabled:=true;
end;

procedure TKRMBRegsForm.KRTimer1Timer(Sender: TObject);
var
  n,i: integer;
begin
  n:=Length(regs)-1;
  for I := 0 to n do
    if regs[i].ch.Checked then
      if regs[i].lbError.Tag<>regs[i].reg.Error then begin
        regs[i].lbError.Tag:=regs[i].reg.Error;
        regs[i].lbError.Caption:=regs[i].reg.ErrorMsg;
      end;
end;

procedure TKRMBRegsForm.save;
var
  i,n: integer;
begin
  if assigned(FMBRegs.FConfig) then begin
    n:=Length(regs);
    FMBRegs.FConfig.IniFile.WriteInteger(FMBRegs.Name,'RegsCount',n);
    for i := 0 to n-1 do
        FMBRegs.FConfig.IniFile.WriteString(FMBRegs.Name,'Reg'+intToStr(i),regs[i].reg.Name);
  end;
end;

procedure TKRMBRegsForm.Timer1Timer(Sender: TObject);
var i,n: integer;
begin
  Timer1.Enabled:=false;
  n:=nIndex;
  regs[n].ch.Free;
  regs[n].lbCaption.Free;
  regs[n].field.Free;
  regs[n].upBtn.Free;
  regs[n].RegIndex.Free;
  regs[n].RegInterval.Free;
  regs[n].RegReadFunc.Free;
  regs[n].RegWriteFunc.Free;

  for i := n to Length(regs)-2 do begin
    regs[i]:=regs[i+1];
    regs[i].ch.Top:=regs[i].ch.Top-65;
    regs[i].lbCaption.Top:=regs[i].lbCaption.Top-65;
    regs[i].field.Top:=regs[i].field.Top-65;
    regs[i].upBtn.Top:=regs[i].upBtn.Top-65;
    regs[i].RegIndex.Top:=regs[i].RegIndex.Top-65;
    regs[i].RegInterval.Top:=regs[i].RegInterval.Top-65;
    regs[i].RegReadFunc.Top:=regs[i].RegReadFunc.Top-65;
    regs[i].RegWriteFunc.Top:=regs[i].RegWriteFunc.Top-65;
  end;

  SetLength(regs,Length(regs)-1);

  save;
end;

procedure TKRMBRegsForm.UpBtnClick(Sender: TObject);
begin
  regs[TButton(Sender).Tag].reg.UpdateValue;
end;

{ TKRMBRegs }

constructor TKRMBRegs.Create(AOwner: TComponent);
begin
  inherited;
  FForm:=TKRMBRegsForm.Create(Application);
  FForm.FMBRegs:=Self;
end;

procedure TKRMBRegs.Execute;
begin
  if FForm.Visible then begin
    SetActiveWindow(FForm.Handle);
    ShowWindow(FForm.Handle, SW_RESTORE);
    SetForegroundWindow(FForm.Handle);
  end else FForm.Show;
end;

procedure TKRMBRegs.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then begin
    if (AComponent = FMBClient)then FMBClient:= nil;
    if (AComponent = FConfig)then FConfig:= nil;
  end;
end;

procedure TKRMBRegs.SetConfig(const Value: TKRIniConfig);
begin
  if FConfig<>Value then begin
    if Assigned(FConfig) then
      FConfig.RemoveFreeNotification(Self);
    FConfig := Value;
    if Assigned(FConfig) then
      FConfig.FreeNotification(Self);
  end;
end;

procedure TKRMBRegs.SetFMBClient(const Value: TKRModbusClient);
begin
  if FMBClient<>Value then begin
    if Assigned(FMBClient) then
      FMBClient.RemoveFreeNotification(Self);
    FMBClient := Value;
    if Assigned(FMBClient) then
      FMBClient.FreeNotification(Self);
  end;
end;

procedure TKRMBRegs.SetName(const Value: TComponentName);
begin
  inherited;
  if Assigned(FForm) then FForm.Caption:=Value;
end;

end.
