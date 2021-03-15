(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMBMon                                                                   *)
(*  Ver.: 16.01.2021                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMBMon;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls,
  {$IFEND}
  KRTypes, KRValueEdit, KRModbusMaster, KRConnector, funcs;

type
  TKRMBMon = class;

  TKRMBMonForm = class(TForm)
    Panel3: TPanel;
    cbAutoScrolling: TCheckBox;
    btnSaveLog: TButton;
    Panel1: TPanel;
    chOnOff: TCheckBox;
    lbLogs: TListBox;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    procedure cbAutoScrollingClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure chOnOffClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FMon: TKRMBMon;
    KRBLValueEdit1: TKRBLValueEdit;
    KRBLValueEdit2: TKRBLValueEdit;
    filtring: boolean;
    procedure send(Sender: TObject; APack: PKRBuffer; ALength: integer);
    procedure recv(Sender: TObject; APack: PKRBuffer; ALength: integer);
  protected
    procedure CreateParams(var Params: TCreateParams);override;
    procedure DoHide; override;
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddLog(Text: String);
  end;

  TKRMBMon = class(TComponent)
  private
    FModbusMaster: TKRModbusMaster;
    FForm: TKRMBMonForm;
    send, recv: TKRConnectorPackEv;
    procedure SetModbusMaster(const Value: TKRModbusMaster);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure Execute;
    property Form: TKRMBMonForm read FForm;
  published
    property ModbusMaster: TKRModbusMaster read FModbusMaster write SetModbusMaster;
  end;

implementation

{$R ..\dfm\KRMBMon.dfm}

{ TKRMBMonForm }

procedure TKRMBMonForm.AddLog(Text: String);
begin
  lbLogs.Items.Add(FormatDateTime('dd.mm.yyyy h:nn:ss.zzz',now)+'    '+Text);

  if cbAutoScrolling.Checked then
    lbLogs.TopIndex:=lbLogs.Items.Count-1;
end;

procedure TKRMBMonForm.btnSaveLogClick(Sender: TObject);
begin
  SaveDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
  SaveDialog1.FileName:='monitoring_'+FormatDateTime('dd_mm_yyyy__h_nn_ss',now)+'.log';
  if SaveDialog1.Execute(handle) then
    lbLogs.Items.SaveToFile(SaveDialog1.FileName);
end;

procedure TKRMBMonForm.Button1Click(Sender: TObject);
begin
  lbLogs.Items.Clear;
end;

procedure TKRMBMonForm.cbAutoScrollingClick(Sender: TObject);
begin
  if cbAutoScrolling.Checked then
    lbLogs.TopIndex:=lbLogs.Items.Count-1;
end;

procedure TKRMBMonForm.chOnOffClick(Sender: TObject);
begin
  btnSaveLog.Enabled:=not chOnOff.Checked;
end;

constructor TKRMBMonForm.Create(AOwner: TComponent);
begin
  inherited;
  KRBLValueEdit1:=TKRBLValueEdit.Create(Panel1);
  KRBLValueEdit1.Parent:=Panel1;
  KRBLValueEdit1.Left:=272;
  KRBLValueEdit1.Top:=6;
  KRBLValueEdit1.Width:=57;
  KRBLValueEdit1.Height:=21;
  KRBLValueEdit1.Color:=clWindow;
  KRBLValueEdit1.InputMax:=255;
  KRBLValueEdit1.InputMin:=0;
  KRBLValueEdit1.ValueType:=vtInteger;
  KRBLValueEdit1.Value:=0;
  KRBLValueEdit1.ReadOnly:=False;
  KRBLValueEdit1.TabOrder:=2;
  KRBLValueEdit1.BLabel.Width:=73;
  KRBLValueEdit1.BLabel.Height:=13;
  KRBLValueEdit1.BLabel.AutoSize:=True;
  KRBLValueEdit1.BLabel.Spacing:=6;
  KRBLValueEdit1.BLabel.Caption:='Device address';

  KRBLValueEdit2:=TKRBLValueEdit.Create(Panel1);
  KRBLValueEdit2.Parent:=Panel1;
  KRBLValueEdit2.Left:=272;
  KRBLValueEdit2.Top:=33;
  KRBLValueEdit2.Width:=57;
  KRBLValueEdit2.Height:=21;
  KRBLValueEdit2.Color:=clWindow;
  KRBLValueEdit2.InputMax:=65535;
  KRBLValueEdit2.InputMin:=0;
  KRBLValueEdit2.ValueType:=vtInteger;
  KRBLValueEdit2.Value:=0;
  KRBLValueEdit2.ReadOnly:=False;
  KRBLValueEdit2.TabOrder:=3;
  KRBLValueEdit2.BLabel.Width:=69;
  KRBLValueEdit2.BLabel.Height:=13;
  KRBLValueEdit2.BLabel.AutoSize:=True;
  KRBLValueEdit2.BLabel.Spacing:=6;
  KRBLValueEdit2.BLabel.Caption:='Register index';
end;

procedure TKRMBMonForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle:=Params.ExStyle or WS_Ex_AppWindow;
  Params.WndParent:=0;
end;

procedure TKRMBMonForm.DoHide;
begin
  if Assigned(FMon.FModbusMaster) and Assigned(FMon.FModbusMaster.Connector) then begin
    FMon.FModbusMaster.Connector.OnSendAsync:=FMon.send;
    FMon.FModbusMaster.Connector.OnRecvAsync:=FMon.recv;
  end;
end;

procedure TKRMBMonForm.DoShow;
begin
  chOnOff.Checked:=false;
  btnSaveLog.Enabled:=true;
  if Assigned(FMon.FModbusMaster) and Assigned(FMon.FModbusMaster.Connector) then begin
    FMon.send:=FMon.FModbusMaster.Connector.OnSendAsync;
    FMon.recv:=FMon.FModbusMaster.Connector.OnRecvAsync;
    FMon.FModbusMaster.Connector.OnSendAsync:=send;
    FMon.FModbusMaster.Connector.OnRecvAsync:=recv;
  end;
end;

procedure TKRMBMonForm.recv(Sender: TObject; APack: PKRBuffer; ALength: integer);
var
  pch: PAnsiChar;
  txt: boolean;
  s: String;
  i: integer;
begin
  if not chOnOff.Checked then exit;

  if not filtring then exit;

  pch:=PAnsiChar(APack);
  txt:=(ALength>3)and(pch[0]=#58)and(pch[ALength-2]=#13)and(pch[ALength-1]=#10);
  if txt then begin
    s:='';
    for I := 0 to ALength-1 do s:=s+Char(pch[i]);
  end;

  if txt then AddLog('RECV: '+s)
  else AddLog('RECV: '+sourceToHex(pch,ALength));
end;

procedure TKRMBMonForm.send(Sender: TObject; APack: PKRBuffer; ALength: integer);
var
  pch: PAnsiChar;
  txt: boolean;
  s: String;
  i: integer;
begin
  if not chOnOff.Checked then exit;

  filtring:=false;
  pch:=PAnsiChar(APack);
  txt:=(ALength>3)and(pch[0]=#58)and(pch[ALength-2]=#13)and(pch[ALength-1]=#10);
  if txt then begin
    s:='';
    for I := 0 to ALength-1 do s:=s+Char(pch[i]);
  end;

  if(ALength>4)and(KRBLValueEdit1.Value>0)then begin
    if txt then begin
      if HexToInt(copy(s,2,2))<>KRBLValueEdit1.Value then exit;
    end else if APack[1]=0 then begin // TCP
      if APack[6]<>KRBLValueEdit1.Value then exit;
    end else if APack[0]<>KRBLValueEdit1.Value then exit;
  end;

  if(ALength>4)and(KRBLValueEdit2.Value>0)then begin
    if txt then begin
      if HexToInt(copy(s,6,4))<>KRBLValueEdit2.Value then exit;
    end else if APack[1]=0 then begin // TCP
      if((Word(APack[8]) shl 8) or APack[9])<>KRBLValueEdit2.Value then exit;
    end else if((Word(APack[2]) shl 8) or APack[3])<>KRBLValueEdit2.Value then exit;
  end;

  if txt then AddLog('SEND: '+s)
  else AddLog('SEND: '+sourceToHex(pch,ALength));
  filtring:=true;
end;

{ TKRMBMon }

constructor TKRMBMon.Create(AOwner: TComponent);
begin
  inherited;
  FForm:=TKRMBMonForm.Create(Application);
  FForm.FMon:=Self;
end;

procedure TKRMBMon.Execute;
begin
  if FForm.Visible then begin
    SetActiveWindow(FForm.Handle);
    ShowWindow(FForm.Handle, SW_RESTORE);
    SetForegroundWindow(FForm.Handle);
  end else FForm.Show;
end;

procedure TKRMBMon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if(Operation = opRemove)then
    if (AComponent = FModbusMaster)then SetModbusMaster(nil);
end;

procedure TKRMBMon.SetModbusMaster(const Value: TKRModbusMaster);
begin
  if FModbusMaster<>Value then begin
    if Assigned(FModbusMaster) then begin
      FModbusMaster.RemoveFreeNotification(self);
      if Assigned(FForm) and FForm.Visible and Assigned(FModbusMaster.Connector) then begin
        FModbusMaster.Connector.OnSendAsync:=send;
        FModbusMaster.Connector.OnRecvAsync:=recv;
      end;
    end;
    FModbusMaster := Value;
    if Assigned(FModbusMaster) then begin
      FModbusMaster.FreeNotification(self);
      if Assigned(FForm) and FForm.Visible and Assigned(FModbusMaster.Connector) then begin
        send:=FModbusMaster.Connector.OnSendAsync;
        recv:=FModbusMaster.Connector.OnRecvAsync;
        FModbusMaster.Connector.OnSendAsync:=FForm.send;
        FModbusMaster.Connector.OnRecvAsync:=FForm.recv;
      end;
    end;
  end;
end;

procedure TKRMBMon.SetName(const Value: TComponentName);
begin
  inherited;
  if Assigned(FForm) then FForm.Caption:=Value;
end;

end.
