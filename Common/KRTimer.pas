(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRTimer                                                                   *)
(*  Ver.: 11.01.2021                                                          *)
(*  https://kandiral.ru/delphi/krtimer.pas.html                               *)
(*                                                                            *)
(******************************************************************************)
unit KRTimer;

interface

uses
  {$IF CompilerVersion >= 23}
    Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils, Vcl.Forms,
  {$ELSE}
    Windows, Messages, Classes, SysUtils, Forms,
  {$IFEND}
    KRRuntimeErrors;

type
  TKRTimer = class(TComponent)
  private
    FWindowHandle: HWND;
    FEnabled: boolean;


    FList: TList;
    FProcessMessages: integer;
    FOnTimer: TNotifyEvent;
    FWorking: boolean;
    FRuntimeErrorEv: TKRRuntimeErrorEv;
    FInterval: Cardinal;
    FTerminate: boolean;

    procedure WndProc(var Msg: TMessage);


    procedure SetProcessMessages(const Value: integer);
    procedure SetEnabled(const Value: boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoTimer;
    procedure AddMon(AComp: TComponent);
    procedure DelMon(AComp: TComponent);
    property Working: boolean read FWorking;
    property Terminate: boolean read FTerminate;
  published
    property Interval: Cardinal read FInterval write SetInterval default 1000;


    property ProcessMessages: integer read FProcessMessages write SetProcessMessages default 0;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property OnRuntimeError: TKRRuntimeErrorEv read FRuntimeErrorEv write FRuntimeErrorEv;
  end;

  IKRTimer = interface
    ['{29BD634E-C75E-4F47-BC05-C80ADA364B6D}']
    procedure DoTimer; stdcall;
  end;


implementation

{ TKRTimer }

procedure TKRTimer.AddMon(AComp: TComponent);
var
  i: integer;
begin
  while FWorking or FTerminate do Application.ProcessMessages;
  if FTerminate then exit;
  if FList.Count>0 then
    for i := 0 to FList.Count-1 do
      if TComponent(FList.Items[i])=AComp then exit;
  FList.Add(AComp);
end;

constructor TKRTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProcessMessages:=0;
  FTerminate:=false;
  FEnabled := False;
  FInterval := 1000;
  FWindowHandle := AllocateHWnd(WndProc);
  FList:=TList.Create;
  FWorking:=false;
end;

procedure TKRTimer.DelMon(AComp: TComponent);
begin
  while FWorking or FTerminate do Application.ProcessMessages;
  if FTerminate then exit;
  FList.Remove(AComp);
end;

destructor TKRTimer.Destroy;
begin
  FTerminate:=true;
  while FWorking do Application.ProcessMessages;
  FList.Free;

  KillTimer(FWindowHandle, 1);
  DeallocateHWnd(FWindowHandle);

  inherited;
end;

procedure TKRTimer.DoTimer;
var
  i,n: integer;
  //s:String;
begin
  if(csDesigning in ComponentState) then exit;
  FWorking:=true;
  KillTimer(FWindowHandle, 1);

  try
    //s:='Start DoTimer;'#13#10;
    if Assigned(FOnTimer) then begin
      //s:=s+'FOnTimer(Self);'#13#10;
      FOnTimer(Self);
    end;

    if FList.Count>0 then begin
      n:=0;
      i:=0;
      repeat
       // s:=s+'DoTimer for Component.Name="'+TComponent(FList.Items[i]).Name+'";'#13#10;
        (TComponent(FList.Items[i]) as IKRTimer).DoTimer;
        inc(n);
        if(n=FProcessMessages)then begin
          //s:=s+'Application.ProcessMessages;'#13#10;
          Application.ProcessMessages;
          n:=0;
        end;
        inc(i);
      until (i>=FList.Count)or(FTerminate);
    end;
  except on E: Exception do
    REEvent(FRuntimeErrorEv,Self,'KRTimer[Name: "'+Name+'"]; procedure DoTimer;'{#13#10+s},E);
  end;
  if(FEnabled)and(FInterval>0)and(not FTerminate)then
    SetTimer(FWindowHandle, 1, FInterval, nil);
  FWorking:=false;
end;

procedure TKRTimer.SetEnabled(const Value: boolean);
begin
  if FEnabled<>Value then begin
    FEnabled := Value;
    if FWorking then exit;
    KillTimer(FWindowHandle, 1);
    if(FEnabled)and(FInterval>0)then
      SetTimer(FWindowHandle, 1, FInterval, nil);
  end;
end;

procedure TKRTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval<>Value then begin
    FInterval:=Value;
    if FInterval<1 then FInterval:=0;
    if FWorking then exit;
    KillTimer(FWindowHandle, 1);
    if(FEnabled)and(FInterval>0)then
      SetTimer(FWindowHandle, 1, FInterval, nil);
  end;
end;

procedure TKRTimer.SetProcessMessages(const Value: integer);
begin
  if Value<1 then FProcessMessages:=0 else FProcessMessages := Value;
end;

procedure TKRTimer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then DoTimer else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
