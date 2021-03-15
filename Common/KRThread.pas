(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRThread                                                                  *)
(*  Ver.: 12.01.2021                                                          *)
(*  https://kandiral.ru/delphi/krthread.pas.html                              *)
(*                                                                            *)
(******************************************************************************)
unit KRThread;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, Vcl.Forms;
  {$ELSE}
    Classes, Forms;
  {$IFEND}

type
  TKRThreadProc = function(AThread: TThread; AData: Pointer): Pointer of object;

  TKRThread = class(TThread)
  private
    FPause: boolean;
    FWorking: boolean;
    FPaused: boolean;
    FWaitTime: Cardinal;
    FWaitPauseTime: Cardinal;
    FUseProcessMessages: boolean;
    function GetActive: boolean;
  protected
    procedure SetActive(const Value: boolean);virtual;
    procedure Execute; override;
    procedure KRExecute; virtual;
    procedure KRExecutePaused; virtual;
    procedure KRExecutePausedFirst; virtual;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    property Active: boolean read GetActive write SetActive;
    property Working: boolean read FWorking;
    property Pause: boolean read FPause write FPause;
    property Paused: boolean read FPaused;
    property WaitTime: Cardinal read FWaitTime write FWaitTime default 20;
    property WaitPauseTime: Cardinal read FWaitPauseTime write FWaitPauseTime default 1000;
    property UseProcessMessages: boolean read FUseProcessMessages write FUseProcessMessages;
  end;

  function KRRunInThread(AData: Pointer; AProc: TKRThreadProc): Pointer;

implementation

type
  TKRRunInThread = class(TThread)
  private
    Proc: TKRThreadProc;
    Data: Pointer;
  protected
    procedure Execute; override;
  public
    DataRes: Pointer;
    constructor Create(AProc: TKRThreadProc; AData: pointer);
  end;

  function KRRunInThread(AData: Pointer; AProc: TKRThreadProc): Pointer;
  var
    thread: TKRRunInThread;
  begin
    thread:=TKRRunInThread.Create(AProc, AData);
    while not thread.Terminated do Application.ProcessMessages;
    Result:=thread.DataRes;
    thread.Free;
  end;

{ TKRThread }

constructor TKRThread.Create;
begin
  FWorking:=false;
  FPause:=true;
  FPaused:=true;
  FWaitTime:=20;
  FWaitPauseTime:=1000;
  FUseProcessMessages:=true;
  inherited Create(false);
end;

destructor TKRThread.Destroy;
begin
  SetActive(false);
  Terminate;
  while FWorking do
    if FUseProcessMessages then Application.ProcessMessages else sleep(20);
  inherited;
end;

procedure TKRThread.Execute;
begin
  FWorking:=true;
  repeat
    if FPause then begin
      if not FPaused then begin
        KRExecutePausedFirst;
        FPaused:=true;
      end;
      KRExecutePaused;
      Sleep(FWaitPauseTime);
    end else begin
      FPaused:=false;
      KRExecute;
      Sleep(FWaitTime);
    end;
  until Terminated;
  FWorking:=false;
end;

function TKRThread.GetActive: boolean;
begin
  Result:=not FPause;
end;

procedure TKRThread.KRExecute;
begin

end;

procedure TKRThread.KRExecutePaused;
begin

end;

procedure TKRThread.KRExecutePausedFirst;
begin

end;

procedure TKRThread.SetActive(const Value: boolean);
begin
  if Value=GetActive then exit;
  FPause:=not Value;
  if FPause then
    while FPause and(not FPaused) do
      if FUseProcessMessages then Application.ProcessMessages else sleep(20);
end;

{ TKRRunInThread }

constructor TKRRunInThread.Create(AProc: TKRThreadProc; AData: pointer);
begin
  Proc:=AProc;
  Data:=AData;
  inherited Create(false);
end;

procedure TKRRunInThread.Execute;
begin
  DataRes:=Proc(Self,Data);
  Terminate;
end;

end.
