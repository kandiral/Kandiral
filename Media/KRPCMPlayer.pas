(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRPCMPlayer                                                               *)
(*  Ver.: 09.01.2017                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRPCMPlayer;

interface

// http://delphiworld.narod.ru/base/about_sound_again.html
uses KRWavePlayer, Windows, MMSystem, KRWaveDiveces, KRThread, Funcs;

const
  KRPCMPLAYER_BUFFERS_COUNT = 100;

type
  TKRVolume = record
    case Integer of
     0: (Volume: Longint);
     1: (LeftVolume,
         RightVolume : Word);
  end;

  TKRPCMPlayerThread = class;

  TKRPCMPlayer = class(TKRWavePlayer)
  private
    wfx: TWAVEFORMATEX;
    FThread: TKRPCMPlayerThread;
    FOpen: boolean;
    FDevice: integer;
    FWaveOut: integer;
    FLock: TRTLCriticalSection;
    FChannels: integer;
    FSamplesPerSec: integer;
    FBitsPerSample: integer;
    FBuffer: pointer;
    FWrBufI, FRdBufI, FWrBufPos: Cardinal;
    FRdBufILast: integer;
    FBufHdr: array[0..KRPCMPLAYER_BUFFERS_COUNT-1] of TWaveHdr;
    FBlockSize: Cardinal;
    function GetVolume: TKRVolume;
    procedure SetVolume(const Value: TKRVolume);
  public
    constructor Create;
    destructor Destroy;override;
    property IsOpen: boolean read FOpen;
    procedure Open;
    procedure Close;
    procedure Play(var ABuffer; ALength: integer; AEnd: boolean = false);override;
    property Device: integer read FDevice write FDevice;
    property WaveOut: integer read FWaveOut write FWaveOut;
    property Volume: TKRVolume read GetVolume write SetVolume;
    property Channels: integer read FChannels write FChannels;
    property SamplesPerSec: integer read FSamplesPerSec  write FSamplesPerSec;
    property BitsPerSample: integer read FBitsPerSample write FBitsPerSample;
  end;

  TKRPCMPlayerThread = class(TKRThread)
  private
    FPlayer: TKRPCMPlayer;
  protected
    procedure KRExecute;override;
  public
    constructor CreateThread(Player: TKRPCMPlayer);
  end;

implementation

{ TKRPCMPlayer }

procedure TKRPCMPlayer.Close;
var i: integer;
begin
  FThread.Active:=false;
  EnterCriticalSection(FLock);
  try
    try
      waveOutReset(FWaveOut);
    except end;
    for I := 0 to KRPCMPLAYER_BUFFERS_COUNT-1 do
      waveOutUnprepareHeader(FWaveOut, @FBufHdr[i], sizeof(TWAVEHDR));
    VirtualFree(FBuffer, 0, MEM_RELEASE);
    WaveOutClose(FWaveOut);
  finally
    LeaveCriticalSection(FLock);
    FOpen:=false;
  end;
end;

constructor TKRPCMPlayer.Create;
begin
  FThread:=TKRPCMPlayerThread.CreateThread(Self);
  FDevice:=KRGetDefaultWaveOutDevice;
  FWaveOut:=0;
  FChannels:=1;
  FSamplesPerSec:=44100;
  FBitsPerSample:=16;
  FOpen:=false;
  InitializeCriticalSection(FLock);
end;

destructor TKRPCMPlayer.Destroy;
begin
  Close;
  FThread.Free;
  DeleteCriticalSection(FLock);
  inherited;
end;

function TKRPCMPlayer.GetVolume: TKRVolume;
begin
  waveOutGetVolume(0, @Result.Volume);
end;

procedure TKRPCMPlayer.Open;
var
  si: TSYSTEMINFO;
  i: integer;
begin
  Close;
  FillChar(wfx, Sizeof(TWAVEFORMATEX), #0);
  wfx.wFormatTag:=WAVE_FORMAT_PCM;
  wfx.nChannels:=FChannels;
  wfx.nSamplesPerSec:=FSamplesPerSec;
  wfx.wBitsPerSample:=FBitsPerSample;
  wfx.nBlockAlign:=FBitsPerSample div 8 * FChannels;
  wfx.nAvgBytesPerSec:=FSamplesPerSec * wfx.nBlockAlign;
  wfx.cbSize:=0;
  GetSystemInfo(si);
  FBlockSize:=wfx.nAvgBytesPerSec div 25;

  if WaveOutOpen(@FWaveOut, FDevice, @wfx, 0, 0, CALLBACK_NULL) = MMSYSERR_NOERROR then begin
    FBuffer:=VirtualAlloc(nil, (FBlockSize * KRPCMPLAYER_BUFFERS_COUNT + si.dwPageSize - 1) div
      si.dwPagesize * si.dwPageSize,
      MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
    for I := 0 to KRPCMPLAYER_BUFFERS_COUNT-1 do begin
      FillChar(FBufHdr[I], sizeof(TWAVEHDR), #0);
      FBufHdr[i].lpData:=Pointer(Cardinal(FBuffer)+Cardinal(i)*FBlockSize);
      FBufHdr[i].dwBufferLength:=FBlockSize;
      waveOutPrepareHeader(FWaveOut, @FBufHdr[i], sizeof(TWAVEHDR));
    end;
    FWrBufI:=0;
    FWrBufPos:=0;
    FRdBufI:=0;
    FRdBufILast:=-1;
    FThread.Active:=true;
    FOpen:=true;
  end;
end;

procedure TKRPCMPlayer.Play(var ABuffer; ALength: integer; AEnd: boolean);
var
  i: integer;
  bt: byte;
  B: PByte;
begin
  if not FOpen then exit;
  EnterCriticalSection(FLock);
  try
    B:=PByte(@ABuffer);
    for I := 0 to ALength-1 do begin
      PByte(Cardinal(FBuffer)+FWrBufPos)^:=B[i];
      inc(FWrBufPos);
      if FWrBufPos=FBlockSize*KRPCMPLAYER_BUFFERS_COUNT then begin
        FWrBufI:=0;
        FWrBufPos:=0;
      end else begin
        FWrBufI:=FWrBufPos div FBlockSize;
      end;
    end;
    if AEnd and(FWrBufPos mod FBlockSize >0) then begin
      while FWrBufPos mod FBlockSize >0 do begin
        if FWrBufPos*8 mod wfx.wBitsPerSample = 0 then bt:=$7f else bt:=$ff;
        PByte(Cardinal(FBuffer)+FWrBufPos)^:=bt;
        inc(FWrBufPos);
        if FWrBufPos=FBlockSize*KRPCMPLAYER_BUFFERS_COUNT then begin
          FWrBufI:=0;
          FWrBufPos:=0;
        end else begin
          FWrBufI:=FWrBufPos div FBlockSize;
        end;
      end;

    end;

  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKRPCMPlayer.SetVolume(const Value: TKRVolume);
begin
  waveOutSetVolume(0, Volume.Volume);
end;

{ TKRPCMPlayerThread }

constructor TKRPCMPlayerThread.CreateThread(Player: TKRPCMPlayer);
begin
  FPlayer:=Player;
  inherited Create;
  WaitTime:=2;
end;

procedure TKRPCMPlayerThread.KRExecute;
begin
  if FPlayer.FRdBufI=FPlayer.FWrBufI then exit;
  waveOutWrite(FPlayer.FWaveOut, @FPlayer.FBufHdr[FPlayer.FRdBufI], sizeof(TWAVEHDR));
  inc(FPlayer.FRdBufI);
  if FPlayer.FRdBufI=KRPCMPLAYER_BUFFERS_COUNT then FPlayer.FRdBufI:=0;
end;

end.
