(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRWavePlayer                                                              *)
(*  Ver.: 29.12.2016                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRWavePlayer;

interface

type
  TKRWavePlayer = class
  public
    procedure Play(var ABuffer; ALength: integer; AEnd: boolean = false);virtual;
  end;

implementation

{ TKRWavePlayer }

procedure TKRWavePlayer.Play(var ABuffer; ALength: integer; AEnd: boolean);
begin

end;

end.
