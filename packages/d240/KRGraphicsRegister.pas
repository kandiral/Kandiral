(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRGraphicsRegister                                                        *)
(*  Ver.: 26.10.2017                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRGraphicsRegister;

interface

uses Classes;

procedure Register;

implementation

uses KRPenStyle, KRPenWidth, KRColorBox;

procedure Register;
begin
  RegisterClasses([TKRPenStyle, TKRBLPenStyle, TKRPenWidth, TKRBLPenWidth, TKRColorBox,
    TKRBLColorBox]);
  RegisterComponents('KRGraphics', [TKRPenStyle, TKRBLPenStyle, TKRPenWidth, TKRBLPenWidth,
    TKRColorBox, TKRBLColorBox]);
end;

end.
