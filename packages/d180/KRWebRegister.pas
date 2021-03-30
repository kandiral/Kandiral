(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  http://kandiral.ru                                                        *)
(*                                                                            *)
(******************************************************************************)
unit KRWebRegister;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes;
  {$ELSE}
    Classes;
  {$IFEND}

procedure Register;

implementation

uses KRGoogleAuth, KRGoogleContacts;

procedure Register;
begin
  RegisterClasses([TKRGoogleAuth, TKRGoogleContacts]);
  RegisterComponents('KRWeb', [TKRGoogleAuth, TKRGoogleContacts]);
end;

end.
