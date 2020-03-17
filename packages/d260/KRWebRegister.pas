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

uses KRGoogleAuth, KRGoogleContacts, KRGoogleSearchConsole;

procedure Register;
begin
  RegisterClasses([TKRGoogleAuth, TKRGoogleContacts, TKRGoogleSearchConsole]);
  RegisterComponents('KRWeb', [TKRGoogleAuth, TKRGoogleContacts, TKRGoogleSearchConsole]);
end;

end.
