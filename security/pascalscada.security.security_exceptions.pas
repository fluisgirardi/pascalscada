unit pascalscada.security.security_exceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pascalscada.security.control_security_manager,
  pascalscada.security.security_texts;

type

  { ESecuritySystemAccessDenied }

  ESecuritySystemAccessDenied = class(Exception)
  public
    constructor Create(Token:UTF8String);
  end;

implementation

{ ESecuritySystemAccessDenied }

constructor ESecuritySystemAccessDenied.Create(Token: UTF8String);
begin
  if GetPascalSCADAControlSecurityManager.HasUserLoggedIn then
    inherited Create(Format(SUserHasNotAllowedToAccessObject,[GetPascalSCADAControlSecurityManager.GetCurrentUserlogin, Token]))
  else
    inherited Create(Format(SNoUserLoggedInToAccessObject,[Token]));
end;

end.

