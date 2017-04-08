unit pascalscada_graphical_security_manager_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pascalscada.security.customized_user_management;


ResourceString
  SPascalSCADASecurityPalette = 'PascalSCADA User management';

procedure Register;

implementation

uses LResources;

procedure Register;
begin
  RegisterComponents(SPascalSCADASecurityPalette,[TpSCADACustomizedUserManagement]);
end;

{$IFDEF FPC}
initialization
  {$I graphical_security_manager.lrs}
{$ENDIF}

end.

