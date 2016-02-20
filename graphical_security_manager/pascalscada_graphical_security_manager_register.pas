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

procedure Register;
begin
  RegisterComponents(SPascalSCADASecurityPalette,[TpSCADACustomizedUserManagement]);
end;

end.

