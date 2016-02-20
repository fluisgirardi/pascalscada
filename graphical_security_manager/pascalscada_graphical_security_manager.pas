{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_graphical_security_manager;

interface

uses
  pascalscada.security.basic_graphical_user_management, 
  pascalscada.security.customized_user_management, 
  pascalscada_graphical_security_manager_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalscada_graphical_security_manager_register', 
    @pascalscada_graphical_security_manager_register.Register);
end;

initialization
  RegisterPackage('pascalscada_graphical_security_manager', @Register);
end.
