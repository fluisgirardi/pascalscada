{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_security;

interface

uses
  pascalscada.security.controls.control_security_manager, 
  pascalscada.security.users.basic_user_management, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_security', @Register);
end.
