{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_security;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.security.control_security_manager, 
  pascalscada.security.basic_user_management, 
  pascalscada.security.security_exceptions, 
  pascalscada.security.security_texts, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_security', @Register);
end.
