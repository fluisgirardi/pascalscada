{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_secure_actions;

interface

uses
  pascalscada.secure_actions.request_authorized_user, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_secure_actions', @Register);
end.
