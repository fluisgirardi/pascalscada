{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_secure_controls;

interface

uses
  pascalscada.secure_controls.stdctrls.secure_label, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_secure_controls', @Register);
end.
