{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_tags;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.tags.base, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_tags', @Register);
end.
