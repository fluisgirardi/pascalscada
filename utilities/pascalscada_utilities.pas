{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_utilities;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.utilities.strings, pascalscada.utilities.datetime, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_utilities', @Register);
end.
