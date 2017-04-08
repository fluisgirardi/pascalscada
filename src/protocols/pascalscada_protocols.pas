{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_protocols;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.protocols.customprotocol, pascalscada.protocols.memory_manager, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_protocols', @Register);
end.
