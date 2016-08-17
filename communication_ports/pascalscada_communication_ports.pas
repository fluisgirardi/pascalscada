{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_communication_ports;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.communication.ports.basecommport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_communication_ports', @Register);
end.
