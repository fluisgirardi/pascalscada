{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_communication_ports;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.communication.ports.basecommport, 
  pascalscada.communication.ports.serial.baseserialport, 
  pascalscada.communication.ports.serial.serialport, 
  pascalscada_communication_ports_reg, 
  pascalscada.communication.ports.sockets.basesocket, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalscada_communication_ports_reg', 
    @pascalscada_communication_ports_reg.Register);
end;

initialization
  RegisterPackage('pascalscada_communication_ports', @Register);
end.
