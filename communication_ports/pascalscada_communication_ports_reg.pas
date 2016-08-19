unit pascalscada_communication_ports_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.communication.ports.serial.serialport,
  pascalscada.communication.ports.sockets.tcp,
  pascalscada.communication.ports.sockets.udp;

procedure Register;

resourcestring
  SPascalSCADA_CommunicationPorts = 'PascalSCADA Ports';

implementation

procedure Register;
begin
  RegisterComponents(SPascalSCADA_CommunicationPorts,[TpSCADASerialPort,
                                                      TpSCADATCPSocket,
                                                      TpSCADAUDPSocket]);
end;

end.

