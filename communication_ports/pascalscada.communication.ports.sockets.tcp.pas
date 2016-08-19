unit pascalscada.communication.ports.sockets.tcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pascalscada.communication.ports.sockets.basesocket

  {$IFDEF UNIX}
  , pascalscada.communication.ports.sockets.unix
  {$ENDIF}
  {$IFDEF WINDOWS}
  , pascalscada.communication.ports.sockets.windows
  {$ENDIF}
  ;

type
  {$IFDEF UNIX}
  TpSCADABaseTCPSocket = TpSCADAUnixSocket;
  {$ENDIF}

  { TpSCADATCPSocket }

  TpSCADATCPSocket = Class(TpSCADABaseTCPSocket)
  protected
    function GetSocketType: TpSCADASocketType; override;
  published
    property Active;
    //: Enables the auto reconnection if a connection is lost or failed.
    property EnableAutoReconnect;
    //: Tells if the communication port is exclusive (if true, avoid it to open in design time).
    property ExclusiveDevice;
    //: IPv4 address of the server to connect.
    property IPv4Address;
    //: Server port to connect. To use Modbus, set this to 502 and to use Siemens ISOTCP set it to 102.
    property Port;
    //: Time to retry a lost connection in milliseconds.
    property ReconnectRetryInterval;
    //: Timeout in milliseconds to I/O operations.
    property Timeout;

    property  OnPortClose;
    property  OnPortCloseError;
    property  OnPortDisconnected;
    property  OnPortOpen;
    property  OnPortOpenError;
    property  OnReadError;
    property  OnWriteError;
  end;

implementation

{ TpSCADATCPSocket }

function TpSCADATCPSocket.GetSocketType: TpSCADASocketType;
begin
  Result:=stTCP;
end;

end.

