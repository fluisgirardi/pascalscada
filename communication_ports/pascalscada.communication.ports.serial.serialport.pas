unit pascalscada.communication.ports.serial.serialport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF UNIX}
  , pascalscada.communication.ports.serial.unixserialport
  {$ENDIF}
  {$IFDEF WINDOWS}

  {$ENDIF};

type

  {$IFDEF UNIX}
  TpSCADABaseSerialPort = class(TpSCADAUnixSerialPort);
  {$ENDIF}

  { TpSCADASerialPort }

  TpSCADASerialPort = class(TpSCADABaseSerialPort)
  published
    property Active;
    property AcceptAnyPortName;
    property BaudRate;
    property ClearBuffersOnCommErrors;
    property DataBits;
    property LastOSErrorNumber;
    property LastOSErrorMessage;
    property Parity;
    property SerialPort;
    property StopBits;
    property Timeout;


    property OnPortOpen;
    property OnPortClose;
    property OnPortOpenError;
    property OnReadError;
    property OnWriteError;
  end;

implementation

end.

