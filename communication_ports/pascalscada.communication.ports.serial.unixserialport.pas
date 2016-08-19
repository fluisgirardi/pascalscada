unit pascalscada.communication.ports.serial.unixserialport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, pascalscada.communication.ports.serial.baseserialport;

type

  { TpSCADALinuxSerialPort }

  { TpSCADAUnixSerialPort }

  TpSCADAUnixSerialPort = class(TpSCADACustomSerialPort)
  private
    FPortHandle:cint;
  protected
    function SerialPortExists(AValue: AnsiString): Boolean; override;
    function PortSettingsOK: Boolean; override;
    function Open: Boolean; override;
    function Close: Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure ClearIOBuffers; override;
    function Read(buffer: PByte; buffer_size, max_retries: LongInt;
      var bytes_read: LongInt): LongInt; override;
    function ReallyActive: Boolean; override;
    function Write(buffer: PByte; buffer_size, max_retries: LongInt;
      var bytes_written: LongInt): LongInt; override;
  end;


{$IFDEF UNIX}
var
  {$IFDEF LINUX}
  PortPrefix:array[0..2] of AnsiString = ('tty','ttyUSB','ttyACM');
  {$ENDIF}
  {$IFDEF FREEBSD}
  PortPrefix:array[0..0] of AnsiString = ('cuad');
  {$ENDIF}
  {$IFDEF NETBSD}
  PortPrefix:array[0..0] of AnsiString = ('cuad');
  {$ENDIF}
  {$IFDEF OPENBSD}
  PortPrefix:array[0..0] of AnsiString = ('cuad');
  {$ENDIF}
  {$ifdef SunOS}
  PortPrefix:array[0..0] of AnsiString = ('tty');
  {$ENDIF}
  //if your OS is not listed here, please send a patch
  //to fabio@pascalscada.com
{$ENDIF}

implementation

uses pascalscada.communication.ports.basecommport, Unix, BaseUnix, termio,
     pascalscada.utilities.datetime, dateutils;

{ TpSCADALinuxSerialPort }

function TpSCADAUnixSerialPort.ReallyActive: Boolean;
begin
  Result:=FPortHandle>=0;
end;

procedure TpSCADAUnixSerialPort.ClearIOBuffers;
begin
  if ReallyActive then begin
    //flush buffers...
    tcflush(FPortHandle, TCIFLUSH);
    tcflush(FPortHandle, TCIOFLUSH);
    //purge comm...
    {$IFDEF LINUX}
    fpioctl(LongInt(FPortHandle), TCIOFLUSH, nil);
    {$ELSE}
    fpioctl(LongInt(FPortHandle), TIOCFLUSH, nil);
    {$ENDIF}
  end;
end;

function TpSCADAUnixSerialPort.SerialPortExists(AValue: AnsiString): Boolean;
var
   c:LongInt;
begin
  if AcceptAnyPortName then begin
    Result:=FileExists('/dev/'+AValue);
    exit;
  end;

  Result := false;
  for c:=0 to high(PortPrefix) do
    if (Pos(PortPrefix[c], AValue)>0) and FileExists('/dev/'+AValue) then begin
      Result:=true;
      exit;
    end;
end;

function TpSCADAUnixSerialPort.PortSettingsOK: Boolean;
begin
  Result:=SerialPortExists(FSerialPortName);
end;

function TpSCADAUnixSerialPort.Open: Boolean;
var
   tios:termios;
   PPortHandle, r: cint;

begin
  Result := false;
  //abre a porta
  //open the serial port

  PPortHandle := fpopen('/dev/'+FSerialPortName, O_RDWR or O_NOCTTY or O_NONBLOCK);
  if PPortHandle<0 then begin
     RefreshLastOSError;
     CallPortOpenErrorHandlers;
     exit;
  end;

  fillchar({%H-}tios, sizeof(tios), #0);

  tios.c_oflag := 0;
  tios.c_lflag := 0;

  //velocidade
  //sets the baudrate
  case FBaudRate of
     sbr110: begin
       tios.c_ispeed := B110;
       tios.c_ospeed := B110;
     end;
     sbr300: begin
       tios.c_ispeed := B300;
       tios.c_ospeed := B300;
     end;
     sbr600: begin
       tios.c_ispeed := B600;
       tios.c_ospeed := B600;
     end;
     sbr1200: begin
       tios.c_ispeed := B1200;
       tios.c_ospeed := B1200;
     end;
     sbr2400: begin
       tios.c_ispeed := B2400;
       tios.c_ospeed := B2400;
     end;
     sbr4800: begin
       tios.c_ispeed := B4800;
       tios.c_ospeed := B4800;
     end;
     sbr9600: begin
       tios.c_ispeed := B9600;
       tios.c_ospeed := B9600;
     end;
     sbr38400: begin
       tios.c_ispeed := B38400;
       tios.c_ospeed := B38400;
     end;
     sbr57600: begin
       tios.c_ispeed := B57600;
       tios.c_ospeed := B57600;
     end;
     sbr115200: begin
       tios.c_ispeed := B115200;
       tios.c_ospeed := B115200;
     end;
     else begin
       tios.c_ispeed := B19200;
       tios.c_ospeed := B19200;
     end;
  end;

  tios.c_cflag := tios.c_ispeed or CREAD or CLOCAL;

  //databits
  case FDataBits of
     sdb5:
        tios.c_cflag := tios.c_cflag or CS5;
     sdb6:
        tios.c_cflag := tios.c_cflag or CS6;
     sdb7:
        tios.c_cflag := tios.c_cflag or CS7;
     else
        tios.c_cflag := tios.c_cflag or CS8;
  end;

  //seta paridade, tamanho do byte, stop bits...
  //data byte size, parity and stop bits
  case FParity of
    spOdd:
      tios.c_cflag := tios.c_cflag or PARENB or PARODD;
    spEven:
      tios.c_cflag := tios.c_cflag or PARENB;
  end;


  if FStopBits=ssb2 then
     tios.c_cflag := tios.c_cflag or CSTOPB;

  tcflush(PPortHandle, TCIFLUSH);

  r := tcsetattr(PPortHandle, TCSANOW, tios);
  if (r = -1) then begin
     RefreshLastOSError;
     CallPortOpenErrorHandlers;
     exit;
  end;

  tcflush(PPortHandle, TCIOFLUSH);

  //seta o uso exclusivo da porta.
  //makes the serial port for exclusive access
  fpioctl(LongInt(PPortHandle), TIOCEXCL, nil);

  ClearIOBuffers;

  InterLockedExchange(FPortHandle, PPortHandle);

  CallPortOpenHandlers;

  Result := true;
end;

function TpSCADAUnixSerialPort.Close: Boolean;
begin
  Result := false;
  if ReallyActive then begin
    ClearIOBuffers;
    FpClose(FPortHandle);
    InterLockedExchange(FPortHandle, -1);
  end;
  CallPortCloseHandlers;
  Result := true;
end;

function TpSCADAUnixSerialPort.Read(buffer: PByte; buffer_size,
  max_retries: LongInt; var bytes_read: LongInt): LongInt;
var
  lidos:cint64;
  tentativas:Cardinal;
  start:TDateTime;
  Req, Rem:TimeSpec;
begin
  Result:=iorNone;

  if BeingDestroyed then exit;

  tentativas := 0;
  start := CrossNow;

  bytes_read := 0;

  While (bytes_read<buffer_size) and (tentativas<max_retries) do begin
    lidos := FpRead(FPortHandle,buffer[bytes_read], buffer_size-bytes_read);
    if lidos>=0 then begin
      bytes_read := bytes_read + lidos;
      if (MilliSecondsBetween(CrossNow, start)>FTimeout) then begin
        inc(tentativas);
        start:=CrossNow;
      end;
      //faz esperar 0,1ms
      //waits 0,1ms
      Req.tv_sec:=0;
      Req.tv_nsec:=100000;
      FpNanoSleep(@Req,@Rem);
    end else begin
      Result:=iorPortError;
      RefreshLastOSError;
      Break;
    end;
  end;

  if buffer_size>bytes_read then begin
    if Result=iorNone then
      Result := iorTimeOut;
    if FClearBufOnErr then
      ClearIOBuffers;
  end else
    Result := bytes_read;

  if Result<0 then
    CallReadErrorHandlers;
end;

function TpSCADAUnixSerialPort.Write(buffer: PByte; buffer_size,
  max_retries: LongInt; var bytes_written: LongInt): LongInt;
var
  escritos:cint64;
  tentativas:Cardinal;
begin
  Result:=iorNone;

  if BeingDestroyed then exit;

  tentativas := 0;

  bytes_written := 0;
  While (bytes_written<buffer_size) and (tentativas<max_retries) do begin
    escritos := FpWrite(FPortHandle, buffer[bytes_written], buffer_size-bytes_written);
    if escritos>=0 then begin
      bytes_written := bytes_written + escritos;
      Inc(tentativas);
    end else begin
      Result:=iorPortError;
      RefreshLastOSError;
      Break;
    end;
  end;

  if buffer_size>bytes_written then begin
    if Result=iorNone then
      Result := iorTimeOut;
    if FClearBufOnErr then
       ClearIOBuffers;
  end else
    Result := bytes_written;
end;

constructor TpSCADAUnixSerialPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPortHandle:=-1;
end;

end.

