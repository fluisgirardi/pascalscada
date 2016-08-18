unit pascalscada.communication.ports.serial.baseserialport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  pascalscada.communication.ports.basecommport;

type

  {$IFDEF PORTUGUES}
  {:
  @name enumera as velocidades possíveis de comunicação serial.
  Essas velocidade são suportados em todos os sistemas operacionais.

  @value sbr110    = 110 bps
  @value sbr300    = 300 bps
  @value sbr600    = 600 bps
  @value sbr1200   = 1200 bps
  @value sbr2400   = 2400 bps
  @value sbr4800   = 4800 bps
  @value sbr9600   = 9600 bps
  @value sbr19200  = 19200 bps
  @value sbr38400  = 38400 bps
  @value sbr57600  = 57600 bps
  @value sbr115200 = 115200 bps
  }
  {$ELSE}
  {:
  @name enumerates all baud rates.
  This baud rates are supported on all OSes.

  @value sbr110    = 110 bps
  @value sbr300    = 300 bps
  @value sbr600    = 600 bps
  @value sbr1200   = 1200 bps
  @value sbr2400   = 2400 bps
  @value sbr4800   = 4800 bps
  @value sbr9600   = 9600 bps
  @value sbr19200  = 19200 bps
  @value sbr38400  = 38400 bps
  @value sbr57600  = 57600 bps
  @value sbr115200 = 115200 bps
  }
  {$ENDIF}
  TpSCADASerialBaudRate = (sbr110, sbr300, sbr600, sbr1200, sbr2400, sbr4800,
                           sbr9600, sbr19200, sbr38400, sbr57600, sbr115200);

  {$IFDEF PORTUGUES}
  {:
  @name enumera todos as possíveis quantidades de bits de parada.
  Essas quantidades são suportados em todos os sistemas operacionais.

  @value ssb1 = 1 stop bit
  @value ssb2 = 2 stop bit
  }
  {$ELSE}
  {:
  @name enumarates all stop bits.
  This values are supported on all OSes

  @value ssb1 = 1 stop bit
  @value ssb2 = 2 stop bit
  }
  {$ENDIF}
  TpSCADASerialStopBits = (ssb1, ssb2);

  {$IFDEF PORTUGUES}
  {:
  @name enumera as possíveis checagens de paridade.
  Essas paridades são suportados em todos os sistemas operacionais.

  @value spNone Não faz checagem de paridade.
  @value spOdd Checagem de erros por paridade impar.
  @value spEven Checagem de erros por paridade par.
  }
  {$ELSE}
  {:
  @name enumerates all parity modes.
  This values are supported on all OSes.

  @value spNone Don't check the parity.
  @value spOdd  Check errors using the odd parity.
  @value spEven Check errors using the even parity.
  }
  {$ENDIF}
  TpSCADASerialParity = (spNone, spOdd, spEven);

  {$IFDEF PORTUGUES}
  {:
  @name enumera os possíveis tamanhos de palavra de dados.
  Essas tamanhos são suportados em todos os sistemas operacionais.

  @value sdb5 A palavra de dados terá 5 bits de tamanho.
  @value sdb6 A palavra de dados terá 6 bits de tamanho.
  @value sdb7 A palavra de dados terá 7 bits de tamanho.
  @value sdb8 A palavra de dados terá 8 bits de tamanho.
  }
  {$ELSE}
  {:
  @name enumerates all data byte sizes.
  This values are supported on all OSes.

  @value sdb5 The data byte will have 5 bits of size.
  @value sdb6 The data byte will have 6 bits of size.
  @value sdb7 The data byte will have 7 bits of size.
  @value sdb8 The data byte will have 8 bits of size.
  }
  {$ENDIF}
  TpSCADASerialDataBits= (sdb5, sdb6, sdb7, sdb8);

  { TpSCADACustomSerialPort }

  TpSCADACustomSerialPort = class(TpSCADACustomCommPort)
  protected
    FBaudRate: TpSCADASerialBaudRate;
    FDataBits: TpSCADASerialDataBits;
    FParity: TpSCADASerialParity;
    FSerialPortName: Ansistring;
    FStopBits: TpSCADASerialStopBits;
    FTimeout: LongWord;
    PAcceptAnyPortName: Boolean;
    function  GetTimeout: LongWord;
    procedure SetBaudRate(AValue: TpSCADASerialBaudRate);
    procedure SetDataBits(AValue: TpSCADASerialDataBits);
    procedure SetParity(AValue: TpSCADASerialParity);
    procedure SetSerialPortName(AValue: Ansistring);
    procedure SetStopBits(AValue: TpSCADASerialStopBits);
    procedure SetTimeout(AValue: LongWord);
  protected
    procedure DoExceptionIfActive;
    procedure ClearIOBuffers; virtual;
    function  SerialPortExists(AValue:AnsiString):Boolean; virtual;
    procedure CallPortOpenHandlers; override;
    procedure CallPortOpenErrorHandlers; override;
    procedure CallPortCloseHandlers; override;
    procedure CallPortCloseErrorHandlers; override;
    procedure CallReadErrorHandlers; override;
    procedure CallWriteErrorHandlers; override;

    property AcceptAnyPortName:Boolean read PAcceptAnyPortName write PAcceptAnyPortName stored true default false;
    property SerialPort:Ansistring read FSerialPortName write SetSerialPortName;
    property BaudRate:TpSCADASerialBaudRate read FBaudRate write SetBaudRate default sbr19200;
    property DataBits:TpSCADASerialDataBits read FDataBits write SetDataBits default sdb8;
    property Parity:  TpSCADASerialParity   read FParity   write SetParity   default spNone;
    property StopBits:TpSCADASerialStopBits read FStopBits Write SetStopBits default ssb1;
    property Timeout:LongWord               read GetTimeout write SetTimeout default 100;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TpSCADACustomSerialPort }

procedure TpSCADACustomSerialPort.SetBaudRate(AValue: TpSCADASerialBaudRate);
begin
  DoExceptionIfActive;
  if FBaudRate=AValue then Exit;
  FBaudRate:=AValue;
end;

function TpSCADACustomSerialPort.GetTimeout: LongWord;
begin
  InterLockedExchange(Result,FTimeout);
end;

procedure TpSCADACustomSerialPort.SetDataBits(AValue: TpSCADASerialDataBits);
begin
  DoExceptionIfActive;
  if FDataBits=AValue then Exit;
  FDataBits:=AValue;
end;

procedure TpSCADACustomSerialPort.SetParity(AValue: TpSCADASerialParity);
begin
  DoExceptionIfActive;
  if FParity=AValue then Exit;
  FParity:=AValue;
end;

procedure TpSCADACustomSerialPort.SetSerialPortName(AValue: Ansistring);
begin
  DoExceptionIfActive;
  if FSerialPortName=AValue then Exit;
  if (Trim(AValue)<>'') and (not SerialPortExists(AValue)) then exit;
  FSerialPortName:=AValue;
end;

procedure TpSCADACustomSerialPort.SetStopBits(AValue: TpSCADASerialStopBits);
begin
  DoExceptionIfActive;
  if FStopBits=AValue then Exit;
  FStopBits:=AValue;
end;

procedure TpSCADACustomSerialPort.SetTimeout(AValue: LongWord);
var
  res: LongWord;
begin
  InterLockedExchange(res, FTimeout);
  if res=AValue then exit;
  InterLockedExchange(FTimeout,AValue);
end;

procedure TpSCADACustomSerialPort.DoExceptionIfActive;
begin
  if ReallyActive then
    exception.Create('Cannot change settings while active!');
end;

procedure TpSCADACustomSerialPort.ClearIOBuffers;
begin

end;

function TpSCADACustomSerialPort.SerialPortExists(AValue: AnsiString): Boolean;
begin
  Result:=false;
end;

procedure TpSCADACustomSerialPort.CallPortOpenHandlers;
begin
  inherited CallPortOpenHandlers;
  TThread.Queue(nil, @DoPortOpen);
end;

procedure TpSCADACustomSerialPort.CallPortOpenErrorHandlers;
begin
  inherited CallPortOpenErrorHandlers;
  TThread.Queue(nil, @DoPortOpenError);
end;

procedure TpSCADACustomSerialPort.CallPortCloseHandlers;
begin
  inherited CallPortCloseHandlers;
  TThread.Queue(nil, @DoPortClose);
end;

procedure TpSCADACustomSerialPort.CallPortCloseErrorHandlers;
begin
  inherited CallPortCloseErrorHandlers;
  TThread.Queue(nil, @DoPortCloseError);
end;

procedure TpSCADACustomSerialPort.CallReadErrorHandlers;
begin
  inherited CallReadErrorHandlers;
  TThread.Queue(nil, @DoReadError);
end;

procedure TpSCADACustomSerialPort.CallWriteErrorHandlers;
begin
  inherited CallWriteErrorHandlers;
  TThread.Queue(nil, @DoWriteError);
end;

constructor TpSCADACustomSerialPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBaudRate := sbr19200;
  FDataBits := sdb8;
  FParity   := spNone;
  FStopBits := ssb1;
  FTimeout  := 100;
  FExclusiveDevice:=true;
end;

end.

