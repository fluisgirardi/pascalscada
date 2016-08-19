unit pascalscada.communication.ports.sockets.unix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets,
  pascalscada.communication.ports.basecommport,
  pascalscada.communication.ports.sockets.basesocket;

type

  { TpSCADAUnixSocket }

  TpSCADAUnixSocket = class(TpSCADACustomSocket)
  protected
    procedure CheckSocket(var Ok: Boolean); override;
    procedure CloseMySocket(var Closed: Boolean); override;
    procedure ConnectSocket(var Ok: Boolean); override;
    function  InvalidSocket:Tsocket; override;
    procedure ReconnectSocket(var Ok: Boolean);  override;
  protected
    function CheckConnection(var CommResult:LongInt; var incRetries:Boolean):Boolean; override;
    function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:TpSCADASockLen; ConnectTimeout:LongInt):LongInt; override;
    function connect_without_timeout(sock:Tsocket; address:PSockAddr; address_len:TpSCADASockLen):LongInt; override;
    function setblockingmode(sock:Tsocket; mode:LongInt):LongInt; override;
    function socket_recv(buf:PByte; len: Cardinal; flags, recv_timeout: LongInt):LongInt; override;
    function socket_send(buf:PByte; len: Cardinal; flags, send_timeout: LongInt):LongInt; override;
  end;

implementation

uses netdb, Unix, baseunix, termio ;

{ TpSCADAUnixTCPSocket }

procedure TpSCADAUnixSocket.CheckSocket(var Ok: Boolean);
begin
  Ok:=ReallyActive;
end;

procedure TpSCADAUnixSocket.CloseMySocket(var Closed: Boolean);
var
  buffer:TBytes;
  lidos:LongInt;
  ASocket: Tsocket = -1;
begin
  InterLockedExchange(ASocket, FSocket);
  InterLockedExchange(FSocket, InvalidSocket);

  closed:=false;
  if ASocket>=0 then begin
    SetLength(buffer,5);
    fpshutdown(ASocket,SHUT_WR);
    lidos := fprecv(ASocket, @Buffer[0], 1, MSG_PEEK);
    while lidos>0 do begin
      lidos := fprecv(ASocket, @Buffer[0], 1, 0);
      lidos := fprecv(ASocket, @Buffer[0], 1, MSG_PEEK);
    end;

    CloseSocket(ASocket);
  end;
  closed:=true;
  SetLength(Buffer,0);
end;

procedure TpSCADAUnixSocket.ConnectSocket(var Ok: Boolean);
var
  ServerAddr:THostEntry;
  channel:sockaddr_in;
  flag, bufsize, sockType, sockProto:LongInt;
  ASocket:Tsocket;
  MustCloseSocket: Boolean;
begin
  Ok:=false;
  MustCloseSocket:=false;
  try
    //##########################################################################
    // RESOLUCAO DE NOMES SOBRE LINUX/FREEBSD e outros.
    // NAME RESOLUTION OVER LINUX/FREEBSD and others.
    //##########################################################################
    if not GetHostByName(FIPv4Address,{%H-}ServerAddr) then begin
      ServerAddr.Addr:=StrToHostAddr(FIPv4Address);
      if ServerAddr.Addr.s_addr=0 then begin
        RefreshLastOSError;
        exit;
      end;
    end;

    //##########################################################################
    // CRIA O SOCKET
    // CREATE THE SOCKET.
    //##########################################################################
    case FSocketType of
      stTCP:
        begin
          sockProto := IPPROTO_TCP;
          sockType  := SOCK_STREAM;
        end;
      stUDP:
        begin
          sockProto := IPPROTO_UDP;
          sockType  := SOCK_DGRAM;
        end
      else begin
        exit;
      end;
    end;

    ASocket := fpSocket(PF_INET, sockType, sockProto);

    if ASocket<0 then begin
      RefreshLastOSError;
      exit;
    end;

    MustCloseSocket:=true;

    //##########################################################################
    //SETA AS OPCOES DO SOCKET
    //OPCOES DE TIMEOUT IRÃO SER FEITAS USANDO SELECT/FPSELECT
    //POIS ESTAS OPÇÕES NÃO SAO SUPORTADAS POR ALGUNS SISTEMAS OPERACIONAIS
    //
    //SOCKET OPTIONS
    //TIMEOUT OPTIONS ARE MADE USING SELECT/FPSELECT, BECAUSE THIS OPTIONS
    //AREN'T SUPPORTED BY SOME OSes LIKE WINDOWS CE
    //##########################################################################
    flag:=1;
    bufsize := 1024*16;
    fpsetsockopt(ASocket, SOL_SOCKET,  SO_RCVBUF,    @bufsize,  sizeof(LongInt));
    fpsetsockopt(ASocket, SOL_SOCKET,  SO_SNDBUF,    @bufsize,  sizeof(LongInt));
    fpsetsockopt(ASocket, IPPROTO_TCP, TCP_NODELAY,  @flag,     sizeof(LongInt));

    //##########################################################################
    //CONFIGURA E ENDERECO QUE O SOCKET VAI CONECTAR
    //SETS THE TARGET ADDRESS TO SOCKET CONNECT
    //##########################################################################
    channel.sin_family      := AF_INET;            //FAMILY
    channel.sin_port        := htons(FPortNumber); //PORT NUMBER
    channel.sin_addr.S_addr := longword(htonl(LongWord(ServerAddr.Addr.s_addr)));


    //##########################################################################
    //SETA O MODO DE OPERACAO DE NAO BLOQUEIO DE CHAMADA.
    //SET THE NON-BLOCKING OPERATING MODE OF THE SOCKET
    //##########################################################################
    setblockingmode(ASocket,MODE_NONBLOCKING);

    if connect_with_timeout(ASocket,@channel,sizeof(channel),FTimeout)<>0 then begin
      RefreshLastOSError;
      exit;
    end;

    Ok:=true;
    InterLockedExchange(FSocket,ASocket);
    CallPortOpenHandlers;
  finally
    if not Ok then begin
      if MustCloseSocket then begin
        fpshutdown(ASocket,SHUT_WR);
        CloseSocket(ASocket);
      end;
      CloseMySocket(Ok);
      Ok:=false;
      CallPortOpenErrorHandlers;
    end;
  end;
end;

function TpSCADAUnixSocket.InvalidSocket: Tsocket;
begin
  Result:=-1;
end;

procedure TpSCADAUnixSocket.ReconnectSocket(var Ok: Boolean);
begin
  CloseMySocket(Ok);
  connectSocket(Ok);
end;

function TpSCADAUnixSocket.CheckConnection(var CommResult: LongInt;
  var incRetries: Boolean): Boolean;
var
  retval, nbytes:LongInt;
  sel:tpollfd;
  closed: Boolean = false;
begin
  Result:=true;

  retval:=0;
  nbytes:=0;
  retval:=FpIOCtl(FSocket,FIONREAD,@nbytes);

  if retval<>0 then begin
    CloseMySocket(closed);
    CallPortDisconnectedHandlers;
    CommResult:=iorPortError;
    Result:=false;
    exit;
  end;

  if (nbytes>0) then begin   // there is something in receive buffer, it doesn't seem the socket has been closed
    Result:=true;
    exit;
  end;


  sel.fd:=FSocket;
  sel.events:=POLLIN or POLLOUT or POLLPRI;
  sel.revents:=0;

  retval:=FpPoll(@sel,1,1);

  if (retval=0) then begin   //timeout, appears to be ok...
    Result:=true;
    CommResult:=iorTimeOut;
    incRetries:=true;
    exit;
  end;

  if (retval<0) then begin //error on socket...
    CloseMySocket(closed);
    CallPortDisconnectedHandlers;
    CommResult:=iorPortError;
    Result:=false;
    exit;
  end;

  if (retval=1) then begin  // seems there is something in our receive buffer!!
    if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then begin
      CloseMySocket(closed);
      CallPortDisconnectedHandlers;
      CommResult:=iorPortError;
      Result:=false;
      exit;
    end;

    // now we check how many bytes are in receive buffer
    retval:=FpIOCtl(FSocket,FIONREAD,@nbytes);

    if (retval<>0) then begin  // some error occured
      CloseMySocket(closed);
      CallPortDisconnectedHandlers;
      CommResult:=iorPortError;
      Result:=false;
      exit;
    end;

    if (nbytes=0) then begin
      CloseMySocket(closed);
      CallPortDisconnectedHandlers;
      CommResult:=iorNotReady;
      Result:=false;
      exit;
    end;

    incRetries:=true;
  end;

end;

function TpSCADAUnixSocket.connect_with_timeout(sock: Tsocket;
  address: PSockAddr; address_len: TpSCADASockLen; ConnectTimeout: LongInt
  ): LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=0;

  if fpconnect(sock, address, address_len) <> 0 then begin
    if fpGetErrno = ESysEINPROGRESS then begin

      sel.fd:=sock;
      sel.events:=POLLIN or POLLPRI or POLLOUT;
      sel.revents:=0;

      mode := FpPoll(@sel,1,ConnectTimeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=0;  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;   //error.
  end;
end;

function TpSCADAUnixSocket.connect_without_timeout(sock: Tsocket;
  address: PSockAddr; address_len: TpSCADASockLen): LongInt;
begin
  Result:=0;

  if fpconnect(sock, address, address_len) <> 0 then begin
    Result := -1;   //error.
  end;
end;

function TpSCADAUnixSocket.setblockingmode(sock: Tsocket; mode: LongInt
  ): LongInt;
var
  oldflags:LongInt;
begin
  oldflags := FpFcntl(sock, F_GETFL, 0);
  if (oldflags < 0) then begin
    Result:= oldflags;
    exit;
  end;

  if mode=MODE_NONBLOCKING then
    oldflags := oldflags or O_NONBLOCK
  else
    oldflags := oldflags xor O_NONBLOCK;

  Result:= FpFcntl(sock, F_SETFL, oldflags);
end;

function TpSCADAUnixSocket.socket_recv(buf: PByte; len: Cardinal; flags,
  recv_timeout: LongInt): LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=fprecv(FSocket, buf, len, flags or msg_nosignal);

  if  Result < 0 then begin
    if fpGetErrno in [ESysEINTR, ESysEAGAIN] then begin
      sel.fd:=FSocket;
      sel.events:=POLLIN;

      mode := FpPoll(@sel, 1, recv_timeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=fprecv(FSocket, buf, len, flags);  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;
  end;
end;

function TpSCADAUnixSocket.socket_send(buf: PByte; len: Cardinal; flags,
  send_timeout: LongInt): LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=fpsend(FSocket, buf, len, flags or msg_nosignal);

  if Result < 0 then begin
    if fpGetErrno in [ESysEINTR, ESysEAGAIN] then begin
      sel.fd:=FSocket;
      sel.events:=POLLOUT;

      mode := FpPoll(@sel, 1, send_timeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=fpsend(FSocket, buf, len, flags);  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;
  end;
end;

end.

