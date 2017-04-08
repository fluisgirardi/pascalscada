{$IFDEF PORTUGUES}
{:
 @abstract(Unit que implementa o enfileiramento de mensagens internas do programa,
           multiplataforma.)

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

 Feito para suprir a necessidade de comunicação inter-processos (threads) de
 forma independente de sistema operacional.

 Sua implementação dos métodos é semelhante ao sistema de mensagens do windows,
 porem com algumas melhorias, tais como prioridade de mensagens (mensagens que
 devem ser processadas antes de outras).
}
{$ELSE}
{:
 @abstract(Unit that implements a multi-platform message queue.)

 @author(Fabio Luis Girardi <fabio@pascalscada.com>)

 Built to allow threads exchange messages independently of operating system.

 The implementation of the message queue is similar of the message system of the
 Windows operating System, but include some improvements, like messages with
 priority (messages that must be processed before others).
}
{$ENDIF}
unit pascalscada.multithreading.message_queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Estrutura que identifica uma mensagem.
  @member MsgID Classifica a mensagem.
  @member lParam Ponteiro para dados (caso necessário).
  @member wParam Ponteiro para dados (caso necessário).
  @member Priority Identifica se é uma mensagem com alta prioridade.
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Message record.
  @member MsgID  Message identification.
  @member lParam Ponteiro para dados (caso necessário).
  @member wParam Ponteiro para dados (caso necessário).
  @member Priority Identifica se é uma mensagem com alta prioridade.
  }
  {$ENDIF}
  TpSCADAMsg=record
    MsgID:Cardinal;
    lParam:Pointer;
    wParam:Pointer;
    Priority:Boolean;
  end;
  {$IFDEF PORTUGUES}
  //: Aponta para uma mensagem
  {$ELSE}
  //: Points to an message.
  {$ENDIF}
  PpSCADAMsg = ^TpSCADAMsg;

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Estrutura usada para montar uma fila de mensagens.
  @member Msg Mensagem do elemento da fila.
  @member NextMsg Aponta para o próximo elemento da fila de mensagens. Tem o valor @code(nil) caso esse elemento seja o último.
  @member PriorMsg Aponta para o elemento anterior da fila de mensagens. Tem o valor @code(nil) caso esse elemento seja o primeiro.
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Queue item record.
  @member Msg Message.
  @member NextMsg Points to the next message. If is the last item of the queue, it's @code(nil).
  @member PriorMsg Points to the prior message. If is the first item of the queue, it's @code(nil).
  }
  {$ENDIF}
  TpSCADAMsgPkg = record
    Msg:TpSCADAMsg;
    NextMsg:Pointer;
    PriorMsg:Pointer;
  end;

  {$IFDEF PORTUGUES}
  //: Ponteiro de um elemento da fila de mensagens.
  {$ELSE}
  //: Points to a queue item.
  {$ENDIF}
  PpSCADAMsgPkg = ^TpSCADAMsgPkg;


  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe de enfileiramento de mensagens internas do programa)

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Classe criada para suprir a necessidade de comunicação inter-processos (threads)
  de forma independente de sistema operacional.

  Sua implementação é semelhante ao sistema de mensagens do Windows,
  porem com algumas melhorias, tais como prioridade de mensagens (mensagens que
  devem ser processadas antes que outras).
  }
  {$ELSE}
  {:
   @abstract(Class of a multi-platform message queue.)

   @author(Fabio Luis Girardi <fabio@pascalscada.com>)

   Built to allow threads exchange messages independently of operating system.

   The implementation of the message queue is similar of the message system of the
   Windows operating System, but include some improvements, like messages with
   priority (messages that must be processed before others).
  }
  {$ENDIF}

  { TpSCADAThreadUnsafeMessageQueue }

  TpSCADAThreadUnsafeMessageQueue = class(TObject)
  protected
    FirstMessage:PpSCADAMsgPkg;
    NormalMsgs:PpSCADAMsgPkg;
    LastMsg:PpSCADAMsgPkg;
    FMsgCount:LongInt;
  public
    {$IFDEF PORTUGUES}
    //: Cria um objeto de enfileiramento de mensagens.
    {$ELSE}
    //: Creates the message queue.
    {$ENDIF}
    constructor Create; virtual;

    {$IFDEF PORTUGUES}
    //: Destroi um objeto de enfileiramento de mensagens.
    {$ELSE}
    //: Destroys the message queue.
    {$ENDIF}
    destructor  Destroy; override;

    function CreateNewMessagePkg(MsgID:Cardinal; wParam, lParam:Pointer; Priority:Boolean):PpSCADAMsgPkg; virtual;

    {$IFDEF PORTUGUES}
    {:
    Procura uma mensagem podendo retirá-la ou não da fila. O critério de seleção
    é que o MsgID seja maior ou igual a uFilterMinMsg e menor ou igual a
    uFilterMaxMsg. Para selecionar a primeira mensagem da fila, passe o valor 0
    (zero) para os parametros uFilterMinMsg e uFilterMaxMsg.

    @param(Msg TMsg. Variável onde é retornada a mensagem da fila.)
    @param(uFilterMinMsg Cardinal. Filtra mensagem com MsgID maiores ou iguais aos
           valores passados nesse parametro.)
    @param(uFilterMaxMsg Cardinal. Filtra mensagem com MsgID menores ou iguais aos
           valores passados nesse parametro.)
    @param(Remove Boolean. @true se a mensagem encontrada deve ser removida
           da fila.)

    @bold(Caso uFilterMinMsg e uFilterMaxMsg sejam iguais e maiores que zero,
          procura as mensagens cujo o MsgID seja igual a esses dois parametros.)

    @return(Retorna @true se encontra alguma mensagem.)

    @seealso(PostMessage)
    }
    {$ELSE}
    {:
    Seeks a message, removing it or not of the queue. To select a message or not,
    their MsgId must be greater or equal than uFilterMinMsg and less or equal
    than uFilterMaxMsg. To select the first message of the queue, pass 0 (zero)
    to the parameters uFilterMinMsg and uFilterMaxMsg.

    @param(Msg TMsg. Variable used to return the message.)
    @param(uFilterMinMsg Cardinal. Select only the messages where the MsgID is
           greater or equal than this parameter.)
    @param(uFilterMaxMsg Cardinal. Select only the messages where the MsgID is
           less or equal than this parameter.)
    @param(Remove Boolean. If @true, the message encountered will be removed
           from the queue.)

    @bold(If uFilterMinMsg and uFilterMaxMsg are equals and greater than 0 (zero),
          @name will seek all messages which the MsgID is equal than this
          parameters.)

    @return(Return @true if found some message.)

    @seealso(PostMessage)
    }
    {$ENDIF}
    function  PeekMessage(out Msg:TpSCADAMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal; Remove:Boolean):Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Insere uma mensagem na Fila.

    @param(MsgID Cardinal. Número de classificação da mensagem.)
    @param(wParam Pointer Ponteiro de dados.)
    @param(lParam Pointer Ponteiro de dados.)
    @param(Priority Boolean. @true se a mensagem deve ser colocada no
           início da fila (Com Prioridade).)


    @seealso(PeekMessage)
    }
    {$ELSE}
    {:
    Inserts a message on queue.

    @param(MsgID Cardinal. Message identification.)
    @param(wParam Pointer Data pointer 1.)
    @param(lParam Pointer Data Pointer 2.)
    @param(Priority Boolean. If @true if the message will be inserted at the
           beginning of the queue (With priority).)

    @seealso(PeekMessage)
    }
    {$ENDIF}
    procedure PostMessage(msg:PpSCADAMsgPkg); virtual;

    {$IFDEF PORTUGUES}
    {:
    Retorna uma nova fila com todas as mensagens enfileiradas na fila atual.

    @seealso(PeekMessage)
    }
    {$ELSE}
    {:
    Returns a new queue with all queued messages on current queue.

    @seealso(PeekMessage)
    }
    {$ENDIF}
    function  GetAllQueuedMessages:TpSCADAThreadUnsafeMessageQueue; virtual;
  end;

  { TpSCADAThreadSafeMessageQueue }

  TpSCADAThreadSafeMessageQueue = class(TpSCADAThreadUnsafeMessageQueue)
  private
    FCS:TCriticalSection;
  public

    constructor Create; override;
    destructor Destroy; override;

    function PeekMessage(out Msg: TpSCADAMsg; uFilterMinMsg,
      uFilterMaxMsg: Cardinal; Remove: Boolean): Boolean; override;

    procedure PostMessage(msg: PpSCADAMsgPkg); override;

    function GetAllQueuedMessages: TpSCADAThreadUnsafeMessageQueue; override;

  end;

ResourceString
  SoutOfMemory = 'Out of memory!';

implementation

{ TpSCADAThreadSafeMessageQueue }

constructor TpSCADAThreadSafeMessageQueue.Create;
begin
  inherited Create;
  FCs := TCriticalSection.Create;
end;

destructor TpSCADAThreadSafeMessageQueue.Destroy;
begin
  FCs.Acquire;
  try
    inherited Destroy;
  finally
    FCs.Release;
  end;
  FreeAndNil(FCs);
end;

function TpSCADAThreadSafeMessageQueue.PeekMessage(out Msg: TpSCADAMsg;
  uFilterMinMsg, uFilterMaxMsg: Cardinal; Remove: Boolean): Boolean;
begin
  if FCs=nil then begin
     Result := false;
     exit;
  end;
  FCs.Acquire;
  try
    Result:=inherited PeekMessage(Msg, uFilterMinMsg, uFilterMaxMsg, Remove);
  finally
    FCs.Release;
  end;
end;

procedure TpSCADAThreadSafeMessageQueue.PostMessage(msg: PpSCADAMsgPkg);
begin
  if FCs=nil then
    exit;

  //pega o acesso a fila.
  //enter on queue mutex.
  FCs.Acquire;
  try
    inherited PostMessage(msg);
  finally
    //libera a fila.
    //releases the mutex.
    FCs.Release;
  end;
end;

function TpSCADAThreadSafeMessageQueue.GetAllQueuedMessages: TpSCADAThreadUnsafeMessageQueue;
begin
  FCs.Enter;
  try
    Result:=inherited GetAllQueuedMessages;
  finally
    FCs.Leave;
  end;
end;

//==============================================================================

constructor TpSCADAThreadUnsafeMessageQueue.Create;
begin
  FirstMessage := nil;
  NormalMsgs := nil;
  LastMsg := nil;
  FMsgCount := 0;
end;

destructor TpSCADAThreadUnsafeMessageQueue.Destroy;
var
  msg, nextmsg:PpSCADAMsgPkg;
begin
  msg := FirstMessage;
  while (msg<>nil) do begin
    nextmsg := (msg^).NextMsg;
    FreeMem(msg);
    msg := nextmsg;
  end;
  inherited Destroy;
end;

function TpSCADAThreadUnsafeMessageQueue.CreateNewMessagePkg(MsgID: Cardinal;
  wParam, lParam: Pointer; Priority: Boolean): PpSCADAMsgPkg;
begin
  //cria uma mensagem na memória.
  //creates a new queue item.
  New(Result);
  if Result=nil then
    raise Exception.Create(SoutOfMemory);

  Result^.Msg.MsgID := MsgID;
  Result^.Msg.lParam := lParam;
  Result^.Msg.wParam := wParam;
  Result^.Msg.Priority := Priority;
  Result^.NextMsg := nil;
  Result^.PriorMsg := nil;
end;

function TpSCADAThreadUnsafeMessageQueue.PeekMessage(out Msg:TpSCADAMsg; uFilterMinMsg, uFilterMaxMsg:Cardinal; Remove:Boolean):Boolean;
var
  curmsg, aux:PpSCADAMsgPkg;
  found:Boolean;
begin
  Result := false;
  found  := false;
  curmsg := FirstMessage;
  while (curmsg<>nil) do begin
    if (uFilterMinMsg<>0) and (uFilterMaxMsg<>0) then begin
      if (curmsg^.Msg.MsgID>=uFilterMinMsg) and (curmsg^.Msg.MsgID<=uFilterMaxMsg) then begin
        found := true;
        break;
      end;
    end else begin
      found := true;
      break;
    end;
    curmsg := curmsg^.NextMsg;
  end;

  if found then begin
    Msg := curmsg^.Msg;
    //se a mensagem encontrada é a primeira e é para remover a mensagem da fila
    //diz q a próxima msg é a primeira.
    //
    //if the message found is the first item and it will be removed from the
    //queue, so, the next message will be the first
    if Remove and (curmsg = FirstMessage) then
      FirstMessage := curmsg^.NextMsg;

    //se a mensagen encontrada é a última e é para remover a mensagem da fila
    //diz q a msg anterior é a última.
    //
    //if the message found is the last item of the queue and it will be removed
    //from the queue, so the last messagewill be the prior message.
    if Remove and (curmsg = LastMsg) then
      LastMsg := curmsg^.PriorMsg;

    //se a mensagem encontrada é a primeira normal
    //reaponta mensagens
    //
    //if the message found is the first normal message (without priority)
    //and it will be removed from the queue, rebuilt the queue.
    if Remove and (curmsg = NormalMsgs) then begin
      NormalMsgs := curmsg^.NextMsg;
      if NormalMsgs<>nil then
        NormalMsgs^.PriorMsg := curmsg^.PriorMsg;
    end;

    //se existe mensagens antes dessa
    //if exists messages prior of this
    if Remove and (curmsg^.PriorMsg<>nil) then begin
      aux := curmsg^.PriorMsg;
      aux^.NextMsg := curmsg^.NextMsg
    end;

    //se existem mensagens após essa msg
    //if exists messages after this.
    if Remove and (curmsg^.NextMsg<>nil) then begin
      aux := curmsg^.NextMsg;
      aux^.PriorMsg := curmsg^.PriorMsg
    end;

    //libera a memória usada pela mensagem
    //caso a ordem seja para remover...
    //
    //if the message will be removed from the queue, free the memory.
    if Remove then begin
      Dispose(curmsg);
      FMsgCount := FMsgCount - 1;
    end;

    Result := true;
  end;
end;

procedure TpSCADAThreadUnsafeMessageQueue.PostMessage(msg: PpSCADAMsgPkg);
var
  aux:PpSCADAMsgPkg;
begin
  //se a primeira msg é nula, a fila esta vazia.
  //if the first queue item is null, the queue is empty.
  if FirstMessage=nil then begin
    FirstMessage := msg;
    LastMsg := msg;
    if not msg^.Msg.Priority then
      NormalMsgs := msg;
  end else begin
    //se existem msgs na fila.
    //if the queue is not empty.

    //e se existe só uma msg
    //if exists only one message on the queue.
    if FirstMessage = LastMsg then begin
      FirstMessage^.NextMsg := msg;
      msg^.PriorMsg := FirstMessage;
      LastMsg := msg;
      if (not msg^.Msg.Priority) and (NormalMsgs=nil) then
        NormalMsgs := msg;
    end else begin
      //se existirem duas ou mais mensagens na fila...
      //if exists two or more messages on queue.
      if msg^.Msg.Priority then
        if NormalMsgs=nil then begin
          LastMsg^.NextMsg := msg;
          msg^.PriorMsg := LastMsg;
          LastMsg := msg;
        end else begin
          aux := NormalMsgs^.PriorMsg;
          //se é nulo, é a primeira msg
          //if is nil, is the first message.
          if aux=nil then
            FirstMessage := msg
          else
            aux^.NextMsg :=  msg;
          msg^.PriorMsg := aux;
          msg^.NextMsg := NormalMsgs;
          NormalMsgs^.PriorMsg := msg;
        end
      else begin
        LastMsg^.NextMsg := msg;
        msg^.PriorMsg := LastMsg;
        LastMsg := msg;
        if (NormalMsgs=nil) then
          NormalMsgs := msg;
      end;
    end;
  end;

  //sinaliza que há mensagens na fila.
  //increment the messages on queue.
  FMsgCount := FMsgCount + 1;
end;

function TpSCADAThreadUnsafeMessageQueue.GetAllQueuedMessages: TpSCADAThreadUnsafeMessageQueue;
begin
  Result:=TpSCADAThreadUnsafeMessageQueue.Create;
  Result.FirstMessage := self.FirstMessage;
  Result.NormalMsgs   := self.NormalMsgs;
  Result.LastMsg      := self.LastMsg;
  Result.FMsgCount    := self.FMsgCount;

  self.FirstMessage := nil;
  self.NormalMsgs   := nil;
  self.LastMsg      := nil;
  self.FMsgCount    := 0;
end;

end.
