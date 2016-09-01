unit pascalscada.utilities.scales.scalequeue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.utilities.scales.basescale;

type

  {$IFDEF PORTUGUES}
  //: Implementa um item de uma coleção de processadores de escala.
  {$ELSE}
  //: Implements a item of a scales processors collection.
  {$ENDIF}
  TScaleQueueItem = class(TCollectionItem)
  private
    SProcessor:TScaleProcessor;
    procedure SetScaleProcessor(SP:TScaleProcessor);
  protected
    //: @exclude
    function  GetDisplayName: AnsiString; override;
  public
    {$IFDEF PORTUGUES}
    {:
    Procedimento chamado para remover a dependencia de um objeto de escalas que
    está sendo destruido.
    }
    {$ELSE}
    {:
    Procedure called to remove a dependency with a scale processor object that
    is being destroyed.
    }
    {$ENDIF}
    procedure RemoveScaleProcessor;

    {$IFDEF PORTUGUES}
    {:
    Repassa a chamada para o método SetInGetOut do processador de escalas
    configurado em ScaleProcessor.

    @param(Sender TComponent: Objeto solicitante.)
    @param(Input Double: Valor puro que será processado pela escala.)

    @returns(O valor processado pela escala associada em ScaleProcessor. Caso
    ScaleProcessor não tenha um objeto associado, retorna o valor passado
    em Input.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ELSE}
    {:
    Calls the procedure SetInGetOut of the scale processor, if it's set.

    @param(Sender TComponent: Object that did the request.)
    @param(Input Double: Value to be processed.)

    @returns(The value processed by the scale processor. If there isn't an
    object associated, returns the value of Input.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double;

    {$IFDEF PORTUGUES}
    {:
    Repassa a chamada para o método SetOutGetIn do processador de escalas
    configurado em ScaleProcessor.

    @param(Sender TComponent: Objeto solicitante.)
    @param(Output Double: Valor processado que se deseja obter um valor puro.)

    @returns(O valor puro retornado pela escala associada em ScaleProcessor. Caso
    ScaleProcessor não tenha um objeto associado, retorna o valor passado
    em Output.)

    @seealso(TScaleProcessor.SetOutGetIn)
    }
    {$ELSE}
    {:
    Calls the procedure SetOutGetIn of the scale processor, if it's set.

    @param(Sender TComponent: Object that did the request.)
    @param(Output Double: Value to be processed.)

    @returns(The value processed by the scale processor. If there isn't an
    object associated, returns the value of Output parameter.)

    @seealso(TScaleProcessor.SetInGetOut)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  published

    {$IFDEF PORTUGUES}
    //: Objeto de escalas responsável por fazer os processamentos desse item.
    {$ELSE}
    //: Scale processor object that does the values transformations of this item.
    {$ENDIF}
    property ScaleProcessor:TScaleProcessor read SProcessor write SetScaleProcessor;
  end;

  {$IFDEF PORTUGUES}
  //: Implementa uma coleção de processadores de escala.
  {$ELSE}
  //: Implements a collection of scale processors.
  {$ENDIF}
  TScaleQueue = class(TCollection)
  private
    FOwner:TPersistent;
  protected
    //: @exclude
    function GetOwner: TPersistent; override;
  public
    //: @exclude
    constructor Create(aOwner:TComponent);

    {$IFDEF PORTUGUES}
    {:
    Adiciona um novo item de processamento de escalas a coleção.
    @returns(O novo item da coleção.)
    }
    {$ELSE}
    {:
    Adds a new item to collection.
    @returns(The new item of the collection.)
    }
    {$ENDIF}
    function Add:TScaleQueueItem;

    {$IFDEF PORTUGUES}
    {:
    Tranforma um valor puro (Entrada) em um valor processado pelas multiplas
    escalas pertencentes a coleção (Saida).

    Para isso ele passa Input para o método SetInGetOut do primeiro item da
    coleção e o resultado ele repassa como parametro do próximo item coleção,
    repetindo isso até atingir o fim da lista.

    @bold(Logo, o primeiro item da lista é primeiro a ser chamado quando o valor
    vem no sentido Equipamento -> Usuário assim como o último item da coleção é
    o primeiro a ser chamado quando o valor vai do Usuário -> Equipamento.)

    @param(Sender TComponent: Quem chamou esse processamento.)
    @param(Input Double: Valor puro a processar.)
    @returns(Retorna o valor processado em função das escalas associadas aos
             itens da coleção. Se não há itens na coleção ou se os itens dela não
             tiverem um processador de escala associado, Input é retornado.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ELSE}
    {:
    Process a raw value (Input) to a value in engineering scale, processed by
    each scale processors of the collection.

    To do this, this method passes the Input parameter to TScalePIPEItem.SetInGetOut
    of the first item of the collection, takes the result and passes it again as
    Input of TScalePIPEItem.SetInGetOut of the next item of the collection,
    until reach the end of the collection.

    @bold(So, the first item of the collection is the first that will be called
    when the value is comming from device and going to User AND so as the last
    item of the collection o is the first to be called when the value is comming
    from the user and going to device.)

    @param(Sender TComponent: Object that has requested the value transformation.)
    @param(Input Double: Value to be transformed to a value in engineering scale.)
    @returns(Returns the value processed by all items of the collection. If the
             collection is empty or all items of the collection aren't set
             correctly (a value processor isn't set), returns the value given in
             Input parameter.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double;

    {$IFDEF PORTUGUES}
    {:
    Tranforma um valor processado pelas multiplas escalas da coleção (Saida) em
    um valor puro (Entrada).

    Para isso ele passa Output para o método SetOutGetIn do último item da
    coleção e o resultado ele repassa como parametro do item que o antecede,
    repetindo isso até atingir o inicio da coleção.

    @bold(Logo, o primeiro item da lista é primeiro a ser chamado quando o valor
    vem no sentido Equipamento -> Usuário assim como o último item da coleção é
    o primeiro a ser chamado quando o valor vai do Usuário -> Equipamento.)

    @param(Sender TComponent: Quem chamou esse processamento.)
    @param(Output Double: Valor processado da qual se deseja obter um valor puro.)
    @returns(Retorna o valor puro em função das escalas associadas aos
             itens da coleção. Se não há itens na coleção ou se os itens dela não
             tiverem um processador de escala associado, Output é retornado.)
    @seealso(TScalePIPEItem.SetOutGetIn)
    }
    {$ELSE}


    {:
    Process a raw value in engineering scale to a raw value, processed by each
    scale processors of the collection.

    Para fazer isso, esse método passa o parâmetro de entrada para
    TScalePIPEItem.SetInGetOut do último item da coleção, leva o resultado e
    passa-lo novamente como entrada de TScalePIPEItem.SetInGetOut do item que
    precede o item atual da coleção, até chegar ao início da coleção.

    @bold(So, the last item of the collection is the first that will be called
          when the value is comming from the user and going to device AND so as
          the first item of the collection o is the last to be called when the
          value is comming from the device and going to user.)

    @param(Sender TComponent: Object that has requested the value transformation.)
    @param(Output Double: Value in engineering scale to be transformed to a raw value.)
    @returns(Returns the value processed by all items of the collection. If the
             collection is empty or all items of the collection aren't set
             correctly (a value processor isn't set), returns the value given in
             Outpu parameter.)
    @seealso(TScalePIPEItem.SetInGetOut)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double;
  end;


  {$IFDEF PORTUGUES}
  //: Componente de enfileiramento de processadores de escala.
  {$ELSE}
  //: Scale processors queue.
  {$ENDIF}

  { TScalesQueue }

  TScalesQueue = class(TScaleProcessor)
  private
    FScaleQueue:TScaleQueue;
    function  GetScaleQueue:TScaleQueue;
    procedure SetScaleQueue(ScaleQueue:TScaleQueue);
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    //: @seealso(TScalePIPE.SetInGetOut)
    function SetInGetOut(Sender: TComponent; aInput: Double): Double; override;
    //: @seealso(TScalePIPE.SetOutGetIn)
    function SetOutGetIn(Sender: TComponent; aOutput: Double): Double; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published

    {$IFDEF PORTUGUES}
    //: Coleção de escalas.
    {$ELSE}
    //: Collection of scale processors.
    {$ENDIF}
    property ScalesQueue:TScaleQueue read GetScaleQueue write SetScaleQueue stored true;
  end;

resourcestring
  SpSCADAInvalidQueueOperation = 'Cannot add the collection owner as a collection item';
  SpSCADACollectionOwnerShouldBeATComponent = 'Collection owner should be a TComponent';
  SpSCADAEmpty = '<Empty>';

implementation

////////////////////////////////////////////////////////////////////////////////
// TScaleQueueItem implementation
////////////////////////////////////////////////////////////////////////////////
procedure TScaleQueueItem.SetScaleProcessor(SP:TScaleProcessor);
begin
  if not (Collection.Owner is TComponent) then
    raise exception.Create(SpSCADACollectionOwnerShouldBeATComponent);

  if SP=Collection.Owner then
    raise Exception.Create(SpSCADAInvalidQueueOperation);

  if sp=SProcessor then exit;

  if SProcessor<>nil then
     SProcessor.RemoveFreeNotification(TComponent(Collection.Owner));

  if SP<>nil then
     SP.FreeNotification(TComponent(Collection.Owner));

  DisplayName:=SP.Name;
  SProcessor := SP;
end;

function TScaleQueueItem.GetDisplayName: AnsiString;
begin
   if SProcessor<>nil then
      Result := SProcessor.Name
   else
      Result := SpSCADAEmpty;
end;

function TScaleQueueItem.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetInGetOut(Sender,Input)
  else
     Result := Input;
end;

function TScaleQueueItem.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
  if SProcessor<>nil then
     Result := SProcessor.SetOutGetIn(Sender,Output)
  else
     Result := Output;
end;

procedure TScaleQueueItem.RemoveScaleProcessor;
begin
  SProcessor := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TScalePIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TScaleQueue.Create(aOwner: TComponent);
begin
  inherited Create(TScaleQueueItem);
  FOwner:=aOwner;
end;

function TScaleQueue.GetOwner:TPersistent;
begin
  Result:=FOwner;
end;

function TScaleQueue.Add:TScaleQueueItem;
begin
   Result := TScaleQueueItem(inherited Add)
end;

function TScaleQueue.SetInGetOut(Sender:TComponent; Input:Double):Double;
var
  c:LongInt;
begin
  Result := Input;
  for c:=0 to Count-1 do
    if GetItem(c) is TScaleQueueItem then
       Result := TScaleQueueItem(GetItem(c)).SetInGetOut(Sender,Result);
end;

function TScaleQueue.SetOutGetIn(Sender:TComponent; Output:Double):Double;
var
  c:LongInt;
begin
  Result := Output;
  for c:=(Count-1) downto 0 do
    if GetItem(c) is TScaleQueueItem then
       Result := TScaleQueueItem(GetItem(c)).SetOutGetIn(Sender,Result);
end;

////////////////////////////////////////////////////////////////////////////////
// TPIPE implementation
////////////////////////////////////////////////////////////////////////////////

constructor TScalesQueue.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FScaleQueue := TScaleQueue.Create(Self);
end;

destructor  TScalesQueue.Destroy;
var
  i: Integer;
begin
  for i:=0 to FScaleQueue.Count-1 do
    if Assigned(TScaleQueueItem(FScaleQueue.Items[i]).ScaleProcessor) then
       TScaleQueueItem(FScaleQueue.Items[i]).ScaleProcessor.RemoveFreeNotification(Self);

  FreeAndNil(FScaleQueue);
  inherited Destroy;
end;

function  TScalesQueue.GetScaleQueue:TScaleQueue;
begin
  Result := FScaleQueue;
end;

procedure TScalesQueue.SetScaleQueue(ScaleQueue:TScaleQueue);
begin
  FScaleQueue.Assign(ScaleQueue);
end;

function TScalesQueue.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
   Result := FScaleQueue.SetInGetOut(Sender,aInput);
end;

function TScalesQueue.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
   Result := FScaleQueue.SetOutGetIn(Sender, aOutput);
end;

procedure TScalesQueue.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  for i:=0 to FScaleQueue.Count-1 do
    if TScaleQueueItem(FScaleQueue.Items[i]).ScaleProcessor=AComponent then
       TScaleQueueItem(FScaleQueue.Items[i]).ScaleProcessor:=nil;
end;

end.

