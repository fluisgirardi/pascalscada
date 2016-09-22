{$IFDEF PORTUGUES}
{:
  @abstract(Classes para organização de blocos de memória de um CLP.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
  @abstract(Set of class to handle blocks of memory of an PLC.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit pascalscada.protocols.memory_manager;

interface

uses SysUtils, DateUtils, SyncObjs, Classes, fgl, pascalscada.tags.base,
  pascalscada.utilities.datetime;

type
  TpSCADADWordList = specialize TFPGList<LongWord>;

  { TpSCADAAddressInfo }

  TpSCADAAddressInfo = class
  private
    FLastReadStatus: TpSCADATagValueState;
    FScanList:TpSCADADWordList;
    FTimestamp: TDateTime;
    function GetRefCount: Integer;
    function GetScan: LongWord;
  public
    ContinuousCount:Cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure AddScan(aScan:LongWord);
    procedure RemoveScan(aScan:LongWord);
    function  NeedUpdate:Boolean;
    procedure Update(aLastReadStatus:TpSCADATagValueState; aReadTimestamp:TDateTime);
  published
    property RefCount:Integer read GetRefCount;
    property CurrentScan:LongWord read GetScan;
    property Timestamp:TDateTime read FTimestamp;
    property LastReadStatus:TpSCADATagValueState read FLastReadStatus;
  end;

  TpSCADAAddressMap = specialize TFPGMap<LongWord,TpSCADAAddressInfo>;


  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  @abstract(Classe que gerência blocos de memórias não continuos (fragmentados)
            e sua melhor organização.)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  @abstract(Class that handles non-continuous memory blocks (fragmented)
            and the better organization of it.)
  }
  {$ENDIF}
  TPLCMemoryManager = class
  private
    FAddressMap:TpSCADAAddressMap;
    FMaxHole,
    FMaxBlockSize:LongWord;
    FMaxDataBlockSize:LongWord;
    //binary search of memory address
    function  FindAddress(const address:LongWord; var idx:LongInt):Boolean;
    procedure AddAddress(Address,Scan:LongWord); overload;
    procedure RemoveAddress(Address,Scan:LongInt); overload;
    procedure SetHoleSize(size:LongWord);
    procedure SetBlockSize(size:LongWord);
    //rebuild the blocks
    procedure RebuildBlocks;
    //returns the size of all block fragments.
    function  GetSize:LongWord;
  public
    {$IFDEF PORTUGUES}
    //: Blocos de memória continuos.
    {$ELSE}
    //: Continous memory blocks.
    {$ENDIF}
    DataArea:Pointer;

    {$IFDEF PORTUGUES}
    //: Cria um gerenciador de memórias não continuas.
    {$ELSE}
    //: Creates the handler of non continuous memory block.
    {$ENDIF}
    constructor Create(MaxAreaSizeInBytes:LongWord); virtual;

    {$IFDEF PORTUGUES}
    //: Destroi o gerenciador de blocos não continuos e todos os seus recursos.
    {$ELSE}
    //: Destroys the handler of non continuous memory block.
    {$ENDIF}
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    Adiciona uma ou mais memórias ao gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo adicionadas ao bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na área.)
    @param(Scan Cardinal. Tempo de varredura da memória.)

    Por exemplo, para adicionar as MW0, MW2 e MW4 (words) de um CLP Siemens (onde a
    menor palavra é o byte) com 1200ms de scan, você chamaria:
    @code(AddAddress(0,3,2,1200);)

    Já nos CLPs Scheneider (onde a menor palvra é de 16 bits), para endereçar
    as words W0, W1 e W2 ficaria assim:
    @code(AddAddress(0,3,1,1200);)

    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    Adds one or more memory into the manager.
    @param(Address Cardinal. Initial address of memory range.)
    @param(Size Cardinal. How many memories will be managed by the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Scan Cardinal. Scan time of your variables)

    For example, to add the MW0, MW2 and MW4 of a Siemens PLC (the smaller word
    is the byte) with 1200ms of scan into the manager, you must call:
    @code(AddAddress(0,3,2,1200);)

    However, on a Schneider PLC (the smaller word has 16 bits), to add the address
    W0, W1 and W2, you must call:
    @code(AddAddress(0,3,1,1200);)

    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    procedure AddAddress(Address,Size,RegSize,Scan:Cardinal); overload; virtual;

    {$IFDEF PORTUGUES}
    {:
    Remove uma ou mais variáveis do gerenciador.
    @param(Address Cardinal. Endereço inicial do intervalo de memória(s).)
    @param(Size Cardinal. Quantidade de variáveis que estão sendo removidas do bloco.)
    @param(RegSize Cardinal. Tamanho da variável em relação a menor palavra disponível na area.)

    Os parametros funcionam de maneira identica a função AddAddress.

    @seealso(AddAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    Removes one or more variables from the manager.
    @param(Address Cardinal. Initial address of memory range.)
    @param(Size Cardinal. How many memories will be removed from the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)

    These parameters works like the of function AddAddress.

    @seealso(AddAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    procedure RemoveAddress(Address, Size, RegSize, Scan: LongWord); overload; virtual;

    {$IFDEF PORTUGUES}
    {:
    @name armazena valores em um intervalo de memórias, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a armazenar.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra de seu equipamento.)
    @param(Values TArrayOfDouble. Valores que irão ser armazenados no gerenciador de variaveis.)
    @param(LastResult TProtocolIOResult. Último resultado de E/S do conjunto de variáveis.)

    Cada valor na array Values representa a um valor da menor palavra de seu equipamento.

    Por exemplo: supondo que você esteja escrevendo em um S7-200 da Siemens, para
    escrever na VW0 você chamaria:

    @code(SetValues(0,1,2,[valor_vb0,valor_vb1]);)

    No Siemens a menor palavra é o Byte, e uma Word são dois bytes.

    Mas em um CLP Schneider ficaria:

    @code(SetValues(0,1,1,[valor_VW0]);)

    Pois o menor tamanho de palavra nesses CLPs é 16 bits.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    @name stores values in a range of memories, continuous or non continuous.

    @param(Address Cardinal. Initial address of memory range.)
    @param(Len Cardinal. How many memory will be stored on the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Values TArrayOfDouble. Values that will be stored in the memory manager.)
    @param(LastResult TProtocolIOResult. Last I/O result of the values being stored.)

    One value on Values array represents the value of the smaller word of your device.

    For example: if you are storing the value of MW0 (word) of an Siemens PLC,
    you must call:

    @code(SetValues(0,1,2,[vb0_value,vb1_value]);)

    Because on Siemens PLC's the smaller word size is the byte, so, one word are two bytes.

    But, on a Schneider PLC, you must call:

    @code(SetValues(0,1,1,[valor_VW0]);)

    Because on this PLC, the smaller word size is the Word (16bits).

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(GetValues)
    }
    {$ENDIF}
    procedure Update(AddrStart, Size, RegSize: LongWord; aTimeStamp:TDateTime;
      LastResult: TpSCADATagValueState); virtual;

    {$IFDEF PORTUGUES}
    {:
    @name lê valores armazenadas no gerenciador, continuas ou não.

    @param(AdrStart Cardinal. Endereço inicial.)
    @param(Len Cardinal. Quantidade de variáveis a ler.)
    @param(RegSise Cardinal. Tamanho da variável em relação ao menor tamanho de palavra.)
    @param(Values TArrayOfDouble. Array onde serão retornados os valores armazenados no gerenciador de variaveis.)
    @param(LastResult TProtocolIOResult. Último resultado de E/S do conjunto de variáveis.)
    @param(ValueTimeStamp TDateTime. Data hora em que os valores foram atualizados no gerenciador de memórias.)

    Cada item da array retornado representa o valor da menor palavra daquela área.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ELSE}
    {:
    @name gets the values stored in memory manager, continuous or non-continuous.

    @param(Address Cardinal. Initial address of memory range.)
    @param(Len Cardinal. How many memories will got from the manager.)
    @param(RegSize Cardinal. Word size of your variable compared with the smaller word of your device.)
    @param(Values TArrayOfDouble. Array that will return the values that are stored in the memory manager.)
    @param(LastResult TProtocolIOResult. Last I/O result of the memory range.)
    @param(ValueTimeStamp TDateTime. Date time of the last update of the values on the memory manager.)

    One value on Values array represents the value of the smaller word of your device.

    @seealso(AddAddress)
    @seealso(RemoveAddress)
    @seealso(SetValues)
    @seealso(GetValues)
    }
    {$ENDIF}
    //function  GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):LongInt; virtual;
  published

    {$IFDEF PORTUGUES}
    {:
    Define quantos endereços podem ficar sem serem usados para manter a
    continuidade de um bloco. Valores grandes formam poucos grupos de tamanho
    grande, enquanto valores pequenos formam muitos grupos de tamanho pequeno.

    Digamos que sejam adicionados os endereços [0, 1] e [3 ,4] e MaxHole=0, logo
    serão formados dois blocos, o primeiro contendo os endereços [0, 1] e o
    segundo os endereços [3, 4].

    Já se for setado MaxHole=1 será criado um único grupo com os endereços
    [0,1,2,3,4] sendo o endereço 2 adicionado automaticamente para manter a
    continuidade do bloco.
    }
    {$ELSE}
    {:
    Tells how many memory address can be missing without break the block on
    two or more smaller blocks.

    For example, if are added the memory address [0, 1] and [3, 4] into the
    Manager with MaxHole=0, will be built two blocks, the first with the address
    [0, 1] and the second block with the address [3, 4].

    However, if the MaxHole is set to 1, will be built only one block with the
    address [0,1,2,3,4]. Will be included the address 2, to avoid the break the
    block on two pieces.
    }
    {$ENDIF}
    property MaxHole:LongWord read FMaxHole write SetHoleSize;

    {$IFDEF PORTUGUES}
    {:
    Define qual o tamanho máximo de cada bloco continuo. Se não há limite de
    tamanho, use 0 nessa propriedade.

    Supondo que foram adicionados os endereços [0,1,2,3,4] e @name=0 será criado
    um único bloco com esses mesmos endereços. Supondo que @name=3 serão criados
    dois grupos, o primeiro com os endereços [0,1,2] e o segundo com os endereços
    [3,4].
    }
    {$ELSE}
    {:
    Tells the max size of the blocks. If has no limit, set @name to 0.

    For example, if are added the memory address [0,1,2,3,4] and  @name=0 will
    be created only one block with these address. However if @name=3, will be
    created two blocks, the first with the address [0,1,2] and the second with
    the address [3,4].
    }
    {$ENDIF}
    property MaxBlockItems:LongWord read FMaxBlockSize write SetBlockSize;

    {$IFDEF PORTUGUES}
    //: Retorna a quantidade total de memórias gerenciadas pelo bloco.
    {$ELSE}
    //: How many memories are handled by the manager.
    {$ENDIF}
    property Size:LongWord read GetSize;
  end;

  TPLCMemoryManagerSafe = class(TPLCMemoryManager)
  private
    FMutex:TCriticalSection;
  public
    //: @seealso(TPLCMemoryManager.Create)
    constructor Create(MaxAreaSizeInBytes: LongWord); override;
    //: @seealso(TPLCMemoryManager.Destroy)
    destructor Destroy; override;
    //: @seealso(TPLCMemoryManager.AddAddress)
    procedure AddAddress(Address,aSize,RegSize,Scan:Cardinal); override;
    //: @seealso(TPLCMemoryManager.RemoveAddress)
    procedure RemoveAddress(Address,aSize,RegSize,Scan:Cardinal); override;
    //: @seealso(TPLCMemoryManager.Update)
    procedure Update(AddrStart, Size, RegSize: LongWord; aTimeStamp: TDateTime;
  LastResult: TpSCADATagValueState); override;
    //: @seealso(TPLCMemoryManager.GetValues)
    //function  GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):LongInt; override;
    //: @seealso(TPLCMemoryManager.SetFault)
    //procedure SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult); override;
  end;

resourcestring
  SpSCADAUnassignedScanList = 'Unassigned scan list';
  SpSCADAUnassignedAddressMap = 'Unassigned address map';
  SpSCADAUnassignedAddressInfo = 'Unassigned address info';
  SpSCADAInvalidAreaSize = 'Invalid area size. It should be greater than zero.';
  SpSCADASizeShouldBeGreaterThanZero = 'Size and RegSize should be greater than zero.';

implementation

uses Math;

{ TpSCADAAddressInfo }

function SortAddressList(const Key1, Key2: LongWord): Integer;
begin
  Result:=Key1-Key2;
  if Result<0 then Result:=-1;
  if Result>0 then Result:=-1;
end;

function TpSCADAAddressInfo.GetRefCount: Integer;
begin
  if assigned(FScanList) then
    Result:=FScanList.Count
  else
    raise Exception.Create(SpSCADAUnassignedScanList);
end;

function TpSCADAAddressInfo.GetScan: LongWord;
begin
  if assigned(FScanList) and (FScanList.Count>0) then
    Result:=FScanList.Items[0]
  else
    raise Exception.Create(SpSCADAUnassignedScanList);
end;

constructor TpSCADAAddressInfo.Create;
begin
  FScanList:=TpSCADADWordList.Create;
  ContinuousCount:=1;
end;

destructor TpSCADAAddressInfo.Destroy;
begin
  FreeAndNil(FScanList);
  inherited Destroy;
end;

procedure TpSCADAAddressInfo.AddScan(aScan: LongWord);
begin
  if assigned(FScanList) then
    FScanList.Add(aScan)
  else
    raise Exception.Create(SpSCADAUnassignedScanList);
end;

procedure TpSCADAAddressInfo.RemoveScan(aScan: LongWord);
begin
  if assigned(FScanList) then
    FScanList.Remove(aScan)
  else
    raise Exception.Create(SpSCADAUnassignedScanList);
end;

function TpSCADAAddressInfo.NeedUpdate: Boolean;
begin
  Result:=(RefCount>0) and (CurrentScan<=MilliSecondsBetween(pSCADA_CrossNow, FTimestamp));
end;

procedure TpSCADAAddressInfo.Update(aLastReadStatus: TpSCADATagValueState;
  aReadTimestamp: TDateTime);
begin
  FTimestamp:=aReadTimestamp;
  FLastReadStatus:=aLastReadStatus;
end;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementacao do TPLCMemoryManager
//                implementation of TPLCMemoryManager.
////////////////////////////////////////////////////////////////////////////////

constructor TPLCMemoryManager.Create(MaxAreaSizeInBytes: LongWord);
begin
  if MaxAreaSizeInBytes=0 then
    raise exception.Create(SpSCADAInvalidAreaSize);

  FAddressMap:=TpSCADAAddressMap.Create;
  FAddressMap.OnKeyCompare:=@SortAddressList;
  FAddressMap.OnCompare:=@SortAddressList;

  FMaxDataBlockSize:=MaxAreaSizeInBytes;
  DataArea:=GetMem(MaxAreaSizeInBytes);
  //o bloco continua caso de enderecos seja <= 5
  //the block stay continuous if the number of missing address is <=5
  FMaxHole := 5;
  //o bloco tem seu tamanho restrito a x, 0 = sem restricao!
  //size limits of the built blocks, 0 = no size limit.
  FMaxBlockSize := 0;
end;

destructor TPLCMemoryManager.Destroy;
var
  i: LongInt;
begin
  if Assigned(FAddressMap) then begin
    for i:=FAddressMap.Count-1 downto 0 do begin
      if Assigned(FAddressMap.KeyData[FAddressMap.Keys[i]]) then
        FAddressMap.KeyData[FAddressMap.Keys[i]].Destroy;
      FAddressMap.Remove(FAddressMap.Keys[i]);
    end;
    FreeAndNil(FAddressMap);
  end;

  if Assigned(DataArea) then Freemem(DataArea);
  inherited Destroy;
end;

function TPLCMemoryManager.FindAddress(const address:LongWord; var idx:LongInt):Boolean;
begin
  if Assigned(FAddressMap) then begin
    Result:=FAddressMap.Find(address, idx);
  end else
    raise exception.Create(SpSCADAUnassignedAddressMap);
end;

procedure TPLCMemoryManager.AddAddress(Address, Scan: LongWord);
var
  idx: LongInt;
begin
  if Assigned(FAddressMap) then begin
    //se o endereço não existe no mapa de enderecos, adiciona
    if FAddressMap.Find(Address,idx)=false then begin
      FAddressMap.Add(Address, TpSCADAAddressInfo.Create);
    end;

    //adiciona o scan do endereço na lista de scans do endereço
    if Assigned(FAddressMap.KeyData[Address]) then
      FAddressMap.KeyData[Address].AddScan(Scan)
    else
      raise Exception.Create(SpSCADAUnassignedAddressInfo);

  end else
    raise exception.Create(SpSCADAUnassignedAddressMap);
end;

procedure TPLCMemoryManager.RemoveAddress(Address, Scan: LongInt);
var
  idx: LongInt;
begin
  if Assigned(FAddressMap) then begin
    if FAddressMap.Find(Address, idx) then begin
      if Assigned(FAddressMap.KeyData[Address]) then begin
        FAddressMap.KeyData[Address].RemoveScan(Scan);
        if FAddressMap.KeyData[Address].RefCount<=0 then begin
          FAddressMap.KeyData[Address].Destroy;
          FAddressMap.KeyData[Address]:=nil;
        end;
      end;
      FAddressMap.Remove(Address);
    end;
  end else
    raise exception.Create(SpSCADAUnassignedAddressMap);
end;

procedure TPLCMemoryManager.SetHoleSize(size: LongWord);
begin
  if size=FMaxHole then exit;
  FMaxHole := size;
  RebuildBlocks;
end;

procedure TPLCMemoryManager.SetBlockSize(size: LongWord);
begin
  if size=FMaxBlockSize then exit;
  FMaxBlockSize := size;
  RebuildBlocks; //rebuild the blocks.
end;

procedure TPLCMemoryManager.RebuildBlocks;
var
  c, c2, c3:LongInt;
  //newBlocks:TRegisterRangeArray;
  adrstart, adrend,BlockItems,BlockEnd,mscan:LongInt;
  BlockOldOffset, BlockNewOffset:LongInt;
  BlockIndex:LongInt;
  found:Boolean;
begin
  //SetLength(newBlocks,0);
  //adrend := 0;
  //adrstart := 0;
  //BlockItems := 0;
  //BlockEnd := 0;
  //mscan := 0;
  //BlockIndex := 0;
  ////refaz blocos de dados
  ////rebuild the memory blocks.
  //for c:=0 to High(FAddress) do begin
  //  if c=0 then begin
  //    adrstart := FAddress[0].Address;
  //    adrend := adrstart;
  //    BlockEnd := adrend + FMaxHole + 1;
  //    mscan := FAddress[0].MinScan;
  //    BlockItems := 1;
  //    if c<High(FAddress) then continue;
  //  end;
  //
  //  if (FAddress[c].Address>BlockEnd) or ((FMaxBlockSize<>0) AND (BlockItems>=FMaxBlockSize)) then begin
  //    //bloco terminou, feche esse e inicie um novo!!
  //    //the block can't be extended, starts another...
  //    SetLength(newBlocks,Length(newBlocks)+1);
  //
  //    newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
  //    newBlocks[BlockIndex].LastUpdate := CrossNow;
  //    newBlocks[BlockIndex].ScanTime := mscan;
  //    inc(BlockIndex);
  //
  //    //pega os enderecos de onde comeca um novo bloco...
  //    //get the address of the new block.
  //    adrstart := FAddress[c].Address;
  //    adrend := adrstart;
  //    BlockEnd := adrend + FMaxHole + 1;
  //    mscan := FAddress[c].MinScan;
  //    BlockItems := 1;
  //  end else begin
  //    //bloco continua, adiciona novos enderecos.
  //    //the block can be extended, add the new address.
  //    adrend := FAddress[c].Address;
  //    BlockEnd := adrend + FMaxHole + 1;
  //    mscan := min(mscan, FAddress[c].MinScan);
  //    Inc(BlockItems);
  //  end;
  //  if c=High(FAddress) then begin
  //    SetLength(newBlocks,Length(newBlocks)+1);
  //    newBlocks[BlockIndex] := CreateRegisterRange(adrStart,adrEnd);
  //    newBlocks[BlockIndex].LastUpdate := CrossNow;
  //    newBlocks[BlockIndex].ScanTime := mscan;
  //    inc(BlockIndex);
  //  end;
  //end;
  //
  ////copia os dados que estavam nos blocos antigos...
  ////baseia-se em varer a array de enderecos, verificar em que bloco ela estava
  ////e para que bloco o endereco foi parar...
  ////
  ////copy the data of the oldest blocks to the new blocks.
  //
  //for c:=0 to High(FAddress) do begin
  //  found := false;
  //  for c2 := 0 to High(Blocks) do
  //    if (FAddress[c].Address>=Blocks[c2].AddressStart) and (FAddress[c].Address<=Blocks[c2].AddressEnd) then begin
  //      found := true;
  //      break;
  //    end;
  //
  //  //se não encontrou aqui é pq o endereco foi adicionado...
  //  //if not found the address here, it was added.
  //  if not found then continue;
  //
  //  found := false;
  //  for c3:= 0 to High(newBlocks) do
  //    if (FAddress[c].Address>=newBlocks[c3].AddressStart) and (FAddress[c].Address<=newBlocks[c3].AddressEnd) then begin
  //      found := true;
  //      break;
  //    end;
  //
  //  //se não encontrou aqui é pq o endereco foi deletado...
  //  //if not found the address here, it was removed.
  //  if not found then continue;
  //  BlockOldOffset := FAddress[c].Address - Blocks[c2].AddressStart;
  //  BlockNewOffset := FAddress[c].Address - newBlocks[c3].AddressStart;
  //  newBlocks[c3].Values[BlockNewOffset] := Blocks[c2].Values[BlockOldOffset];
  //
  //  //coloca o menor tempo de atualização para priorizar quem necessita de refresh mais urgente..
  //  //set the the block with the small timestamp.
  //  newBlocks[c3].LastUpdate := Min(newBlocks[c3].LastUpdate,Blocks[c2].LastUpdate);
  //end;
  ////destroi os blocos antigos
  ////remove the old blocks.
  //for c:=0 to High(Blocks) do
  //  Blocks[c].Destroy;
  //SetLength(Blocks, 0);
  //
  ////copia os valores dos novos blocos para o bloco velho
  ////copy the values from the new block to the old block.
  //Blocks := newBlocks;
  //
  ////zera o auxiliar de blocos novos...
  ////releases the memory
  //SetLength(newBlocks,0);
end;

function TPLCMemoryManager.GetSize: LongWord;
begin
  if Assigned(FAddressMap) then
    Result:=FAddressMap.Count
  else
    raise exception.Create(SpSCADAUnassignedAddressMap);
end;

procedure TPLCMemoryManager.AddAddress(Address,Size,RegSize,Scan:Cardinal);
var
  c, items:Cardinal;
  len:LongInt;
begin
  if (Size<=0) or (RegSize<=0) then
    raise Exception.Create(SpSCADASizeShouldBeGreaterThanZero);

  if not Assigned(FAddressMap) then
    raise Exception.Create(SpSCADAUnassignedAddressMap);

  //captura o tamanho da array de enderecos...
  //gets the size of address map.
  len := FAddressMap.Count;

  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    AddAddress(c,Scan);
    inc(c);
  end;

  FAddressMap.Sort;

  //dipara o rebuild blocks, pq foram adicionados enderecos
  //rebuild the blocks, because address are added.
  if len<>FAddressMap.Count then
    RebuildBlocks;
end;

procedure TPLCMemoryManager.RemoveAddress(Address,Size,RegSize,Scan:LongWord);
var
  c, items:Cardinal;
  len:LongInt;
begin
  if (Size<=0) or (RegSize=0) then
    raise Exception.Create(SpSCADASizeShouldBeGreaterThanZero);

  if not Assigned(FAddressMap) then
    raise Exception.Create(SpSCADAUnassignedAddressMap);

  //captura o tamanho atual...
  //gets the actual size of address array.
  len := FAddressMap.Count;
  c:=Address;
  items := Size*RegSize + Address;
  while c<items do begin
    RemoveAddress(c, Scan);
    inc(c);
  end;

  //dipara o rebuild blocks, pq foram adicionados enderecos
  //rebuild the blocks, because address are removed.
  if len<>FAddressMap.Count then
    RebuildBlocks;
end;

procedure TPLCMemoryManager.Update(AddrStart, Size, RegSize: LongWord;
  aTimeStamp: TDateTime; LastResult: TpSCADATagValueState);
var
  items: Int64;
  c: LongWord;
  idx: Integer;
begin
  if (Size<=0) or (RegSize=0) then
    raise Exception.Create(SpSCADASizeShouldBeGreaterThanZero);

  if not Assigned(FAddressMap) then
    raise Exception.Create(SpSCADAUnassignedAddressMap);

  c:=AddrStart;
  items := Size*RegSize + AddrStart;
  while c<items do begin
    if FAddressMap.Find(c,idx) then
      FAddressMap.KeyData[c].Update(LastResult, aTimeStamp);
  end;
end;

constructor TPLCMemoryManagerSafe.Create(MaxAreaSizeInBytes: LongWord);
begin
  inherited Create(MaxAreaSizeInBytes);
  FMutex:=TCriticalSection.Create;
end;

destructor  TPLCMemoryManagerSafe.Destroy;
begin
  FMutex.Destroy;
  inherited Destroy;
end;

procedure   TPLCMemoryManagerSafe.AddAddress(Address,aSize,RegSize,Scan:Cardinal);
begin
  try
    FMutex.Enter;
    inherited AddAddress(Address,aSize,RegSize,Scan);
  finally
    FMutex.Leave;
  end;
end;

procedure TPLCMemoryManagerSafe.RemoveAddress(Address, aSize, RegSize,
  Scan: Cardinal);
begin
  try
    FMutex.Enter;
    inherited RemoveAddress(Address,aSize,RegSize,Scan);
  finally
    FMutex.Leave;
  end;
end;

procedure TPLCMemoryManagerSafe.Update(AddrStart, Size, RegSize: LongWord;
  aTimeStamp: TDateTime; LastResult: TpSCADATagValueState);
begin
  try
    FMutex.Enter;
    inherited Update(AddrStart, Size, RegSize, Values, aTimeStamp, LastResult);
  finally
    FMutex.Leave;
  end;
end;

//function    TPLCMemoryManagerSafe.GetValues(AdrStart,Len,RegSize:Cardinal; var Values:TArrayOfDouble; var LastResult:TProtocolIOResult; var ValueTimeStamp:TDateTime):LongInt;
//begin
//  try
//    FMutex.Enter;
//    Result := inherited GetValues(AdrStart, Len, RegSize, Values, LastResult, ValueTimeStamp);
//  finally
//    FMutex.Leave;
//  end;
//end;
//
//procedure   TPLCMemoryManagerSafe.SetFault(AdrStart,Len,RegSize:Cardinal; Fault:TProtocolIOResult);
//begin
//  try
//    FMutex.Enter;
//    inherited SetFault(AdrStart, Len, RegSize, Fault);
//  finally
//    FMutex.Leave;
//  end;
//end;

end.
