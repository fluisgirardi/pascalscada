unit pascalscada.tags.base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TpSCADATagDataType = (tdtBool,
                        tdtUInt8,  tdtInt8,
                        tdtUInt16, tdtInt16,
                        tdtUInt32, tdtInt32,
                        tdtUInt64, tdtInt64,
                        tdtFloat32,tdtFloat64,
                        tdtRawTag);

  TpSCADAValidatePropertyChange = procedure(PropNumber:Word; OldValue, NewValue:Int64) of object;

  { TpSCADAAddressInfo }

  TpSCADAAddressInfo = class(TObject)
  protected
    FValidadePropertyChange: TpSCADAValidatePropertyChange;
    procedure AddressChanged; virtual; abstract;
    procedure DoChangeValidation(PropertyNumber:Word; OldValue, NewValue:Int64); virtual;
  published
    property ValidadePropertyChange:TpSCADAValidatePropertyChange read FValidadePropertyChange write FValidadePropertyChange;
  end;

  { TpSCADAPLCAddressInfo }

  TpSCADAPLCAddressInfo = class(TpSCADAAddressInfo)
  private
    FMemAddress: LongInt;
    FMemAddressArea: LongInt;
    FMemAddressAreaForWrite: LongInt;
    FMemBitAddress: Byte;
    FMemDataType: TpSCADATagDataType;
    FMemFileNumber: LongInt;
    FMemSize: DWord;
    FPLCRack: LongInt;
    FPLCSlot: LongInt;
    FPLCStation: LongInt;
    procedure SetBitAddress(AValue: Byte);
    procedure SetMemAddress(AValue: LongInt);
    procedure SetMemAddressArea(AValue: LongInt);
    procedure SetMemAddressAreaForWrite(AValue: LongInt);
    procedure SetMemDataType(AValue: TpSCADATagDataType);
    procedure SetMemFileNumber(AValue: LongInt);
    procedure SetMemSize(AValue: DWord);
    procedure SetPLCRack(AValue: LongInt);
    procedure SetPLCSlot(AValue: LongInt);
    procedure SetPLCStation(AValue: LongInt);
  private const
    PropMemAddress             = 0;
    PropMemAddressArea         = 1;
    PropMemAddressAreaForWrite = 2;
    PropMemBitAddress          = 3;
    PropMemDataType            = 4;
    PropMemFileNumber          = 5;
    PropMemSize                = 6;
    PropPLCRack                = 7;
    PropPLCSlot                = 8;
    PropPLCStation             = 9;
  protected
    procedure AddressChanged; override;
  public
 {0}property MemAddress:LongInt read FMemAddress write SetMemAddress;
 {1}property MemAddressArea:LongInt read FMemAddressArea write SetMemAddressArea;
 {2}property MemAddressAreaForWrite:LongInt read FMemAddressAreaForWrite write SetMemAddressAreaForWrite;
 {3}property MemBitAddress:Byte read FMemBitAddress write SetBitAddress;
 {4}property MemDataType:TpSCADATagDataType read FMemDataType write SetMemDataType;
 {5}property MemFileNumber:LongInt read FMemFileNumber write SetMemFileNumber;
 {6}property MemSize:DWord read FMemSize write SetMemSize;
 {7}property PLCRack:LongInt read FPLCRack write SetPLCRack;
 {8}property PLCSlot:LongInt read FPLCSlot write SetPLCSlot;
 {9}property PLCStation:LongInt read FPLCStation write SetPLCStation;
  end;

  { TpSCADAArrayElementAddressInfo }

  TpSCADAArrayItemAddressInfo = class(TpSCADAAddressInfo)
  private
    FIndex: LongInt;
    procedure SetIndex(AValue: LongInt);
  private const
    PropIndex = 1000;
  public
    property Index:LongInt read FIndex write SetIndex;
  end;

  { TpSCADAStructItemAddressInfo }

  TpSCADAStructItemAddressInfo = class(TpSCADAAddressInfo)
  private
    FDataType: TpSCADATagDataType;
    FOffset: LongInt;
    procedure SetDataType(AValue: TpSCADATagDataType);
    procedure SetOffset(AValue: LongInt);
  private const
    PropDataType = 2000;
    PropOffset   = 2001;
  public
    property DataType:TpSCADATagDataType read FDataType write SetDataType;
    property Offset:LongInt read FOffset write SetOffset;
  end;

  IpSCADAPLCLinkedInterface = interface
  ['{856323E9-BECF-4BBE-B608-64449FB9903A}']
  end;

  { TpSCADATagbookItem }

  TpSCADATagbookItem = class(TComponent)
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure ReadSubItens(Reader: TReader); virtual;
    procedure WriteSubItens(Writer: TWriter); virtual;
  end;

  { TpSCADATagbookFolder }

  TpSCADATagbookFolder = class(TpSCADATagbookItem)
  protected
    procedure ValidateInsert(AComponent: TComponent); override;
  end;

  TpSCADATagbookTag = class(TpSCADATagbookItem);

  TpSCADATagbookFixedSizeTag = class(TpSCADATagbookTag);

  { TpSCADATabgookNumericTag }

  TpSCADATagbookNumericTag = class(TpSCADATagbookFixedSizeTag)
  protected
    function SwapBytes(const aWord:Word):Word; virtual;
    function SwapWords(const aDWord: DWord): DWord; virtual;
    function SwapDWords(const aQWord: QWord): QWord; virtual;
  end;

  { TpSCADATagbookIntegerTag }

  TpSCADATagbookIntegerTag = class(TpSCADATagbookNumericTag)
  protected
    procedure ValidateInsert(AComponent: TComponent); override;
  end;

  TpSCADATagbookBitmaskTag = class(TpSCADATagbookIntegerTag);

resourcestring
  SpSCADASubComponentsNotAllowed = 'This class %s dont accept %s class as child.';

implementation

{ TpSCADATagbookIntegerTag }

procedure TpSCADATagbookIntegerTag.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TpSCADATagbookBitmaskTag then exit;
  raise Exception.Create(Format(SpSCADASubComponentsNotAllowed,[Self.ClassName, AComponent.ClassName]));
end;

{ TpSCADATabgookNumericTag }

function TpSCADATagbookNumericTag.SwapBytes(const aWord: Word): Word;
var
  bytes:array[0..1] of byte absolute Result;
  aux:byte;
begin
  Result:=aWord;
  aux:=bytes[1];
  bytes[1]:=bytes[0];
  bytes[0]:=aux;
end;

function TpSCADATagbookNumericTag.SwapWords(const aDWord: DWord): DWord;
var
  words:array[0..1] of Word absolute Result;
  aux:Word;
begin
  Result:=aDWord;
  aux:=words[1];
  words[1]:=words[0];
  words[0]:=aux;
end;

function TpSCADATagbookNumericTag.SwapDWords(const aQWord: QWord): QWord;
var
  dwords:array[0..1] of DWord absolute Result;
  aux:DWord;
begin
  Result:=aQWord;
  aux:=dwords[1];
  dwords[1]:=dwords[0];
  dwords[0]:=aux;
end;

{ TpSCADATagbookItem }

procedure TpSCADATagbookItem.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('SubItens', @ReadSubItens, @WriteSubItens, True);
end;

procedure TpSCADATagbookItem.ReadSubItens(Reader: TReader);
begin

end;

procedure TpSCADATagbookItem.WriteSubItens(Writer: TWriter);
begin

end;

{ TpSCADAStructItemAddressInfo }

procedure TpSCADAStructItemAddressInfo.SetDataType(AValue: TpSCADATagDataType);
begin
  if FDataType=AValue then Exit;
  DoChangeValidation(PropDataType,ord(FDataType), Ord(AValue));
  FDataType:=AValue;
  AddressChanged;
end;

procedure TpSCADAStructItemAddressInfo.SetOffset(AValue: LongInt);
begin
  if FOffset=AValue then Exit;
  DoChangeValidation(PropOffset,FOffset,AValue);
  FOffset:=AValue;
  AddressChanged;
end;

{ TpSCADAArrayElementAddressInfo }

procedure TpSCADAArrayItemAddressInfo.SetIndex(AValue: LongInt);
begin
  if FIndex=AValue then Exit;
  DoChangeValidation(PropIndex,FIndex,AValue);
  FIndex:=AValue;
  AddressChanged;
end;

{ TpSCADAAddressInfo }

procedure TpSCADAAddressInfo.DoChangeValidation(PropertyNumber: Word; OldValue,
  NewValue: Int64);
begin
  if Assigned(FValidadePropertyChange) then
    FValidadePropertyChange(PropertyNumber,OldValue,NewValue);
end;

{ TpSCADAPLCAddressInfo }

procedure TpSCADAPLCAddressInfo.SetBitAddress(AValue: Byte);
begin
  if FMemBitAddress=AValue then Exit;
  DoChangeValidation(PropMemBitAddress,FMemBitAddress,AValue);
  FMemBitAddress:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemAddress(AValue: LongInt);
begin
  if FMemAddress=AValue then Exit;
  DoChangeValidation(PropMemAddress,FMemAddress,AValue);
  FMemAddress:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemAddressArea(AValue: LongInt);
begin
  if FMemAddressArea=AValue then Exit;
  DoChangeValidation(PropMemAddressArea,FMemAddressArea,AValue);
  FMemAddressArea:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemAddressAreaForWrite(AValue: LongInt);
begin
  if FMemAddressAreaForWrite=AValue then Exit;
  DoChangeValidation(PropMemAddressAreaForWrite,FMemAddressAreaForWrite,AValue);
  FMemAddressAreaForWrite:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemDataType(AValue: TpSCADATagDataType);
begin
  if FMemDataType=AValue then Exit;
  DoChangeValidation(PropMemDataType,Ord(FMemDataType),Ord(AValue));
  FMemDataType:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemFileNumber(AValue: LongInt);
begin
  if FMemFileNumber=AValue then Exit;
  DoChangeValidation(PropMemFileNumber,FMemFileNumber,AValue);
  FMemFileNumber:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetMemSize(AValue: DWord);
begin
  if FMemSize=AValue then Exit;
  DoChangeValidation(PropMemSize,FMemSize,AValue);
  FMemSize:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetPLCRack(AValue: LongInt);
begin
  if FPLCRack=AValue then Exit;
  DoChangeValidation(PropPLCRack,FPLCRack,AValue);
  FPLCRack:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetPLCSlot(AValue: LongInt);
begin
  if FPLCSlot=AValue then Exit;
  DoChangeValidation(PropPLCSlot,FPLCSlot,AValue);
  FPLCSlot:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.SetPLCStation(AValue: LongInt);
begin
  if FPLCStation=AValue then Exit;
  DoChangeValidation(PropPLCStation,FPLCStation,AValue);
  FPLCStation:=AValue;
  AddressChanged;
end;

procedure TpSCADAPLCAddressInfo.AddressChanged;
begin

end;

{ TpSCADATagbookFolder }

procedure TpSCADATagbookFolder.ValidateInsert(AComponent: TComponent);
begin
  if (AComponent<>self) AND (AComponent is TpSCADATagbookFolder) then exit;
  if Supports(AComponent, IpSCADAPLCLinkedInterface) then exit;
  raise Exception.Create(Format(SpSCADASubComponentsNotAllowed,[Self.ClassName, AComponent.ClassName]));
end;

end.

