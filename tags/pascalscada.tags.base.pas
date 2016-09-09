unit pascalscada.tags.base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, pascalscada.utilities.scales.basescale;

type

  TpSCADATagValueState = (ioNone,ioOk,ioDriverError, ioCommError, ioTimeOut,
                          ioIllegalFunction, ioIllegalRegAddress,ioIllegalValue,
                          ioPLCError, ioTagError, ioNullDriver, ioIllegalRequest,
                          ioIllegalStationAddress, ioObjectNotExists,
                          ioIllegalMemoryAddress, ioUnknownError, ioEmptyPacket,
                          ioPartialOk, ioAcknowledge, ioBusy, ioNACK,
                          ioMemoryParityError, ioGatewayUnavailable,
                          ioDeviceGatewayFailedToRespond);

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
  public
    procedure Assign(Source:TpSCADAAddressInfo); virtual;
  published
    property ValidadePropertyChange:TpSCADAValidatePropertyChange read FValidadePropertyChange write FValidadePropertyChange;
  end;

  TpSCADAGetIntegerValue  = procedure(const addresInfo:TpSCADAAddressInfo; var Value:Int64;     var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState) of object;
  TpSCADAGetStringValue   = procedure(const addresInfo:TpSCADAAddressInfo; var Value:String;    var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState) of object;
  TpSCADAGetDoubleValue   = procedure(const addresInfo:TpSCADAAddressInfo; var Value:Double;    var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState) of object;
  TpSCADAGetDateTimeValue = procedure(const addresInfo:TpSCADAAddressInfo; var Value:TDateTime; var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState) of object;

  TpSCADASetIntegerValue  = procedure(const addresInfo:TpSCADAAddressInfo; const Value:Int64;     var ValueQuality: TpSCADATagValueState) of object;
  TpSCADASetStringValue   = procedure(const addresInfo:TpSCADAAddressInfo; const Value:String;    var ValueQuality: TpSCADATagValueState) of object;
  TpSCADASetDoubleValue   = procedure(const addresInfo:TpSCADAAddressInfo; const Value:Double;    var ValueQuality: TpSCADATagValueState) of object;
  TpSCADASetDateTimeValue = procedure(const addresInfo:TpSCADAAddressInfo; const Value:TDateTime; var ValueQuality: TpSCADATagValueState) of object;

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
    procedure Assign(Source: TpSCADAAddressInfo); override;
 {0}property  MemAddress:LongInt read FMemAddress write SetMemAddress;
 {1}property  MemAddressArea:LongInt read FMemAddressArea write SetMemAddressArea;
 {2}property  MemAddressAreaForWrite:LongInt read FMemAddressAreaForWrite write SetMemAddressAreaForWrite;
 {3}property  MemBitAddress:Byte read FMemBitAddress write SetBitAddress;
 {4}property  MemDataType:TpSCADATagDataType read FMemDataType write SetMemDataType;
 {5}property  MemFileNumber:LongInt read FMemFileNumber write SetMemFileNumber;
 {6}property  MemSize:DWord read FMemSize write SetMemSize;
 {7}property  PLCRack:LongInt read FPLCRack write SetPLCRack;
 {8}property  PLCSlot:LongInt read FPLCSlot write SetPLCSlot;
 {9}property  PLCStation:LongInt read FPLCStation write SetPLCStation;
  end;

  { TpSCADAArrayItemAddressInfo }

  TpSCADAArrayItemAddressInfo = class(TpSCADAAddressInfo)
  private
    FIndex: LongInt;
    procedure SetIndex(AValue: LongInt);
  private const
    PropIndex = 1000;
  public
    procedure Assign(Source: TpSCADAAddressInfo); override;
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
    procedure Assign(Source: TpSCADAAddressInfo); override;
    property  DataType:TpSCADATagDataType read FDataType write SetDataType;
    property  Offset:LongInt read FOffset write SetOffset;
  end;

  { TpSCADATagBitAddressInfo }

  TpSCADATagBitAddressInfo = class(TpSCADAAddressInfo)
  private
    FBit: Byte;
    FBitCount: Byte;
    FUseValueRaw: Boolean;
    procedure SetBit(AValue: Byte);
    procedure SetBitCount(AValue: Byte);
    procedure SetUseValueRaw(AValue: Boolean);
  private const
    PropBit         = 3000;
    PropBitCount    = 3001;
    PropUseValueRaw = 3002;
  public
    procedure Assign(Source: TpSCADAAddressInfo); override;
    property  Bit:Byte read FBit write SetBit;
    property  BitCount:Byte read FBitCount Write SetBitCount;
    property  UseValueRaw:Boolean read FUseValueRaw write SetUseValueRaw;
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

  { TpSCADATagbookTag }

  TpSCADATagbookTag = class(TpSCADATagbookItem)
  protected
    FSyncValueQuality: TpSCADATagValueState;
    FSyncWriteQuality: TpSCADATagValueState;
    FValueQuality: TpSCADATagValueState;
    FWriteQuality: TpSCADATagValueState;
    FValueTimestamp:TDateTime;

    property ValueQuality:TpSCADATagValueState read FValueQuality;
    property ValueTimestamp:TDateTime read FValueTimestamp;
    property WriteQuality:TpSCADATagValueState read FWriteQuality;
    property SyncValueQuality:TpSCADATagValueState read FSyncValueQuality;
    property SyncWriteQuality:TpSCADATagValueState read FSyncWriteQuality;
  end;

  TpSCADATagbookFixedSizeTag = class(TpSCADATagbookTag);

  { TpSCADATagbookNumericTag }

  TpSCADATagbookNumericTag = class(TpSCADATagbookFixedSizeTag)
  protected
    FScale: TpSCADAScaleProcessor;
    procedure SetScale(AValue: TpSCADAScaleProcessor);

    function SwapBytes(const aWord:Word):Word; virtual;
    function SwapWords(const aDWord: DWord): DWord; virtual;
    function SwapDWords(const aQWord: QWord): QWord; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property Scale:TpSCADAScaleProcessor read FScale write SetScale;
  end;

  { TpSCADATagbookIntegerTag }

  TpSCADATagbookIntegerTag = class(TpSCADATagbookNumericTag)
  private
    function GetValue: Int64;
    procedure SetValue(AValue: Int64);
  protected
    FGetValueProc:TpSCADAGetIntegerValue;
    FSetValueProc:TpSCADASetIntegerValue;
    function GetValueRaw: Int64; virtual; abstract;
    procedure SetValueRaw(AValue: Int64); virtual; abstract;

    procedure ReadTagBitValue(const addresInfo:TpSCADAAddressInfo; var Value:Int64;   var aValueTimeStamp:TDateTime; var aValueQuality: TpSCADATagValueState);
    procedure WriteTagBitValue(const addresInfo:TpSCADAAddressInfo; const Value:Int64; var aValueQuality: TpSCADATagValueState);

    function ReadBits(BitIndex:Byte; UseRawValue:Boolean; BitCount:Byte = 1):Int64;
    procedure WriteBits(BitIndex:Byte; UseRawValue:Boolean; Value:Int64; BitCount:Byte = 1);

    procedure ValidateInsert(AComponent: TComponent); override;
    property Value:Int64 read GetValue write SetValue;
    property ValueRaw:Int64 read GetValueRaw write SetValueRaw;
  public
    constructor Create(AOwner: TComponent; GetValueProc:TpSCADAGetIntegerValue; SetValueProc:TpSCADASetIntegerValue); virtual;
  end;

  { TpSCADATagbookBitmapTag }

  TpSCADATagbookBitmapTag = class(TpSCADATagbookIntegerTag)
  private
    FIntegerTagLoaded:TpSCADATagbookIntegerTag;
    function getIntegerTagStored: Boolean;
  protected
    FBitAddressInfo:TpSCADATagBitAddressInfo;
    FIntegerTag: TpSCADATagbookIntegerTag;
    function GetBitAddress: TpSCADATagBitAddressInfo;
    function GetIntegerTag: TpSCADATagbookIntegerTag;
    procedure SetBitAddress(AValue: TpSCADATagBitAddressInfo);
    function GetValueRaw: Int64; override;
    procedure SetIntegerTag(AValue: TpSCADATagbookIntegerTag);
    procedure SetValueRaw(AValue: Int64); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent; GetValueProc: TpSCADAGetIntegerValue; SetValueProc: TpSCADASetIntegerValue); override;
    destructor Destroy; override;
  published
    property BitAddress:TpSCADATagBitAddressInfo read GetBitAddress write SetBitAddress;
    property Scale;
    property Value;
    property ValueRaw;
    property IntegerTag:TpSCADATagbookIntegerTag read GetIntegerTag write SetIntegerTag stored getIntegerTagStored;
  end;

  { TpSCADATagbookRealTag }

  TpSCADATagbookRealTag = class(TpSCADATagbookNumericTag)
  private
    function GetValue: Double;
    procedure SetValue(AValue: Double);
  protected
    FGetValueProc:TpSCADAGetDoubleValue;
    FSetValueProc:TpSCADASetDoubleValue;
    function GetValueRaw: Double; virtual; abstract;
    procedure SetValueRaw(AValue: Double); virtual; abstract;

    procedure ValidateInsert(AComponent: TComponent); override;
    property Value:Double read GetValue write SetValue;
    property ValueRaw:Double read GetValueRaw write SetValueRaw;
  public
    constructor Create(AOwner: TComponent; GetValueProc:TpSCADAGetDoubleValue; SetValueProc:TpSCADASetDoubleValue); virtual;
  end;

resourcestring
  SpSCADASubComponentsNotAllowed       = 'This class %s dont accept %s class as child.';
  SpSCADAAssignError                   = 'Assign error: The class %s cannot assign %s class';
  SpSCADABitCountError                 = 'The bit count must be greater than zero.';
  SpSCADAInvalidTagAddress             = 'The address information object is not a %s class, required for %s values in class %s';
  SpSCADAGet                           = 'get';
  SpSCADASet                           = 'set';
  SpSCADABitCountMustBeGreaterThanZero = 'Bit count must be greater than zero';
  SpSCADABitMaskOutOfBounds            = 'Bit index added with bit count exceeds the Int64 Limit. BitIndex=%d and BitCount=%d';

implementation

uses math;

{ TpSCADATagbookRealTag }

function TpSCADATagbookRealTag.GetValue: Double;
begin
  if Assigned(FScale) then
    Result := FScale.SetPLCValueGetSysValue(Self, GetValueRaw)
  else
    Result:=GetValueRaw;
end;

procedure TpSCADATagbookRealTag.SetValue(AValue: Double);
begin
  if Assigned(FScale) then
    SetValueRaw(FScale.SetSysValueGetPLCValue(Self, AValue))
  else
    SetValueRaw(AValue);
end;

procedure TpSCADATagbookRealTag.ValidateInsert(AComponent: TComponent);
begin
  raise Exception.Create(SpSCADASubComponentsNotAllowed,[Self.ClassName,AComponent.ClassName]);
end;

constructor TpSCADATagbookRealTag.Create(AOwner: TComponent;
  GetValueProc: TpSCADAGetDoubleValue; SetValueProc: TpSCADASetDoubleValue);
begin
  inherited Create(AOwner);
  FSetValueProc:=SetValueProc;
  FGetValueProc:=GetValueProc;
end;

{ TpSCADATagbookBitmaskTag }

function TpSCADATagbookBitmapTag.getIntegerTagStored: Boolean;
begin
  Result:=not (assigned(Owner) and (Owner is TpSCADATagbookIntegerTag));
end;

function TpSCADATagbookBitmapTag.GetBitAddress: TpSCADATagBitAddressInfo;
begin
  Result:=TpSCADATagBitAddressInfo.Create;
  Result.Assign(FBitAddressInfo);
end;

function TpSCADATagbookBitmapTag.GetIntegerTag: TpSCADATagbookIntegerTag;
begin
  if assigned(Owner) and (Owner is TpSCADATagbookIntegerTag) then
    Result:=Owner as TpSCADATagbookIntegerTag
  else
    Result:=FIntegerTag;
end;

procedure TpSCADATagbookBitmapTag.SetBitAddress(
  AValue: TpSCADATagBitAddressInfo);
begin
  if Assigned(FBitAddressInfo) then
    FBitAddressInfo.Assign(AValue);
end;

function TpSCADATagbookBitmapTag.GetValueRaw: Int64;
begin
  if Assigned(FGetValueProc) then
    FGetValueProc(FBitAddressInfo, Result, FValueTimestamp, FValueQuality);
end;

procedure TpSCADATagbookBitmapTag.SetIntegerTag(
  AValue: TpSCADATagbookIntegerTag);
begin
  if assigned(Owner) and (Owner is TpSCADATagbookIntegerTag) then exit;

  if [csReading, csLoading]*ComponentState<>[] then begin
    FIntegerTagLoaded:=AValue;
    exit;
  end;

  if FIntegerTag=AValue then Exit;

  if Assigned(FIntegerTag) then begin
    FIntegerTag.RemoveFreeNotification(Self);
    FSetValueProc:=nil;
    FGetValueProc:=nil;
  end;

  FIntegerTag:=AValue;

  if Assigned(FIntegerTag) then begin
    FIntegerTag.FreeNotification(Self);
    FGetValueProc:=@FIntegerTag.ReadTagBitValue;
    FSetValueProc:=@FIntegerTag.WriteTagBitValue;
  end;
end;

procedure TpSCADATagbookBitmapTag.SetValueRaw(AValue: Int64);
begin
  if Assigned(FSetValueProc) then
    FSetValueProc(FBitAddressInfo, AValue, FWriteQuality);
end;

constructor TpSCADATagbookBitmapTag.Create(AOwner: TComponent;
  GetValueProc: TpSCADAGetIntegerValue; SetValueProc: TpSCADASetIntegerValue);
begin
  inherited Create(AOwner, GetValueProc, SetValueProc);
  FBitAddressInfo:=TpSCADATagBitAddressInfo.Create;
  if Assigned(AOwner) and (AOwner is TpSCADATagbookIntegerTag) then begin
    FGetValueProc:=@(AOwner as TpSCADATagbookIntegerTag).ReadTagBitValue;
    FSetValueProc:=@(AOwner as TpSCADATagbookIntegerTag).WriteTagBitValue;
  end;
end;

destructor TpSCADATagbookBitmapTag.Destroy;
begin
  FreeAndNil(FBitAddressInfo);
  inherited Destroy;
end;

procedure TpSCADATagbookBitmapTag.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) and Assigned(FIntegerTag) and (AComponent=FIntegerTag) then begin
    FIntegerTag:=nil;
    FGetValueProc:=nil;
    FSetValueProc:=nil;
  end else
    inherited Notification(AComponent, Operation);
end;

procedure TpSCADATagbookBitmapTag.Loaded;
begin
  inherited Loaded;
  if getIntegerTagStored then
    SetIntegerTag(FIntegerTagLoaded);
end;

{ TpSCADATagBitAddressInfo }

procedure TpSCADATagBitAddressInfo.SetBit(AValue: Byte);
begin
  if FBit=AValue then Exit;
  DoChangeValidation(PropBit,FBit, AValue);
  FBit:=AValue;
  AddressChanged;
end;

procedure TpSCADATagBitAddressInfo.SetBitCount(AValue: Byte);
begin
  if FBitCount=AValue then Exit;
  if AValue<=0 then
    raise exception.Create(SpSCADABitCountError);
  DoChangeValidation(PropBitCount, FBitCount, AValue);
  FBitCount:=AValue;
  AddressChanged;
end;

procedure TpSCADATagBitAddressInfo.SetUseValueRaw(AValue: Boolean);
begin
  if FUseValueRaw=AValue then Exit;
  DoChangeValidation(PropUseValueRaw, ifthen(FUseValueRaw,1,0), ifthen(AValue, 1, 0));
  FUseValueRaw:=AValue;
  AddressChanged;
end;

procedure TpSCADATagBitAddressInfo.Assign(Source: TpSCADAAddressInfo);
var
  aSource: TpSCADATagBitAddressInfo;
begin
  if Assigned(Source) and (Source is TpSCADATagBitAddressInfo) then begin
    aSource:=TpSCADATagBitAddressInfo(Source);
    FBit      :=asource.FBit;
    FBitCount :=asource.FBitCount;
  end else
    inherited Assign(Source);
end;

{ TpSCADATagbookIntegerTag }

function TpSCADATagbookIntegerTag.GetValue: Int64;
begin
  if Assigned(FScale) then
    Result := Trunc(FScale.SetPLCValueGetSysValue(Self, GetValueRaw))
  else
    Result:=GetValueRaw;
end;

procedure TpSCADATagbookIntegerTag.SetValue(AValue: Int64);
begin
  if Assigned(FScale) then
    SetValueRaw(Trunc(FScale.SetSysValueGetPLCValue(Self, AValue)))
  else
    SetValueRaw(AValue);
end;

procedure TpSCADATagbookIntegerTag.ReadTagBitValue(
  const addresInfo: TpSCADAAddressInfo; var Value: Int64;
  var aValueTimeStamp: TDateTime; var aValueQuality: TpSCADATagValueState);
var
  aBitInfo: TpSCADATagBitAddressInfo;
begin
  if not (addresInfo is TpSCADATagBitAddressInfo) then
    raise Exception.Create(Format(SpSCADAInvalidTagAddress,[TpSCADATagBitAddressInfo.ClassName, SpSCADAGet, Self.ClassName]));

  aBitInfo := addresInfo as TpSCADATagBitAddressInfo;

  Value           := ReadBits(aBitInfo.Bit,aBitInfo.UseValueRaw,aBitInfo.BitCount);
  aValueQuality   := ValueQuality;
  aValueTimeStamp := aValueTimeStamp;
end;

procedure TpSCADATagbookIntegerTag.WriteTagBitValue(
  const addresInfo: TpSCADAAddressInfo; const Value: Int64;
  var aValueQuality: TpSCADATagValueState);
var
  aBitInfo: TpSCADATagBitAddressInfo;
begin
  if not (addresInfo is TpSCADATagBitAddressInfo) then
    raise Exception.Create(Format(SpSCADAInvalidTagAddress,[TpSCADATagBitAddressInfo.ClassName, SpSCADASet, Self.ClassName]));

  aBitInfo := addresInfo as TpSCADATagBitAddressInfo;

  WriteBits(aBitInfo.Bit, aBitInfo.UseValueRaw, Value, aBitInfo.BitCount);
  aValueQuality   := ioOk;
end;

function TpSCADATagbookIntegerTag.ReadBits(BitIndex: Byte;
  UseRawValue: Boolean; BitCount: Byte): Int64;
var
  aValue, aBitIndex, aMask: Int64;
begin
  if BitCount<1 then
    raise exception.Create(SpSCADABitCountMustBeGreaterThanZero);

  if (not (BitIndex in [0..63])) or ((BitIndex+BitCount)>64) then
    raise exception.Create(Format(SpSCADABitMaskOutOfBounds,[BitIndex, BitCount]));

  aBitIndex:=BitIndex;

  if UseRawValue then
    aValue:=GetValueRaw
  else
    aValue:=GetValue;

  aMask:=trunc(power(2,BitCount))-1;

  aValue:=(aValue shr aBitIndex) and aMask;
end;

procedure TpSCADATagbookIntegerTag.WriteBits(BitIndex: Byte;
  UseRawValue: Boolean; Value: Int64; BitCount: Byte);
var
  aBitIndex, aMask, aValue, aValue2, aNotMask: Int64;
begin
  if BitCount<1 then
    raise exception.Create(SpSCADABitCountMustBeGreaterThanZero);

  if (not (BitIndex in [0..63])) or ((BitIndex+BitCount)>64) then
    raise exception.Create(Format(SpSCADABitMaskOutOfBounds,[BitIndex, BitCount]));

  aBitIndex:=BitIndex;

  if UseRawValue then
    aValue:=GetValueRaw
  else
    aValue:=GetValue;

  aMask    := trunc(power(2,BitCount))-1;
  aValue2  := (Value and aMask) shl aBitIndex;

  aNotMask := not (aMask shl aBitIndex);
  aMask    := aMask shl aBitIndex;

  aValue:=aValue and aNotMask;
  aValue:=aValue or  aValue2;

  if UseRawValue then
    SetValueRaw(aValue)
  else
    SetValue(aValue);
end;

procedure TpSCADATagbookIntegerTag.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TpSCADATagbookBitmapTag then exit;
  raise Exception.Create(Format(SpSCADASubComponentsNotAllowed,[Self.ClassName, AComponent.ClassName]));
end;

constructor TpSCADATagbookIntegerTag.Create(AOwner: TComponent;
  GetValueProc: TpSCADAGetIntegerValue; SetValueProc: TpSCADASetIntegerValue);
begin
  inherited Create(AOwner);
  FSetValueProc:=SetValueProc;
  FGetValueProc:=GetValueProc;
end;

{ TpSCADATabgookNumericTag }

procedure TpSCADATagbookNumericTag.SetScale(AValue: TpSCADAScaleProcessor);
begin
  if FScale=AValue then Exit;
  if Assigned(FScale) then
    FScale.RemoveFreeNotification(Self);
  if Assigned(AValue) then
    AValue.FreeNotification(Self);
  FScale:=AValue;
end;

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

procedure TpSCADATagbookNumericTag.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FScale) then
    FScale:=nil;
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

procedure TpSCADAStructItemAddressInfo.Assign(Source: TpSCADAAddressInfo);
var
  aSource: TpSCADAStructItemAddressInfo;
begin
  if Assigned(Source) and (Source is TpSCADAStructItemAddressInfo) then begin
    aSource:=TpSCADAStructItemAddressInfo(Source);
    FDataType:=asource.FDataType;
    FOffset  :=asource.FOffset;
  end else
    inherited Assign(Source);
end;

{ TpSCADAArrayElementAddressInfo }

procedure TpSCADAArrayItemAddressInfo.SetIndex(AValue: LongInt);
begin
  if FIndex=AValue then Exit;
  DoChangeValidation(PropIndex,FIndex,AValue);
  FIndex:=AValue;
  AddressChanged;
end;

procedure TpSCADAArrayItemAddressInfo.Assign(Source: TpSCADAAddressInfo);
begin
  if Assigned(Source) and (Source is TpSCADAArrayItemAddressInfo) then
    FIndex := TpSCADAArrayItemAddressInfo(Source).FIndex
  else
    inherited Assign(Source);
end;

{ TpSCADAAddressInfo }

procedure TpSCADAAddressInfo.Assign(Source: TpSCADAAddressInfo);
begin
  raise exception.Create(Format(SpSCADAAssignError,[ClassName, Source.ClassName]));
end;

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

procedure TpSCADAPLCAddressInfo.Assign(Source: TpSCADAAddressInfo);
var
  aSource: TpSCADAPLCAddressInfo;
begin
  if Assigned(Source) and (Source is TpSCADAPLCAddressInfo) then begin
    aSource:=TpSCADAPLCAddressInfo(Source);
    FMemAddress             := aSource.FMemAddress;
    FMemAddressArea         := aSource.FMemAddressArea;
    FMemAddressAreaForWrite := aSource.FMemAddressAreaForWrite;
    FMemBitAddress          := aSource.FMemBitAddress;
    FMemDataType            := aSource.FMemDataType;
    FMemFileNumber          := aSource.FMemFileNumber;
    FMemSize                := aSource.FMemSize;
    FPLCRack                := aSource.FPLCRack;
    FPLCSlot                := aSource.FPLCSlot;
    FPLCStation             := aSource.FPLCStation;
  end else
    inherited Assign(Source);
end;

{ TpSCADATagbookFolder }

procedure TpSCADATagbookFolder.ValidateInsert(AComponent: TComponent);
begin
  if (AComponent<>self) AND (AComponent is TpSCADATagbookFolder) then exit;
  if Supports(AComponent, IpSCADAPLCLinkedInterface) then exit;
  raise Exception.Create(Format(SpSCADASubComponentsNotAllowed,[Self.ClassName, AComponent.ClassName]));
end;

end.

