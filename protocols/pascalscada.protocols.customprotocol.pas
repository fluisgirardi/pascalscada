unit pascalscada.protocols.customprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  pascalscada.multithreading.core_affinity_threads,
  pascalscada.multithreading.generic_thread_list,
  pascalscada.tags.base;


type
  TpSCADAThreadTagAddressInfoList = specialize TpSCADAGenericThreadList<TpSCADAPLCAddressInfo>;

  TpSCADATagTimerTickThread = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FVerifyTagsProc:TThreadMethod;
  protected
    procedure Loop; override;
  public
    constructor Create(CreateSuspended: Boolean; VerifyTagProc:TThreadMethod);
    destructor Destroy; override;
  end;


  TpSCADACustomProtocol = class(TComponent)
  private
    FTagList:TpSCADAThreadTagAddressInfoList;
    FTagScanTick:TpSCADATagTimerTickThread;
  protected
    procedure TagScanTick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    //functions to add tags to scan list
    function  RegisterTag(TagAddressInfo:TpSCADAAddressInfo):Pointer; virtual;
    function  UpdateTag(OldTagAddressInfo, NewTagAddressInfo:TpSCADAAddressInfo):Pointer; virtual;
    procedure UnregisterTag(TagAddressInfo:TpSCADAAddressInfo); virtual;
    function  ValidateTag(TagAddressInfo:TpSCADAAddressInfo):Boolean; virtual;

    procedure RequestScanRead(TagAddressInfo:TpSCADAAddressInfo);

    procedure GetIntegerValue (const addresInfo:TpSCADAAddressInfo; var Value:Int64;     var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure GetStringValue  (const addresInfo:TpSCADAAddressInfo; var Value:String;    var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure GetDoubleValue  (const addresInfo:TpSCADAAddressInfo; var Value:Double;    var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure GetDateTimeValue(const addresInfo:TpSCADAAddressInfo; var Value:TDateTime; var ValueTimeStamp:TDateTime; var ValueQuality: TpSCADATagValueState); virtual; abstract;

    procedure SetAsyncIntegerValue (const addresInfo:TpSCADAAddressInfo; const Value:Int64    ); virtual; abstract;
    procedure SetAsyncStringValue  (const addresInfo:TpSCADAAddressInfo; const Value:String   ); virtual; abstract;
    procedure SetAsyncDoubleValue  (const addresInfo:TpSCADAAddressInfo; const Value:Double   ); virtual; abstract;
    procedure SetAsyncDateTimeValue(const addresInfo:TpSCADAAddressInfo; const Value:TDateTime); virtual; abstract;

    procedure SetIntegerValue (const addresInfo:TpSCADAAddressInfo; const Value:Int64;     var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure SetStringValue  (const addresInfo:TpSCADAAddressInfo; const Value:String;    var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure SetDoubleValue  (const addresInfo:TpSCADAAddressInfo; const Value:Double;    var ValueQuality: TpSCADATagValueState); virtual; abstract;
    procedure SetDateTimeValue(const addresInfo:TpSCADAAddressInfo; const Value:TDateTime; var ValueQuality: TpSCADATagValueState); virtual; abstract;
  end;

resourcestring
  SpSCADAInvalidTagAddressInfo = 'Invalid tag address information object';

implementation

{ TpSCADACustomProtocol }

procedure TpSCADACustomProtocol.TagScanTick;
var
  list: TpSCADAThreadTagAddressInfoList.TpSCADAGenericThreadListType;
  i: Integer;
begin
  if Assigned(FTagList) then begin
    list:=FTagList.LockList;
    try
      for i:=0 to list.Count-1 do begin
        if list.Items[i].ShouldBeUpdate then begin
          TThread.Queue(nil, @list.Items[i].Update);
        end;
      end;
      Sleep(1);
    finally
      FTagList.LockList;
    end;
  end;
end;

constructor TpSCADACustomProtocol.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TpSCADACustomProtocol.Destroy;
begin
  inherited Destroy;
end;

function TpSCADACustomProtocol.RegisterTag(TagAddressInfo: TpSCADAAddressInfo
  ): Pointer;
begin
  ValidateTag(TagAddressInfo);
  FTagList.Add(TagAddressInfo as TpSCADAPLCAddressInfo);
  Result:=nil;
end;

function TpSCADACustomProtocol.UpdateTag(OldTagAddressInfo,
  NewTagAddressInfo: TpSCADAAddressInfo): Pointer;
begin
  ValidateTag(NewTagAddressInfo);
  FTagList.Remove(OldTagAddressInfo as TpSCADAPLCAddressInfo);
  FTagList.Add(NewTagAddressInfo as TpSCADAPLCAddressInfo);
  Result:=nil;
end;

procedure TpSCADACustomProtocol.UnregisterTag(TagAddressInfo: TpSCADAAddressInfo
  );
begin
  ValidateTag(TagAddressInfo);
  FTagList.Remove(TagAddressInfo as TpSCADAPLCAddressInfo);
end;

function TpSCADACustomProtocol.ValidateTag(TagAddressInfo: TpSCADAAddressInfo
  ): Boolean;
begin
  if not (TagAddressInfo is TpSCADAPLCAddressInfo) then
    raise exception.Create(SpSCADAInvalidTagAddressInfo);
  Result:=true;
end;

procedure TpSCADACustomProtocol.RequestScanRead(
  TagAddressInfo: TpSCADAAddressInfo);
begin
  ValidateTag(TagAddressInfo);
  TThread.Queue(nil, @TpSCADAPLCAddressInfo(TagAddressInfo).Update);
end;

{ TpSCADATagTimerTickThread }

procedure TpSCADATagTimerTickThread.Loop;
begin
  if Assigned(FVerifyTagsProc) then
    FVerifyTagsProc;
end;

constructor TpSCADATagTimerTickThread.Create(CreateSuspended: Boolean;
  VerifyTagProc: TThreadMethod);
begin
  inherited Create(CreateSuspended);
  FVerifyTagsProc:=VerifyTagProc;
end;

destructor TpSCADATagTimerTickThread.Destroy;
begin
  RemoveQueuedEvents(self);
  inherited Destroy;
end;

end.

