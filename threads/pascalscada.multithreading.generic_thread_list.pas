{:
   @abstract(Generic Thread List based on Freepascal TThreadList)
   @author(Fabio Luis Girardi fabio@pascalscada.com)
}
unit pascalscada.multithreading.generic_thread_list;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  generic TpSCADAGenericThreadList<T> = class(TObject)
  public type
    TpSCADAGenericThreadListType = specialize TFPGList<T>;
  private
    FList: TpSCADAGenericThreadListType;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(Item: T);
    procedure   Clear;
    function    LockList: TpSCADAGenericThreadListType;
    procedure   Remove(Item: T);
    procedure   UnlockList;
    property    Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

resourceString
  SpSCADADuplicateItem = 'Duplicates not allowed in this list ($0%x)';

implementation


constructor TpSCADAGenericThreadList.Create;
  begin
    inherited Create;
    FDuplicates:=dupIgnore;
    InitCriticalSection(FLock);
    FList:=TpSCADAGenericThreadListType.Create;
  end;


destructor TpSCADAGenericThreadList.Destroy;
  begin
    LockList;
    try
      FList.Free;
      inherited Destroy;
    finally
      UnlockList;
      DoneCriticalSection(FLock);
    end;
  end;


procedure TpSCADAGenericThreadList.Add(Item: T);
  begin
    LockList;
    try
      if (Duplicates=dupAccept) or
        // make sure it's not already in the list
        (FList.IndexOf(Item)=-1) then
         FList.Add(Item)
       else if (Duplicates=dupError) then
         FList.Error(SpSCADADuplicateItem,PtrUInt(Item));
    finally
      UnlockList;
    end;
  end;


procedure TpSCADAGenericThreadList.Clear;
  begin
    Locklist;
    try
      FList.Clear;
    finally
      UnLockList;
    end;
  end;


function TpSCADAGenericThreadList.LockList: TpSCADAGenericThreadListType;
  begin
    Result:=FList;
    System.EnterCriticalSection(FLock);
  end;


procedure TpSCADAGenericThreadList.Remove(Item: T);
  begin
    LockList;
    try
      FList.Remove(Item);
    finally
      UnlockList;
    end;
  end;


procedure TpSCADAGenericThreadList.UnlockList;
  begin
    System.LeaveCriticalSection(FLock);
  end;

end.

