unit pascalscada.communication.ports.basecommport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, syncobjs;

type
  TNotificationList=specialize TFPGList<TThreadMethod>;

  {: TCustomCommPort }

  { TCustomCommPort }

  TCustomCommPort = class(TComponent)
  private
    FPortBeingDestroyed:Integer;

    FOnPortClose: TNotifyEvent;
    FOnPortCloseError: TNotifyEvent;
    FOnPortDisconnected: TNotifyEvent;
    FOnPortOpen: TNotifyEvent;
    FOnPortOpenError: TNotifyEvent;
    FOnReadError: TNotifyEvent;
    FOnWriteError: TNotifyEvent;

    //handler list.
    FPortOpenHandlerList,
    FPortOpenErrorHandlerList,
    FPortCloseHandlerList,
    FPortCloseErrorHandlerList,
    FPortDisconnectedHandlerList,
    FWriteErrorHandlerList,
    FReadErrorHandlerList:TNotificationList;

    //handler list mutexes
    FPortOpenHandlerListCS,
    FPortOpenErrorHandlerListCS,
    FPortCloseHandlerListCS,
    FPortCloseErrorHandlerListCS,
    FPortDisconnectedHandlerListCS,
    FWriteErrorHandlerListCS,
    FReadErrorHandlerListCS:TCriticalSection;

  protected
    procedure AddPortOpenHandler(handler:TThreadMethod);
    procedure AddPortOpenErrorHandler(handler:TThreadMethod);
    procedure AddPortCloseHandler(handler:TThreadMethod);
    procedure AddPortCloseErrorHandler(handler:TThreadMethod);
    procedure AddPortDisconnectedHandler(handler:TThreadMethod);
    procedure AddReadErrorHandler(handler:TThreadMethod);
    procedure AddWriteErrorHandler(handler:TThreadMethod);
    procedure RemoveHandler(handler:TThreadMethod);
    procedure RemoveHandlersOfObject(AnObject:TObject);

    procedure CallPortOpenHandlers; virtual;
    procedure CallPortOpenErrorHandlers; virtual;
    procedure CallPortCloseHandlers; virtual;
    procedure CallPortCloseErrorHandlers; virtual;
    procedure CallPortDisconnectedHandlers; virtual;
    procedure CallReadErrorHandlers; virtual;
    procedure CallWriteErrorHandlers; virtual;

    procedure DoPortOpen; virtual;
    procedure DoPortOpenError; virtual;
    procedure DoPortClose; virtual;
    procedure DoPortCloseError; virtual;
    procedure DoPortDisconnected; virtual;
    procedure DoReadError; virtual;
    procedure DoWriteError; virtual;

    function ReallyActive:Boolean; virtual;

    property OnPortOpen:TNotifyEvent read FOnPortOpen write FOnPortOpen;
    property OnPortClose:TNotifyEvent read FOnPortClose write FOnPortClose;
    property OnPortOpenError:TNotifyEvent read FOnPortOpenError write FOnPortOpenError;
    property OnPortCloseError:TNotifyEvent read FOnPortCloseError write FOnPortCloseError;
    property OnPortDisconnected:TNotifyEvent read  FOnPortDisconnected write FOnPortDisconnected;
    property OnReadError:TNotifyEvent read FOnReadError write FOnReadError;
    property OnWriteError:TNotifyEvent read FOnWriteError write FOnWriteError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TCustomCommPort }

procedure TCustomCommPort.AddPortOpenHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenHandlerListCS.Enter;
  try
    if FPortOpenHandlerList.IndexOf(handler)=-1 then
      FPortOpenHandlerList.Add(handler);
  finally
    FPortOpenHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddPortOpenErrorHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenErrorHandlerListCS.Enter;
  try
    if FPortOpenErrorHandlerList.IndexOf(handler)=-1 then
      FPortOpenErrorHandlerList.Add(handler);
  finally
    FPortOpenErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddPortCloseHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortCloseHandlerListCS.Enter;
  try
    if FPortCloseHandlerList.IndexOf(handler)=-1 then
      FPortCloseHandlerList.Add(handler);
  finally
    FPortCloseHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddPortCloseErrorHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortCloseErrorHandlerListCS.Enter;
  try
    if FPortCloseErrorHandlerList.IndexOf(handler)=-1 then
      FPortCloseErrorHandlerList.Add(handler);
  finally
    FPortCloseErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddPortDisconnectedHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortDisconnectedHandlerListCS.Enter;
  try
    if FPortDisconnectedHandlerList.IndexOf(handler)=-1 then
      FPortDisconnectedHandlerList.Add(handler);
  finally
    FPortDisconnectedHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddReadErrorHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FReadErrorHandlerListCS.Enter;
  try
    if FReadErrorHandlerList.IndexOf(handler)=-1 then
      FReadErrorHandlerList.Add(handler);
  finally
    FReadErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.AddWriteErrorHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FWriteErrorHandlerListCS.Enter;
  try
    if FWriteErrorHandlerList.IndexOf(handler)=-1 then
      FWriteErrorHandlerList.Add(handler);
  finally
    FWriteErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.RemoveHandler(handler: TThreadMethod);
var
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenHandlerListCS.Enter;
  try
    FPortOpenHandlerList.Remove(handler);
  finally
    FPortOpenHandlerListCS.Leave;
  end;

  FPortOpenErrorHandlerListCS.Enter;
  try
    FPortOpenErrorHandlerList.Remove(handler);
  finally
    FPortOpenErrorHandlerListCS.Leave;
  end;

  FPortCloseHandlerListCS.Enter;
  try
    FPortCloseHandlerList.Remove(handler);
  finally
    FPortCloseHandlerListCS.Leave;
  end;

  FPortCloseErrorHandlerListCS.Enter;
  try
    FPortCloseErrorHandlerList.Remove(handler);
  finally
    FPortCloseErrorHandlerListCS.Leave;
  end;

  FPortDisconnectedHandlerListCS.Enter;
  try
    FPortDisconnectedHandlerList.Remove(handler);
  finally
    FPortDisconnectedHandlerListCS.Leave;
  end;

  FReadErrorHandlerListCS.Enter;
  try
    FReadErrorHandlerList.Remove(handler);
  finally
    FReadErrorHandlerListCS.Leave;
  end;

  FWriteErrorHandlerListCS.Enter;
  try
    FWriteErrorHandlerList.Remove(handler);
  finally
    FWriteErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.RemoveHandlersOfObject(AnObject: TObject);
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenHandlerListCS.Enter;
  try
    for i:=0 to FPortOpenHandlerList.Count-1 do
      if TMethod(FPortOpenHandlerList.Items[i]).Data=Pointer(AnObject) then
        FPortOpenHandlerList.Delete(i);
  finally
    FPortOpenHandlerListCS.Leave;
  end;

  FPortOpenErrorHandlerListCS.Enter;
  try
    for i:=0 to FPortOpenErrorHandlerList.Count-1 do
      if TMethod(FPortOpenErrorHandlerList.Items[i]).Data=Pointer(AnObject) then
        FPortOpenErrorHandlerList.Delete(i);
  finally
    FPortOpenErrorHandlerListCS.Leave;
  end;

  FPortCloseHandlerListCS.Enter;
  try
    for i:=0 to FPortCloseHandlerList.Count-1 do
      if TMethod(FPortCloseHandlerList.Items[i]).Data=Pointer(AnObject) then
        FPortCloseHandlerList.Delete(i);
  finally
    FPortCloseHandlerListCS.Leave;
  end;

  FPortCloseErrorHandlerListCS.Enter;
  try
    for i:=0 to FPortCloseErrorHandlerList.Count-1 do
      if TMethod(FPortCloseErrorHandlerList.Items[i]).Data=Pointer(AnObject) then
        FPortCloseErrorHandlerList.Delete(i);
  finally
    FPortCloseErrorHandlerListCS.Leave;
  end;

  FPortDisconnectedHandlerListCS.Enter;
  try
    for i:=0 to FPortDisconnectedHandlerList.Count-1 do
      if TMethod(FPortDisconnectedHandlerList.Items[i]).Data=Pointer(AnObject) then
        FPortDisconnectedHandlerList.Delete(i)
  finally
    FPortDisconnectedHandlerListCS.Leave;
  end;

  FReadErrorHandlerListCS.Enter;
  try
    for i:=0 to FReadErrorHandlerList.Count-1 do
      if TMethod(FReadErrorHandlerList.Items[i]).Data=Pointer(AnObject) then
        FReadErrorHandlerList.Delete(i)
  finally
    FReadErrorHandlerListCS.Leave;
  end;

  FWriteErrorHandlerListCS.Enter;
  try
    for i:=0 to FWriteErrorHandlerList.Count-1 do
      if TMethod(FWriteErrorHandlerList.Items[i]).Data=Pointer(AnObject) then
        FWriteErrorHandlerList.Delete(i)
  finally
    FWriteErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallPortOpenHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenHandlerListCS.Enter;
  try
    for i:=0 to FPortOpenHandlerList.Count-1 do
      try
        if Assigned(FPortOpenHandlerList.Items[i]) then
         FPortOpenHandlerList.Items[i]();
      except
      end;
  finally
    FPortOpenHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallPortOpenErrorHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortOpenErrorHandlerListCS.Enter;
  try
    for i:=0 to FPortOpenErrorHandlerList.Count-1 do
      try
        if Assigned(FPortOpenErrorHandlerList.Items[i]) then
         FPortOpenErrorHandlerList.Items[i]();
      except
      end;
  finally
    FPortOpenErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallPortCloseHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortCloseHandlerListCS.Enter;
  try
    for i:=0 to FPortCloseHandlerList.Count-1 do
      try
        if Assigned(FPortCloseHandlerList.Items[i]) then
         FPortCloseHandlerList.Items[i]();
      except
      end;
  finally
    FPortCloseHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallPortCloseErrorHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortCloseErrorHandlerListCS.Enter;
  try
    for i:=0 to FPortCloseErrorHandlerList.Count-1 do
      try
        if Assigned(FPortCloseErrorHandlerList.Items[i]) then
         FPortCloseErrorHandlerList.Items[i]();
      except
      end;
  finally
    FPortCloseErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallPortDisconnectedHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FPortDisconnectedHandlerListCS.Enter;
  try
    for i:=0 to FPortDisconnectedHandlerList.Count-1 do
      try
        if Assigned(FPortDisconnectedHandlerList.Items[i]) then
         FPortDisconnectedHandlerList.Items[i]();
      except
      end;
  finally
    FPortDisconnectedHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallReadErrorHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FReadErrorHandlerListCS.Enter;
  try
    for i:=0 to FReadErrorHandlerList.Count-1 do
      try
        if Assigned(FReadErrorHandlerList.Items[i]) then
         FReadErrorHandlerList.Items[i]();
      except
      end;
  finally
    FReadErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.CallWriteErrorHandlers;
var
  i: Integer;
  res: cardinal;
begin
  InterLockedExchange(res,FPortBeingDestroyed);
  if res=1 then exit;

  FWriteErrorHandlerListCS.Enter;
  try
    for i:=0 to FWriteErrorHandlerList.Count-1 do
      try
        if Assigned(FWriteErrorHandlerList.Items[i]) then
         FWriteErrorHandlerList.Items[i]();
      except
      end;
  finally
    FWriteErrorHandlerListCS.Leave;
  end;
end;

procedure TCustomCommPort.DoPortOpen;
begin
  if Assigned(FOnPortOpen) then
    FOnPortOpen(Self);
end;

procedure TCustomCommPort.DoPortOpenError;
begin
  if Assigned(FOnPortOpenError) then
    FOnPortOpenError(Self);
end;

procedure TCustomCommPort.DoPortClose;
begin
  if Assigned(FOnPortClose) then
    FOnPortClose(Self);
end;

procedure TCustomCommPort.DoPortCloseError;
begin
  if Assigned(FOnPortCloseError) then
    FOnPortCloseError(Self);
end;

procedure TCustomCommPort.DoPortDisconnected;
begin
  if Assigned(FOnPortDisconnected) then
    FOnPortDisconnected(Self);
end;

procedure TCustomCommPort.DoReadError;
begin
  if Assigned(FOnReadError) then
    FOnReadError(Self);
end;

procedure TCustomCommPort.DoWriteError;
begin
  if Assigned(FOnWriteError) then
    FOnWriteError(Self);
end;

function TCustomCommPort.ReallyActive: Boolean;
begin
  Result:=false;
end;

constructor TCustomCommPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPortBeingDestroyed:=0;

  FPortOpenHandlerList         := TNotificationList.Create;
  FPortOpenErrorHandlerList    := TNotificationList.Create;
  FPortCloseHandlerList        := TNotificationList.Create;
  FPortCloseErrorHandlerList   := TNotificationList.Create;
  FPortDisconnectedHandlerList := TNotificationList.Create;
  FWriteErrorHandlerList       := TNotificationList.Create;
  FReadErrorHandlerList        := TNotificationList.Create;

  FPortOpenHandlerListCS         := TCriticalSection.Create;
  FPortOpenErrorHandlerListCS    := TCriticalSection.Create;
  FPortCloseHandlerListCS        := TCriticalSection.Create;
  FPortCloseErrorHandlerListCS   := TCriticalSection.Create;
  FPortDisconnectedHandlerListCS := TCriticalSection.Create;
  FWriteErrorHandlerListCS       := TCriticalSection.Create;
  FReadErrorHandlerListCS        := TCriticalSection.Create;
end;

destructor TCustomCommPort.Destroy;
begin
  InterLockedExchange(FPortBeingDestroyed, 1);

  TThread.RemoveQueuedEvents(@DoPortOpen);
  TThread.RemoveQueuedEvents(@DoPortOpenError);
  TThread.RemoveQueuedEvents(@DoPortClose);
  TThread.RemoveQueuedEvents(@DoPortCloseError);
  TThread.RemoveQueuedEvents(@DoPortDisconnected);
  TThread.RemoveQueuedEvents(@DoReadError);
  TThread.RemoveQueuedEvents(@DoWriteError);

  FreeAndNil(FPortOpenHandlerListCS);
  FreeAndNil(FPortOpenErrorHandlerListCS);
  FreeAndNil(FPortCloseHandlerListCS);
  FreeAndNil(FPortCloseErrorHandlerListCS);
  FreeAndNil(FPortDisconnectedHandlerListCS);
  FreeAndNil(FWriteErrorHandlerListCS);
  FreeAndNil(FReadErrorHandlerListCS);

  FreeAndNil(FPortOpenHandlerList);
  FreeAndNil(FPortOpenErrorHandlerList);
  FreeAndNil(FPortCloseHandlerList);
  FreeAndNil(FPortCloseErrorHandlerList);
  FreeAndNil(FPortDisconnectedHandlerList);
  FreeAndNil(FWriteErrorHandlerList);
  FreeAndNil(FReadErrorHandlerList);

  inherited Destroy;
end;

end.

