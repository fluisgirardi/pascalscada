unit pascalscada.multithreading.core_affinity_threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, pascalscada.multithreading.event_synchronization
  {$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
  , Windows;
  {$ELSEIF defined(freebsd) or defined(darwin)}
  , sysctl;
  {$ELSEIF defined(linux)}
  {$linklib c};
  {$IFEND}

type

  { TpSCADACoreAffinityThread }

  TpSCADACoreAffinityThread = class(TThread)
  public
    procedure MoveThreadToTheNextCore;
    procedure SetAffinity(CPUIndex:cint32);
    class function GetSystemThreadCount: cint32;
  end;

  TpSCADACoreAffinityThreadWithLoop = class(TpSCADACoreAffinityThread)
  private
    FEndLoop:TpSCADAThreadSyncEvent;
  protected
    procedure Execute; override;
    procedure Loop; virtual;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
      DefaultStackSize);
    destructor Destroy; override;
    procedure  WaitForLoopTerminates;
  end;

  {$IFDEF Linux}
  const _SC_NPROCESSORS_ONLN = 83;
  function sysconf(i: cint): clong; cdecl; external name 'sysconf';
  {$ENDIF}

  {$IFDEF UNIX}
  function sched_getaffinity(pid:Ptruint; cpusetsize:longint; cpuset:pointer):longint; cdecl; external;
  function sched_setaffinity(pid:Ptruint; cpusetsize:longint; cpuset:pointer):longint; cdecl; external;

  function pthread_setaffinity_np(pid:TThreadID; cpusetsize:longint; cpuset:pointer):longint; cdecl; external;
  function pthread_getaffinity_np(pid:TThreadID; cpusetsize:longint; cpuset:pointer):longint; cdecl; external;
  {$ENDIF}


implementation

uses syncobjs;

{ TpSCADACoreAffinityThreadWithLoop }

procedure TpSCADACoreAffinityThreadWithLoop.Execute;
begin
  while not FEndLoop.ResetEvent do;

  while not Terminated do
    Loop;

  while not FEndLoop.SetEvent do;
end;

procedure TpSCADACoreAffinityThreadWithLoop.Loop;
begin
  Sleep(1000); //avoid use entire CPU...
end;

constructor TpSCADACoreAffinityThreadWithLoop.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FEndLoop:=TpSCADAThreadSyncEvent.Create(true, false);
end;

destructor TpSCADACoreAffinityThreadWithLoop.Destroy;
begin
  Terminate;
  WaitForLoopTerminates;
  FreeAndNil(FEndLoop);
  inherited Destroy;
end;

procedure TpSCADACoreAffinityThreadWithLoop.WaitForLoopTerminates;
begin
  while FEndLoop.WaitFor(1)<>wrSignaled do
    CheckSynchronize(1);
end;

{ TMultiCoreThread }

class function TpSCADACoreAffinityThread.GetSystemThreadCount: cint32;
// returns a good default for the number of threads on this system
{$IF defined(WIN32) or defined(WIN64)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: cint32;
  ProcessAffinityMask, SystemAffinityMask: crossNativeUInt;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := 1 shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(WINCE)}
begin
  Result:=1;
end;
{$ELSEIF defined(solaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array [0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  {$IF defined(freebsd)}
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  {$ELSEIF defined(darwin)}
  fpsysctl(pcint(@mib), 2, @t, @len, Nil, 0);
  {$IFEND}
  Result:=t;
end;
{$ELSEIF defined(linux)}
begin
   Result:=sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ELSE}
begin
  Result:=1;
end;
{$IFEND}

var
  CurrentCore:cint32 = 0;
procedure TpSCADACoreAffinityThread.MoveThreadToTheNextCore;
var
  CPUSet,
  CurCore:cint32;
begin
  CurCore:=0;
  InterLockedExchange(CurCore,CurrentCore);
  CPUSet:=1 shl CurCore;
  CurCore+=1;
  if (CurCore>=32) OR (CurCore>=GetSystemThreadCount) then InterLockedExchange(CurrentCore, 0);
  {$IFDEF UNIX}
  pthread_setaffinity_np(ThreadID,SizeOf(cint32),@CPUSet);
  {$ELSE}
  {$ERROR Implementation missing for Windows!}
  {$ENDIF}
end;

procedure TpSCADACoreAffinityThread.SetAffinity(CPUIndex: cint32);
var
  CPUSet:cint32;
begin
  CPUSet:=1 shl CPUIndex;
  {$IFDEF UNIX}
  pthread_setaffinity_np(ThreadID,SizeOf(cint32),@CPUSet);
  {$ELSE}
  {$ERROR Implementation missing for Windows!}
  {$ENDIF}
end;

var
  AllCoresSet:cuint32 = $ffffffff;
initialization
  //sets the process affinity with all processor threads.
  {$IFDEF UNIX}
  sched_setaffinity(GetProcessID,SizeOf(AllCoresSet), @AllCoresSet);
  {$ELSE}
  {$ERROR Implementation missing for Windows!}
  {$ENDIF}
end.

