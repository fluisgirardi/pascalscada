unit pascalscada.multithreading.core_affinity_threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes
  {$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
  , Windows;
  {$ELSEIF defined(freebsd) or defined(darwin)}
  , sysctl;
  {$ELSEIF defined(linux)}
  {$linklib c};
  {$IFEND}

type

  { TMultiCoreThread }

  TpSCADACoreAffinityThread = class(TThread)
  public
    procedure GotoNextCore;
    class function GetSystemThreadCount: cint32;
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
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
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
procedure TpSCADACoreAffinityThread.GotoNextCore;
var
  CPUSet,
  CurCore:cint32;
begin
  CurCore:=0;
  InterLockedExchange(CurCore,CurrentCore);
  CPUSet:=1 shl CurCore;
  CurCore+=1;
  if (CurCore>=32) OR (CurCore>=GetSystemThreadCount) then InterLockedExchange(CurrentCore, 0);
  pthread_setaffinity_np(ThreadID,SizeOf(cint32),@CPUSet);
end;

var
  AllCoresSet, aux, i:cint32;
initialization

  AllCoresSet:=0;
  for i:=0 to TpSCADACoreAffinityThread.GetSystemThreadCount-1 do begin
    aux:=1 shl i;
    AllCoresSet+=aux;
  end;

  sched_setaffinity(GetProcessID,SizeOf(AllCoresSet), @AllCoresSet);
end.

