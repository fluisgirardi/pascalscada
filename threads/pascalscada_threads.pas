{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_threads;

interface

uses
  pascalscada.multithreading.core_affinity_threads, 
  pascalscada.multithreading.event_synchronization, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_threads', @Register);
end.
