{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_hashs;

interface

uses
  pascalscada.hash.crc_16, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_hashs', @Register);
end.
