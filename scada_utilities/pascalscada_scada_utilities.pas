{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_scada_utilities;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.utilities.scales.basescale, 
  pascalscada.utilities.scales.linearscale, 
  pascalscada.utilities.scales.scalequeue, 
  pascalscada.utilities.scales.userscale, 
  pascalscada_scada_utilities_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalscada_scada_utilities_register', 
    @pascalscada_scada_utilities_register.Register);
end;

initialization
  RegisterPackage('pascalscada_scada_utilities', @Register);
end.
