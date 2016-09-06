unit pascalscada_scada_utilities_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pascalscada.utilities.scales.userscale,
  pascalscada.utilities.scales.scalequeue,
  pascalscada.utilities.scales.linearscale;

procedure Register;

resourcestring
  SpSCADAUtilitiesPalette = 'PascalSCADA Utils';

implementation

procedure Register;
begin
  RegisterComponents(SpSCADAUtilitiesPalette,[TpSCADALinearScale,
                                              TpSCADAScalesQueue,
                                              TpSCADAUserScale]);
end;

end.

