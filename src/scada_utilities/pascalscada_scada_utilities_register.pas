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

uses LResources;

procedure Register;
begin
  RegisterComponents(SpSCADAUtilitiesPalette,[TpSCADALinearScale,
                                              TpSCADAScalesQueue,
                                              TpSCADAUserScale]);
end;

{$IFDEF FPC}
initialization
  {$I scada_utilities.lrs}
{$ENDIF}

end.

