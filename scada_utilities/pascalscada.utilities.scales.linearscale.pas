{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o componente de escalonamento linear.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements a linear scale.)
}
{$ENDIF}
unit pascalscada.utilities.scales.linearscale;

interface

uses
  SysUtils, Classes,
  pascalscada.utilities.scales.basescale;

type

  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Componente de escalas lineares.
  @seealso(TPIPE)
  @seealso(TScaleProcessor)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Linear scale component.
  @seealso(TPIPE)
  @seealso(TScaleProcessor)
  }
  {$ENDIF}
  TpSCADALinearScale = class(TpSCADAScaleProcessor)
  private
    FProperties:Array[0..3] of double;
    function GetSysMin:Double;
    function GetSysMax:Double;
    function GetPLCMin:Double;
    function GetPLCMax:Double;
    procedure SetSysMin(v:double);
    procedure SetSysMax(v:double);
    procedure SetPLCMin(v:double);
    procedure SetPLCMax(v:double);
  protected
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;

    {$IFDEF PORTUGUES}
    {:
    Pega um valor na escala do CLP e converte para a escala do sistema.
    @param(sender Objeto que chamou a função.)
    @param(Entrada Valor do dispositivo a ser convertido.)
    @returns(O valor convertido para a escala do sistema)
    }
    {$ELSE}
    {:
    Convert a value from the device scale to the system scale.
    @param(sender Object that calls the convertion.)
    @param(Entrada Device value to be converted.)
    @returns(The value converted to the system Scale.)
    }
    {$ENDIF}
    function SetPLCValueGetSysValue(Sender:TComponent; Entrada:Double):Double; override;

    {$IFDEF PORTUGUES}
    {:
    Pega um valor na escala do sistema e converte para a escala do CLP.
    @param(sender Objeto que chamou a função.)
    @param(Entrada Valor do sistema a ser convertido.)
    @returns(O valor convertido para a escala do CLP)
    }
    {$ELSE}
    {:
    Convert a value from the device scale to the system scale.
    @param(sender Object that calls the convertion.)
    @param(Entrada Device value to be converted.)
    @returns(The value converted to the system Scale.)
    }
    {$ENDIF}
    function SetSysValueGetPLCValue(Sender:TComponent; Saida:Double):Double; override;
  published
    {$IFDEF PORTUGUES}
    //: Valor minimo de escala do sistema (Saida).
    {$ELSE}
    //: Minimum value of the system scale (output).
    {$ENDIF}
    property SysMin:Double read GetSysMin write SetSysMin Stored true;

    {$IFDEF PORTUGUES}
    //: Valor máximo da escala do dispositivo (entrada).
    {$ELSE}
    //: Maximum value of the device (PLC) scale (input).
    {$ENDIF}
    property SysMax:Double read GetSysMax write SetSysMax Stored true;

    {$IFDEF PORTUGUES}
    //: Valor mínimo da escala do dispositivo (entrada).
    {$ELSE}
    //: Minimum value of the device (PLC) scale (input).
    {$ENDIF}
    property PLCMin:Double read GetPLCMin write SetPLCMin Stored true;

    {$IFDEF PORTUGUES}
    //: Valor máximo da escala do dispositivo (entrada).
    {$ELSE}
    //: Maximum value of the device (PLC) scale (input).
    {$ENDIF}
    property PLCMax:Double read GetPLCMax write SetPLCMax Stored true;
  end;

resourcestring
  SpSCADAInvalidValue = 'Invalid value';
  SpSCADAValueShouldBeDifferent = 'The value of property "%s" should be different of value of property "%s"';

implementation

constructor TpSCADALinearScale.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then begin
    FProperties[0] := 0;
    FProperties[1] := 100;
    FProperties[2] := 0;
    FProperties[3] := 32000;
  end else begin
    FProperties[0] := 0;
    FProperties[1] := 0;
    FProperties[2] := 0;
    FProperties[3] := 0;
  end;
end;

function TpSCADALinearScale.GetSysMin:Double;
begin
  Result := FProperties[0];
end;

function TpSCADALinearScale.GetSysMax:Double;
begin
  Result := FProperties[1];
end;

function TpSCADALinearScale.GetPLCMin:Double;
begin
  Result := FProperties[2];
end;

function TpSCADALinearScale.GetPLCMax:Double;
begin
  Result := FProperties[3];
end;

procedure TpSCADALinearScale.SetSysMin(v:double);
begin
  if (v=FProperties[1]) and (ComponentState*[csReading,csLoading]=[]) then
    raise exception.Create(Format(SpSCADAValueShouldBeDifferent,['SysMin', 'SysMax']));
  FProperties[0] := v;
end;

procedure TpSCADALinearScale.SetSysMax(v:double);
begin
  if (v=FProperties[0]) and (ComponentState*[csReading,csLoading]=[]) then
    raise exception.Create(Format(SpSCADAValueShouldBeDifferent,['SysMax', 'SysMin']));
  FProperties[1] := v;
end;

procedure TpSCADALinearScale.SetPLCMin(v:double);
begin
  if (v=FProperties[3]) and (ComponentState*[csReading,csLoading]=[]) then
    raise exception.Create(Format(SpSCADAValueShouldBeDifferent,['PLCMin', 'PLCMax']));
  FProperties[2] := v;
end;

procedure TpSCADALinearScale.SetPLCMax(v:double);
begin
  if (v=FProperties[2]) and (ComponentState*[csReading,csLoading]=[]) then
    raise exception.Create(Format(SpSCADAValueShouldBeDifferent,['PLCMax', 'PLCMin']));
  FProperties[3] := v;
end;

function  TpSCADALinearScale.SetPLCValueGetSysValue(Sender:TComponent; Entrada:Double):Double;
var
  divisor:Double;
begin
  divisor := (FProperties[3]-FProperties[2]);
  if divisor=0 then divisor:=1;
  Result := (Entrada-FProperties[2])*(FProperties[1]-FProperties[0])/divisor+FProperties[0];
end;

function TpSCADALinearScale.SetSysValueGetPLCValue(Sender:TComponent; Saida:Double):Double;
var
  divisor:Double;
begin
  divisor := (FProperties[1]-FProperties[0]);
  if divisor=0 then divisor:=1;
  Result := (Saida-FProperties[0])*(FProperties[3]-FProperties[2])/divisor+FProperties[2];
end;

procedure TpSCADALinearScale.Loaded;
begin
  inherited Loaded;
  if (FProperties[0]=FProperties[1]) or (FProperties[2]=FProperties[3]) then
    raise Exception.Create(SpSCADAInvalidValue);
end;

end.
