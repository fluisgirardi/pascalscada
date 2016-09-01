{$IFDEF PORTUGUES}
{:
  @abstract(Implementação de processadores de escala.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the scale processors.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit pascalscada.utilities.scales.basescale;

interface

uses
  SysUtils, Classes;

type
  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe base processadora de escalas.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    @abstract(Base class for all scale processors.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TScaleProcessor = class(TComponent)
  private
    FValueIn:Double;
    procedure SetInput(value:Double);
    function  GetOutput:Double; virtual;
    procedure SetOutput(value:Double); virtual;
  public

    {$IFDEF PORTUGUES}
    {:
    Fornece um valor na escala de engenharia a partir de um valor puro em função
    e dos parametros da escala, se existirem.

    @bold(Geralmente é a informação no sentido Equipamento -> Usuário.)

    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Input Double: Valor de entrada.)
    @returns(Double. Valor processado para a escala de engenharia.)
    }
    {$ELSE}
    {:
    Returns a value in engineering scale based on a raw value and in the scales
    parameters, if there exists.

    @bold(Usually, this value is comming from device and going to user.)

    @param(Sender TComponent: Who is requesting this transformation.)
    @param(Input Double: Input value to be processed.)
    @returns(Double. The value tranformed to engineering scale.)
    }
    {$ENDIF}
    function SetInGetOut(Sender:TComponent; Input:Double):Double; virtual;

    {$IFDEF PORTUGUES}
    {:
    Fornece um valor puro a partir de um valor processado em função dos
    parametros da escala, se existirem.

    @bold(Geralmente é a informação no sentido Usuário -> Equipamento.)

    @param(Sender TComponent: Quem está solicitando esse processamento.)
    @param(Output Double: Valor na escala de engenharia a ser transformado em um valor puro.)
    @returns(Double. Valor puro em função dos parametros da escala.)
    }
    {$ELSE}
    {:
    Returns a raw value based on a value in engineering scale and in scales
    parameters, if there exists.

    @bold(Usually, this value is comming from a user input going to device.)

    @param(Sender TComponent: Who is requesting this value transformation.)
    @param(Output Double: Value in engineering scale to be processed to a raw value.)
    @returns(Double. The tranformed raw value.)
    }
    {$ENDIF}
    function SetOutGetIn(Sender:TComponent; Output:Double):Double; virtual;

  published

    {$IFDEF PORTUGUES}
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em OutPut.

    Se for escrito em OutPut, o valor processado será entregue em @name.

    @seealso(OutPut)
    }
    {$ELSE}
    {:
    Property to test the scale processor.

    If something is written in @name, the transformed value will be returned on OutPut property.

    If something is written in OutPut, the transformed value will be returned on @name property.

    @seealso(OutPut)
    }
    {$ENDIF}
    property Input:Double read FValueIn write SetInput Stored false;

    {$IFDEF PORTUGUES}
    {:
    Propriedade para testes da escala.

    Se for escrito em @name, o valor processado será entregue em Input.

    Se for escrito em Input, o valor processado será entregue em @name.

    @seealso(OutPut)
    }
    {$ELSE}
    {:
    Property to test the scale processor.

    If something is written in @name, the transformed value will be returned on Input property.

    If something is written in Input, the transformed value will be returned on @name property.

    @seealso(Input)
    }
    {$ENDIF}
    property Output:Double read GetOutput write SetOutput Stored false;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TScaleProcessor implementation
////////////////////////////////////////////////////////////////////////////////

function TScaleProcessor.SetInGetOut(Sender:TComponent; Input:Double):Double;
begin
  Result := Input;
end;

function TScaleProcessor.SetOutGetIn(Sender:TComponent; Output:Double):Double;
begin
  Result := Output;
end;

procedure TScaleProcessor.SetInput(value:Double);
begin
  FValueIn := value;
end;

procedure TScaleProcessor.SetOutput(value:Double);
begin
  FValueIn := SetOutGetIn(self, value);
end;

function  TScaleProcessor.GetOutput:Double;
begin
  Result := SetInGetOut(self, FValueIn);
end;

end.
