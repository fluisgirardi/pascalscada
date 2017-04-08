{$IFDEF PORTUGUES}
{:
  @abstract(Escala definida pelo usuário.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(User defined scales.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit pascalscada.utilities.scales.userscale;

interface

uses
  Classes, pascalscada.utilities.scales.basescale;

type

   {$IFDEF PORTUGUES}
   //: Definição do evento chamado para processar valores.
   {$ELSE}
   //: Definition of event used to process values.
   {$ENDIF}
   TpSCADAScaleEvent = procedure(Sender:TObject; const Input:Double; var Output:Double) of object;

   {$IFDEF PORTUGUES}
   {:
   @abstract(Classe para processamento de escalas personalizaveis pelo usuário.)
   @author(Fabio Luis Girardi <fabio@pascalscada.com>)
   }
   {$ELSE}
   {:
   @abstract(Classe para processamento de escalas personalizaveis pelo usuário.)
   @author(Fabio Luis Girardi <fabio@pascalscada.com>)
   }
   {$ENDIF}
   TpSCADAUserScale = class(TpSCADAScaleProcessor)
   private
     PPLCToSys:TpSCADAScaleEvent;
     PSysToPLC:TpSCADAScaleEvent;
   public
     {:
     @seealso(TScaleProcessor.SetInGetOut)
     @seealso(OnPLCToUser)
     }
     function SetPLCValueGetSysValue(Sender:TComponent; Entrada:Double):Double; override;
     {:
     @seealso(TScaleProcessor.SetOutGetIn)
     @seealso(OnUserToPLC)
     }
     function SetSysValueGetPLCValue(Sender:TComponent; Saida:Double):Double; override;
   published

     {$IFDEF PORTUGUES}
     {:
     Evento chamado através da função SetInGetOut para o usuário fazer sua escala
     customizada no sentido Equipamento -> Usuário.

     Os parametros do evento correspondem aos seguintes paramestros da função
     SetSetInGetOut:

     Sender => Sender da função.

     Input  => Entrada da função.

     Output => Resultado da função.

     }
     {$ELSE}
     {:
     Event called from procedure SetInGetOut to allow the developer make your own scale.
     The value is comming from device and going to final user.

     The event paremeters are the fallowing:

     Sender => Scale that call the conversion.

     Input  => Value comming from device.

     Output => Converted value.

     }
     {$ENDIF}
     property OnPLCToSys:TpSCADAScaleEvent read PPLCToSys write PPLCToSys;

     {$IFDEF PORTUGUES}
     {:
     Evento chamado através da função SetOutGetIn para o usuário fazer sua escala
     customizada no sentido Usuário -> Equipamento.

     Os parametros do evento correspondem aos seguintes paramestros da função
     SetSetInGetOut:

     Sender => Sender da função.

     Input  => Saida da função.

     Output => Resultado da função.
     }
     {$ELSE}
     {:
     Event called from procedure SetOutGetIn to allow the developer make your own scale.
     The value is comming from the user (a tag write) and going to device.

     The event paremeters are the fallowing:

     Sender => Scale that call the conversion.

     Input  => Value comming from user.

     Output => Converted value to sent to device.
     }
     {$ENDIF}

     property OnUserToPLC:TpSCADAScaleEvent read PSysToPLC write PSysToPLC;
   end;

implementation

function TpSCADAUserScale.SetPLCValueGetSysValue(Sender: TComponent; Entrada: Double
  ): Double;
begin
  if Assigned(PPLCToSys) then begin
     Result := Entrada;
     PPLCToSys(Sender,Entrada,Result);
  end else
     Result := Entrada;
end;

function TpSCADAUserScale.SetSysValueGetPLCValue(Sender: TComponent; Saida: Double
  ): Double;
begin
  if Assigned(PSysToPLC) then begin
     Result := Saida;
     PSysToPLC(Sender,Saida,Result);
  end else
     Result := Saida;
end;

end.
