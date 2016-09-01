unit pascalscada.tags.tagdictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { tipos de tags
    pastas             (0)
    booleanos          (1)
    numericos inteiros (1,2,4,8)
    numericos reais    (4,8)
    CString,           (1..255)
    SiemensString
    estruturas         (1..maxint)
    arrays             (1..maxint)


    name: ansistring(255)


    blocksize: cardinal

    CfgAddresss[1..10] of Cardinal;
    UseCfgAddress[1..10] of Boolean;
  }

  TValidName = function(name:ShortString):Boolean of object;

  { TOrganizerElement }

  TOrganizerElement = class
  private
    FElementName:ShortString;
    procedure SetElementName(AValue: ShortString);
  public
    constructor Create(AOwner:TOrganizerElement);
    constructor CreateWithName(AOwner:TOrganizerElement);
    destructor Destroy; override;
    property Name:ShortString read FElementName write SetElementName;

  end;

implementation

{ TOrganizerElement }

procedure TOrganizerElement.SetElementName(AValue: ShortString);
begin
  if FElementName=AValue then Exit;
  FElementName:=AValue;
end;

constructor TOrganizerElement.Create(AOwner: TOrganizerElement);
begin

end;

constructor TOrganizerElement.CreateWithName(AOwner: TOrganizerElement);
begin

end;

destructor TOrganizerElement.Destroy;
begin
  inherited Destroy;
end;

end.

