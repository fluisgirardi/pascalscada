unit pascalscada.protocols.customprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.tags.base;

type
  TpSCADACustomProtocol = class(TComponent)
  public
    function  RegisterTag(TagAddressInfo:TpSCADAAddressInfo):Pointer; virtual;
    function  UpdateTag(OldTagAddressInfo, NewTagAddressInfo:TpSCADAAddressInfo):Pointer
    procedure UnregisterTag(TagAddressInfo:TpSCADAAddressInfo);
    function  ValidateTag(TagAddressInfo:TpSCADAAddressInfo):Boolean; virtual;

    procedure RequestScanRead(TagAddressInfo:TpSCADAAddressInfo);

  end;

implementation

end.

