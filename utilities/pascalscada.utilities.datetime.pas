unit pascalscada.utilities.datetime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function CrossNow:TDateTime;

implementation

function CrossNow: TDateTime;
begin
  Result:=Now;
  //todo: ifdef wince...
end;

end.

