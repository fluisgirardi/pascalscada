unit pascalscada.utilities.datetime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function pSCADA_CrossNow:TDateTime;

implementation

function pSCADA_CrossNow: TDateTime;
begin
  Result:=Now;
  //todo: ifdef wince...
end;

end.

