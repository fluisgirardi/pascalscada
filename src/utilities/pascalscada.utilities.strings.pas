unit pascalscada.utilities.strings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TStringArray = array of String;

function ExplodeString(delimiter:string; str:string; limit:integer=MaxInt):TStringArray;
function IsIPv4Address(aipv4:AnsiString):Boolean;

implementation

function ExplodeString(delimiter:string; str:string; limit:integer=MaxInt):TStringArray;
var
  p,cc,dsize:integer;
begin
  cc := 0;
  dsize := length(delimiter);
  if dsize = 0 then
  begin
    setlength(result,1);
    result[0] := str;
    exit;
  end;
  while cc+1 < limit do
  begin
    p := pos(delimiter,str);
    if p > 0 then
    begin
      inc(cc);
      setlength(result,cc);
      result[cc-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(cc);
  setlength(result,cc);
  result[cc-1] := str;
end;

function IsIPv4Address(aipv4: AnsiString): Boolean;
var
  ip: TStringArray;
  i, ZeroCount, FFCount: Integer;
  octeto: Longint;
begin
  Result:=false;

  if (trim(aipv4)='') then
    exit;

  ip:=ExplodeString('.',aipv4);

  if Length(ip)<>4 then
    exit;

  ZeroCount:=0;
  FFCount:=0;
  for i:=0 to 3 do begin
    if Length(ip[i])>3 then exit;
    if TryStrToInt(ip[i],octeto)=false then exit;
    if not (octeto in [0..255]) then exit;
    if ((i=0) or (i=3)) and ((octeto=0) or (octeto=255)) then exit;
    if octeto=0   then ZeroCount:=ZeroCount + 1;
    if octeto=255 then FFCount  :=FFCount   + 1;
  end;
  if ZeroCount=4 then exit;
  if FFCount=4   then exit;

  Result:=true;
end;

end.

