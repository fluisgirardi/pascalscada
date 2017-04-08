unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  pascalscada.multithreading.core_affinity_threads;

type

  { TInutilThread }

  TInutilThread = class(TpSCADACoreAffinityThread)
  protected
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    threads_array:array of TInutilThread;
  public
    { public declarations }

  end;

var
  Form1: TForm1;

implementation

uses BaseUnix, dateutils;

{$R *.lfm}

{ TInutilThread }

procedure TInutilThread.Execute;
var
  i:QWord = 0;
  p:Pointer;
  LastAlloc: TDateTime;
begin
  LastAlloc:=Now;
  while true do begin
    //if (MilliSecondsBetween(Now,LastAlloc)>=1) then begin
    //  p:=GetMem(2*1024*1024);
    //  LastAlloc:=Now;
    //end;
    i+=1;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SetLength(threads_array,TpSCADACoreAffinityThread.GetSystemThreadCount);
  for i:=0 to High(threads_array) do begin
    threads_array[i]:=TInutilThread.Create(true);
    threads_array[i].SetAffinity(i);
    threads_array[i].Start;
  end;
end;

end.

