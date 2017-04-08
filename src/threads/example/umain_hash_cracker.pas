unit umain_hash_cracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, fgl,
  pascalscada.multithreading.core_affinity_threads;

type
  THashProc = function(input:String):String of object;

  THashList = specialize TFPGList<THashProc>;

  { THashCrackerClass }

  THashCrackerClass = class(TpSCADACoreAffinityThread)
  private
    FSenha:String;
    procedure EncontrouSenha;
  protected
    FHashTarget:String;
    FHashList:THashList;
    FFirstChar,
    FLastChar,
    FCharactersLimit:Integer;
    FTimeHashing:TDateTime;

    procedure Execute; override;
    function EncontrarSenha(var aEncontrouSenha:Boolean): String;

    function  CompareHash(aCombination:String):Boolean;
  public
    constructor Create(ASuspended: Boolean; HashTarget: String;
      aLimiteCaracteres, primeiroCaracter, UltimoCaracter: Integer; aHashList: THashList);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FHashCrackThread:array of THashCrackerClass;
    function ComputeHashMD4(aCombination: String): String;
    function ComputeHashMD2(aCombination: String): String;
    function ComputeHashMD5(aCombination: String): String;
    function ComputeHashSHA1(aCombination: String): String;
    { private declarations }
  public
    { public declarations }
    procedure StopAllThreads;
    procedure ThreadTerminated(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses md5, sha1, math;

{$R *.lfm}

{ TMD4HashCracker }

function TForm1.ComputeHashMD4(aCombination: String): String;
begin
  Result:=LowerCase(MD4Print(MD4String(aCombination)));
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  hashlist:THashList;
  i: Integer;
  fatia, inicio, final: Int64;
begin
  hashlist:=THashList.Create;
  if (ComboBox1.ItemIndex=0) or (ComboBox1.ItemIndex=1) then
    hashlist.Add(@ComputeHashMD2);

  if (ComboBox1.ItemIndex=0) or (ComboBox1.ItemIndex=2) then
    hashlist.Add(@ComputeHashMD4);

  if (ComboBox1.ItemIndex=0) or (ComboBox1.ItemIndex=3) then
    hashlist.Add(@ComputeHashMD5);

  if (ComboBox1.ItemIndex=0) or (ComboBox1.ItemIndex=4) then
    hashlist.Add(@ComputeHashSHA1);

  SetLength(FHashCrackThread,TpSCADACoreAffinityThread.GetSystemThreadCount);
  fatia:=trunc(round(95/TpSCADACoreAffinityThread.GetSystemThreadCount));
  for i:=0 to {0 do begin //} TpSCADACoreAffinityThread.GetSystemThreadCount-1 do begin
    //if i<>1 then continue;
    inicio:=i*fatia+32;
    final :=(i+1)*fatia+31;
    FHashCrackThread[i]:=THashCrackerClass.Create(True, Edit1.Text, 20, inicio, final, hashlist);
    FHashCrackThread[i].OnTerminate:=@ThreadTerminated;
    FHashCrackThread[i].FreeOnTerminate:=true;
    FHashCrackThread[i].SetAffinity(i);
    FHashCrackThread[i].Start;
  end;
  Label5.Caption:=FormatDateTime('hh:nn:ss.zzz',Now);
  Label6.Caption:='';
  Label7.Caption:='';
  Button1.Enabled:=false;
  ComboBox1.Enabled:=false;
  Button2.Enabled:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  StopAllThreads;
end;

procedure TForm1.StopAllThreads;
var
  i: Integer;
begin
  for i:=0 to High(FHashCrackThread) do
    if Assigned(FHashCrackThread[i]) then FHashCrackThread[i].Terminate;

  Button1.Enabled:=true;
  ComboBox1.Enabled:=true;
  Button2.Enabled:=false;
end;

procedure TForm1.ThreadTerminated(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to High(FHashCrackThread) do
    if Assigned(FHashCrackThread[i]) then FHashCrackThread[i]:=nil;
end;

function TForm1.ComputeHashMD2(aCombination: String): String;
begin
  Result:=LowerCase(MD2Print(MD2String(aCombination)));
end;

function TForm1.ComputeHashMD5(aCombination: String): String;
begin
  Result:=LowerCase(MD5Print(MD5String(aCombination)));
end;

function TForm1.ComputeHashSHA1(aCombination: String): String;
begin
  Result:=LowerCase(SHA1Print(SHA1String(aCombination)));
end;

procedure THashCrackerClass.EncontrouSenha;
begin
  Form1.Label4.Caption:=FSenha;
  Form1.Label6.Caption:=FormatDateTime('hh:nn:ss.zzz',Now);
  Form1.Label7.Caption:=FormatDateTime('hh:nn:ss.zzz',FTimeHashing);
  Form1.StopAllThreads;
end;

procedure THashCrackerClass.Execute;
var
  aSenha: String;
  encontrou: Boolean;
begin
  encontrou:=false;
  FTimeHashing:=0;
  aSenha:=EncontrarSenha(encontrou);
  if encontrou then begin;
    FSenha:=aSenha;
    Synchronize(@EncontrouSenha);
  end;
end;

function THashCrackerClass.EncontrarSenha(var aEncontrouSenha: Boolean): String;
var
  level, il: Integer;
  iencontrou: Boolean;
  aSenha, senhainicial: String;

  function GeraSenha(SenhaAnterior:String; levelAtual, TamanhoMaximo:Integer; var encontrou:Boolean):String;
  var
    i, inicio, final: Integer;
  begin
    Result:='';
    encontrou:=false;

    if levelAtual<=TamanhoMaximo then begin
      if levelAtual=1 then begin
        inicio:=FFirstChar;
        final :=FLastChar;
      end else begin
        inicio:=32;
        final :=126;
      end;
      for i:=inicio to final do begin
        if Terminated then exit;

        if levelAtual=TamanhoMaximo then begin
          if CompareHash(SenhaAnterior+chr(i)) then begin
            Result:=SenhaAnterior+chr(i);
            encontrou:=true;
            exit;
          end;
        end;

        if levelAtual<TamanhoMaximo then
          Result:=GeraSenha(SenhaAnterior+chr(i),levelAtual+1,TamanhoMaximo, encontrou);
        if encontrou then exit;
      end;
    end;
  end;

begin
  level:=1;
  aEncontrouSenha:=false;
  iencontrou:=false;

  while (level<FCharactersLimit) or (FCharactersLimit<=0) do begin
    if Terminated then exit;

    aSenha:=GeraSenha('', 1, level, iencontrou);
    if iencontrou then begin
      Result:=aSenha;
      aEncontrouSenha:=true;
      exit;
    end else
      level+=1;
  end;
end;

function THashCrackerClass.CompareHash(aCombination: String): Boolean;
var
  i: Integer;
  HashStart: TDateTime;
begin
  HashStart:=Now;
  Result:=false;
  if Assigned(FHashList) then begin
    for i:=0 to FHashList.Count-1 do begin
      if FHashList.Items[i](aCombination)=FHashTarget then begin
        Result:=true;
        exit;
      end;
    end;
  end;
  FTimeHashing:=FTimeHashing+(Now-HashStart);
end;

constructor THashCrackerClass.Create(ASuspended: Boolean; HashTarget: String;
  aLimiteCaracteres, primeiroCaracter, UltimoCaracter: Integer;
  aHashList: THashList);
begin
  inherited Create(ASuspended);
  FHashList   := aHashList;
  FHashTarget := LowerCase(HashTarget);
  FFirstChar  := Max(32,  primeiroCaracter);
  FLastChar   := Min(126, UltimoCaracter);
  FCharactersLimit:=aLimiteCaracteres;
end;

end.

