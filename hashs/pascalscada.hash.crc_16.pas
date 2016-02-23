{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit das funções CRC-16)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implement CRC-16 functions)
}
{$ENDIF}
unit pascalscada.hash.crc_16;

interface

uses sysutils, ctypes;

{$IFDEF PORTUGUES}
{:
Testa um pacote de PByte até o tamanho de Pkg - 2 e retorna @true caso o
calculo retorne o mesmo resultado com o que deve estar armazenado nos últimos
dois bytes do buffer.

Caso o pacote tenha 10 bytes, ele irá calcular com os oito primeiros bytes e vai
verificar se o calculo resultante é igual aos bytes 9 e 10 de Pkg. Byte 9 é o
mais significativo.

@param(Pkg PByte. Buffer que contem os dados que irá ser verificado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@param(pkgSize cuint32. Tamanho do buffer em bytes.)

@returns(@true caso o calculo do CRC16 do pacote feche com o CRC16 que
         está armazenado nos últimos dois bytes.)
}
{$ELSE}
{:
Verifies an package of PByte ultil their length - 2 and returns @true
if the value calculated is the same on that is stored in the last 2 bytes
of the package.

If the Pkg has 10 bytes of length, it will calculate the CRC using the
first 8 bytes and it will check the result with bytes 9 and 10 of Pkg.
The byte number 9 is the most significative.

@param(Pkg PByte. Pointer to buffer that stores the data that will be checked
       the CRC-16. Must have at least 2 bytes of length.)

@param(pkgSize cuint32. Size in bytes of buffer.)

@returns(@true if the CRC-16 calculated is the same that is stored on the last 2
         bytes of Pkg.)
}
{$ENDIF}
function Test_crc(const Pkg:PByte; pkgSize:cuint32):Boolean; overload;

{$IFDEF PORTUGUES}
{:
Testa um pacote de TBytes até o tamanho de Pkg - 2 e retorna @true caso o
calculo retorne o mesmo resultado com o que deve estar armazenado nos últimos
dois bytes do buffer.

Caso o pacote tenha 10 bytes, ele irá calcular com os oito primeiros bytes e vai
verificar se o calculo resultante é igual aos bytes 9 e 10 de Pkg. Byte 9 é o
mais significativo.

@param(Pkg TBytes. Buffer que contem os dados que irá ser verificado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@returns(@true caso o calculo do CRC16 do pacote feche com o CRC16 que
         está armazenado nos últimos dois bytes.)
}
{$ELSE}
{:
Verifies an package of TBytes ultil their length - 2 and returns @true
if the value calculated is the same on that is stored in the last 2 bytes
of the package.

If the Pkg has 10 bytes of length, it will calculate the CRC using the
first 8 bytes and it will check the result with bytes 9 and 10 of Pkg.
The byte number 9 is the most significative.

@param(Pkg TBytes. Pointer to buffer that stores the data that will be checked
       the CRC-16. Must have at least 2 bytes of length.)

@returns(@true if the CRC-16 calculated is the same that is stored on the last 2
         bytes of Pkg.)
}
{$ENDIF}
function Test_crc(const Pkg:TBytes):Boolean;

{$IFDEF PORTUGUES}
{:
Calcula o CRC-16 do pacote até seu tamanho - 2. Caso um pacote tenha 10 bytes,
ele irá pegar para o calculo os 8 primeiros e irá armazenar nos bytes 9 e 10 o
calculo resultante. Byte 9 é o mais significativo.

@param(Pkg PByte. Aponta para o buffer que contem os dados que irá ser calculado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@param(pkgSize cuint32. Tamanho do buffer em bytes.)

@returns(Uma cuint16 com o valor CRC16 do calculo feito com o tamanho de pkg - 2)
}
{$ELSE}
{:
Calculate the CRC-16 of the package until their size - 2. If the package has
10 bytes of length, it will use to calculate the first 8 bytes and will store
the result of the calculation on bytes 9 and 10. The byte number 9 is the most
significative.

@param(Pkg PByte. Pointer to buffer that contains the data that will be used to calculate
       the CRC-16 and that will store the result on the last 2 bytes. Must have
       at least 2 bytes of length.)

@param(pkgSize cuint32. Size in bytes of buffer.)

@returns(A cuint16 number with the CRC-16 calculated with length of Pkg - 2.)
}
{$ENDIF}
function Calculate_crc(Pkg:PByte; pkgSize:cuint32):cuint16; overload;

{$IFDEF PORTUGUES}
{:
Calcula o CRC-16 do pacote até seu tamanho - 2. Caso um pacote tenha 10 bytes,
ele irá pegar para o calculo os 8 primeiros e irá armazenar nos bytes 9 e 10 o
calculo resultante. Byte 9 é o mais significativo.

@param(Pkg TBytes. Aponta para o buffer que contem os dados que irá ser calculado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@returns(Uma cuint16 com o valor CRC16 do calculo feito com o tamanho de pkg - 2)
}
{$ELSE}
{:
Calculate the CRC-16 of the package until their size - 2. If the package has
10 bytes of length, it will use to calculate the first 8 bytes and will store
the result of the calculation on bytes 9 and 10. The byte number 9 is the most
significative.

@param(Pkg TBytes. Pointer to buffer that contains the data that will be used to calculate
       the CRC-16 and that will store the result on the last 2 bytes. Must have
       at least 2 bytes of length.)

@returns(A cuint16 number with the CRC-16 calculated with length of Pkg - 2.)
}
{$ENDIF}
function Calculate_crc(var Pkg:TBytes):cuint16;

implementation

function Test_crc(const Pkg: PByte; pkgSize: cuint32): Boolean;
var
  crc,j,carry_flag,a:cuint32;
  i,n:cint32;
begin
  n := pkgSize-2;
  crc := $FFFF;
  i := 0;
  while (i<n) do begin
    crc := crc xor Cardinal(pkg[i]);
    for j:=0 to 7 do begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag=1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;

  Result := ((n+2)<=pkgSize) and ((Cardinal(pkg[n+1])=(crc shr 8)) or (Cardinal(pkg[n])=(crc and 255)));

end;

function Test_crc(const Pkg: TBytes): Boolean;
begin
  Result:=Test_crc(@Pkg[0],Length(Pkg));
end;

function Calculate_crc(Pkg: PByte; pkgSize: cuint32): cuint16;
var
  crc,j,carry_flag,a:cuint32;
  i,n:cint32;
begin
  n:=pkgSize-2;
  crc := $FFFF;
  i := 0;
  while (i<n) do begin
    crc :=crc xor Cardinal(pkg[i]);
    for j:=0 to 7 do begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag=1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;
  pkg[n+1] := ((crc and $FF00) shr 8);
  pkg[n]   := (crc and 255);
  result := crc;
end;

function Calculate_crc(var Pkg: TBytes): cuint16;
begin
  Result:=Calculate_crc(@pkg[0],Length(pkg));
end;

end.

