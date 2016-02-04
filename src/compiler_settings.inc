{
****************************** History  *******************************
***********************************************************************
07/2013 - Define CONSOLEPASCALSCADA if fpc >= 2.7.1
@author(Juanjo Montero <juanjo.montero@gmail.com>)

12/2015 - Cut off Delphi Support
@author(Fabio Luis Girardi <fabio@pascalscada.com>)
***********************************************************************

PascalSCADA Project.
}

{$IFDEF UNIX}
  {$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 7) and (FPC_PATCH >= 1))}
    {$DEFINE CONSOLEPASCALSCADA}
  {$IFEND}
{$ENDIF}

