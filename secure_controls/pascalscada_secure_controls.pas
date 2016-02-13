{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_secure_controls;

interface

uses
  pascalscada.secure_controls.stdctrls.secure_label, 
  pascalscada.secure_controls.stdctrls.secure_edit, 
  pascalscada.secure_controls.stdctrls.secure_memo, 
  pascalscada.secure_controls.stdctrls.secure_custom_checkbox, 
  pascalscada.secure_controls.stdctrls.secure_togglebox, 
  pascalscada.secure_controls.stdctrls.secure_checkbox, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_secure_controls', @Register);
end.
