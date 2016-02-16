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
  pascalscada.secure_controls.stdctrls.secure_checkbox, 
  pascalscada.secure_controls.stdctrls.secure_radiobutton, 
  pascalscada.secure_controls.stdctrls.secure_listbox, 
  pascalscada.secure_controls.stdctrls.secure_combobox, 
  pascalscada.secure_controls.stdctrls.secure_scrollbar, 
  pascalscada.secure_controls.stdctrls.secure_groupbox, 
  pascalscada.secure_controls.extctrls.secure_radiogroup, 
  pascalscada.secure_controls.extctrls.secure_checkgroup, 
  pascalscada.secure_controls.extctrls.secure_panel, 
  pascalscada.secure_forms.forms.secure_form, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascalscada_secure_controls', @Register);
end.
