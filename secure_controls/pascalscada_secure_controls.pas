{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_secure_controls;

{$warn 5023 off : no warning about unused units}
interface

uses
  pascalscada.secure_controls.buttons.secure_bitbtn, 
  pascalscada.secure_controls.buttons.secure_speedbutton, 
  pascalscada.secure_controls.checklst.secure_checklistbox, 
  pascalscada.secure_controls.comctrls.secure_progressbar, 
  pascalscada.secure_controls.comctrls.secure_trackbar, 
  pascalscada.secure_controls.comctrls.secure_updown, 
  pascalscada.secure_controls.dbctrls.secure_dbedit, 
  pascalscada.secure_controls.dbctrls.secure_dbmemo, 
  pascalscada.secure_controls.dbctrls.secure_dbnavigator, 
  pascalscada.secure_controls.dbctrls.secure_dbtext, 
  pascalscada.secure_controls.extctrls.secure_checkgroup, 
  pascalscada.secure_controls.extctrls.secure_image, 
  pascalscada.secure_controls.extctrls.secure_panel, 
  pascalscada.secure_controls.extctrls.secure_radiogroup, 
  pascalscada.secure_controls.forms.secure_scrollbox, 
  pascalscada.secure_controls.maskedit.secure_maskedit, 
  pascalscada.secure_controls.stdctrls.secure_button, 
  pascalscada.secure_controls.stdctrls.secure_checkbox, 
  pascalscada.secure_controls.stdctrls.secure_combobox, 
  pascalscada.secure_controls.stdctrls.secure_custom_checkbox, 
  pascalscada.secure_controls.stdctrls.secure_edit, 
  pascalscada.secure_controls.stdctrls.secure_groupbox, 
  pascalscada.secure_controls.stdctrls.secure_label, 
  pascalscada.secure_controls.stdctrls.secure_listbox, 
  pascalscada.secure_controls.stdctrls.secure_memo, 
  pascalscada.secure_controls.stdctrls.secure_radiobutton, 
  pascalscada.secure_controls.stdctrls.secure_scrollbar, 
  pascalscada.secure_controls.stdctrls.secure_statictext, 
  pascalscada.secure_controls.stdctrls.secure_togglebox, 
  pascalscada.secure_forms.forms.secure_form, 
  pascalscada.secure_frame.frame.secure_frame, pascalscada_secure_objects_reg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalscada_secure_objects_reg', 
    @pascalscada_secure_objects_reg.Register);
end;

initialization
  RegisterPackage('pascalscada_secure_controls', @Register);
end.
