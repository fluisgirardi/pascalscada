unit pascalscada_secure_objects_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FormEditingIntf,
  ProjectIntf,
  LazIDEIntf,
  pascalscada.secure_controls.extctrls.secure_checkgroup,
  pascalscada.secure_controls.extctrls.secure_panel,
  pascalscada.secure_controls.extctrls.secure_radiogroup,
  pascalscada.secure_controls.stdctrls.secure_checkbox,
  pascalscada.secure_controls.stdctrls.secure_combobox,
  pascalscada.secure_controls.stdctrls.secure_edit,
  pascalscada.secure_controls.stdctrls.secure_groupbox,
  pascalscada.secure_controls.stdctrls.secure_label,
  pascalscada.secure_controls.stdctrls.secure_listbox,
  pascalscada.secure_controls.stdctrls.secure_memo,
  pascalscada.secure_controls.stdctrls.secure_radiobutton,
  pascalscada.secure_controls.stdctrls.secure_scrollbar,
  pascalscada.secure_controls.stdctrls.secure_togglebox,
  pascalscada.secure_forms.forms.secure_form;

type

  { TPascalSCADASecureFormFileDescriptor }

  TPascalSCADASecureFormFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;


Resourcestring
  SPascalSCADA_Secure_Form = 'PascalSCADA Secure Form';
  SPascalSCADA_Secure_Form_Desc = 'Create a new unit with a LCL form, with security features.';
  SPascalSCADA_HCL_Std_Controls = 'PascalSCADA HCL - Std. Controls';

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SPascalSCADA_HCL_Std_Controls, [TSecureCheckBox,
                                                     TSecureComboBox,
                                                     TSecureEdit,
                                                     TSecureGroupBox,
                                                     TSecureLabel,
                                                     TSecureListBox,
                                                     TSecureMemo,
                                                     TSecureRadioButton,
                                                     TSecureScrollBar,
                                                     TSecureToggleBox,

                                                     TSecureCheckGroup,
                                                     TSecurePanel,
                                                     TSecureRadioGroup]);

    RegisterProjectFileDescriptor(TPascalSCADASecureFormFileDescriptor.Create);
    FormEditingHook.RegisterDesignerBaseClass(TSecureForm);


end;

{ TPascalSCADASecureFormFileDescriptor }

constructor TPascalSCADASecureFormFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TSecureForm;
  Name:=SPascalSCADA_Secure_Form;
  UseCreateFormStatements:=true;
  RequiredPackages:=RequiredPackages+'LCL;pascalscada_secure_controls';
end;

function TPascalSCADASecureFormFileDescriptor.GetLocalizedName: String;
begin
  Result:=SPascalSCADA_Secure_Form;
end;

function TPascalSCADASecureFormFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SPascalSCADA_Secure_Form_Desc;
end;

function TPascalSCADASecureFormFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', pascalscada.secure_forms.forms.secure_form';
end;

end.

