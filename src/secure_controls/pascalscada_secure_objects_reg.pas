unit pascalscada_secure_objects_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FormEditingIntf,
  ProjectIntf,
  LazIDEIntf,
  pascalscada.secure_controls.checklst.secure_checklistbox,
  pascalscada.secure_controls.comctrls.secure_trackbar,
  pascalscada.secure_controls.comctrls.secure_updown,
  pascalscada.secure_controls.comctrls.secure_progressbar,
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
  pascalscada.secure_controls.stdctrls.secure_edit,
  pascalscada.secure_controls.stdctrls.secure_groupbox,
  pascalscada.secure_controls.stdctrls.secure_label,
  pascalscada.secure_controls.stdctrls.secure_listbox,
  pascalscada.secure_controls.stdctrls.secure_memo,
  pascalscada.secure_controls.stdctrls.secure_radiobutton,
  pascalscada.secure_controls.stdctrls.secure_scrollbar,
  pascalscada.secure_controls.stdctrls.secure_togglebox,
  pascalscada.secure_forms.forms.secure_form,
  pascalscada.secure_frame.frame.secure_frame;

type

  { TPascalSCADASecureFormFileDescriptor }

  TpSCADASecureFormFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;

  { TpSCADASecureFrameFileDescriptor }

  TpSCADASecureFrameFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;


Resourcestring
  SPascalSCADA_Secure_Form = 'PascalSCADA Secure Form';
  SPascalSCADA_Secure_Form_Desc = 'Create a new unit with a LCL form, with security features.';
  SPascalSCADA_Secure_Frame = 'PascalSCADA Secure Frame';
  SPascalSCADA_Secure_Frame_Desc = 'Create a new unit with a frame, with security features.';
  SPascalSCADA_HCL_Std_Controls = 'PascalSCADA HCL - Std. Controls';
  SPascalSCADA_HCL_DB_Controls = 'PascalSCADA HCL - DB Controls';

procedure Register;

implementation

uses LResources;

procedure Register;
begin
  RegisterComponents(SPascalSCADA_HCL_Std_Controls, [TSecureButton,
                                                     TSecureCheckBox,
                                                     TSecureCheckGroup,
                                                     TSecureCheckListBox,
                                                     TSecureComboBox,
                                                     TSecureEdit,
                                                     TSecureGroupBox,
                                                     TSecureImage,
                                                     TSecureLabel,
                                                     TSecureListBox,
                                                     TSecureMaskEdit,
                                                     TSecureMemo,
                                                     TSecurePanel,
                                                     TSecureProgressBar,
                                                     TSecureRadioButton,
                                                     TSecureRadioGroup,
                                                     TSecureScrollBar,
                                                     TSecureScrollbox,
                                                     TSecureToggleBox,
                                                     TSecureTrackBar,
                                                     TSecureUpDown]);

  RegisterComponents(SPascalSCADA_HCL_DB_Controls, [TSecureDBEdit,
                                                    TSecureDBMemo,
                                                    TSecureDBNavigator,
                                                    TSecureDBText]);



  RegisterProjectFileDescriptor(TpSCADASecureFormFileDescriptor.Create);
  RegisterProjectFileDescriptor(TpSCADASecureFrameFileDescriptor.Create);
  FormEditingHook.RegisterDesignerBaseClass(TpSCADASecureForm);
  FormEditingHook.RegisterDesignerBaseClass(TpSCADASecureFrame);


end;

{ TpSCADASecureFrameFileDescriptor }

constructor TpSCADASecureFrameFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TpSCADASecureFrame;
  Name:=SPascalSCADA_Secure_Frame;
  UseCreateFormStatements:=true;
  RequiredPackages:=RequiredPackages+'LCL;pascalscada_secure_controls';
end;

function TpSCADASecureFrameFileDescriptor.GetLocalizedName: String;
begin
  Result:=SPascalSCADA_Secure_Frame;
end;

function TpSCADASecureFrameFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SPascalSCADA_Secure_Frame_Desc;
end;

function TpSCADASecureFrameFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', pascalscada.secure_frame.frame.secure_frame';
end;

{ TPascalSCADASecureFormFileDescriptor }

constructor TpSCADASecureFormFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:=TpSCADASecureForm;
  Name:=SPascalSCADA_Secure_Form;
  UseCreateFormStatements:=true;
  RequiredPackages:=RequiredPackages+'LCL;pascalscada_secure_controls';
end;

function TpSCADASecureFormFileDescriptor.GetLocalizedName: String;
begin
  Result:=SPascalSCADA_Secure_Form;
end;

function TpSCADASecureFormFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=SPascalSCADA_Secure_Form_Desc;
end;

function TpSCADASecureFormFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection+', pascalscada.secure_forms.forms.secure_form';
end;

{$IFDEF FPC}
initialization
  {$I secure_controls.lrs}
{$ENDIF}

end.

