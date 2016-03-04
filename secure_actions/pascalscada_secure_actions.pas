{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscada_secure_actions;

interface

uses
  pascalscada.secure_actions.authorized_by_user_management_action, 
  pascalscada.secure_actions.login_action, 
  pascalscada.secure_actions.login_logout_action, 
  pascalscada.secure_actions.logout_action, 
  pascalscada.secure_actions.manage_users_and_groups_action, 
  pascalscada.secure_actions.request_authorized_user, 
  pascalscada.secure_actions.secure_action, 
  pascalscada.secure_actions.tag_value_actions, 
  pascalscada.secure_actions.texts, pascalscada_secure_actions_register, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pascalscada_secure_actions_register', 
    @pascalscada_secure_actions_register.Register);
end;

initialization
  RegisterPackage('pascalscada_secure_actions', @Register);
end.
