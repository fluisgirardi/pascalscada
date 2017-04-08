unit pascalscada_secure_actions_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList,
  pascalscada.secure_actions.request_authorized_user,
  pascalscada.secure_actions.login_action,
  pascalscada.secure_actions.logout_action,
  pascalscada.secure_actions.login_logout_action,
  pascalscada.secure_actions.manage_users_and_groups_action,
  pascalscada.secure_actions.secure_action;

procedure Register;

resourcestring
  strUserManagement   = 'PascalSCADA User Management';

implementation

procedure Register;
begin
  //////////////////////////////////////////////////////////////////////////////
  //Actions
  //////////////////////////////////////////////////////////////////////////////
  RegisterActions(strUserManagement,[TpSCADARequestAuthorizedUserAction,
                                     TpSCADALoginAction,
                                     TpSCADALogoutAction,
                                     TpSCADALogin_LogoutAction,
                                     TpSCADAManageUsersAndGroupsAction,
                                     TpSCADASecureAction],nil);
end;

end.

