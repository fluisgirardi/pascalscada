unit pascalscada.secure_actions.manage_users_and_groups_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.security.control_security_manager,
  pascalscada.security.basic_user_management,
  pascalscada.secure_actions.authorized_by_user_management_action;

type

  { TpSCADAManageUsersAndGroupsAction }

  TpSCADAManageUsersAndGroupsAction = class(TpSCADAAuthorizedByUserManagementAction)
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  end;

implementation

{ TpSCADAManageUsersAndGroupsAction }

procedure TpSCADAManageUsersAndGroupsAction.CanBeAccessed(a: Boolean);
begin
  if GetPascalSCADAControlSecurityManager.UserManagement<>nil then
    with GetPascalSCADAControlSecurityManager.UserManagement as TpSCADABasicUserManagement do
      inherited CanBeAccessed(UserLogged);
end;

procedure TpSCADAManageUsersAndGroupsAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TpSCADAManageUsersAndGroupsAction.ExecuteTarget(Target: TObject);
begin
  GetPascalSCADAControlSecurityManager.Manage;
end;

end.
