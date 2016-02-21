unit pascalscada.secure_actions.logout_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.security.control_security_manager,
  pascalscada.security.basic_user_management,
  pascalscada.secure_actions.authorized_by_user_management_action;

type

  { TpSCADALogoutAction }

  TpSCADALogoutAction = class(TpSCADAAuthorizedByUserManagementAction)
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  end;

implementation

{ TpSCADALogoutAction }

procedure TpSCADALogoutAction.CanBeAccessed(a: Boolean);
begin
  if GetPascalSCADAControlSecurityManager.UserManagement<>nil then
    with GetPascalSCADAControlSecurityManager.UserManagement as TpSCADABasicUserManagement do
      inherited CanBeAccessed(not UserLogged);
end;

procedure TpSCADALogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TpSCADALogoutAction.ExecuteTarget(Target: TObject);
begin
  GetPascalSCADAControlSecurityManager.Logout;
end;

end.
