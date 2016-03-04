unit pascalscada.secure_actions.secure_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.security.control_security_manager,
  pascalscada.secure_actions.authorized_by_user_management_action;

type

  { TpSCADASecureAction }

  TpSCADASecureAction = class(TpSCADAAuthorizedByUserManagementAction)
  protected
    procedure SetSecurityCode(sc:String);
  public
    procedure UpdateTarget(Target: TObject); override;
    function Execute: Boolean; override;
  published
    property DisableIfNotAuthorized;
    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses pascalscada.secure_actions.texts;

{ TpSCADASecureAction }

procedure TpSCADASecureAction.SetSecurityCode(sc: String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetPascalSCADAControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

procedure TpSCADASecureAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(FAccessAllowed);
  inherited UpdateTarget(Target);
end;

function TpSCADASecureAction.Execute: Boolean;
begin
  if GetPascalSCADAControlSecurityManager.CanAccess(FSecurityCode) then
    Result:=inherited Execute
  else begin
    Result:=false;
    raise exception.Create(SAccessDenied);
  end;
end;

end.

