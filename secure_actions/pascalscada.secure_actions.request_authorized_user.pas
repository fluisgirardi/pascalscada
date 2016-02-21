unit pascalscada.secure_actions.request_authorized_user;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, SysUtils, pascalscada.security.control_security_manager;

type

  { TpSCADARequestAuthorizedUserAction }

  TpSCADARequestAuthorizedUserAction = class(TAction)
  private
    FAuthorizedBy: String;
    FRequireLoginAlways: Boolean;
    FSecurityCode: String;
    procedure SetSecurityCode(AValue: String);
  public
    function Execute: Boolean; override;
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property AuthorizedBy:String read FAuthorizedBy;
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
    property RequireLoginAlways:Boolean read FRequireLoginAlways write FRequireLoginAlways;
  end;

implementation

{ TpSCADARequestAuthorizedUserAction }

procedure TpSCADARequestAuthorizedUserAction.SetSecurityCode(AValue: String);
begin
  if FSecurityCode=AValue then Exit;

  if Trim(AValue)<>'' then
    with GetPascalSCADAControlSecurityManager do begin
      ValidateSecurityCode(AValue);
      if not SecurityCodeExists(AValue) then
        RegisterSecurityCode(AValue);
    end;

  FSecurityCode:=AValue;
end;

function TpSCADARequestAuthorizedUserAction.Execute: Boolean;
begin
  if GetPascalSCADAControlSecurityManager.CheckIfUserIsAllowed(FSecurityCode, FRequireLoginAlways, FAuthorizedBy) then
    Result:=inherited Execute
  else
    Result:=false;
end;

function TpSCADARequestAuthorizedUserAction.HandlesTarget(Target: TObject): Boolean;
begin
  inherited HandlesTarget(Target);
  Result:=true;
end;

end.

