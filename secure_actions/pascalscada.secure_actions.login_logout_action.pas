unit pascalscada.secure_actions.login_logout_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascalscada.security.control_security_manager,
  pascalscada.secure_actions.authorized_by_user_management_action,
  pascalscada.security.basic_user_management;

type

  { TpSCADALogin_LogoutAction }

  TpSCADALogin_LogoutAction = class(TpSCADAAuthorizedByUserManagementAction)
  private
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FWithUserLoggedInImageIndex,
    FWithoutUserLoggedInImageIndex:LongInt;
    FWithUserLoggedInCaption,
    FWithoutUserLoggedInCaption,
    FWithUserLoggedInHint,
    FWithoutUserLoggedInHint:String;
    function GetCurrentCaption: String;
    function GetCurrentHintMessage: String;
    function GetCurrentImageIndex: LongInt;
    procedure SetWithUserLoggedInCaption(const AValue: String);
    procedure SetWithUserLoggedInHint(const AValue: String);
    procedure SetWithUserLoggedInImageIndex(const AValue: LongInt);
    procedure SetWithoutUserLoggedInCaption(const AValue: String);
    procedure SetWithoutUserLoggedInHint(const AValue: String);
    procedure SetWithoutUserLoggedInImageIndex(const AValue: LongInt);
    procedure UpdateMyState;
  protected
    procedure CanBeAccessed({%H-}a: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget({%H-}Target: TObject); override;
    procedure ExecuteTarget({%H-}Target: TObject); override;
  published
    property Caption:String read GetCurrentCaption;
    property Hint:String read GetCurrentHintMessage;
    property ImageIndex:LongInt read GetCurrentImageIndex;
    property WithUserLoggedInCaption:String        read FWithUserLoggedInCaption    write SetWithUserLoggedInCaption;
    property WithUserLoggedInHint:String           read FWithUserLoggedInHint       write SetWithUserLoggedInHint;
    property WithUserLoggedInImageIndex:LongInt    read FWithUserLoggedInImageIndex write SetWithUserLoggedInImageIndex;

    property WithoutUserLoggedInCaption:String     read FWithoutUserLoggedInCaption    write SetWithoutUserLoggedInCaption;
    property WithoutUserLoggedInHint:String        read FWithoutUserLoggedInHint       write SetWithoutUserLoggedInHint;
    property WithoutUserLoggedInImageIndex:LongInt read FWithoutUserLoggedInImageIndex write SetWithoutUserLoggedInImageIndex;
    property BeforeLogin:TNotifyEvent              read FBeforeLogin                   write FBeforeLogin;
    property AfterLogin:TNotifyEvent               read FAfterLogin                    write FAfterLogin;
  end;

implementation

{ TpSCADALogin_LogoutAction }

function TpSCADALogin_LogoutAction.GetCurrentCaption: String;
begin
  Result:=inherited Caption;
end;

function TpSCADALogin_LogoutAction.GetCurrentHintMessage: String;
begin
  Result:=inherited Hint;
end;

function TpSCADALogin_LogoutAction.GetCurrentImageIndex: LongInt;
begin
  Result:=inherited ImageIndex;
end;

procedure TpSCADALogin_LogoutAction.SetWithUserLoggedInCaption(
  const AValue: String);
begin
  if FWithUserLoggedInCaption=AValue then exit;
  FWithUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.SetWithUserLoggedInHint(const AValue: String
  );
begin
  if FWithUserLoggedInHint=AValue then exit;
  FWithUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.SetWithUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithUserLoggedInImageIndex=AValue then exit;
  FWithUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.SetWithoutUserLoggedInCaption(
  const AValue: String);
begin
  if FWithoutUserLoggedInCaption=AValue then exit;
  FWithoutUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.SetWithoutUserLoggedInHint(
  const AValue: String);
begin
  if FWithoutUserLoggedInHint=AValue then exit;
  FWithoutUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.SetWithoutUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithoutUserLoggedInImageIndex=AValue then exit;
  FWithoutUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TpSCADALogin_LogoutAction.UpdateMyState;
begin
  if GetPascalSCADAControlSecurityManager.UserManagement<>nil then
    if TpSCADABasicUserManagement(GetPascalSCADAControlSecurityManager.UserManagement).UserLogged then begin
      inherited Caption   :=FWithUserLoggedInCaption;
      inherited Hint      :=FWithUserLoggedInHint;
      inherited ImageIndex:=FWithUserLoggedInImageIndex;
    end else begin
      inherited Caption   :=FWithoutUserLoggedInCaption;
      inherited Hint      :=FWithoutUserLoggedInHint;
      inherited ImageIndex:=FWithoutUserLoggedInImageIndex;
    end;
end;

procedure TpSCADALogin_LogoutAction.CanBeAccessed(a: Boolean);
begin
  inherited CanBeAccessed(true); //it can be accessed always.
  UpdateMyState;
end;

constructor TpSCADALogin_LogoutAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWithUserLoggedInImageIndex:=-1;
  FWithoutUserLoggedInImageIndex:=-1;
end;

procedure TpSCADALogin_LogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TpSCADALogin_LogoutAction.ExecuteTarget(Target: TObject);
begin
  if GetPascalSCADAControlSecurityManager.UserManagement<>nil then
    if TpSCADABasicUserManagement(GetPascalSCADAControlSecurityManager.UserManagement).UserLogged then begin
      GetPascalSCADAControlSecurityManager.Logout;
    end else begin
      if Assigned(FBeforeLogin) then
        FBeforeLogin(Self);

      GetPascalSCADAControlSecurityManager.Login;

      if Assigned(FAfterLogin) then
        FAfterLogin(Self);
    end;
  UpdateMyState;
end;

end.

