unit pascalscada.security.control_security_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  pascalscada.security.basic_user_management;

type

  {$IFDEF PORTUGUES}
  //: @name define a interface entre o gerente de segurança e os controles seguros.
  {$ELSE}
  //: @name defines a interface between the security manager and secure controls.
  {$ENDIF}
  ISecureControlInterface = interface
    ['{A950009B-A2E7-4ED8-BDB3-B6E191D184FB}']

    {$IFDEF PORTUGUES}
    //: Retorna o código de acesso do controle.
    {$ELSE}
    //: Gets the access code of the control.
    {$ENDIF}
    function GetControlSecurityCode:String;

    {$IFDEF PORTUGUES}
    //: Remove o codigo de segurança do controle, tornando-o inseguro.
    {$ELSE}
    //: Clear the security of the control, making it unsecure.
    {$ENDIF}
    procedure MakeUnsecure;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o controle pelas suas permissões. @seealso(Enabled)
    {$ELSE}
    //: Enables/disables the control. @seealso(Enabled)
    {$ENDIF}
    procedure CanBeAccessed(a:Boolean);
  end;

  TFPGSecureControlsList = specialize TFPGList<ISecureControlInterface>;

  { TpSCADAControlSecurityManager }

  TpSCADAControlSecurityManager = class(TComponent)
  private
    FSecureControls:TFPGSecureControlsList;
    FUserManagement:TpSCADABasicUserManagement;
    procedure SetUserManagement(um:TpSCADABasicUserManagement);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   Login(Userlogin, Userpassword: String; var UID: Integer):Boolean; overload;
    function   Login:Boolean;

    procedure  Logout;
    procedure  Manage;
    function   GetCurrentUserlogin:String;
    procedure  TryAccess(sc:String);
    procedure  RegisterControl(control:ISecureControlInterface);
    procedure  UnRegisterControl(control:ISecureControlInterface);
    procedure  UpdateControls;
    function   CanAccess(sc:String):Boolean;
    procedure  ValidateSecurityCode(sc:String);
    procedure  RegisterSecurityCode(sc:String);
    procedure  UnregisterSecurityCode(sc:String);
    function   SecurityCodeExists(sc:String):Boolean;
    function   GetRegisteredAccessCodes:TFPGStringList;
    function   CheckIfUserIsAllowed(sc:String; RequireUserLogin:Boolean; var userlogin:String):Boolean;
  published
    property UserManagement:TpSCADABasicUserManagement read FUserManagement write SetUserManagement;
  end;

  function GetPascalSCADAControlSecurityManager:TpSCADAControlSecurityManager;
  procedure SetControlSecurityCode(var CurrentSecurityCode:String; const NewSecurityCode:String; ControlSecurityIntf:ISecureControlInterface);

resourcestring
    SAccessDenied                         = 'Access denied!';
    SSecurityCodeIsInUseYet               = 'Security code still being used!';
    SInvalidUserManagementComponent       = 'Invalid user manager component';
    SUserManagementIsSet                  = 'User management component already set!';
    SControlSecurityManagerStillBeingUsed = 'Control security manager still being used. Maybe some control forget the unregister on their destructor?';

implementation

constructor TpSCADAControlSecurityManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserManagement:=nil;
  FSecureControls:=TFPGSecureControlsList.Create;
end;

destructor TpSCADAControlSecurityManager.Destroy;
begin
  if FSecureControls.Count>0 then
    raise Exception.Create(SControlSecurityManagerStillBeingUsed);
  FreeAndNil(FSecureControls);
  inherited Destroy;
end;

function TpSCADAControlSecurityManager.Login(Userlogin, Userpassword: String; var UID:Integer): Boolean; overload;
begin
  if FUserManagement<>nil then
    Result:=TpSCADABasicUserManagement(FUserManagement).Login(Userlogin,Userpassword,UID)
  else
    Result:=false;
end;

function   TpSCADAControlSecurityManager.Login:Boolean;
begin
  if FUserManagement<>nil then
    Result:=TpSCADABasicUserManagement(FUserManagement).Login
  else
    Result:=false;
end;

procedure  TpSCADAControlSecurityManager.Logout;
begin
  if FUserManagement<>nil then
    TpSCADABasicUserManagement(FUserManagement).Logout
end;

procedure  TpSCADAControlSecurityManager.Manage;
begin
  if FUserManagement<>nil then
    TpSCADABasicUserManagement(FUserManagement).Manage;
end;

function TpSCADAControlSecurityManager.GetCurrentUserlogin: String;
begin
  Result:='';
  if FUserManagement<>nil then
    Result:=TpSCADABasicUserManagement(FUserManagement).CurrentUserLogin;
end;

procedure  TpSCADAControlSecurityManager.TryAccess(sc:String);
begin
  if FUserManagement<>nil then
    if not TpSCADABasicUserManagement(FUserManagement).CanAccess(sc) then
      raise Exception.Create(SAccessDenied);
end;

procedure TpSCADAControlSecurityManager.SetUserManagement(um:TpSCADABasicUserManagement);
begin
  if (um<>nil) and (not (um is TpSCADABasicUserManagement)) then
    raise Exception.Create(SInvalidUserManagementComponent);

  if (um<>nil) and (FUserManagement<>nil) then
    raise Exception.Create(SUserManagementIsSet);

  FUserManagement:=um;
  UpdateControls;
end;

procedure  TpSCADAControlSecurityManager.RegisterControl(control:ISecureControlInterface);
begin
  if FSecureControls.IndexOf(control)=-1 then begin;
    FSecureControls.Add(control);
    control.CanBeAccessed(CanAccess(control.GetControlSecurityCode));
  end;
end;

procedure  TpSCADAControlSecurityManager.UnRegisterControl(control:ISecureControlInterface);
var
  idx:LongInt;
begin
  idx:=FSecureControls.IndexOf(control);
  if idx<>-1 then
    FSecureControls.Delete(idx);
end;

procedure  TpSCADAControlSecurityManager.UpdateControls;
var
  c:LongInt;
  intf: ISecureControlInterface;
begin
  for c:=0 to FSecureControls.Count-1 do begin
    intf:=ISecureControlInterface(FSecureControls.Items[c]);
    intf.CanBeAccessed(CanAccess(intf.GetControlSecurityCode));
  end;
end;

function   TpSCADAControlSecurityManager.CanAccess(sc:String):Boolean;
begin
  Result:=true;

  if sc='' then exit;

  if (FUserManagement<>nil) and (FUserManagement is TpSCADABasicUserManagement) then
    Result:=TpSCADABasicUserManagement(FUserManagement).CanAccess(sc);
end;

procedure  TpSCADAControlSecurityManager.ValidateSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TpSCADABasicUserManagement(FUserManagement).ValidateSecurityCode(sc);
end;

procedure  TpSCADAControlSecurityManager.RegisterSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TpSCADABasicUserManagement(FUserManagement).RegisterSecurityCode(sc);
end;

procedure  TpSCADAControlSecurityManager.UnregisterSecurityCode(sc:String);
var
  being_used:Boolean;
  c:LongInt;
begin
  being_used:=false;
  for c:=0 to FSecureControls.Count-1 do
    being_used:=being_used or (ISecureControlInterface(FSecureControls.Items[c]).GetControlSecurityCode=sc);

  if being_used then
    raise Exception.Create(SSecurityCodeIsInUseYet);

  if FUserManagement<>nil then
    TpSCADABasicUserManagement(FUserManagement).UnregisterSecurityCode(sc);
end;

function   TpSCADAControlSecurityManager.SecurityCodeExists(sc:String):Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TpSCADABasicUserManagement(FUserManagement).SecurityCodeExists(sc);
end;

function TpSCADAControlSecurityManager.GetRegisteredAccessCodes: TFPGStringList;
begin
  if FUserManagement=nil then begin
    Result:=TFPGStringList.Create
  end else
    Result:=TpSCADABasicUserManagement(FUserManagement).GetRegisteredAccessCodes;
end;

function TpSCADAControlSecurityManager.CheckIfUserIsAllowed(sc: String;
  RequireUserLogin: Boolean; var userlogin: String): Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TpSCADABasicUserManagement(FUserManagement).CheckIfUserIsAllowed(sc, RequireUserLogin, userlogin);
end;

var
  QPascalSCADAControlSecurityManager:TpSCADAControlSecurityManager;

function GetPascalSCADAControlSecurityManager: TpSCADAControlSecurityManager;
begin
  Result:=QPascalSCADAControlSecurityManager;
end;

procedure SetControlSecurityCode(var CurrentSecurityCode: String;
  const NewSecurityCode: String; ControlSecurityIntf: ISecureControlInterface);
begin
  if CurrentSecurityCode=NewSecurityCode then Exit;

  if Trim(NewSecurityCode)='' then
    ControlSecurityIntf.CanBeAccessed(true)
  else
    with GetPascalSCADAControlSecurityManager do begin
      ValidateSecurityCode(NewSecurityCode);
      if not SecurityCodeExists(NewSecurityCode) then
        RegisterSecurityCode(NewSecurityCode);

      ControlSecurityIntf.CanBeAccessed(CanAccess(NewSecurityCode));
    end;

  CurrentSecurityCode:=NewSecurityCode;
end;

initialization
  QPascalSCADAControlSecurityManager:=TpSCADAControlSecurityManager.Create(nil);
finalization
  FreeAndNil(QPascalSCADAControlSecurityManager);

end.

