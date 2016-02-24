unit pascalscada.secure_actions.authorized_by_user_management_action;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, SysUtils, pascalscada.security.control_security_manager;

type

  { TpSCADAAuthorizedByUserManagementAction }

  TpSCADAAuthorizedByUserManagementAction = class(TCustomAction, ISecureControlInterface)
  private
    FDisableIfNotAuthorized: Boolean;
    procedure SetDisableIfNotAuthorized(AValue: Boolean);
  protected
    FEnabled,
    FAccessAllowed:Boolean;
    FSecurityCode:String;

    procedure SetEnabled(AValue: Boolean); virtual;

    function  GetControlSecurityCode:String; virtual;
    procedure MakeUnsecure; virtual;
    procedure CanBeAccessed(a:Boolean); virtual;
    property DisableIfNotAuthorized:Boolean read FDisableIfNotAuthorized write SetDisableIfNotAuthorized default false;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property DisableIfNoHandler default False;
    property Enabled:Boolean read FEnabled write SetEnabled default true;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property OnExecute;
    property OnHint;
    property OnUpdate;
    property SecondaryShortCuts;
    property ShortCut;
    property Visible;
  end;

implementation

{ TpSCADAAuthorizedByUserManagementAction }

constructor TpSCADAAuthorizedByUserManagementAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled:=true;
  FDisableIfNotAuthorized:=false;
  DisableIfNoHandler:=False;
  GetPascalSCADAControlSecurityManager.RegisterControl(Self);
end;

destructor TpSCADAAuthorizedByUserManagementAction.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self);
  inherited Destroy;
end;

procedure TpSCADAAuthorizedByUserManagementAction.SetDisableIfNotAuthorized(
  AValue: Boolean);
begin
  if FDisableIfNotAuthorized=AValue then Exit;
  FDisableIfNotAuthorized:=AValue;
  CanBeAccessed(GetPascalSCADAControlSecurityManager.CanAccess(FSecurityCode));
end;

procedure TpSCADAAuthorizedByUserManagementAction.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

function TpSCADAAuthorizedByUserManagementAction.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TpSCADAAuthorizedByUserManagementAction.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TpSCADAAuthorizedByUserManagementAction.CanBeAccessed(a: Boolean);
begin
  FAccessAllowed:=a or (FDisableIfNotAuthorized=false);
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

function TpSCADAAuthorizedByUserManagementAction.HandlesTarget(Target: TObject
  ): Boolean;
begin
  inherited HandlesTarget(Target);
  Result:=true;
end;

end.

