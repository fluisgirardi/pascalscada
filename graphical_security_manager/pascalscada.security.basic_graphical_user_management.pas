unit pascalscada.security.basic_graphical_user_management;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls,
  sysutils,
  pascalscada.security.basic_user_management,
  pascalscada.security.control_security_manager;

type

  TFocusedControl = (fcUserName, fcPassword);

  { TFrmLogin }

  TFrmLogin = Class(TCustomForm)
  private
    lblLogin,
    lblPassword:TLabel;
    edtLogin,
    edtPassword:TEdit;
    btnButtons:TButtonPanel;
    FFocusedControl: TFocusedControl;
    function GetUserLogin: String;
    function GetUserPassword: String;
    procedure SetUserLogin(AValue: String);
    procedure SetUserPassword(AValue: String);
  protected
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DisableEntry;
    procedure EnableEntry;
    property FocusedControl:TFocusedControl read FFocusedControl write FFocusedControl;
    property UserLogin:String read GetUserLogin write SetUserLogin;
    property UserPassword:String read GetUserPassword write SetUserPassword;
  end;


  { TpSCADABasicGraphicalUserManagement }

  TpSCADABasicGraphicalUserManagement = class(TpSCADABasicUserManagement)
  private
    procedure UnfreezeLogin(Sender: TObject);
  protected
    frmLogin:TFrmLogin;
  public
    function Login: Boolean; override; overload;
    function CheckIfUserIsAllowed(sc: String; RequireUserLogin: Boolean;
      var userlogin: String): Boolean; override;
  end;

ResourceString
  strUserLogin         = '&Login';
  strUserPass          = '&Password';
  SSpecialLoginCaption = 'Enter a user that can access the "%s" token.';

implementation

{ TFrmLogin }

function TFrmLogin.GetUserLogin: String;
begin
  Result:=edtLogin.Text;
end;

function TFrmLogin.GetUserPassword: String;
begin
  Result:=edtPassword.Text;
end;

procedure TFrmLogin.SetUserLogin(AValue: String);
begin
  edtLogin.Text:=AValue;
end;

procedure TFrmLogin.SetUserPassword(AValue: String);
begin
  edtPassword.Text:=AValue;
end;

procedure TFrmLogin.DoShow;
begin
  case FFocusedControl of
    fcUserName: if edtLogin.Enabled then edtLogin.SetFocus;
    fcPassword: if edtPassword.Enabled then edtPassword.SetFocus;
  end;
end;

constructor TFrmLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle:=bsDialog;
  SetBounds(0,0,300,150);
  Position:=poScreenCenter;
  FormStyle:=fsSystemStayOnTop;

  lblLogin:=TLabel.Create(Self);
  lblLogin.Caption:=strUserLogin;
  lblLogin.SetBounds(8,8,58,14);
  lblLogin.Parent:=Self;

  edtLogin:=TEdit.Create(Self);
  edtLogin.Text:='';
  edtLogin.Name:='PASCALSCADA_FRMLOGIN_edtLogin';
  edtLogin.SetBounds(8,24, 290, 86);
  edtLogin.Parent:=Self;

  lblLogin.FocusControl:=edtLogin;

  lblPassword:=TLabel.Create(Self);
  lblPassword.Caption:=strUserPass;
  lblPassword.SetBounds(8,56,54,14);
  lblPassword.Parent:=Self;

  edtPassword:=TEdit.Create(Self);
  edtPassword.Text:='';
  edtPassword.Name:='PASCALSCADA_FRMLOGIN_edtPassword';
  edtPassword.PasswordChar:='*';
  edtPassword.SetBounds(8,72, 290, 86);
  edtPassword.Parent:=Self;

  lblPassword.FocusControl:=edtPassword;

  btnButtons:=TButtonPanel.Create(Self);
  btnButtons.ShowButtons:=[pbOK, pbCancel];
end;

procedure TFrmLogin.EnableEntry;
begin
  edtPassword.Enabled:=true;
  edtLogin.Enabled:=true;
  btnButtons.Enabled:=true;
  DoShow;
end;

procedure TFrmLogin.DisableEntry;
begin
  edtPassword.Enabled:=false;
  edtLogin.Enabled:=false;
  btnButtons.Enabled:=false;
end;

{ TpSCADABasicGraphicalUserManagement }

procedure TpSCADABasicGraphicalUserManagement.UnfreezeLogin(Sender: TObject);
begin
  if sender is TTimer then begin
    TTimer(sender).Enabled:=false;
    if Assigned(frmLogin) then begin
      frmLogin.Close;
      frmLogin.EnableEntry;
    end;
  end;
end;

function TpSCADABasicGraphicalUserManagement.Login: Boolean;
var
  frozenTimer:TTimer;
  retries:LongInt;
  aborted, loggedin:Boolean;
  aUserID: Integer;
begin
  if Assigned(frmLogin) then begin
    Result:=false;
    frmLogin.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=1; //login
  frozenTimer.OnTimer:=@UnfreezeLogin;
  frmLogin:=TFrmLogin.Create(nil);
  retries:=0;
  aborted:=false;
  loggedin:=False;
  Result:=false;
  try
    frmLogin.UserLogin:='';
    frmLogin.FocusedControl:=fcUserName;
    while (not loggedin) and (not aborted) do begin
      frmLogin.edtPassword.Text:='';
      if frmLogin.ShowModal=mrOk then begin
        if CheckUserAndPassword(frmLogin.UserLogin, frmLogin.UserPassword, {%H-}aUserID, true) then begin
          FLoggedUser:=true;
          FUID:=aUserID;
          loggedin:=true;
          FCurrentUserLogin:=frmLogin.UserLogin;
          FLoggedSince:=Now;
          Result:=true;
          GetPascalSCADAControlSecurityManager.UpdateControls;
          DoSuccessfulLogin;
        end else begin
          DoFailureLogin;
          inc(retries);
          if (FLoginRetries>0) and (retries>=FLoginRetries) then begin
            frmLogin.DisableEntry;
            frozenTimer.Enabled:=true;
            frmLogin.ShowModal;
            retries:=0;
          end;
        end;
      end else
        aborted:=true;
      frmLogin.FocusedControl:=fcPassword;
    end;
  finally
    FreeAndNil(frmLogin);
    FreeAndNil(frozenTimer);
  end;
end;

function TpSCADABasicGraphicalUserManagement.CheckIfUserIsAllowed(sc: String;
  RequireUserLogin: Boolean; var userlogin: String): Boolean;
var
  frozenTimer:TTimer;
  aUserID:Integer;

begin
  Result:=false;

  //se o usuário logado tem permissão, evita
  //abrir o dialog que irá solicitar a permissão
  //de outro usuário.
  if UserLogged and CanAccess(sc) and (RequireUserLogin=false) then begin
    userlogin:=GetCurrentUserLogin;
    Result:=true;
    exit;
  end;

  //se existe um dialogo de permissão especial aberto
  //chama ele...
  if Assigned(frmLogin) then begin
    Result:=false;
    frmLogin.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=2; //Check
  frozenTimer.OnTimer:=@UnfreezeLogin;
  frmLogin:=TFrmLogin.Create(nil);

  frmLogin.Caption:=Format(SSpecialLoginCaption, [sc]);
  Result:=false;
  try
    frmLogin.FocusedControl:=fcUserName;
    frmLogin.UserLogin:='';
    frmLogin.UserPassword:='';
    if frmLogin.ShowModal=mrOk then begin
      if CheckUserAndPassword(frmLogin.UserLogin, frmLogin.UserPassword, {%H-}aUserID, false) then begin
        if CanAccess(sc,aUserID) then begin
          Result:=true;
          userlogin:=frmLogin.UserLogin;
        end else
          Result:=false;
      end;
    end;
  finally
    FreeAndNil(frmLogin);
    FreeAndNil(frozenTimer);
  end;
end;

end.

