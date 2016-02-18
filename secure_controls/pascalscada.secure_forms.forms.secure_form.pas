unit pascalscada.secure_forms.forms.secure_form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Controls,
  Classes,
  pascalscada.security.control_security_manager;

type

  { TSecureForm }

  TSecureForm = class(TForm, ISecureControlInterface)
  private
    FAllowUnauthorizedShowForm: Boolean;
    procedure SetAllowUnauthorizedShowForm(AValue: Boolean);
  protected
    FSecurityCode: String;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    //: @exclude
    procedure SetSecurityCode(AValue: String); virtual;

    //: @seealso(ISecureControlInterface.GetControlSecurityCode)
    function GetControlSecurityCode:String; virtual;

    //: @seealso(ISecureControlInterface.MakeUnsecure)
    procedure MakeUnsecure; virtual;

    //: @seealso(ISecureControlInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean); virtual;

    //: @exclude
    procedure SetEnabled(Value: Boolean); override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    //: @exclude
    property Enabled read FIsEnabled write SetEnabled default true;
    {$IFDEF PORTUGUES}
    //: Codigo de segurança do controle. Deixe vazio para desabilitar o sistema de segurança.
    {$ELSE}
    //: Control security code. Empty disable the security manager over the control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;

    property AllowUnauthorizedShowForm:Boolean read FAllowUnauthorizedShowForm write SetAllowUnauthorizedShowForm default true;
  end;


implementation

{ TSecureForm }

constructor TSecureForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAllowUnauthorizedShowForm:=true;
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureForm.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureForm.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureForm.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureForm.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureForm.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureForm.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure TSecureForm.SetAllowUnauthorizedShowForm(AValue: Boolean);
begin
  if FAllowUnauthorizedShowForm=AValue then Exit;
  FAllowUnauthorizedShowForm:=AValue;

  if Visible and
     (FAllowUnauthorizedShowForm=false) and
     (not GetPascalSCADAControlSecurityManager.CanAccess(FSecurityCode)) then
    Close;
end;

end.

