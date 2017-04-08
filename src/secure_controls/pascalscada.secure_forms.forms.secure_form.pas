unit pascalscada.secure_forms.forms.secure_form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Controls,
  Classes,
  pascalscada.security.control_security_manager,
  pascalscada.security.security_exceptions;

type

  { TpSCADASecureForm }

  TpSCADASecureForm = class(TForm, ISecureControlInterface)
  private
    FAllowUnauthorizedShowForm,
    FAllowUnauthorizedShowFormLoaded: Boolean;
    procedure SetAllowUnauthorizedShowForm(AValue: Boolean);
  protected
    FSecurityCode: String;
    FIsEnabled,
    FIsVisible,
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

    //: @exclude
    procedure Loaded; override;

    //:@excluee
    procedure SetVisible(Value: boolean); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor  Destroy; override;
    function ShowModal: Integer; override;
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

    property Visible stored false;
  end;


implementation

{ TSecureForm }

constructor TpSCADASecureForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FIsEnabled:=true;
  FAllowUnauthorizedShowForm:=true;
  if ComponentState*[csDesigning]<>[] then begin
    FIsEnabledBySecurity:=true;
    FSecurityCode:='';
  end;

  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TpSCADASecureForm.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

function TpSCADASecureForm.ShowModal: Integer;
begin
  if GetPascalSCADAControlSecurityManager.CanAccess(FSecurityCode)=false then
    raise ESecuritySystemAccessDenied.Create(FSecurityCode);

  Result:=inherited ShowModal;
end;

procedure TpSCADASecureForm.SetSecurityCode(AValue: String);
begin
  if [csReading,csLoading]*ComponentState=[] then
    SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface))
  else
    FSecurityCode:=AValue;
end;

function TpSCADASecureForm.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TpSCADASecureForm.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TpSCADASecureForm.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
  if FAllowUnauthorizedShowForm=false then;
    SetVisible(FIsVisible);
  if (FAllowUnauthorizedShowForm=false) and (a=false) and Visible then
    Close;
end;

procedure TpSCADASecureForm.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure TpSCADASecureForm.Loaded;
var
  AValue: String;
begin
  inherited Loaded;
  SetAllowUnauthorizedShowForm(FAllowUnauthorizedShowFormLoaded);
  AValue:=FSecurityCode;
  FSecurityCode:='';
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

procedure TpSCADASecureForm.SetVisible(Value: boolean);
begin
  FIsVisible:=Value;
  inherited SetVisible(FIsVisible and (FIsEnabledBySecurity or FAllowUnauthorizedShowForm));
end;

procedure TpSCADASecureForm.SetAllowUnauthorizedShowForm(AValue: Boolean);
begin
  if [csReading,csLoading]*ComponentState<>[] then begin
    FAllowUnauthorizedShowFormLoaded:=AValue;
    exit;
  end;

  if FAllowUnauthorizedShowForm=AValue then Exit;
  FAllowUnauthorizedShowForm:=AValue;

  if Visible and
     (FAllowUnauthorizedShowForm=false) and
     (not GetPascalSCADAControlSecurityManager.CanAccess(FSecurityCode)) then
    Close;
end;

end.

