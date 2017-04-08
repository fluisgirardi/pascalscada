unit pascalscada.secure_frame.frame.secure_frame;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Controls,
  Classes,
  pascalscada.security.control_security_manager;

type

  TpSCADASecureFrame = class(TFrame, ISecureControlInterface)
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

    //: @exclude
    procedure Loaded; override;
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
  end;


implementation

{ TpSCADASecureFrame }

constructor TpSCADASecureFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  if ComponentState*[csDesigning]<>[] then begin
    FIsEnabledBySecurity:=true;
    FSecurityCode:='';
  end;

  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TpSCADASecureFrame.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TpSCADASecureFrame.SetSecurityCode(AValue: String);
begin
  if [csReading,csLoading]*ComponentState=[] then
    SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface))
  else
    FSecurityCode:=AValue;
end;

function TpSCADASecureFrame.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TpSCADASecureFrame.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TpSCADASecureFrame.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TpSCADASecureFrame.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure TpSCADASecureFrame.Loaded;
var
  AValue: String;
begin
  inherited Loaded;
  AValue:=FSecurityCode;
  FSecurityCode:='';
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

end.

