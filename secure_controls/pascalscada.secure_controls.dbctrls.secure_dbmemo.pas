unit pascalscada.secure_controls.dbctrls.secure_dbmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  DbCtrls,
  pascalscada.security.control_security_manager;

type

  { TSecureDBMemo }

  TSecureDBMemo = class(TDBMemo, ISecureControlInterface)
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

  published
    //: @exclude
    property Enabled read FIsEnabled write SetEnabled default true;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança do controle. Deixe vazio para desabilitar o sistema de segurança.
    {$ELSE}
    //: Control security code. Empty disable the security manager over the control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TSecureDBMemo }

constructor TSecureDBMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureDBMemo.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureDBMemo.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureDBMemo.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureDBMemo.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureDBMemo.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureDBMemo.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

