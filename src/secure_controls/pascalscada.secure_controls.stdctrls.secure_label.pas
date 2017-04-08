unit pascalscada.secure_controls.stdctrls.secure_label;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls,
  Classes,
  pascalscada.security.control_security_manager;

type

  { TSecureCustomLabel }

  TSecureCustomLabel = class(TCustomLabel, ISecureControlInterface)
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

  TSecureLabel = class(TSecureCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SecurityCode;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

implementation

{ TSecureCustomLabel }

constructor TSecureCustomLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomLabel.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomLabel.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomLabel.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomLabel.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomLabel.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomLabel.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.
