unit pascalscada.secure_controls.dbctrls.secure_dbnavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Controls,
  DbCtrls,
  Graphics,
  pascalscada.security.control_security_manager;

type

  { TSecureCustomDBNavigator }

  TSecureCustomDBNavigator = class(TDBCustomNavigator, ISecureControlInterface)
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

  TSecureDBNavigator = class(TSecureCustomDBNavigator)
  published
    property Align default alNone;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BeforeAction;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color default clBackground;
    property ConfirmDelete;
    property DataSource;
    property Direction;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Hints;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property Options;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SecurityCode;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property VisibleButtons;
    property Images;
  end;

implementation

{ TSecureCustomDBNavigator }

constructor TSecureCustomDBNavigator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomDBNavigator.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomDBNavigator.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomDBNavigator.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomDBNavigator.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomDBNavigator.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomDBNavigator.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

