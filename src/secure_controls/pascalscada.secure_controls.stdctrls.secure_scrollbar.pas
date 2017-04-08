unit pascalscada.secure_controls.stdctrls.secure_scrollbar;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls,
  Classes,
  pascalscada.security.control_security_manager;

type

  { TSecureCustomScrollBar }

  TSecureCustomScrollBar = class(TCustomScrollBar, ISecureControlInterface)
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

  TSecureScrollBar = class(TSecureCustomScrollBar)
  published
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property PageSize;
    property ParentBidiMode;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property SecurityCode;
    property ShowHint;
    property SmallChange;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

{ TSecureCustomScrollBar }

constructor TSecureCustomScrollBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomScrollBar.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomScrollBar.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomScrollBar.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomScrollBar.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomScrollBar.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomScrollBar.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

