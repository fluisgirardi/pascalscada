unit pascalscada.secure_controls.comctrls.secure_trackbar;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ComCtrls,
  pascalscada.security.control_security_manager;

type

  { TSecureCustomTrackbar }

  TSecureCustomTrackbar = class(TCustomTrackBar, ISecureControlInterface)
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

  { TSecureTrackBar }

  TSecureTrackBar = class(TSecureCustomTrackBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Orientation;
    property PageSize;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property Reversed;
    property ScalePos;
    property SecurityCode;
    property SelEnd;
    property SelStart;
    property ShowHint;
    property ShowSelRange;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property Visible;
  end;

implementation

{ TSecureCustomTrackbar }

constructor TSecureCustomTrackbar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomTrackbar.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomTrackbar.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomTrackbar.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomTrackbar.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomTrackbar.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomTrackbar.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

