unit pascalscada.secure_controls.buttons.secure_bitbtn;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls,
  Classes,
  Buttons,
  pascalscada.security.control_security_manager;

type

  { TSecureCustomBitBtn }

  TSecureCustomBitBtn = class(TCustomBitBtn, ISecureControlInterface)
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

  TSecureBitBtn = class(TSecureCustomBitBtn)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property DefaultCaption;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphShowMode;
    property Kind;
    property Layout;
    property Margin;
    property ModalResult;
    property NumGlyphs;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
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
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SecurityCode;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

implementation

{ TSecureCustomBitBtn }

constructor TSecureCustomBitBtn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

destructor TSecureCustomBitBtn.Destroy;
begin
  GetPascalSCADAControlSecurityManager.UnRegisterControl(Self as ISecureControlInterface);
  inherited Destroy;
end;

procedure TSecureCustomBitBtn.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomBitBtn.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomBitBtn.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomBitBtn.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomBitBtn.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

