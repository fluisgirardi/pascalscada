unit pascalscada.secure_controls.stdctrls.secure_togglebox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LCLType,
  WSStdCtrls,
  pascalscada.secure_controls.stdctrls.secure_custom_checkbox;

type

  { TSecureCustomToggleBox }

  TSecureCustomToggleBox = class(TSecureCustomCheckBoxBase)
  protected
    class procedure WSRegisterClass; override;
    procedure CreateParams(var Params: TCreateParams); override;
    property ParentColor default false;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TSecureToggleBox }

  TSecureToggleBox = class(TSecureCustomToggleBox)
  published
    property AllowGrayed;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnClick;
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
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SecurityCode;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

implementation

{ TSecureCustomToggleBox }

class procedure TSecureCustomToggleBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterToggleBox;
end;

procedure TSecureCustomToggleBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not BS_3STATE) or BS_AUTOCHECKBOX or BS_PUSHLIKE;
end;

constructor TSecureCustomToggleBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csToggleBox;
  TabStop := True;
  ParentColor := False;
end;

end.

