unit pascalscada.secure_controls.stdctrls.secure_checkbox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LCLType,
  pascalscada.secure_controls.stdctrls.secure_custom_checkbox;

type

  { TSecureCustomCheckBox }

  TSecureCustomCheckBox = class(TSecureCustomCheckBoxBase)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TSecureCheckBox = class(TSecureCustomCheckBox)
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
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
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property PopupMenu;
    property SecurityCode;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

implementation

{ TSecureCustomCheckBox }

constructor TSecureCustomCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csCheckbox;
  TabStop := True;
  AutoSize := True;
  //FState := cbUnchecked;
  //FAllowGrayed := false;
end;

end.

