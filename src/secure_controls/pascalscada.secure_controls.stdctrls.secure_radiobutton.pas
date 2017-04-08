unit pascalscada.secure_controls.stdctrls.secure_radiobutton;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LCLType,
  WSStdCtrls,
  LMessages,
  Controls,
  StdCtrls,
  pascalscada.secure_controls.stdctrls.secure_custom_checkbox;

type

  { TSecureCustomRadioButton }

  TSecureCustomRadioButton = class(TSecureCustomCheckBoxBase)
  protected
    class procedure WSRegisterClass; override;
    procedure ApplyChanges; override;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure DoClickOnChange; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TSecureRadioButton = class(TSecureCustomRadioButton)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
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
  end;

implementation

uses Forms;

{ TSecureCustomRadioButton }

class procedure TSecureCustomRadioButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterRadioButton;
end;

procedure TSecureCustomRadioButton.ApplyChanges;
var
  Sibling: TControl;
  i: Integer;
begin
  inherited ApplyChanges;
  //the siblings are unchecked by the widgetset. When the handle is not allocated,
  //the widgetset is not notified so it cannot update the siblings
  if not HandleAllocated and (RetrieveState = cbChecked) and
   (Parent <> nil) and (not (csLoading in ComponentState)) then
  begin
    for i := 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[i];
      if (Sibling is TSecureRadioButton) and (Sibling <> Self) then
        TSecureRadioButton(Sibling).Checked := False;
    end;
  end;
end;

function TSecureCustomRadioButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    Result := True;
  end else
    Result := inherited;
end;

procedure TSecureCustomRadioButton.RealSetText(const Value: TCaption);
begin
  if Text = Value then
    Exit;
  inherited RealSetText(Value);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TSecureCustomRadioButton.DoClickOnChange;
begin
  TabStop := RetrieveState = cbChecked;
  //inherited calls DoOnChange
  if RetrieveState = cbChecked then
    inherited DoClickOnChange
  else
    DoOnChange;
end;

procedure TSecureCustomRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // BS_AUTORADIOBUTTON may hang the application,
  // if the radiobuttons are not consecutive controls.
  Params.Style := (Params.Style and not BS_3STATE) or BS_RADIOBUTTON;
end;

constructor TSecureCustomRadioButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csRadioButton;
  AutoSize := True;
end;

end.

