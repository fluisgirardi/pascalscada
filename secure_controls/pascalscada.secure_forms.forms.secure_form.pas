unit pascalscada.secure_forms.forms.secure_form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  WSForms,
  Controls,
  LCLVersion,
  Classes,
  pascalscada.security.controls.control_security_manager;

type

  { TSecureCustomForm }

  TSecureCustomForm = class(TCustomForm, ISecureControlInterface)
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
  end;

  { TSecureForm }

  TSecureForm = class(TSecureCustomForm)
  private
    FLCLVersion: string;
    function LCLVersionIsStored: boolean;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;

    { mdi related routine}
    procedure Cascade;
    { mdi related routine}
    procedure Next;
    { mdi related routine}
    procedure Previous;
    { mdi related routine}
    procedure Tile;
    { mdi related property}
    property ClientHandle;

    property DockManager;
  published
    property Action;
    property ActiveControl;
    property Align;
    property AllowDropFiles;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FormStyle;
    property HelpFile;
    property Icon;
    property KeyPreview;
    property Menu;
    property OnActivate;
    property OnChangeBounds;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDeactivate;
    property OnDestroy;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHelp;
    property OnHide;
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
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnShowHint;
    property OnStartDock;
    property OnUnDock;
    property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property SecurityCode;
    property SessionProperties;
    property ShowHint;
    property ShowInTaskBar;
    property UseDockManager;
    property LCLVersion: string read FLCLVersion write FLCLVersion stored LCLVersionIsStored;
    property Visible;
    property WindowState;
  end;

implementation

{ TSecureForm }

function TSecureForm.LCLVersionIsStored: boolean;
begin
  Result:=Parent=nil;
end;

procedure TSecureForm.CreateWnd;
begin
  if (Application<>nil) then
    Application.UpdateMainForm(TForm(Self));
  inherited CreateWnd;
end;

procedure TSecureForm.Loaded;
begin
  inherited Loaded;
  FLCLVersion:=lcl_version;
end;

constructor TSecureForm.Create(TheOwner: TComponent);
begin
  FLCLVersion:=lcl_version;
  inherited Create(TheOwner);
end;

procedure TSecureForm.Cascade;
begin
  if (FormStyle <> fsMDIForm) then
    exit;
  if HandleAllocated and not (csDesigning in ComponentState) then
    TWSCustomFormClass(WidgetSetClass).Cascade(Self);
end;

procedure TSecureForm.Next;
begin
  if (FormStyle <> fsMDIForm) then
    exit;
  if HandleAllocated and not (csDesigning in ComponentState) then
    TWSCustomFormClass(WidgetSetClass).Next(Self);
end;

procedure TSecureForm.Previous;
begin
  if (FormStyle <> fsMDIForm) then
    exit;
  if HandleAllocated and not (csDesigning in ComponentState) then
    TWSCustomFormClass(WidgetSetClass).Previous(Self);
end;

procedure TSecureForm.Tile;
begin
  if (FormStyle <> fsMDIForm) then
    exit;
  if HandleAllocated and not (csDesigning in ComponentState) then
    TWSCustomFormClass(WidgetSetClass).Tile(Self);
end;

{ TSecureCustomForm }

constructor TSecureCustomForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsEnabled:=true;
  FIsEnabledBySecurity:=true;
  FSecurityCode:='';
  GetPascalSCADAControlSecurityManager.RegisterControl(Self as ISecureControlInterface);
end;

procedure TSecureCustomForm.SetSecurityCode(AValue: String);
begin
  SetControlSecurityCode(FSecurityCode,AValue,(Self as ISecureControlInterface));
end;

function TSecureCustomForm.GetControlSecurityCode: String;
begin
  Result:=FSecurityCode;
end;

procedure TSecureCustomForm.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TSecureCustomForm.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure TSecureCustomForm.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.

