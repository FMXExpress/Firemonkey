unit FMX.MultiView.CustomPresentation;

interface

uses
  System.Messaging,  System.UITypes, System.Classes, FMX.MultiView, FMX.MultiView.Presentations, FMX.MultiView.Types,
  FMX.StdCtrls;

type

{ TMultiViewAlertPresentation }

  TMultiViewAlertPresentation = class(TMultiViewPresentation)
  private
    FDetailOverlay: TShadowedOverlayLayout;
    FFrame: TPanel;
    { Messaging }
    procedure DoFormReleased(const Sender: TObject; const M: TMessage);
  protected
    function GetDisplayName: string; override;
    procedure DoOpen(const ASpeed: Single); override;
    procedure DoClose(const ASpeed: Single); override;
    procedure DoInstall; override;
    procedure DoUninstall; override;
    { Mouse events }
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); virtual;
  public
    constructor Create(AMultiView: TCustomMultiView); override;
    destructor Destroy; override;
    procedure UpdateSettings; override;
    procedure Realign; override;
  end;

implementation

uses
  FMX.Types, FMX.Forms, FMX.Ani, System.Types;

{ TMultiViewAlertPresentation }

constructor TMultiViewAlertPresentation.Create(AMultiView: TCustomMultiView);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TFormReleasedMessage, DoFormReleased);

  // Detail overlay layer for catching mouse events
  FDetailOverlay := TShadowedOverlayLayout.Create(nil);
  FDetailOverlay.Stored := False;
  FDetailOverlay.Mode := TCustomOverlayLayout.TOverlayMode.AllLocalArea;
  FDetailOverlay.EnabledShadow := MultiView.ShadowOptions.Enabled;
  FDetailOverlay.Color := MultiView.ShadowOptions.Color;
  FDetailOverlay.Opacity := 0;
  FDetailOverlay.Align := TAlignLayout.Contents;
  FDetailOverlay.Lock;
  FDetailOverlay.Visible := False;
  FDetailOverlay.OnMouseDown := DoMouseDown;

  FFrame := TPanel.Create(nil);
  FFrame.Padding.Rect := TRectF.Create(1, 1, 1, 1);
end;

destructor TMultiViewAlertPresentation.Destroy;
begin
  inherited;
  TMessageManager.DefaultManager.Unsubscribe(TFormReleasedMessage, DoFormReleased);
  FDetailOverlay.Free;
end;

procedure TMultiViewAlertPresentation.DoClose(const ASpeed: Single);
begin
  inherited;
  FFrame.Parent := nil;
  FDetailOverlay.Visible := False;
  MultiView.MasterContent.Parent := MultiView;
end;

procedure TMultiViewAlertPresentation.DoFormReleased(const Sender: TObject; const M: TMessage);
begin
  if Sender = FDetailOverlay.Parent then
    FDetailOverlay.Parent := nil;
end;

procedure TMultiViewAlertPresentation.DoInstall;
begin
  inherited;
  MultiView.Visible := False;
  MultiView.Align := TAlignLayout.None;
  if MultiView.Scene <> nil then
    FDetailOverlay.Parent := (MultiView.Scene.GetObject as TCommonCustomForm);
  if MultiView.HasMasterButton then
    MultiView.MasterButton.Visible := True;
end;

procedure TMultiViewAlertPresentation.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  Close;
end;

procedure TMultiViewAlertPresentation.DoOpen(const ASpeed: Single);
var
  SceneForm: TCommonCustomForm;
begin
  inherited;
  // Install content into Alert Panel
  FFrame.Opacity := 0;
  FFrame.Width := MultiView.Width;
  FFrame.Height := MultiView.PopoverOptions.PopupHeight;
  if MultiView.Scene <> nil then
  begin
    SceneForm := MultiView.Scene.GetObject as TCommonCustomForm;
    FFrame.Parent := SceneForm;
    FFrame.Position.Point := TPointF.Create(SceneForm.Width / 2 - FFrame.Width / 2, SceneForm.Height / 2 - FFrame.Height / 2)
  end;
  MultiView.MasterContent.Parent := FFrame;
  FDetailOverlay.Visible := True;
  TAnimator.AnimateFloat(FDetailOverlay, 'opacity', MultiView.ShadowOptions.Opacity, MultiView.DrawerOptions.DurationSliding);
  TAnimator.AnimateFloat(FFrame, 'opacity', 1, MultiView.DrawerOptions.DurationSliding);
end;

procedure TMultiViewAlertPresentation.DoUninstall;
begin
  MultiView.Visible := True;
  FDetailOverlay.Parent := nil;
  inherited;
end;

function TMultiViewAlertPresentation.GetDisplayName: string;
begin
  Result := 'Alert window';
end;

procedure TMultiViewAlertPresentation.Realign;
var
  SceneForm: TCommonCustomForm;
begin
  inherited;
  if MultiView.Scene <> nil then
  begin
    SceneForm := MultiView.Scene.GetObject as TCommonCustomForm;
    FFrame.Position.Point := TPointF.Create(SceneForm.Width / 2 - FFrame.Width / 2, SceneForm.Height / 2 - FFrame.Height / 2)
  end;
end;

procedure TMultiViewAlertPresentation.UpdateSettings;
begin
  inherited;
  if not Opened then
    FDetailOverlay.Opacity := 0
  else
    FDetailOverlay.Opacity := MultiView.ShadowOptions.Opacity;
  FDetailOverlay.EnabledShadow := MultiView.ShadowOptions.Enabled;
  FDetailOverlay.Color := MultiView.ShadowOptions.Color;
end;

end.
