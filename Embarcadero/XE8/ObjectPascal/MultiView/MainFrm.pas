unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.MultiView, FMX.Objects, FMX.TabControl,
  FMX.StdCtrls, FMX.Layouts, FMX.ListView.Types, FMX.ListView, FMX.ExtCtrls, FMX.Ani, FMX.Gestures, FMX.ListBox,
  FMX.DateTimeCtrls, FMX.Edit, FMX.MultiView.Types, FMX.Colors, FMX.MultiView.CustomPresentation,
  FMX.EditBox, FMX.NumberBox, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    MultiView1: TMultiView;
    ToolBar1: TToolBar;
    MasterButton: TSpeedButton;
    ToolBar2: TToolBar;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    DetailPanel: TPanel;
    Layout2: TLayout;
    Layout4: TLayout;
    Label5: TLabel;
    cbMultiViewMode: TComboBox;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Shadow: TTabItem;
    Layout3: TLayout;
    Label4: TLabel;
    cbSlidingMode: TComboBox;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    Layout5: TLayout;
    Label8: TLabel;
    nbTouchAreaSize: TNumberBox;
    Layout7: TLayout;
    Label11: TLabel;
    cbSlidingDirection: TComboBox;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    Layout6: TLayout;
    Label10: TLabel;
    nbPopupHeight: TNumberBox;
    Layout1: TLayout;
    Label3: TLabel;
    swShadow: TSwitch;
    Layout8: TLayout;
    Label12: TLabel;
    nbDurationSliding: TNumberBox;
    Layout9: TLayout;
    Label13: TLabel;
    Layout10: TLayout;
    Label14: TLabel;
    nbShadowOpacity: TNumberBox;
    cbShadowColor: TComboColorBox;
    Layout11: TLayout;
    Label6: TLabel;
    cbSplitViewPlacement: TComboBox;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    Layout12: TLayout;
    Label7: TLabel;
    nmAppearanceDuration: TNumberBox;
    Layout13: TLayout;
    Label9: TLabel;
    cbPopoverTint: TComboColorBox;
    ListBoxItem16: TListBoxItem;
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure swShadowSwitch(Sender: TObject);
    procedure cbSlidingModeChange(Sender: TObject);
    procedure cbMultiViewModeChange(Sender: TObject);
    procedure nbPopupHeightChange(Sender: TObject);
    procedure nbTouchAreaSizeChange(Sender: TObject);
    procedure cbSlidingDirectionChange(Sender: TObject);
    procedure nbShadowOpacityChangeTracking(Sender: TObject);
    procedure cbShadowColorChange(Sender: TObject);
    procedure cbSplitViewPlacementChange(Sender: TObject);
    procedure nbDurationSlidingChange(Sender: TObject);
    procedure nmAppearanceDurationChange(Sender: TObject);
    procedure cbPopoverTintChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  FMX.MultiView.Presentations;

{$R *.fmx}

procedure TForm1.cbMultiViewModeChange(Sender: TObject);
begin
  if cbMultiViewMode.ItemIndex >= 0 then
    MultiView1.Mode := TMultiViewMode(cbMultiViewMode.ItemIndex);
end;

procedure TForm1.cbPopoverTintChange(Sender: TObject);
begin
  MultiView1.PopoverOptions.TintColor := cbPopoverTint.Color;
end;

procedure TForm1.cbShadowColorChange(Sender: TObject);
begin
  MultiView1.ShadowOptions.Color := cbShadowColor.Color;
end;

procedure TForm1.cbSlidingDirectionChange(Sender: TObject);
begin
  if cbSlidingDirection.ItemIndex >= 0 then
    MultiView1.DrawerOptions.Placement := TPanelPlacement(cbSlidingDirection.ItemIndex);
end;

procedure TForm1.cbSlidingModeChange(Sender: TObject);
begin
  if cbSlidingMode.ItemIndex >= 0 then
    MultiView1.DrawerOptions.Mode := TSlidingMode(cbSlidingMode.ItemIndex);
end;

procedure TForm1.cbSplitViewPlacementChange(Sender: TObject);
begin
  if cbSplitViewPlacement.ItemIndex >= 0 then
    MultiView1.SplitViewOptions.Placement := TPanelPlacement(cbSplitViewPlacement.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MultiView1.CustomPresentationClass := TMultiViewAlertPresentation;
end;

procedure TForm1.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  Item.IsSelected := False;
  MultiView1.HideMaster;
end;

procedure TForm1.nbDurationSlidingChange(Sender: TObject);
begin
  MultiView1.DrawerOptions.DurationSliding := nbDurationSliding.Value;
end;

procedure TForm1.nbPopupHeightChange(Sender: TObject);
begin
  MultiView1.PopoverOptions.PopupHeight := nbPopupHeight.Value;
end;

procedure TForm1.nbShadowOpacityChangeTracking(Sender: TObject);
begin
  MultiView1.ShadowOptions.Opacity := nbShadowOpacity.Value;
end;

procedure TForm1.nbTouchAreaSizeChange(Sender: TObject);
begin
  MultiView1.DrawerOptions.TouchAreaSize := nbTouchAreaSize.Value;
end;

procedure TForm1.nmAppearanceDurationChange(Sender: TObject);
begin
  MultiView1.PopoverOptions.AppearanceDuration := nmAppearanceDuration.Value;
end;

procedure TForm1.swShadowSwitch(Sender: TObject);
begin
  MultiView1.ShadowOptions.Enabled := swShadow.IsChecked;
end;

end.
