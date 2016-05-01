//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Layouts, FMX.Effects,
  FMX.Filter.Effects, FMX.Filter, FMX.Ani, FMX.Graphics,
  FMX.Controls.Presentation, FMX.ListBox;

type
  TFilterClass = class of TFilter;

  TBaseMainForm = class(TForm)
    ButtonTakePhotoFromCamera: TButton;
    ButtonRemovePhoto: TButton;
    ToolBarBottom: TToolBar;
    ActionList: TActionList;
    ActionTakePhotoFromLibrary: TTakePhotoFromLibraryAction;
    ActionTakePhotoFromCamera: TTakePhotoFromCameraAction;
    ButtonSendImage: TButton;
    ActionShowShareSheet: TShowShareSheetAction;
    ButtonTakePhotoFromLibrary: TButton;
    ToolBarTop: TToolBar;
    ImageContainer: TImage;
    ActionClearImage: TAction;
    LayoutFilterSettings: TLayout;
    RemoveBtnAnimation: TFloatAnimation;
    TopHelp: TLayout;
    Image1: TImage;
    Text1: TText;
    Image2: TImage;
    Text2: TText;
    ActionBlurEffect: TAction;
    ActionPixelateEffect: TAction;
    ActionSharpenEffect: TAction;
    ActionResetEffect: TAction;
    ActionWaveEffect: TAction;
    ActionContrastEffect: TAction;
    ActionPaperSketchEffect: TAction;
    FilterComboBox: TComboBox;
    procedure ActionBlurEffectExecute(Sender: TObject);
    procedure ActionPixelateEffectExecute(Sender: TObject);
    procedure ActionSharpenEffectExecute(Sender: TObject);
    procedure ActionClearImageExecute(Sender: TObject);
    procedure ActionTakePhotoFromLibraryDidFinishTaking(Image: TBitmap);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionShowShareSheetBeforeExecute(Sender: TObject);
    procedure ActionResetEffectExecute(Sender: TObject);
    procedure ActionWaveEffectExecute(Sender: TObject);
    procedure ActionPaperSketchEffectExecute(Sender: TObject);
    procedure ActionContrastEffectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FilterComboBoxChange(Sender: TObject);
  protected
    FEffect: TFilter;
    FRawBitmap: TBitmap;
    procedure LoadFilterSettings(Rec: TFilterRec);
    procedure DoOnChangedEffectParam(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetEffect(const AFilterName: string);
    procedure UpdateEffect;
  end;

var
  BaseMainForm: TBaseMainForm;

implementation

{$R *.fmx}

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawBitmap := TBitmap.Create(0, 0);
end;

destructor TBaseMainForm.Destroy;
begin
  FreeAndNil(FRawBitmap);
  inherited Destroy;
end;

procedure TBaseMainForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  LayoutFilterSettings.Visible := not ActionResetEffect.Checked;
  ActionClearImage.Enabled := not ImageContainer.Bitmap.IsEmpty;
  ActionShowShareSheet.Enabled := not FRawBitmap.IsEmpty;
  ActionBlurEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionPixelateEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionSharpenEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionWaveEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionContrastEffect.Enabled := not FRawBitmap.IsEmpty;
  ActionPaperSketchEffect.Enabled := not FRawBitmap.IsEmpty;
  TopHelp.Visible := FRawBitmap.IsEmpty and ActionWaveEffect.Visible;
end;

procedure TBaseMainForm.ActionBlurEffectExecute(Sender: TObject);
begin
  SetEffect('GaussianBlur');
end;

procedure TBaseMainForm.ActionPaperSketchEffectExecute(Sender: TObject);
begin
  SetEffect('PaperSketch');
end;

procedure TBaseMainForm.ActionPixelateEffectExecute(Sender: TObject);
begin
  SetEffect('Pixelate');
end;

procedure TBaseMainForm.ActionResetEffectExecute(Sender: TObject);
begin
  FreeAndNil(FEffect);
  ImageContainer.Bitmap.Assign(FRawBitmap);
  ActionResetEffect.Checked := True;
end;

procedure TBaseMainForm.ActionSharpenEffectExecute(Sender: TObject);
begin
  SetEffect('Sharpen');
end;

procedure TBaseMainForm.ActionShowShareSheetBeforeExecute(Sender: TObject);
begin
  ActionShowShareSheet.Bitmap := ImageContainer.Bitmap;
end;

procedure TBaseMainForm.ActionClearImageExecute(Sender: TObject);
begin
  RemoveBtnAnimation.Start;
  FRawBitmap.SetSize(0, 0);
  ImageContainer.Bitmap.SetSize(0, 0);
  ImageContainer.Bitmap.Assign(FRawBitmap);
  ActionResetEffect.Execute;
end;

procedure TBaseMainForm.ActionContrastEffectExecute(Sender: TObject);
begin
  SetEffect('Contrast');
end;

procedure TBaseMainForm.SetEffect(const AFilterName: string);
var
  Rec: TFilterRec;
begin
  ActionResetEffect.Checked := False;
  FreeAndNil(FEffect);
  FEffect := TFilterManager.FilterByName(AFilterName);
  if Assigned(FEffect) then
  begin
    // Create settings
    Rec := FEffect.FilterAttr;
    UpdateEffect;
    LoadFilterSettings(Rec);
  end;
end;

procedure TBaseMainForm.ActionTakePhotoFromLibraryDidFinishTaking(Image: TBitmap);
var
  ScaleFactor: Single;
begin
  if Image.Width > 1024 then
  begin
    ScaleFactor := Image.Width / 1024;
    Image.Resize(Round(Image.Width / ScaleFactor), Round(Image.Height / ScaleFactor));
  end;
  FRawBitmap.Assign(Image);
  ImageContainer.Bitmap.Assign(Image);
  UpdateEffect;
  FilterComboBox.ItemIndex := 0;
end;

procedure TBaseMainForm.ActionWaveEffectExecute(Sender: TObject);
begin
  SetEffect('Wave');
end;

procedure TBaseMainForm.DoOnChangedEffectParam(Sender: TObject);
var
  TrackBarTmp: TTrackBar;
begin
  if not (Sender is TTrackBar) then
    Exit;

  TrackBarTmp := Sender as TTrackBar;
  FEffect.ValuesAsFloat[TrackBarTmp.TagString] := TrackBarTmp.Value;
  UpdateEffect;
end;

procedure TBaseMainForm.FilterComboBoxChange(Sender: TObject);
begin
  if FilterComboBox.Selected.Text = 'None' then
    ActionResetEffect.Execute
  else if FilterComboBox.Selected.Text = 'Blur' then
    ActionBlurEffect.Execute
  else if FilterComboBox.Selected.Text = 'Paper' then
    ActionPaperSketchEffect.Execute
  else if FilterComboBox.Selected.Text = 'Pixelate' then
    ActionPixelateEffect.Execute
  else if FilterComboBox.Selected.Text = 'Wave' then
    ActionWaveEffect.Execute
  else if FilterComboBox.Selected.Text = 'Contrast' then
    ActionContrastEffect.Execute
  else if FilterComboBox.Selected.Text = 'Sharpen' then
    ActionSharpenEffect.Execute;
 end;

procedure TBaseMainForm.FormCreate(Sender: TObject);
begin
  FilterComboBox.Items.Add('None');
  if TFilterManager.FilterByName('GaussianBlur') <> nil then
    FilterComboBox.Items.Add('Blur');
  if TFilterManager.FilterByName('Pixelate') <> nil then
    FilterComboBox.Items.Add('Pixelate');
  if TFilterManager.FilterByName('Wave') <> nil then
    FilterComboBox.Items.Add('Wave');
  if TFilterManager.FilterByName('Contrast') <> nil then
    FilterComboBox.Items.Add('Contrast');
  if TFilterManager.FilterByName('PaperSketch') <> nil then
    FilterComboBox.Items.Add('Paper');
  if TFilterManager.FilterByName('Sharpen') <> nil then
    FilterComboBox.Items.Add('Sharpen');
end;

procedure TBaseMainForm.UpdateEffect;
begin
  if Assigned(FEffect) then
  begin
    FEffect.ValuesAsBitmap['Input'] := FRawBitmap;
    ImageContainer.Bitmap := FEffect.ValuesAsBitmap['Output'];
  end;
end;

procedure TBaseMainForm.LoadFilterSettings(Rec: TFilterRec);
var
  TB: TTrackBar;
  RecValue: TFilterValueRec;
begin
  LayoutFilterSettings.DeleteChildren;
  for RecValue in Rec.Values do
  begin
    if RecValue.ValueType <> TFilterValueType.Float then
      Continue;
    TB := TTrackBar.Create(Self);
    TB.Parent := LayoutFilterSettings;
    TB.Orientation := TOrientation.Vertical;
    TB.Align := TAlignLayout.Left;
    TB.Min := RecValue.Min.AsExtended;
    TB.Max := RecValue.Max.AsExtended;
    TB.Value := RecValue.Value.AsExtended;
    TB.TagString := RecValue.Name;
    TB.Tracking := False;
    TB.OnChange := DoOnChangedEffectParam;
  end;
end;

end.
